{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Rzk.VSCode.Handlers (
  typecheckFromConfigFile,
  provideCompletions,
  formatDocument,
  provideSemanticTokens,
  handleFilesChanged,
) where

import           Control.Exception             (SomeException, evaluate, try)
import           Control.Lens
import           Control.Monad                 (forM_, when)
import           Control.Monad.Except          (ExceptT (ExceptT),
                                                MonadError (throwError),
                                                modifyError, runExceptT)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Default.Class
import           Data.List                     (isSuffixOf, sort, (\\))
import           Data.Maybe                    (fromMaybe, isNothing)
import qualified Data.Text                     as T
import qualified Data.Yaml                     as Yaml
import           Language.LSP.Diagnostics      (partitionBySource)
import           Language.LSP.Protocol.Lens    (HasDetail (detail),
                                                HasDocumentation (documentation),
                                                HasLabel (label),
                                                HasParams (params),
                                                HasTextDocument (textDocument),
                                                HasUri (uri), changes, uri)
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server
import           Language.LSP.VFS              (virtualFileText)
import           System.FilePath               (makeRelative, (</>))
import           System.FilePath.Glob          (compile, globDir)

import           Language.Rzk.Free.Syntax      (RzkPosition (RzkPosition),
                                                VarIdent (getVarIdent))
import           Language.Rzk.Syntax           (Module, VarIdent' (VarIdent),
                                                parseModuleFile,
                                                parseModuleSafe, printTree)
import           Language.Rzk.VSCode.Config    (ServerConfig (ServerConfig, formatEnabled))
import           Language.Rzk.VSCode.Env
import           Language.Rzk.VSCode.Logging
import           Language.Rzk.VSCode.Tokenize  (tokenizeModule)
import           Rzk.Format                    (FormattingEdit (..),
                                                formatTextEdits)
import           Rzk.Project.Config            (ProjectConfig (include))
import           Rzk.TypeCheck

-- | Given a list of file paths, reads them and parses them as Rzk modules,
--   returning the same list of file paths but with the parsed module (or parse error)
parseFiles :: [FilePath] -> IO [(FilePath, Either String Module)]
parseFiles [] = pure []
parseFiles (x:xs) = do
  errOrMod <- parseModuleFile x
  rest <- parseFiles xs
  return $ (x, errOrMod) : rest

-- | Given the list of possible modules returned by `parseFiles`, this segregates the errors
--   from the successfully parsed modules and returns them in separate lists so the errors
--   can be reported and the modules can be typechecked.
collectErrors :: [(FilePath, Either String Module)] -> ([(FilePath, String)], [(FilePath, Module)])
collectErrors [] = ([], [])
collectErrors ((path, result) : paths) =
  case result of
    Left err      -> ((path, err) : errors, [])
    Right module_ -> (errors, (path, module_) : modules)
  where
    (errors, modules) = collectErrors paths

-- | The maximum number of diagnostic messages to send to the client
maxDiagnosticCount :: Int
maxDiagnosticCount = 100

filePathToNormalizedUri :: FilePath -> NormalizedUri
filePathToNormalizedUri = toNormalizedUri . filePathToUri


typecheckFromConfigFile :: LSP ()
typecheckFromConfigFile = do
  logInfo "Looking for rzk.yaml"
  root <- getRootPath
  case root of
    Nothing -> do
      logWarning "Workspace has no root path, cannot find rzk.yaml"
      sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Warning "Cannot find the workspace root")
    Just rootPath -> do
      let rzkYamlPath = rootPath </> "rzk.yaml"
      eitherConfig <- liftIO $ Yaml.decodeFileEither @ProjectConfig rzkYamlPath
      case eitherConfig of
        Left err -> do
          logError ("Invalid or missing rzk.yaml: " ++ Yaml.prettyPrintParseException err)

        Right config -> do
          logDebug "Starting typechecking"
          rawPaths <- liftIO $ globDir (map compile (include config)) rootPath
          let paths = concatMap sort rawPaths

          cachedModules <- getCachedTypecheckedModules
          let cachedPaths = map fst cachedModules
              modifiedFiles = paths \\ cachedPaths

          logDebug ("Found " ++ show (length cachedPaths) ++ " files in the cache")
          logDebug (show (length modifiedFiles) ++ " files have been modified")

          (parseErrors, parsedModules) <- liftIO $ collectErrors <$> parseFiles modifiedFiles
          tcResults <- liftIO $ try $ evaluate $
            defaultTypeCheck (typecheckModulesWithLocationIncremental cachedModules parsedModules)

          (typeErrors, _checkedModules) <- case tcResults of
            Left (ex :: SomeException) -> do
              -- Just a warning to be logged in the "Output" panel and not shown to the user as an error message
              --  because exceptions are expected when the file has invalid syntax
              logWarning ("Encountered an exception while typechecking:\n" ++ show ex)
              return ([], [])
            Right (Left err) -> do
              logError ("An impossible error happened! Please report a bug:\n" ++ ppTypeErrorInScopedContext' BottomUp err)
              return ([err], [])    -- sort of impossible
            Right (Right (checkedModules, errors)) -> do
                -- cache well-typed modules
                logInfo (show (length checkedModules) ++ " modules successfully typechecked")
                logInfo (show (length errors) ++ " errors found")
                cacheTypecheckedModules checkedModules
                return (errors, checkedModules)

          -- Reset all published diags
          -- TODO: remove this after properly grouping by path below, after which there can be an empty list of errors
          -- TODO: handle clearing diagnostics for files that got removed from the project (rzk.yaml)
          forM_ modifiedFiles $ \path -> do
            publishDiagnostics 0 (filePathToNormalizedUri path) Nothing (partitionBySource [])

          -- Report parse errors to the client
          forM_ parseErrors $ \(path, err) -> do
            publishDiagnostics maxDiagnosticCount (filePathToNormalizedUri path) Nothing (partitionBySource [diagnosticOfParseError err])

          -- TODO: collect all errors for one file in one list

          -- Report typechecking errors to the client
          forM_ typeErrors $ \err -> do
            let errPath = filepathOfTypeError err
                errDiagnostic = diagnosticOfTypeError err
            publishDiagnostics maxDiagnosticCount (filePathToNormalizedUri errPath) Nothing (partitionBySource [errDiagnostic])
  where
    filepathOfTypeError :: TypeErrorInScopedContext var -> FilePath
    filepathOfTypeError (PlainTypeError err) =
      case location (typeErrorContext err) >>= locationFilePath of
        Just path -> path
        _         -> error "the impossible happened! Please contact Abdelrahman immediately!!!"
    filepathOfTypeError (ScopedTypeError _orig err) = filepathOfTypeError err

    diagnosticOfTypeError :: TypeErrorInScopedContext VarIdent -> Diagnostic
    diagnosticOfTypeError err = Diagnostic
                      (Range (Position line 0) (Position line 99)) -- 99 to reach end of line and be visible until we actually have information about it
                      (Just DiagnosticSeverity_Error)
                      (Just $ InR "type-error") -- diagnostic code
                      Nothing                   -- diagonstic description
                      (Just "rzk")              -- A human-readable string describing the source of this diagnostic
                      (T.pack msg)
                      Nothing                   -- tags
                      (Just [])                 -- related information
                      Nothing                   -- data that is preserved between different calls
      where
        msg = ppTypeErrorInScopedContext' TopDown err

        extractLineNumber :: TypeErrorInScopedContext var -> Maybe Int
        extractLineNumber (PlainTypeError e)    = do
          loc <- location (typeErrorContext e)
          lineNo <- locationLine loc
          return (lineNo - 1) -- VS Code indexes lines from 0, but locationLine starts with 1
        extractLineNumber (ScopedTypeError _ e) = extractLineNumber e

        line = fromIntegral $ fromMaybe 0 $ extractLineNumber err

    diagnosticOfParseError :: String -> Diagnostic
    diagnosticOfParseError err = Diagnostic (Range (Position 0 0) (Position 0 0))
                      (Just DiagnosticSeverity_Error)
                      (Just $ InR "parse-error")
                      Nothing
                      (Just "rzk")
                      (T.pack err)
                      Nothing
                      (Just [])
                      Nothing

instance Default T.Text where def = ""
instance Default CompletionItem
instance Default CompletionItemLabelDetails

provideCompletions :: Handler LSP 'Method_TextDocumentCompletion
provideCompletions req res = do
  logInfo "Providing text completions"
  root <- getRootPath
  when (isNothing root) $ logDebug "Not in a workspace. Cannot find root path for relative paths"
  let rootDir = fromMaybe "/" root
  cachedModules <- getCachedTypecheckedModules
  logDebug ("Found " ++ show (length cachedModules) ++ " modules in the cache")
  let currentFile = fromMaybe "" $ uriToFilePath $ req ^. params . textDocument . uri
  -- Take all the modules up to and including the currently open one
  let modules = takeWhileInc ((/= currentFile) . fst) cachedModules
        where
          takeWhileInc _ [] = []
          takeWhileInc p (x:xs)
            | p x       = x : takeWhileInc p xs
            | otherwise = [x]

  let items = concatMap (declsToItems rootDir) modules
  logDebug ("Sending " ++ show (length items) ++ " completion items")
  res $ Right $ InL items
  where
    declsToItems :: FilePath -> (FilePath, [Decl']) -> [CompletionItem]
    declsToItems root (path, decls) = map (declToItem root path) decls
    declToItem :: FilePath -> FilePath -> Decl' -> CompletionItem
    declToItem rootDir path (Decl name type' _ _ _) = def
      & label .~ T.pack (printTree $ getVarIdent name)
      & detail ?~ T.pack (show type')
      & documentation ?~ InR (MarkupContent MarkupKind_Markdown $ T.pack $
          "---\nDefined" ++
          (if line > 0 then " at line " ++ show line else "")
          ++ " in *" ++ makeRelative rootDir path ++ "*")
      where
        (VarIdent pos _) = getVarIdent name
        (RzkPosition _path pos') = pos
        line = maybe 0 fst pos'
        _col = maybe 0 snd pos'

formattingEditToTextEdit :: FormattingEdit -> TextEdit
formattingEditToTextEdit (FormattingEdit startLine startCol endLine endCol newText) =
  TextEdit
    (Range
      (Position (fromIntegral startLine - 1) (fromIntegral startCol - 1))
      (Position (fromIntegral endLine - 1) (fromIntegral endCol - 1))
    )
    (T.pack newText)

formatDocument :: Handler LSP 'Method_TextDocumentFormatting
formatDocument req res = do
  let doc = req ^. params . textDocument . uri . to toNormalizedUri
  logInfo $ "Formatting document: " <> show doc
  ServerConfig {formatEnabled = fmtEnabled} <- getConfig
  if fmtEnabled then do
    mdoc <- getVirtualFile doc
    possibleEdits <- case virtualFileText <$> mdoc of
      Nothing         -> return (Left "Failed to get file contents")
      Just sourceCode -> do
        let edits = formatTextEdits (filter (/= '\r') $ T.unpack sourceCode)
        return (Right $ map formattingEditToTextEdit edits)
    case possibleEdits of
      Left err    -> res $ Left $ ResponseError (InR ErrorCodes_InternalError) err Nothing
      Right edits -> do
        res $ Right $ InL edits
  else do
    logDebug "Formatting is disabled in config"
    res $ Right $ InR Null

provideSemanticTokens :: Handler LSP 'Method_TextDocumentSemanticTokensFull
provideSemanticTokens req responder = do
  let doc = req ^. params . textDocument . uri . to toNormalizedUri
  mdoc <- getVirtualFile doc
  possibleTokens <- case virtualFileText <$> mdoc of
    Nothing         -> return (Left "Failed to get file content")
    Just sourceCode -> fmap (fmap tokenizeModule) $ liftIO $
      parseModuleSafe (filter (/= '\r') $ T.unpack sourceCode)
  case possibleTokens of
    Left err -> do
      -- Exception occurred when parsing the module
      logWarning ("Failed to tokenize file: " ++ err)
    Right tokens -> do
      let encoded = encodeTokens defaultSemanticTokensLegend $ relativizeTokens tokens
      case encoded of
        Left _err -> do
          -- Failed to encode the tokens
          return ()
        Right list ->
          responder (Right (InL (SemanticTokens Nothing list)))


data IsChanged
  = HasChanged
  | NotChanged

-- | Detects if the given path has changes in its declaration compared to what's in the cache
isChanged :: RzkTypecheckCache -> FilePath -> LSP IsChanged
isChanged cache path = toIsChanged $ do
  cachedDecls <- maybeToEitherLSP $ lookup path cache
  module' <- toExceptTLifted $ parseModuleFile path
  e <- toExceptTLifted $ try @SomeException $ evaluate $
    defaultTypeCheck (typecheckModulesWithLocationIncremental (takeWhile ((/= path) . fst) cache) [(path, module')])
  (checkedModules, _errors) <- toExceptT $ return e
  decls' <- maybeToEitherLSP $ lookup path checkedModules
  return $ if decls' == cachedDecls
    then NotChanged
    else HasChanged
  where
    toExceptT = modifyError (const ()) . ExceptT
    toExceptTLifted = toExceptT . liftIO
    maybeToEitherLSP = \case
      Nothing -> throwError ()
      Just x -> return x
    toIsChanged m = runExceptT m >>= \case
      Left _ -> return HasChanged -- in case of error consider the file has changed
      Right x -> return x

hasNotChanged :: RzkTypecheckCache -> FilePath -> LSP Bool
hasNotChanged cache path = isChanged cache path >>= \case
  HasChanged -> return False
  NotChanged -> return True

-- | Monadic 'dropWhile'
dropWhileM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
dropWhileM _ []     = return []
dropWhileM p (x:xs) = do
  q <- p x
  if q
    then dropWhileM p xs
    else return (x:xs)

handleFilesChanged :: Handler LSP 'Method_WorkspaceDidChangeWatchedFiles
handleFilesChanged msg = do
  let modifiedPaths = msg ^.. params . changes . traverse . uri . to uriToFilePath . _Just
  if any ("rzk.yaml" `isSuffixOf`) modifiedPaths
    then do
      logDebug "rzk.yaml modified. Clearing module cache"
      resetCacheForAllFiles
    else do
      cache <- getCachedTypecheckedModules
      actualModified <- dropWhileM (hasNotChanged cache) modifiedPaths
      resetCacheForFiles actualModified
  typecheckFromConfigFile
