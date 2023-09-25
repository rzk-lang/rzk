{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Rzk.VSCode.Handlers where

import           Control.Exception             (SomeException, evaluate, try)
import           Control.Lens
import           Control.Monad.Cont            (MonadIO (liftIO), forM_)
import           Data.Default.Class
import           Data.List                     (sort, (\\))
import           Data.Maybe                    (fromMaybe)
import qualified Data.Text                     as T
import qualified Data.Yaml                     as Yaml
import           Language.LSP.Diagnostics      (partitionBySource)
import           Language.LSP.Protocol.Lens    (HasDetail (detail),
                                                HasDocumentation (documentation),
                                                HasLabel (label),
                                                HasParams (params),
                                                HasTextDocument (textDocument),
                                                HasUri (uri))
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server
import           System.FilePath               (makeRelative, (</>))
import           System.FilePath.Glob          (compile, globDir)

import           Language.Rzk.Free.Syntax      (RzkPosition (RzkPosition),
                                                VarIdent (getVarIdent))
import           Language.Rzk.Syntax           (Module, VarIdent' (VarIdent),
                                                parseModuleFile, printTree)
import           Language.Rzk.VSCode.Env
import           Language.Rzk.VSCode.State     (ProjectConfig (include))
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
    Left err      -> ((path, err) : errors, modules)
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
  root <- getRootPath
  case root of
    Nothing -> do
      sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Warning "Cannot find the workspace root")
    Just rootPath -> do
      let rzkYamlPath = rootPath </> "rzk.yaml"
      eitherConfig <- liftIO $ Yaml.decodeFileEither @ProjectConfig rzkYamlPath
      case eitherConfig of
        Left err -> do
          sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Warning (T.pack $ "Invalid or missing rzk.yaml: " ++ Yaml.prettyPrintParseException err))

        Right config -> do
          rawPaths <- liftIO $ globDir (map compile (include config)) rootPath
          let paths = concatMap sort rawPaths

          cachedModules <- getCachedTypecheckedModules
          let cachedPaths = map fst cachedModules
              modifiedFiles = paths \\ cachedPaths

          (parseErrors, parsedModules) <- liftIO $ collectErrors <$> parseFiles modifiedFiles
          tcResults <- liftIO $ try $ evaluate $
            defaultTypeCheck (typecheckModulesWithLocationIncremental cachedModules parsedModules)

          (typeErrors, _checkedModules) <- case tcResults of
            Left (_ex :: SomeException) -> return ([], [])   -- FIXME: publish diagnostics about an exception during typechecking!
            Right (Left err) -> return ([err], [])    -- sort of impossible
            Right (Right (checkedModules, errors)) -> do
                -- cache well-typed modules
                cacheTypecheckedModules checkedModules
                return (errors, checkedModules)

          -- Reset all published diags
          -- TODO: remove this after properly grouping by path below, after which there can be an empty list of errors
          forM_ paths $ \path -> do
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
        msg = ppTypeErrorInScopedContext' err

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
  root <- getRootPath
  let rootDir = fromMaybe "/" root
  cachedModules <- getCachedTypecheckedModules
  let currentFile = fromMaybe "" $ uriToFilePath $ req ^. params . textDocument . uri
  -- Take all the modules up to and including the currently open one
  let modules = takeWhileInc ((/= currentFile) . fst) cachedModules
        where
          takeWhileInc _ [] = []
          takeWhileInc p (x:xs)
            | p x       = x : takeWhileInc p xs
            | otherwise = [x]

  let items = concatMap (declsToItems rootDir) modules
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
