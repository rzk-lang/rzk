{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE RecordWildCards     #-}

module Language.Rzk.VSCode.Handlers (
  typecheckFromConfigFile,
  provideCompletions,
  provideSymbols,
  provideWorkspaceSymbols,
  findDefinition,
  findReferences,
  provideHover,
  formatSignature,
  formatDocument,
  provideSemanticTokens,
  handleFilesChanged,
) where

import           Control.Applicative           ((<|>))
import           Control.Exception             (SomeException, evaluate, try)
import           Control.Lens
import           Control.Monad                 (forM, forM_, when)
import           Control.Monad.Except          (ExceptT (ExceptT),
                                                MonadError (throwError),
                                                modifyError, runExceptT)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Default.Class
import           Data.List                     (find, intercalate, isSuffixOf,
                                                nub, sort, (\\))
import qualified Data.Map.Strict               as Map
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                    (fromMaybe, isNothing)
import qualified Data.Text                     as T
import qualified Data.Yaml                     as Yaml
import           Language.LSP.Diagnostics      (partitionBySource)
import           Language.LSP.Protocol.Lens    (HasContext (context),
                                                HasDetail (detail),
                                                HasDocumentation (documentation),
                                                HasLabel (label),
                                                HasParams (params),
                                                HasPosition (position),
                                                HasQuery (query),
                                                HasTextDocument (textDocument),
                                                HasUri (uri), changes, uri)
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Types as LSP
import           Language.LSP.Server
import           Language.LSP.VFS              (virtualFileText)
import           System.FilePath               (makeRelative, (</>))
import           System.FilePath.Glob          (compile, globDir)

import           Data.Char                     (isDigit)
import           Language.Rzk.Free.Syntax      (RzkPosition (RzkPosition),
                                                VarIdent (getVarIdent),
                                                fromTerm')
import           Language.Rzk.Syntax           (Module, Term,
                                                Term' (ASCII_TypeFun, TypeFun),
                                                VarIdent' (VarIdent),
                                                parseModuleFile,
                                                parseModuleSafe, printTree)
import qualified Language.Rzk.VSCode.Config    as RzkConfig
import           Language.Rzk.VSCode.Env
import qualified Language.Rzk.VSCode.ReferenceIndex as RefInd
import           Language.Rzk.VSCode.Logging
import           Language.Rzk.VSCode.Tokenize  (mergeTokens, tokenizeModule,
                                                tokenizeSyntaxSymbols)
import           Free.Scoped                   (untyped)
import qualified Rzk.Diagnostic                as Diag
import           Rzk.Format                    (format)
import           Rzk.Project.Config            (ProjectConfig (include))
import           Rzk.TypeCheck
import           Text.Read                     (readMaybe)

-- | Given a list of file paths, reads them and parses them as Rzk modules,
--   returning the same list of file paths but with the parsed module (or parse error)
parseFiles :: [FilePath] -> IO [(FilePath, Either T.Text Module)]
parseFiles [] = pure []
parseFiles (x:xs) = do
  errOrMod <- parseModuleFile x
  rest <- parseFiles xs
  return $ (x, errOrMod) : rest

-- | Given the list of possible modules returned by `parseFiles`, this segregates the errors
--   from the successfully parsed modules and returns them in separate lists so the errors
--   can be reported and the modules can be typechecked.
collectErrors :: [(FilePath, Either T.Text Module)] -> ([(FilePath, T.Text)], [(FilePath, Module)])
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

tshow :: Show a => a -> T.Text
tshow = T.pack . show

fromLspUri :: LSP.Uri -> RefInd.Uri
fromLspUri u = RefInd.Uri { uriPath = fromMaybe "" (uriToFilePath u) }

toLspUri :: RefInd.Uri -> LSP.Uri
toLspUri (RefInd.Uri { uriPath = p }) = filePathToUri p

fromLspPosition :: LSP.Position -> RefInd.Position
fromLspPosition (LSP.Position l c) =
  RefInd.Position (fromIntegral l) (fromIntegral c)

toLspPosition :: RefInd.Position -> LSP.Position
toLspPosition (RefInd.Position l c) =
  LSP.Position (fromIntegral l) (fromIntegral c)

toLspRange :: RefInd.Range -> Range
toLspRange (RefInd.Range s e) = Range (toLspPosition s) (toLspPosition e)

toLspLocation :: RefInd.Location -> LSP.Location
toLspLocation (RefInd.Location u r) =
  LSP.Location (toLspUri u) (toLspRange r)

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
          logError ("Invalid or missing rzk.yaml: " <> T.pack (Yaml.prettyPrintParseException err))

        Right config -> do
          logDebug "Starting typechecking"
          rawPaths <- liftIO $ globDir (map compile (include config)) rootPath
          let paths = concatMap sort rawPaths

          typecheckedCachedModules <- getCachedTypecheckedModules
          let cachedModules = map (\(path, RzkCachedModule{..}) -> (path, cachedModuleDecls)) typecheckedCachedModules
          let cachedPaths = map fst cachedModules
              modifiedFiles = paths \\ cachedPaths

          logDebug ("Found " <> tshow (length cachedPaths) <> " files in the cache")
          logDebug (tshow (length modifiedFiles) <> " files have been modified")

          (parseErrors, parsedModules) <- liftIO $ collectErrors <$> parseFiles modifiedFiles
          -- Run in lenient hole mode so holes are collected (and surfaced as
          -- hints) rather than reported as errors while editing.
          tcResults <- liftIO $ try $ evaluate $
            defaultTypeCheckWithHoles (typecheckModulesWithLocationIncremental cachedModules parsedModules)

          (typeErrors, holeInfos) <- case tcResults of
            Left (ex :: SomeException) -> do
              -- Just a warning to be logged in the "Output" panel and not shown to the user as an error message
              --  because exceptions are expected when the file has invalid syntax
              logWarning ("Encountered an exception while typechecking:\n" <> tshow ex)
              return ([], [])
            Right (Left err) -> do
              logError ("An impossible error happened! Please report a bug:\n" <> T.pack (ppTypeErrorInScopedContext' BottomUp err))
              return ([err], [])    -- sort of impossible
            Right (Right ((checkedModules, errors), foundHoles)) -> do
                -- cache well-typed modules
                logInfo (tshow (length checkedModules) <> " modules successfully typechecked")
                logInfo (tshow (length errors) <> " errors found")
                logInfo (tshow (length foundHoles) <> " holes found")
                let checkedModules' = map (\(path, decls) -> (path, RzkCachedModule decls (filter ((== path) . filepathOfTypeError) errors))) checkedModules
                cacheTypecheckedModules checkedModules'
                return (errors, foundHoles)

          -- Reset all published diags
          -- TODO: remove this after properly grouping by path below, after which there can be an empty list of errors
          -- TODO: handle clearing diagnostics for files that got removed from the project (rzk.yaml)
          forM_ modifiedFiles $ \path -> do
            publishDiagnostics 0 (filePathToNormalizedUri path) Nothing (partitionBySource [])

          -- Report parse errors to the client
          forM_ parseErrors $ \(path, err) -> do
            publishDiagnostics maxDiagnosticCount (filePathToNormalizedUri path) Nothing (partitionBySource [diagnosticOfParseError err])

          -- Report typechecking errors and holes to the client, grouped by file
          -- so all diagnostics for a file are published in a single call
          -- (publishDiagnostics replaces a source's diagnostics per URI, so
          -- publishing them one at a time would clobber all but the last).
          let errDiagnostics  = [ (filepathOfTypeError err, diagnosticOfTypeError err)
                                | err <- typeErrors ]
              holeDiagnostics = [ (path, diagnosticOfHole hole)
                                | hole <- holeInfos
                                , Just path <- [holeLocation hole >>= locationFilePath] ]
              -- group by file path (NE.groupAllWith sorts then groups, and
              -- yields NonEmpty groups so taking the key is total)
              diagnosticsByFile =
                map (\grp -> (fst (NE.head grp), map snd (NE.toList grp))) $
                NE.groupAllWith fst (errDiagnostics <> holeDiagnostics)
          forM_ diagnosticsByFile $ \(path, diags) ->
            publishDiagnostics maxDiagnosticCount (filePathToNormalizedUri path) Nothing (partitionBySource diags)
  where
    filepathOfTypeError :: TypeErrorInScopedContext var -> FilePath
    filepathOfTypeError (PlainTypeError err) =
      case location (typeErrorContext err) >>= locationFilePath of
        Just path -> path
        _         -> error "the impossible happened! Please contact Abdelrahman immediately!!!"
    filepathOfTypeError (ScopedTypeError _orig err) = filepathOfTypeError err

    -- Map a structured library diagnostic to an LSP diagnostic. The range is
    -- line-level (whole line), reflecting the granularity rzk currently retains.
    lspDiagnosticOf :: Diag.Diagnostic -> Diagnostic
    lspDiagnosticOf d = Diagnostic
                      (Range (Position line 0) (Position line 99)) -- 99 to reach end of line and be visible until we actually have column information
                      (Just (lspSeverity (Diag.diagnosticSeverity d)))
                      (Just (InR (T.pack (Diag.diagnosticCode d))))
                      Nothing                   -- diagnostic description
                      (Just "rzk")              -- A human-readable string describing the source of this diagnostic
                      (T.pack (Diag.diagnosticMessage d))
                      Nothing                   -- tags
                      (Just [])                 -- related information
                      Nothing                   -- data that is preserved between different calls
      where
        line = fromIntegral $ fromMaybe 0 $ do
          loc <- Diag.diagnosticLocation d
          lineNo <- locationLine loc
          return (lineNo - 1) -- VS Code indexes lines from 0, but locationLine starts with 1

    lspSeverity :: Diag.Severity -> DiagnosticSeverity
    lspSeverity = \case
      Diag.SeverityError       -> DiagnosticSeverity_Error
      Diag.SeverityWarning     -> DiagnosticSeverity_Warning
      Diag.SeverityInformation -> DiagnosticSeverity_Information
      Diag.SeverityHint        -> DiagnosticSeverity_Hint

    diagnosticOfTypeError :: TypeErrorInScopedContext VarIdent -> Diagnostic
    diagnosticOfTypeError = lspDiagnosticOf . Diag.diagnoseTypeError TopDown

    diagnosticOfHole :: HoleInfo -> Diagnostic
    diagnosticOfHole = lspDiagnosticOf . Diag.diagnoseHole

    diagnosticOfParseError :: T.Text -> Diagnostic
    diagnosticOfParseError err = Diagnostic (Range (Position errLine errColumnStart) (Position errLine errColumnEnd))
                      (Just DiagnosticSeverity_Error)
                      (Just $ InR "parse-error")
                      Nothing
                      (Just "rzk")
                      err
                      Nothing
                      (Just [])
                      Nothing
      where
        errStr = T.unpack err
        (errLine, errColumnStart, errColumnEnd) = fromMaybe (0, 0, 0) $
          case words errStr of
            -- Happy parse error
            (take 9 -> ["syntax", "error", "at", "line", lineStr, "column", columnStr, "before", token]) -> do
              line <- readMaybe (takeWhile isDigit lineStr)
              columnStart <- readMaybe (takeWhile isDigit columnStr)
              return (line - 1, columnStart - 1, columnStart + fromIntegral (length token) - 3)
            -- Happy parse error due to lexer error
            (take 7 -> ["syntax", "error", "at", "line", lineStr, "column", columnStr]) -> do
              line <- readMaybe (takeWhile isDigit lineStr)
              columnStart <- readMaybe (takeWhile isDigit columnStr)
              return (line - 1, columnStart - 1, columnStart - 1)
            -- BNFC layout resolver error
            (take 14 -> ["Layout", "error", "at", "line", _lineStr, "column", _columnStr, "found", token, "at", "line", lineStr', "column", columnStr']) -> do
              -- line <- readMaybe (takeWhile isDigit lineStr)
              -- columnStart <- readMaybe (takeWhile isDigit columnStr)
              line' <- readMaybe (takeWhile isDigit lineStr')
              columnStart' <- readMaybe (takeWhile isDigit columnStr')
              return (line' - 1, columnStart', columnStart' + fromIntegral (length token) - 2)
            _ -> Nothing

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
  logDebug ("Found " <> tshow (length cachedModules) <> " modules in the cache")
  let currentFile = fromMaybe "" $ uriToFilePath $ req ^. params . textDocument . uri
  -- Take all the modules up to and including the currently open one
  let modules = map ignoreErrors $ takeWhileInc ((/= currentFile) . fst) cachedModules
        where
          ignoreErrors (path, RzkCachedModule{..}) = (path, cachedModuleDecls)
          takeWhileInc _ [] = []
          takeWhileInc p (x:xs)
            | p x       = x : takeWhileInc p xs
            | otherwise = [x]

  let items = concatMap (declsToItems rootDir) modules
  logDebug ("Sending " <> T.pack (show (length items)) <> " completion items")
  res $ Right $ InL items
  where
    declsToItems :: FilePath -> (FilePath, [Decl']) -> [CompletionItem]
    declsToItems root (path, decls) = map (declToItem root path) decls
    declToItem :: FilePath -> FilePath -> Decl' -> CompletionItem
    declToItem rootDir path (Decl name type' _ _ _ _loc) = def

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

-- | Full-document range for LSP (0-based line and character).
--   End position is exclusive. Computed from the actual text so that every
--   character (including trailing newlines) is included; using T.lines would
--   drop trailing newlines and leave them in place after the edit (extra blank line).
fullDocumentRange :: T.Text -> Range
fullDocumentRange source
  | T.null source = Range (Position 0 0) (Position 0 0)
  | otherwise =
      let newlineCount = T.count (T.singleton '\n') source
          endLine = newlineCount
          -- Length of last line (after last newline); if no newline, whole text is one line
          endCharacter
            | T.last source == '\n' = 0
            | Just i <- T.findIndex (== '\n') (T.reverse source) = fromIntegral i
            | otherwise = fromIntegral (T.length source)
      in Range (Position 0 0) (Position (fromIntegral endLine) endCharacter)

formatDocument :: Handler LSP 'Method_TextDocumentFormatting
formatDocument req res = do
  let doc = req ^. params . textDocument . uri . to toNormalizedUri
  logInfo $ "Formatting document: " <> T.pack (show doc)
  RzkConfig.ServerConfig {RzkConfig.formatEnabled = fmtEnabled} <- getConfig
  if fmtEnabled then do
    mdoc <- getVirtualFile doc
    possibleEdits <- case virtualFileText <$> mdoc of
      Nothing         -> return (Left "Failed to get file contents")
      Just sourceCode -> do
        let source = T.filter (/= '\r') sourceCode
            formatted = format source
            -- Preserve trailing newlines of the source so formatting is idempotent.
            formatted'
              | T.null source = formatted
              | otherwise =
                  let inputTrailing = T.length (T.takeWhileEnd (== '\n') source)
                      outTrailing = T.length (T.takeWhileEnd (== '\n') formatted)
                  in if outTrailing > inputTrailing
                     then T.dropEnd (outTrailing - inputTrailing) formatted
                     else if outTrailing < inputTrailing
                          then formatted <> T.replicate (inputTrailing - outTrailing) (T.singleton '\n')
                          else formatted
            -- Never send trailing newlines: some clients add one when applying a
            -- full-document edit, so we send content ending with no newline to avoid
            -- an extra blank line on each format.
            formatted'' = T.dropWhileEnd (== '\n') formatted'
            range = fullDocumentRange source
        return (Right [TextEdit range formatted''])
    case possibleEdits of
#if MIN_VERSION_lsp(2,7,0)
      Left err    -> res $ Left $ TResponseError (InR ErrorCodes_InternalError) err Nothing
#else
      Left err    -> res $ Left $ ResponseError (InR ErrorCodes_InternalError) err Nothing
#endif
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
    Just sourceCode -> do
      let src = T.filter (/= '\r') sourceCode
      -- Fixed symbols (commands, keywords, operators) are highlighted from
      -- the lexer token stream, so they survive parse failures; identifiers
      -- need the parsed module.
      astTokens <- liftIO (parseModuleSafe src) >>= \case
        Left err -> do
          logWarning ("Failed to parse file for semantic tokens: " <> err)
          return []
        Right rzkModule -> return (tokenizeModule rzkModule)
      return (Right (mergeTokens astTokens (tokenizeSyntaxSymbols src)))
  case possibleTokens of
    Left err -> do
      -- Exception occurred when parsing the module
      logWarning ("Failed to tokenize file: " <> err)
    Right tokens -> do
      let encoded = encodeTokens defaultSemanticTokensLegend $ relativizeTokens tokens
      case encoded of
        Left _err -> do
          -- Failed to encode the tokens
          return ()
        Right list ->
          responder (Right (InL (SemanticTokens Nothing list)))

findDefinition :: Handler LSP 'Method_TextDocumentDefinition
findDefinition req res = do
  let uri' = req ^. params . textDocument . uri
      currentFile = fromMaybe "" (uriToFilePath uri')
  referenceIndex <- indexProject currentFile
  case RefInd.lookupAt referenceIndex (fromLspUri uri') (fromLspPosition (req ^. params . position)) of
    Just binding -> res $ Right $ InL $ Definition $ InL (toLspLocation (RefInd.bindingDef binding))
    Nothing      -> res $ Right $ InR $ InR Null

findReferences :: Handler LSP 'Method_TextDocumentReferences
findReferences req res = do
  let uri' = req ^. params . textDocument . uri
      currentFile = fromMaybe "" (uriToFilePath uri')
      includeDeclaration = req ^. params . context . to (\(ReferenceContext incl) -> incl)
  referenceIndex <- indexProject currentFile
  case RefInd.lookupAt referenceIndex (fromLspUri uri') (fromLspPosition (req ^. params . position)) of
    Just binding ->
      let sites
            | includeDeclaration = RefInd.bindingSites binding
            | otherwise          = RefInd.bindingRefs binding
      in res $ Right $ InL (map toLspLocation sites)
    Nothing -> res $ Right $ InL []

indexProject :: FilePath -> LSP RefInd.ReferenceIndex
indexProject currentFile = do
  cached <- getCachedTypecheckedModules
  let paths = nub (currentFile : map fst cached)
  mdoc <- getVirtualFile (filePathToNormalizedUri currentFile)
  let msrc = T.filter (/= '\r') <$> (virtualFileText <$> mdoc)
  ReferenceIndexCache oldModules oldResult <- getCachedReferenceIndex
  -- Re-parse only what changed: the current file is revalidated against the
  -- editor buffer, while other files keep their cached parse until a
  -- file-change notification invalidates it (see resetCacheForFiles).
  let reusable p pm = case parsedSource pm of
        ParseInvalidated   -> False
        ParsedFromBuffer t -> p /= currentFile || msrc == Just t
        ParsedFromDisk     -> p /= currentFile || isNothing msrc
  entries <- forM paths $ \p ->
    case Map.lookup p oldModules of
      Just pm | reusable p pm -> return (p, pm, False)
      mold -> do
        parsed <- parseProjectFile currentFile msrc p
        let source = if p == currentFile
              then maybe ParsedFromDisk ParsedFromBuffer msrc
              else ParsedFromDisk
            -- A failed parse (a syntax error mid-edit) keeps the last good
            -- module, so hover and navigation stay available; the source is
            -- still updated, so the parse is retried once per edit, not once
            -- per request.
            module_ = parsed <|> (parsedModule =<< mold)
        return (p, ParsedModule source module_, True)
  let reparsed = or [ r | (_, _, r) <- entries ]
  case oldResult of
    Just (ps, cachedIndex) | ps == paths, not reparsed -> return cachedIndex
    _ -> do
      let referenceIndex = RefInd.indexModules
            [ (p, m) | (p, ParsedModule _ (Just m), _) <- entries ]
      cacheReferenceIndex $ ReferenceIndexCache
        (Map.fromList [ (p, pm) | (p, pm, _) <- entries ])
        (Just (paths, referenceIndex))
      return referenceIndex

parseProjectFile :: FilePath -> Maybe T.Text -> FilePath -> LSP (Maybe Module)
parseProjectFile currentFile msrc p
  | p == currentFile = case msrc of
      Just src -> parseOr (parseModuleSafe src)
      Nothing  -> parseOr (parseModuleFile p)
  | otherwise = parseOr (parseModuleFile p)
  where
    parseOr act = either (const Nothing) Just <$> liftIO act

-- | A signature for the hover code block. A long function type is split
-- with one parameter per line, in the style rzk definitions are written:
--
-- > is-equiv
-- >   : ( A : U)
-- >   → ( B : U)
-- >   → ( f : A → B)
-- >   → U
formatSignature :: String -> Term -> String
formatSignature name ty
  | length inline <= 60 = name ++ " : " ++ inline
  | (piParams@(_ : _), ret) <- peelPi ty =
      intercalate "\n" (name : zipWith (++) ("  : " : repeat "  → ") (piParams ++ [printTree ret]))
  | otherwise = name ++ " : " ++ inline
  where
    inline = printTree ty
    peelPi (TypeFun _ param ret)       = let (ps, r) = peelPi ret in (printTree param : ps, r)
    peelPi (ASCII_TypeFun _ param ret) = let (ps, r) = peelPi ret in (printTree param : ps, r)
    peelPi r                           = ([], r)

provideHover :: Handler LSP 'Method_TextDocumentHover
provideHover req res = do
  let uri' = req ^. params . textDocument . uri
      pos  = fromLspPosition (req ^. params . position)
      currentFile = fromMaybe "" $ uriToFilePath uri'
  referenceIndex <- indexProject currentFile
  case RefInd.lookupAt referenceIndex (fromLspUri uri') pos of
    Nothing -> res $ Right $ InR Null
    Just binding -> do
      cached <- getCachedTypecheckedModules
      let body = hoverContent binding cached
      res $ Right $ InL $ Hover
        (InL (MarkupContent MarkupKind_Markdown body))
        (Just (toLspRange (RefInd.locationRange (RefInd.bindingDef binding))))
  where
    hoverContent binding cached =
      T.pack (file ++ "\n\n```rzk\n" ++ signature ++ "\n```")
      where
        file = RefInd.locationPath (RefInd.bindingDef binding)
        name = RefInd.bindingName binding
        defLine = RefInd.positionLine (RefInd.rangeStart (RefInd.locationRange (RefInd.bindingDef binding)))
        decls = maybe [] cachedModuleDecls (lookup file cached)
        -- The elaborated type is the default: for a local binder, from the
        -- binder-type walk over the cached declarations; for a top-level
        -- name, from the declaration itself (preferring the one on the same
        -- line, so that a local that shadows a global does not show the
        -- global's type). The surface annotation from the reference index is
        -- the fallback, e.g. for mid-edit or ill-typed code with no cache.
        defCol = RefInd.positionCharacter (RefInd.rangeStart (RefInd.locationRange (RefInd.bindingDef binding)))
        -- All cached declarations go in scope, so that splitting a pair
        -- binder can unfold defined Σ-types from any file of the project.
        allDecls = concatMap (cachedModuleDecls . snd) cached
        elaboratedLocal = lookup (defLine, defCol)
          [ ((l - 1, c - 1), t)
          | (v, t) <- binderTypesInScopeOf allDecls decls
          , let VarIdent (RzkPosition _path mpos) _ = getVarIdent v
          , Just (l, c) <- [mpos]
          ]
        signature = case elaboratedLocal of
          Just (TypeView t)       -> formatSignature (T.unpack name) (fromTerm' t)
          Just (ShapeView c tope) -> T.unpack name ++ " : " ++ show c ++ " | " ++ show tope
          Nothing -> case find declOnSameLine decls of
            Just (Decl _ ty _ _ _ _) -> formatSignature (T.unpack name) (fromTerm' (untyped ty))
            Nothing -> case RefInd.bindingType binding of
              Just ann -> T.unpack name ++ " : " ++ T.unpack ann
              Nothing  -> case find declWithName decls of
                Just (Decl _ ty _ _ _ _) -> formatSignature (T.unpack name) (fromTerm' (untyped ty))
                Nothing                  -> T.unpack name ++ " : ?"
        declWithName (Decl v _ _ _ _ _) =
          T.pack (printTree (getVarIdent v)) == name
        declOnSameLine d@(Decl _ _ _ _ _ mloc) =
          declWithName d && (locationLine =<< mloc) == Just (defLine + 1)

-- | The printed name of a declaration and the range of its defining
-- occurrence, shared by the document and workspace symbol providers.
declNameRange :: Decl' -> (T.Text, Range)
declNameRange (Decl name _ _ _ _ _) = (T.pack (printTree ident), range)
  where
    ident = getVarIdent name
    VarIdent pos _ = ident
    RzkPosition _path pos' = pos
    (line, col) = fromMaybe (0, 0) pos'
    len = length (printTree ident)
    pos0 = Position (fromIntegral (max 0 (line - 1))) (fromIntegral (max 0 (col - 1)))
    end  = Position (fromIntegral (max 0 (line - 1))) (fromIntegral (max 0 (col - 1) + len))
    range = Range pos0 end

provideSymbols :: Handler LSP 'Method_TextDocumentDocumentSymbol
provideSymbols req res = do
  let currentFile = fromMaybe "" $ uriToFilePath $ req ^. params . textDocument . uri
  cachedModules <- getCachedTypecheckedModules
  let decls = maybe [] cachedModuleDecls (lookup currentFile cachedModules)
  res $ Right $ InR $ InL $ map declToSymbol decls
  where
    declToSymbol :: Decl' -> DocumentSymbol
    declToSymbol decl@(Decl _ type' _ _ _ _loc) = DocumentSymbol
      { _name           = symbolName
      , _detail         = Just (T.pack (show (untyped type')))
      , _kind           = SymbolKind_Function
      , _tags           = Nothing
      , _deprecated     = Nothing
      , _range          = range
      , _selectionRange = range
      , _children       = Nothing
      }
      where
        (symbolName, range) = declNameRange decl

-- | Workspace-wide symbol search over every typechecked module in the cache.
-- The query is matched case-insensitively as an infix of the definition name;
-- an empty query lists all definitions (clients filter further as the user
-- types).
provideWorkspaceSymbols :: Handler LSP 'Method_WorkspaceSymbol
provideWorkspaceSymbols req res = do
  let symbolQuery = T.toLower (req ^. params . query)
  cachedModules <- getCachedTypecheckedModules
  let symbols =
        [ WorkspaceSymbol
            { _name          = symbolName
            , _kind          = SymbolKind_Function
            , _tags          = Nothing
            , _containerName = Nothing
            , _location      = InL (Location (filePathToUri path) range)
            , _data_         = Nothing
            }
        | (path, cachedModule) <- cachedModules
        , decl <- cachedModuleDecls cachedModule
        , let (symbolName, range) = declNameRange decl
        , symbolQuery `T.isInfixOf` T.toLower symbolName
        ]
  res $ Right $ InR $ InL symbols


data IsChanged
  = HasChanged
  | NotChanged

-- | Detects if the given path has changes in its declaration compared to what's in the cache
isChanged :: RzkTypecheckCache -> FilePath -> LSP IsChanged
isChanged cache path = toIsChanged $ do
  let cacheWithoutErrors = map (fmap cachedModuleDecls) cache
  errors <- maybeToEitherLSP $ cachedModuleErrors <$> lookup path cache
  cachedDecls <- maybeToEitherLSP $ cachedModuleDecls <$> lookup path cache
  module' <- toExceptTLifted $ parseModuleFile path
  e <- toExceptTLifted $ try @SomeException $ evaluate $
    defaultTypeCheck (typecheckModulesWithLocationIncremental (takeWhile ((/= path) . fst) cacheWithoutErrors) [(path, module')])
  (checkedModules, errors') <- toExceptT $ return e
  decls' <- maybeToEitherLSP $ lookup path checkedModules
  return $ if null errors' && null errors && decls' == cachedDecls
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
