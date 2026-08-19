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
  useSiteTokens,
  handleFilesChanged,
) where

import           Control.Applicative           ((<|>))
import           Control.Exception             (SomeAsyncException (..),
                                                SomeException, evaluate,
                                                fromException, throwIO, try)
import           Control.Lens
import           Control.Monad                 (forM, forM_, unless, when)
import           Control.Monad.Except          (ExceptT (ExceptT),
                                                MonadError (throwError),
                                                modifyError, runExceptT)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Default.Class
import           Data.List                     (find, intercalate, isSuffixOf,
                                                nub, sort, (\\))
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (fromMaybe, isNothing)
import qualified Data.Text                     as T
import qualified Data.Yaml                     as Yaml
import           Language.LSP.Diagnostics      (partitionBySource)
import           Language.LSP.Protocol.Lens    (HasContext (context),
                                                HasDetail (detail),
                                                HasDocumentation (documentation),
                                                HasKind (kind),
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
import           Language.Rzk.Foil.Names       (RzkPosition (RzkPosition),
                                                VarIdent (getVarIdent))
import           Language.Rzk.Syntax           (Module, Term,
                                                Term' (ASCII_TypeFun, TypeFun),
                                                VarIdent' (VarIdent),
                                                parseModuleFile,
                                                parseModuleSafe, printTree)
import qualified Language.Rzk.VSCode.Config    as RzkConfig
import           Language.Rzk.VSCode.Env
import qualified Language.Rzk.VSCode.PositionEncoding as Enc
import qualified Language.Rzk.VSCode.ReferenceIndex as RefInd
import           Language.Rzk.VSCode.Logging
import           Language.Rzk.VSCode.Tokenize  (mergeTokens, tokenizeModule,
                                                tokenizeSyntaxSymbols)
import qualified Rzk.Diagnostic                as Diag
import           Rzk.Format                    (format)
import           Rzk.Project.Config            (ProjectConfig (include))
import           Rzk.TypeCheck
import           Text.Read                     (readMaybe)

-- | Like 'try', but re-throws asynchronous exceptions (a worker restart) and
-- 'ProgressCancelledException' (a client-side progress cancel; delivered by
-- 'Control.Concurrent.Async.cancelWith', so 'fromException' does not classify
-- it as asynchronous). Cancellation must abort the whole run instead of being
-- reported as a typechecker failure of the current module.
tryTypecheck :: IO a -> IO (Either SomeException a)
tryTypecheck action = try action >>= \case
  Left e
    | Just (SomeAsyncException _) <- fromException e -> throwIO e
    | Just cancelled <- fromException @ProgressCancelledException e -> throwIO cancelled
  result -> return result

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

-- | The astral-line map of a file, for position conversion at the LSP
-- boundary (see "Language.Rzk.VSCode.PositionEncoding"): from the editor
-- buffer when the file is open, from disk otherwise. A file that cannot be
-- read converts as all-BMP, i.e. the conversion is the identity.
astralLinesOfFile :: FilePath -> LSP Enc.AstralLines
astralLinesOfFile = fmap Enc.astralLines . sourceOfFile

-- | The text of a file as the client sees it: from the editor buffer when the
-- file is open, from disk otherwise. A file that cannot be read is empty.
sourceOfFile :: FilePath -> LSP T.Text
sourceOfFile path = do
  mdoc <- getVirtualFile (filePathToNormalizedUri path)
  case virtualFileText <$> mdoc of
    Just text -> return (T.filter (/= '\r') text)
    Nothing -> liftIO $ do
      result <- try @SomeException (readFile path)
      return $ case result of
        Left _    -> ""
        Right src -> T.filter (/= '\r') (T.pack src)

-- | The lines of a document, by 0-based line number, for reading the token a
-- diagnostic marks.
newtype SourceLines = SourceLines (Map.Map Int T.Text)

sourceLinesOf :: T.Text -> SourceLines
sourceLinesOf src = SourceLines (Map.fromDistinctAscList (zip [0 ..] (T.lines src)))

-- | A line outside the document is empty, as is one in a file that could not be
-- read; a marking there falls back to a single character.
lineOf :: SourceLines -> Int -> T.Text
lineOf (SourceLines ls) line = Map.findWithDefault "" line ls

fromLspPosition :: Enc.AstralLines -> LSP.Position -> RefInd.Position
fromLspPosition als (LSP.Position l c) =
  RefInd.Position (fromIntegral l)
    (Enc.colFromUtf16 als (fromIntegral l) (fromIntegral c))

toLspPosition :: Enc.AstralLines -> RefInd.Position -> LSP.Position
toLspPosition als (RefInd.Position l c) =
  LSP.Position (fromIntegral l)
    (fromIntegral (Enc.colToUtf16 als l c))

toLspRange :: Enc.AstralLines -> RefInd.Range -> Range
toLspRange als (RefInd.Range s e) =
  Range (toLspPosition als s) (toLspPosition als e)

-- | Convert locations to LSP, fetching the astral-line map once per file.
toLspLocations :: [RefInd.Location] -> LSP [LSP.Location]
toLspLocations locations = do
  let files = nub (map RefInd.locationPath locations)
  alss <- Map.fromList <$> forM files (\f -> (,) f <$> astralLinesOfFile f)
  return
    [ LSP.Location (toLspUri u) (toLspRange als r)
    | RefInd.Location u r <- locations
    , Just als <- [Map.lookup (RefInd.uriPath u) alss]
    ]

toLspLocation :: RefInd.Location -> LSP LSP.Location
toLspLocation (RefInd.Location u r) = do
  als <- astralLinesOfFile (RefInd.uriPath u)
  return (LSP.Location (toLspUri u) (toLspRange als r))

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

          cachedModules <- getCachedTypecheckedModules
          let cachedPaths = map fst cachedModules
              modifiedFiles = paths \\ cachedPaths

          logDebug ("Found " <> tshow (length cachedPaths) <> " files in the cache")
          logDebug (tshow (length modifiedFiles) <> " files have been modified")

          (parseErrors, parsedModules) <- liftIO $ collectErrors <$> parseFiles modifiedFiles

          -- Report parse errors to the client
          forM_ parseErrors $ \(path, err) -> do
            als <- astralLinesOfFile path
            publishDiagnostics maxDiagnosticCount (filePathToNormalizedUri path) Nothing (partitionBySource [diagnosticOfParseError als err])

          -- Files after the first parse error are not typechecked at all
          -- ('collectErrors' stops collecting modules there); mark the ones
          -- without a parse error of their own as blocked.
          case parseErrors of
            [] -> return ()
            (blockingPath, _) : _ -> do
              let reported = map fst parsedModules <> map fst parseErrors
              publishBlockedDiagnostics rootPath blockingPath
                (filter (`notElem` reported) modifiedFiles)

          -- Typecheck the modified modules one at a time on top of the cached
          -- prefix, reporting progress to the client. Each module is cached
          -- and its diagnostics are published as soon as it is checked, so a
          -- cancelled run keeps the modules it has finished and the next run
          -- continues from there.
          unless (null parsedModules) $
            withProgress "rzk typechecking" Nothing Cancellable $ \reportProgress ->
              checkModulesInProject reportProgress rootPath cachedModules parsedModules
  where
    checkModulesInProject
      :: (ProgressAmount -> LSP ())
      -> FilePath                -- ^ Workspace root (for progress messages).
      -> RzkTypecheckCache       -- ^ Cached results for the unchanged prefix.
      -> [(FilePath, Module)]    -- ^ Modified modules, in project order.
      -> LSP ()
    checkModulesInProject reportProgress rootPath cache modules = go (0 :: Int) cache modules
      where
        total = length modules

        go _ _ [] = return ()
        go i checked ((path, module_) : rest) = do
          reportProgress (ProgressAmount
            (Just (fromIntegral (100 * i `div` total)))
            (Just (T.pack (makeRelative rootPath path))))
          -- Run in lenient hole mode so holes are collected (and surfaced as
          -- hints) rather than reported as errors while editing.
          -- Resume from the context of the last module that is still cached: it
          -- /is/ the elaborated prefix, so nothing is replayed or re-elaborated.
          let prefix = case reverse checked of
                (_, entry) : _ -> cachedModuleChecked entry
                []             -> emptyChecked
          tcResult <- liftIO $ tryTypecheck $ evaluate $
            recheckFrom prefix [(path, module_)]
          case tcResult of
            Left (ex :: SomeException) -> do
              -- Just a warning to be logged in the "Output" panel and not shown to the user as an error message
              --  because exceptions are expected when the file has invalid syntax
              logWarning ("Encountered an exception while typechecking:\n" <> tshow ex)
              publishBlockedDiagnostics rootPath path (map fst rest)
            Right (Left err) -> do
              logError ("An impossible error happened! Please report a bug:\n" <> T.pack (ppTypeErrorInScopedContext BottomUp err))
              publishModuleDiagnostics path [err] [] []    -- sort of impossible
              publishBlockedDiagnostics rootPath path (map fst rest)
            Right (Right (checkedNow, holeInfos)) -> do
              let errors = checkedErrors checkedNow
              logDebug (T.pack path <> ": " <> tshow (length errors) <> " errors, "
                <> tshow (length holeInfos) <> " holes")
              let decls = fromMaybe [] (lookup path (declViews checkedNow))
                  checked' = checked ++
                    [(path, RzkCachedModule checkedNow decls
                        (filter ((== path) . filepathOfTypeError) errors))]
              cacheTypecheckedModules checked'
              publishModuleDiagnostics path errors (checkedWarnings checkedNow) holeInfos
              -- Stop at the first module with errors, like the batch checker
              -- ('typecheckModulesWithLocation'') does: later modules depend
              -- on this one and would report cascading errors. Mark the
              -- modules this run will not reach.
              if null errors
                then go (i + 1) checked' rest
                else publishBlockedDiagnostics rootPath path (map fst rest)

    -- Publish the diagnostics of one checked module, grouped by file so all
    -- diagnostics for a file are published in a single call
    -- (publishDiagnostics replaces a source's diagnostics per URI, so
    -- publishing them one at a time would clobber all but the last). The
    -- module's own file is always published, possibly with an empty list,
    -- replacing stale diagnostics from the previous run.
    --
    -- An empty list needs care: the lsp diagnostic store unions the new
    -- per-source map over the old one, and @partitionBySource []@ has no
    -- "rzk" key, so the old diagnostics would survive and be re-sent. A
    -- max count of 0 forces an empty publish to the client, clearing it.
    publishModuleDiagnostics :: FilePath -> [TypeErrorInScopedContext] -> [CheckWarning] -> [HoleInfo] -> LSP ()
    publishModuleDiagnostics path typeErrors warnings holeInfos = do
      let errDiagnostics  = [ (filepathOfTypeError err, [Diag.diagnoseTypeError TopDown err])
                            | err <- typeErrors ]
          warnDiagnostics = [ (path', [Diag.diagnoseCheckWarning warning])
                            | warning <- warnings
                            , let path' = fromMaybe path
                                    (warningLocation warning >>= locationFilePath) ]
          holeDiagnostics = [ (path', [Diag.diagnoseHole hole])
                            | hole <- holeInfos
                            , Just path' <- [holeLocation hole >>= locationFilePath] ]
          diagnosticsByFile = Map.insertWith (flip (<>)) path [] $
            Map.fromListWith (flip (<>)) (errDiagnostics <> warnDiagnostics <> holeDiagnostics)
      -- The source of each file with something to report, read once: a
      -- diagnostic marks the token it points at, and its column is converted
      -- to the UTF-16 the client counts in.
      forM_ (Map.toList diagnosticsByFile) $ \(path', diags) -> do
        lspDiags <- case diags of
          [] -> return []
          _  -> do
            src <- sourceOfFile path'
            let als = Enc.astralLines src
                sourceLines = sourceLinesOf src
            return (map (lspDiagnosticOf als sourceLines) diags)
        publishDiagnostics (if null lspDiags then 0 else maxDiagnosticCount)
          (filePathToNormalizedUri path') Nothing (partitionBySource lspDiags)

    -- Modules that a run never reaches (they come after a module with an
    -- error, and every rzk module depends on all earlier ones) get a single
    -- warning diagnostic naming the blocker, instead of keeping whatever
    -- diagnostics a previous run left behind. Warning severity keeps the
    -- file visible in the explorer (yellow badge) while staying distinct
    -- from a real error in the file itself. It is replaced by real
    -- diagnostics once the blocker is fixed and the module is reached
    -- again.
    publishBlockedDiagnostics :: FilePath -> FilePath -> [FilePath] -> LSP ()
    publishBlockedDiagnostics rootPath blockingPath notReached =
      forM_ notReached $ \path ->
        publishDiagnostics maxDiagnosticCount (filePathToNormalizedUri path) Nothing
          (partitionBySource [blockedDiagnostic])
      where
        blockedDiagnostic = Diagnostic
          (Range (Position 0 0) (Position 0 99))
          (Just DiagnosticSeverity_Warning)
          (Just (InR "not-checked"))
          Nothing                   -- diagnostic description
          (Just "rzk")              -- A human-readable string describing the source of this diagnostic
          ("Not checked: blocked by an error in " <> T.pack (makeRelative rootPath blockingPath))
          Nothing                   -- tags
          (Just [])                 -- related information
          Nothing                   -- data that is preserved between different calls

    filepathOfTypeError :: TypeErrorInScopedContext -> FilePath
    filepathOfTypeError (TypeErrorInScopedContext ctx _err) =
      case ctxLocation ctx >>= locationFilePath of
        Just path -> path
        _         -> error "the impossible happened! Please contact Abdelrahman immediately!!!"

    -- Map a structured library diagnostic to an LSP diagnostic, marking the
    -- term it is about in the given source.
    lspDiagnosticOf :: Enc.AstralLines -> SourceLines -> Diag.Diagnostic -> Diagnostic
    lspDiagnosticOf als sourceLines d = Diagnostic
                      (Enc.rangeToUtf16 als (diagnosticRange sourceLines d))
                      (Just (lspSeverity (Diag.diagnosticSeverity d)))
                      (Just (InR (T.pack (Diag.diagnosticCode d))))
                      Nothing                   -- diagnostic description
                      (Just "rzk")              -- A human-readable string describing the source of this diagnostic
                      (T.pack (Diag.diagnosticMessage d))
                      Nothing                   -- tags
                      (Just [])                 -- related information
                      Nothing                   -- data that is preserved between different calls

    -- What the diagnostic marks: the token the term it is about starts with.
    --
    -- The checker knows where a term begins and not where it ends (the surface
    -- syntax records only the start of a node), so the marked span is the head
    -- token. That puts the squiggle on the right thing without claiming an
    -- extent that was never measured, and a hole is marked exactly, since @?@
    -- and @?name@ are the whole term. A diagnostic that has no column is about
    -- a whole declaration and still marks the line.
    diagnosticRange :: SourceLines -> Diag.Diagnostic -> Range
    diagnosticRange sourceLines d = case Diag.diagnosticLocation d of
      Just loc
        | Just lineNo <- locationLine loc
        , Just col <- locationColumn loc
        , let line = fromIntegral (lineNo - 1)  -- LSP counts lines from 0
        , let start = fromIntegral (col - 1)    -- and columns too
        -> Range (Position line start)
                 (Position line (start + tokenWidth sourceLines line start))
      Just loc
        | Just lineNo <- locationLine loc
        , let line = fromIntegral (lineNo - 1)
        -> Range (Position line 0) (Position line 99) -- to the end of the line
      _ -> Range (Position 0 0) (Position 0 99)

    -- The width of the token starting at a position, in code points, and never
    -- zero: an empty marking shows nothing at all. A token that opens with a
    -- bracket or a separator is one character wide, since those delimit rather
    -- than name; anything else runs to the next space or delimiter.
    tokenWidth :: SourceLines -> UInt -> UInt -> UInt
    tokenWidth sourceLines line start =
      case T.drop (fromIntegral start) (lineOf sourceLines (fromIntegral line)) of
        rest | Just (c, _) <- T.uncons rest, not (isDelimiter c)
             -> max 1 (fromIntegral (T.length (T.takeWhile (not . isDelimiter) rest)))
        _    -> 1
      where
        isDelimiter c = c `elem` (" \t()[]{},;" :: String)

    lspSeverity :: Diag.Severity -> DiagnosticSeverity
    lspSeverity = \case
      Diag.SeverityError       -> DiagnosticSeverity_Error
      Diag.SeverityWarning     -> DiagnosticSeverity_Warning
      Diag.SeverityInformation -> DiagnosticSeverity_Information
      Diag.SeverityHint        -> DiagnosticSeverity_Hint

    diagnosticOfParseError :: Enc.AstralLines -> T.Text -> Diagnostic
    diagnosticOfParseError als err = Diagnostic (Enc.rangeToUtf16 als (Range (Position errLine errColumnStart) (Position errLine errColumnEnd)))
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
    declsToItems :: FilePath -> (FilePath, [DeclView]) -> [CompletionItem]
    declsToItems root (path, decls) = map (declToItem root path) decls
    declToItem :: FilePath -> FilePath -> DeclView -> CompletionItem
    declToItem rootDir path (DeclView name type' _ _loc declKind) = def

      & label .~ T.pack (printTree $ getVarIdent name)
      & kind ?~ completionKindOfDecl declKind
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
          -- Length of the last line (after the last newline; if no newline,
          -- the whole text is one line), in UTF-16 units as LSP counts them.
          endCharacter
            | T.last source == '\n' = 0
            | otherwise = fromIntegral (Enc.utf16Length (T.takeWhileEnd (/= '\n') source))
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
      currentFile = fromMaybe "" (uriToFilePath (req ^. params . textDocument . uri))
  mdoc <- getVirtualFile doc
  -- Use-site classification needs name resolution (an occurrence of a
  -- constructor is a plain variable to the AST walk): the reference index
  -- resolves the occurrences, and the typecheck cache knows each
  -- declaration's kind.
  referenceIndex <- indexProject currentFile
  cachedModules <- getCachedTypecheckedModules
  let declsByFile = [ (path, cachedModuleDecls m) | (path, m) <- cachedModules ]
      overlay = useSiteTokens declsByFile referenceIndex currentFile
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
      -- On overlapping positions: the AST walk wins (it has the declaration
      -- modifiers), then the use-site overlay, then the lexer baseline.
      return (Right (Enc.tokensToUtf16 (Enc.astralLines src)
        (mergeTokens (mergeTokens astTokens overlay) (tokenizeSyntaxSymbols src))))
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
  als <- astralLinesOfFile currentFile
  case RefInd.lookupAt referenceIndex (fromLspUri uri') (fromLspPosition als (req ^. params . position)) of
    Just binding -> do
      location <- toLspLocation (RefInd.bindingDef binding)
      res $ Right $ InL $ Definition $ InL location
    Nothing      -> res $ Right $ InR $ InR Null

findReferences :: Handler LSP 'Method_TextDocumentReferences
findReferences req res = do
  let uri' = req ^. params . textDocument . uri
      currentFile = fromMaybe "" (uriToFilePath uri')
      includeDeclaration = req ^. params . context . to (\(ReferenceContext incl) -> incl)
  referenceIndex <- indexProject currentFile
  als <- astralLinesOfFile currentFile
  case RefInd.lookupAt referenceIndex (fromLspUri uri') (fromLspPosition als (req ^. params . position)) of
    Just binding -> do
      let sites
            | includeDeclaration = RefInd.bindingSites binding
            | otherwise          = RefInd.bindingRefs binding
      locations <- toLspLocations sites
      res $ Right $ InL locations
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
      currentFile = fromMaybe "" $ uriToFilePath uri'
  referenceIndex <- indexProject currentFile
  als <- astralLinesOfFile currentFile
  let pos = fromLspPosition als (req ^. params . position)
  case RefInd.lookupAt referenceIndex (fromLspUri uri') pos of
    Nothing -> res $ Right $ InR Null
    Just binding -> do
      cached <- getCachedTypecheckedModules
      let body = hoverContent binding cached
      LSP.Location _ defRange <- toLspLocation (RefInd.bindingDef binding)
      res $ Right $ InL $ Hover
        (InL (MarkupContent MarkupKind_Markdown body))
        (Just defRange)
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
        -- The whole checked project is in scope, so that splitting a pair binder
        -- can unfold defined Σ-types from any file of it.
        binderTypes = case reverse cached of
          (_, entry) : _ -> binderTypesOfFile (cachedModuleChecked entry) file
          []             -> []
        elaboratedLocal = lookup (defLine, defCol)
          [ ((l - 1, c - 1), t)
          | (v, t) <- binderTypes
          , let VarIdent (RzkPosition _path mpos) _ = getVarIdent v
          , Just (l, c) <- [mpos]
          ]
        signature = case elaboratedLocal of
          Just (TypeView t)       -> formatSignature (T.unpack name) (getRendered t)
          Just (ShapeView c tope) -> T.unpack name ++ " : " ++ show c ++ " | " ++ show tope
          Nothing -> case find declOnSameLine decls of
            Just d  -> formatSignature (T.unpack name) (getRendered (declViewType d))
            Nothing -> case RefInd.bindingType binding of
              Just ann -> T.unpack name ++ " : " ++ T.unpack ann
              Nothing  -> case find declWithName decls of
                Just d  -> formatSignature (T.unpack name) (getRendered (declViewType d))
                Nothing -> T.unpack name ++ " : ?"
        declWithName (DeclView v _ _ _ _) =
          T.pack (printTree (getVarIdent v)) == name
        declOnSameLine d@(DeclView _ _ _ mloc _) =
          declWithName d && (locationLine =<< mloc) == Just (defLine + 1)

-- | The printed name of a declaration and the range of its defining
-- occurrence, shared by the document and workspace symbol providers.
declNameRange :: Enc.AstralLines -> DeclView -> (T.Text, Range)
declNameRange als (DeclView name _ _ _ _) = (T.pack (printTree ident), range)
  where
    ident = getVarIdent name
    VarIdent pos _ = ident
    RzkPosition _path pos' = pos
    (line, col) = fromMaybe (0, 0) pos'
    len = length (printTree ident)
    line0 = max 0 (line - 1)
    col0 = max 0 (col - 1)
    pos0 = Position (fromIntegral line0) (fromIntegral (Enc.colToUtf16 als line0 col0))
    end  = Position (fromIntegral line0) (fromIntegral (Enc.colToUtf16 als line0 (col0 + len)))
    range = Range pos0 end

provideSymbols :: Handler LSP 'Method_TextDocumentDocumentSymbol
provideSymbols req res = do
  let currentFile = fromMaybe "" $ uriToFilePath $ req ^. params . textDocument . uri
  cachedModules <- getCachedTypecheckedModules
  als <- astralLinesOfFile currentFile
  let decls = maybe [] cachedModuleDecls (lookup currentFile cachedModules)
  res $ Right $ InR $ InL $ outline als decls
  where
    -- A #data contributes one symbol with its constructors as children; the
    -- generated eliminators stay out of the outline (they are not in the
    -- source; workspace symbol search still finds them).
    outline :: Enc.AstralLines -> [DeclView] -> [DocumentSymbol]
    outline _ [] = []
    outline als (d : ds) = case declViewKind d of
      DeclKindData ->
        let isChildOf c = case declViewKind c of
              DeclKindDataCon parent -> parent == declViewName d
              _                      -> False
            (childDecls, rest) = span isChildOf ds
        in declToSymbol als (Just (map (declToSymbol als Nothing) childDecls)) d
             : outline als rest
      DeclKindDataElim _ -> outline als ds
      _ -> declToSymbol als Nothing d : outline als ds

    declToSymbol :: Enc.AstralLines -> Maybe [DocumentSymbol] -> DeclView -> DocumentSymbol
    declToSymbol als mchildren decl@(DeclView _ type' _ _loc declKind) = DocumentSymbol
      { _name           = symbolName
      , _detail         = Just (T.pack (show type'))
      , _kind           = symbolKindOfDecl declKind
      , _tags           = Nothing
      , _deprecated     = Nothing
      , _range          = range
      , _selectionRange = range
      , _children       = mchildren
      }
      where
        (symbolName, range) = declNameRange als decl

-- | The LSP symbol kind of a declaration, mirroring the semantic token
-- choices at the declaration site (class for a data type, enum member for a
-- constructor, function otherwise).
symbolKindOfDecl :: DeclKind -> SymbolKind
symbolKindOfDecl = \case
  DeclKindData       -> SymbolKind_Class
  DeclKindDataCon _  -> SymbolKind_EnumMember
  DeclKindDataElim _ -> SymbolKind_Function
  DeclKindPostulate  -> SymbolKind_Function
  DeclKindDefine     -> SymbolKind_Function

-- | The completion kind of a declaration, mirroring 'symbolKindOfDecl'.
completionKindOfDecl :: DeclKind -> CompletionItemKind
completionKindOfDecl = \case
  DeclKindData       -> CompletionItemKind_Class
  DeclKindDataCon _  -> CompletionItemKind_EnumMember
  DeclKindDataElim _ -> CompletionItemKind_Function
  DeclKindPostulate  -> CompletionItemKind_Function
  DeclKindDefine     -> CompletionItemKind_Function

-- | The checker-derived token overlay for identifier /uses/: an occurrence
-- that resolves to a product of a @#data@ declaration is coloured by its
-- kind wherever it appears — a constructor as an enum member, the type as a
-- class, a generated eliminator as a library function — and an occurrence
-- of a postulate or an assumption is marked abstract (declared, but not
-- proven), so a proof that leans on an axiom is visible at a glance.
-- Postulates, top-level assumptions, and in-section assumptions get
-- distinct type/modifier combinations, in decreasing order of severity.
-- Occurrences are
-- matched to declarations by definition site (file and line) /and/ name, so
-- a local that shadows a constructor stays plain, and plain definitions are
-- left to the lexer baseline. Positions are code points, like every other
-- token source; the UTF-16 conversion happens after merging.
useSiteTokens
  :: [(FilePath, [DeclView])]  -- ^ the typechecked declarations, per file
  -> RefInd.ReferenceIndex
  -> FilePath                  -- ^ the file to produce tokens for
  -> [SemanticTokenAbsolute]
useSiteTokens declsByFile refIndex path =
  [ SemanticTokenAbsolute
      { _line = fromIntegral l
      , _startChar = fromIntegral s
      , _length = fromIntegral (e - s)
      , _tokenType = tokenType
      , _tokenModifiers = modifiers
      }
  | (binding, l, s, e) <- RefInd.fileOccurrences refIndex path
  , Just (tokenType, modifiers) <- [classify binding]
  ]
  where
    classify binding
      -- Assumptions do not survive to the typechecked declarations (the
      -- section mechanism folds them into their users), so they are
      -- recognised by their syntactic definition site instead. A
      -- top-level assumption is a file-wide axiom; one inside a section
      -- is a hypothesis discharged at its #end, so it keeps the
      -- parameter token type and only gains the abstract modifier.
      | Just scope <- RefInd.assumeScopeAt refIndex (RefInd.bindingDef binding) =
          Just $ case scope of
            RefInd.AssumeTopLevel ->
              (SemanticTokenTypes_Function, [SemanticTokenModifiers_Abstract])
            RefInd.AssumeInSection ->
              (SemanticTokenTypes_Parameter, [SemanticTokenModifiers_Abstract])
      | otherwise = do
      let RefInd.Location (RefInd.Uri defPath) range = RefInd.bindingDef binding
          defStart = RefInd.rangeStart range
          key = ( defPath
                , RefInd.positionLine defStart
                , RefInd.positionCharacter defStart
                , RefInd.bindingName binding )
      declKind <- Map.lookup key kindTable
      case declKind of
        DeclKindData       -> Just (SemanticTokenTypes_Class, [])
        DeclKindDataCon _  -> Just (SemanticTokenTypes_EnumMember, [])
        DeclKindDataElim _ ->
          Just (SemanticTokenTypes_Function, [SemanticTokenModifiers_DefaultLibrary])
        -- The abstract and static modifiers are in the default legend, so
        -- no custom legend is needed; clients style function.abstract
        -- distinctly (the VS Code extension maps it to a bright scope).
        -- The static modifier only distinguishes a postulate (a permanent
        -- axiom) from a top-level assumption (discharged at module end),
        -- so a postulate can be styled louder.
        DeclKindPostulate  ->
          Just ( SemanticTokenTypes_Function
               , [SemanticTokenModifiers_Abstract, SemanticTokenModifiers_Static] )
        DeclKindDefine     -> Nothing
    -- Keyed by the declared name's own position, which is what the index
    -- records as the definition site (a constructor's key is where it is
    -- written, also in a multi-line declaration). The generated eliminators
    -- share the type name's position — its derived zero-width entries — so
    -- the name is part of the key.
    kindTable = Map.fromList
      [ ((file, line - 1, col - 1, name), declViewKind d)
      | (_, decls) <- declsByFile
      , d <- decls
      , let ident = getVarIdent (declViewName d)
      , let name = T.pack (printTree ident)
      , VarIdent (RzkPosition mpath mpos) _ <- [ident]
      , Just file <- [mpath]
      , Just (line, col) <- [mpos]
      ]

-- | Workspace-wide symbol search over every typechecked module in the cache.
-- The query is matched case-insensitively as an infix of the definition name;
-- an empty query lists all definitions (clients filter further as the user
-- types).
provideWorkspaceSymbols :: Handler LSP 'Method_WorkspaceSymbol
provideWorkspaceSymbols req res = do
  let symbolQuery = T.toLower (req ^. params . query)
  cachedModules <- getCachedTypecheckedModules
  symbols <- fmap concat $ forM cachedModules $ \(path, cachedModule) -> do
    als <- astralLinesOfFile path
    return
      [ WorkspaceSymbol
          { _name          = symbolName
          , _kind          = symbolKindOfDecl (declViewKind decl)
          , _tags          = Nothing
          , _containerName = Nothing
          , _location      = InL (Location (filePathToUri path) range)
          , _data_         = Nothing
          }
      | decl <- cachedModuleDecls cachedModule
      , let (symbolName, range) = declNameRange als decl
      , symbolQuery `T.isInfixOf` T.toLower symbolName
      ]
  res $ Right $ InR $ InL symbols


data IsChanged
  = HasChanged
  | NotChanged

-- | Detects if the given path has changes in its declaration compared to what's in the cache
isChanged :: RzkTypecheckCache -> FilePath -> LSP IsChanged
isChanged cache path = toIsChanged $ do
  errors <- maybeToEitherLSP $ cachedModuleErrors <$> lookup path cache
  cachedDecls <- maybeToEitherLSP $ cachedModuleDecls <$> lookup path cache
  module' <- toExceptTLifted $ parseModuleFile path
  -- Re-check this file from the context of the prefix before it.
  let prefix = case reverse (takeWhile ((/= path) . fst) cache) of
        (_, entry) : _ -> cachedModuleChecked entry
        []             -> emptyChecked
  e <- toExceptTLifted $ try @SomeException $ evaluate $
    recheckFrom prefix [(path, module')]
  (checkedNow, _holes) <- toExceptT $ return e
  decls' <- maybeToEitherLSP $ lookup path (declViews checkedNow)
  return $ if null (checkedErrors checkedNow) && null errors && decls' == cachedDecls
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

-- | The cache eviction and the re-typecheck run on the typecheck worker
-- thread, so this handler returns immediately and later requests (e.g. a
-- formatting request from format-on-save) are answered while the project
-- re-check is still running. Spawning the worker cancels the previous one,
-- so a newer change restarts the re-check.
handleFilesChanged :: Handler LSP 'Method_WorkspaceDidChangeWatchedFiles
handleFilesChanged msg = do
  let modifiedPaths = msg ^.. params . changes . traverse . uri . to uriToFilePath . _Just
  spawnTypecheckWorker $ do
    if any ("rzk.yaml" `isSuffixOf`) modifiedPaths
      then do
        logDebug "rzk.yaml modified. Clearing module cache"
        resetCacheForAllFiles
      else do
        cache <- getCachedTypecheckedModules
        actualModified <- dropWhileM (hasNotChanged cache) modifiedPaths
        resetCacheForFiles actualModified
    typecheckFromConfigFile
