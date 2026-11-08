{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE TupleSections       #-}
-- | Declarations, commands and the module driver.
--
-- A top-level entry is a name bound in the outermost scope, so the scope
-- grows as a module is checked: each @#define@, @#postulate@ and @#assume@
-- enters a binder and the rest of the module is checked under it. The
-- driver is written in continuation-passing style, and the result is
-- packaged with the scope it was produced in.
--
-- A section (and a module, which is an anonymous section) is closed by
-- abstracting its definitions over the assumptions they use. The
-- dependencies are read off the elaborated 'Core' each top-level entry
-- keeps, as the old checker read them off its terms ("Rzk.TypeCheck.Decl").
--
-- @#data@ is not covered yet: such a command is reported and skipped.
module Rzk.Check.Decl
  ( Decl (..)
  , checkModules
  , elaborate
  ) where

import           Control.Applicative      ((<|>))
import           Control.Monad            (forM, when)
import           Control.Monad.Foil       (DExt, Distinct)
import qualified Control.Monad.Foil       as Foil
import           Control.Monad.Free.Foil  (AST (..), ScopedAST (..))
import qualified Data.IntSet              as IntSet
import           Data.List                (genericLength)
import qualified Data.Map                 as Map
import           Data.Maybe               (fromMaybe)
import qualified FreeFoil.NbE             as NbE
import           Debug.Trace              (trace)

import           Language.Rzk.Foil.Names  (TModality (..),
                                           VarIdent, markUnresolved,
                                           patternToTerm, varIdentAt)
import qualified Language.Rzk.Syntax      as Rzk
import           Rzk.Check.Context
import           Rzk.Check.Convert
import           Rzk.Check.Display
import           Rzk.Check.Error
import           Rzk.Check.Monad
import           Rzk.Check.Rules
import           Rzk.Check.Syntax
import           Rzk.Check.Value
import           Rzk.TypeCheck.Decl       (duplicateBinders)

-- | A top-level declaration, as checked.
data Decl n = Decl
  { declName     :: VarIdent
  , declNameOf   :: Foil.Name n
  , declType     :: Value n
  , declValue    :: Maybe (Value n)
  , declLocation :: Maybe LocationInfo
  }

instance Foil.Sinkable Decl where
  sinkabilityProof rename decl = decl
    { declNameOf = rename (declNameOf decl)
    , declType = Foil.sinkabilityProof rename (declType decl)
    , declValue = Foil.sinkabilityProof rename <$> declValue decl
    }

sinkDecls :: DExt n l => [Decl n] -> [Decl l]
sinkDecls = Foil.sink1

-- | A surface term in the current scope: a free identifier resolves to the
-- entry it names, and an unknown one is marked for the checker to report
-- where it is used.
elaborate :: (Checking m, Distinct n) => Rzk.Term -> m n (Raw n)
elaborate term = do
  ctx <- askContext
  let env name = case lookupNamed name ctx of
        Just v  -> Var v
        Nothing -> rawNode noPosition (HoleF (Just (markUnresolved name)))
  pure (toRaw (ctxScope ctx) env term)

-- | Bind a top-level entry and run the rest of the module under it. The
-- entry keeps its elaborated type and body, and is recorded in the
-- innermost open section.
withTopLevel
  :: (Checking m, Distinct n)
  => VarIdent -> Core n -> Value n -> Maybe (Core n) -> EntryKind -> [Foil.Name n]
  -> (forall l. (DExt n l, Distinct l) => Decl l -> m l r)
  -> m n r
withTopLevel name tyCore ty mval kind uses k = do
  checkTopLevelDuplicate name
  ctx <- askContext
  let scope = ctxScope ctx
  Foil.withFresh scope $ \binder -> do
    let pat = PatternVar (Just name) binder
        value = fmap (evalCore scope ctx) mval
        entry = Entry
          { entryType = Foil.sink ty
          , entryValue = Foil.sink <$> value
          , entryModality = Id
          , entryLocks = Id
          , entryKind = kind
          , entryName = Just name
          , entryLocation = ctxLocation ctx
          , entryUses = map Foil.sink uses
          , entryTypeCore = Just (Foil.sink tyCore)
          , entryValueCore = Foil.sink <$> mval
          }
        ctx' = recordInSection (Foil.nameOf binder) (enterPattern pat [entry] ctx)
        decl = Decl
          { declName = name
          , declNameOf = Foil.nameOf binder
          , declType = Foil.sink ty
          , declValue = Foil.sink <$> value
          , declLocation = ctxLocation ctx
          }
    inContext ctx' (k decl)

-- | Record a new entry in the innermost open section.
recordInSection :: Foil.Name n -> Context d n -> Context d n
recordInSection name ctx = ctx
  { ctxSections = case ctxSections ctx of
      []       -> [Section Nothing [name]]
      s : rest -> s { sectionEntries = name : sectionEntries s } : rest
  }

-- | Assume names of one type, each a top-level entry.
assume
  :: (Checking m, Distinct n)
  => [VarIdent] -> Core n -> Value n
  -> (forall l. (DExt n l, Distinct l) => [Decl l] -> m l r)
  -> m n r
assume [] _ _ k = k []
assume (name : names) tyCore ty k =
  withTopLevel name tyCore ty Nothing EntryAssumption [] $ \decl ->
    assume names (Foil.sink tyCore) (Foil.sink ty) $ \decls ->
      k (Foil.sink decl : decls)

-- | The names a declaration says it uses.
usedNames :: (Checking m, Distinct n) => Maybe FilePath -> [Rzk.VarIdent] -> m n [Foil.Name n]
usedNames path vars = forM vars $ \var -> do
  ctx <- askContext
  let name = varIdentAt path var
  maybe (issueTypeError (TypeErrorUndefined name)) pure (lookupNamed name ctx)

checkTopLevelDuplicate :: (Checking m, Distinct n) => VarIdent -> m n ()
checkTopLevelDuplicate name = do
  ctx <- askContext
  case lookupNamed name ctx of
    Nothing -> pure ()
    Just v -> case entryName (lookupEntry v ctx) of
      Just previous -> issueTypeError (TypeErrorDuplicateTopLevel [previous] name)
      Nothing       -> issueTypeError (TypeErrorDuplicateTopLevel [] name)

-- * Commands

-- | The surface parameters of a declaration as binders of its type.
paramToParamDecl :: Rzk.Param -> Either String [Rzk.ParamDecl]
paramToParamDecl = \case
  Rzk.ParamPatternShape loc pats cube tope ->
    Right [ Rzk.ParamTermShape loc (patternToTerm pat) cube tope | pat <- pats ]
  Rzk.ParamPatternType loc pats ty ->
    Right [ Rzk.ParamTermType loc (patternToTerm pat) ty | pat <- pats ]
  Rzk.ParamPattern{} -> Left "untyped pattern in parameters"
  Rzk.ParamPatternModalType loc pats mc ty ->
    Right [ Rzk.ParamTermModalType loc (patternToTerm pat) mc ty | pat <- pats ]
  Rzk.ParamPatternModalShape loc pats mc cube tope ->
    Right [ Rzk.ParamTermModalShape loc (patternToTerm pat) mc cube tope | pat <- pats ]

addParamDecls :: [Rzk.ParamDecl] -> Rzk.Term -> Rzk.Term
addParamDecls [] = id
addParamDecls (paramDecl : paramDecls) = Rzk.TypeFun Nothing paramDecl . addParamDecls paramDecls

addParams :: [Rzk.Param] -> Rzk.Term -> Rzk.Term
addParams []     = id
addParams params = Rzk.Lambda Nothing params

-- | The type of a declaration with its parameters, checked: the names it
-- says it uses, the type as elaborated, and as a value.
checkDeclType
  :: (Checking m, Distinct n)
  => Maybe FilePath -> [Rzk.VarIdent] -> [Rzk.Param] -> Rzk.Term
  -> m n ([Foil.Name n], Core n, Value n)
checkDeclType path vars params ty = do
  used <- usedNames path vars
  paramDecls <- either (issueTypeError . TypeErrorOther) (pure . concat) (mapM paramToParamDecl params)
  tyTerm <- elaborate (addParamDecls paramDecls ty)
  ty' <- atSurface ty $ check tyTerm universe
  (used, ty',) <$> evalM ty'

setOption :: (Checking m, Distinct n) => String -> String -> m n a -> m n a
setOption name value = case (name, value) of
  ("verbosity", "debug")  -> withOptions (\o -> o { optVerbosity = Debug })
  ("verbosity", "normal") -> withOptions (\o -> o { optVerbosity = Normal })
  ("verbosity", "silent") -> withOptions (\o -> o { optVerbosity = Silent })
  ("verbosity", _) -> const $ issueTypeError $ TypeErrorOther
    "unknown verbosity level (use \"debug\", \"normal\", or \"silent\")"
  ("warn-overhang", "yes") -> withOptions (\o -> o { optWarnOverhang = True })
  ("warn-overhang", "no")  -> withOptions (\o -> o { optWarnOverhang = False })
  ("warn-overhang", _) -> const $ issueTypeError $ TypeErrorOther
    "unknown value for \"warn-overhang\" (use \"yes\" or \"no\")"
  ("warn-tope-family-domain", "yes") -> withOptions (\o -> o { optWarnTopeFamilyDomain = True })
  ("warn-tope-family-domain", "no")  -> withOptions (\o -> o { optWarnTopeFamilyDomain = False })
  ("warn-tope-family-domain", _) -> const $ issueTypeError $ TypeErrorOther
    "unknown value for \"warn-tope-family-domain\" (use \"yes\" or \"no\")"
  ("render", "svg") -> withOptions (\o -> o { optRender = Just "svg" })
  ("render", "latex") -> withOptions (\o -> o { optRender = Just "latex" })
  ("render", "none") -> withOptions (\o -> o { optRender = Nothing })
  ("render", _) -> const $ issueTypeError $ TypeErrorOther
    "unknown render backend (use \"svg\", \"latex\", or \"none\")"
  -- options of checks the new checker does not run yet are accepted
  ("render-hide-term", _) -> id
  ("rstt-safe", _) -> id
  ("warn-meta-prefix", _) -> id
  ("warn-free-standing-restriction", _) -> id
  ("warn-shape-dependency", _) -> id
  ("warn-meta-binder", _) -> id
  _ -> const $ issueTypeError $ TypeErrorOther ("unknown option " <> show name)

unsetOption :: (Checking m, Distinct n) => String -> m n a -> m n a
unsetOption name = case name of
  "verbosity" -> withOptions (\o -> o { optVerbosity = optVerbosity defaultOptions })
  "warn-overhang" -> withOptions (\o -> o { optWarnOverhang = optWarnOverhang defaultOptions })
  "warn-tope-family-domain" ->
    withOptions (\o -> o { optWarnTopeFamilyDomain = optWarnTopeFamilyDomain defaultOptions })
  "render" -> withOptions (\o -> o { optRender = Nothing })
  "render-hide-term" -> id
  "rstt-safe" -> id
  "warn-meta-prefix" -> id
  "warn-free-standing-restriction" -> id
  "warn-shape-dependency" -> id
  "warn-meta-binder" -> id
  _ -> const $ issueTypeError $ TypeErrorOther ("unknown option " <> show name)

-- | Run a check at a command: the location points at the command, and a
-- judgement inside it narrows it to the sub-term it is about.
atCommand :: Checking m => Rzk.Command -> m n a -> m n a
atCommand command = localContext $ \ctx -> ctx { ctxLocation = at <$> ctxLocation ctx }
  where
    at loc = loc { locationLine = fst <$> pos, locationColumn = snd <$> pos }
    pos = Rzk.hasPosition command

-- | Run a check with the location narrowed to where a surface term was
-- written.
atSurface :: (Checking m, Rzk.HasPosition a) => a -> m n b -> m n b
atSurface x = localContext $ \ctx -> ctx { ctxLocation = narrow <$> ctxLocation ctx }
  where
    narrow loc = case Rzk.hasPosition x of
      Nothing          -> loc
      Just (line, col) -> loc { locationLine = Just line, locationColumn = Just col }

-- * The module driver

-- | Check a module's commands, extending the scope with each definition.
-- The continuation runs in the final scope with the declarations and the
-- errors found.
checkCommands
  :: forall m n r. (Checking m, Distinct n)
  => Maybe FilePath -> Integer -> Integer -> [Rzk.Command]
  -> (forall l. (DExt n l, Distinct l) => [Decl l] -> [FailureOf m] -> m l r)
  -> m n r
checkCommands path i total commands k = case commands of
  [] -> k [] []
  command : more | (dup, previous) : _ <- duplicateBinders command -> do
    result <- attempt $ atCommand command $ issueTypeError (TypeErrorRepeatedBinder dup previous)
    case result of
      Left err -> skipping err path i total more k
      Right () -> checkCommands path (i + 1) total more k

  Rzk.CommandUnsetOption _loc optionName : more ->
    announce ("Unsetting option " <> optionName) $
      unsetOption optionName $ checkCommands path (i + 1) total more k

  command@(Rzk.CommandSetOption _loc optionName optionValue) : more ->
    announce ("Setting option " <> optionName <> " = " <> optionValue) $ do
      result <- attempt $ atCommand command $ setOption optionName optionValue (pure ())
      case result of
        Left err -> skipping err path i total more k
        Right () -> setOption optionName optionValue $ checkCommands path (i + 1) total more k

  command@(Rzk.CommandDefine _loc name (Rzk.DeclUsedVars _ vars) params ty term) : more ->
    announce (" Checking #define " <> Rzk.printTree name) $ atCommand command $ do
      typeResult <- attempt $ checkDeclType path vars params ty
      case typeResult of
        Left err -> skipping err path i total more k
        Right (used, tyC, tyV) -> do
          -- the body is checked on its own, so that a definition whose
          -- type is fine but whose proof is not still enters the scope
          result <- attempt $ do
            valTerm <- elaborate (addParams params term)
            val' <- atSurface term $ check valTerm tyV
            case force tyV of
              VNodeUniverse -> evalM val' >>= ensureTypeAtU "a definition at type U"
              _ -> pure ()
            pure val'
          let (mvalue, bodyErrors) = case result of
                Right val' -> (Just val', [])
                Left err   -> (Nothing, [err])
          renderResult <- attempt $ atCommand command $ do
            render <- asksContext (optRender . ctxOptions)
            case render of
              Just "latex" | Just _ <- mvalue -> issueTypeError $
                TypeErrorOther "\"latex\" rendering is not yet supported"
              _ -> pure ()
          let renderErrors = either pure (const []) renderResult
          withTopLevel (varIdentAt path name) tyC tyV mvalue EntryTopLevel used $ \decl ->
            checkCommands path (i + 1) total more $ \decls errs ->
              k (Foil.sink decl : decls) (bodyErrors <> renderErrors <> errs)

  command@(Rzk.CommandPostulate _loc name (Rzk.DeclUsedVars _ vars) params ty) : more ->
    announce (" Checking #postulate " <> Rzk.printTree name) $ atCommand command $ do
      typeResult <- attempt $ checkDeclType path vars params ty
      case typeResult of
        Left err -> skipping err path i total more k
        Right (used, tyC, tyV) ->
          withTopLevel (varIdentAt path name) tyC tyV Nothing EntryTopLevel used $ \decl ->
            checkCommands path (i + 1) total more $ \decls errs ->
              k (Foil.sink decl : decls) errs

  command@(Rzk.CommandCheck _loc term ty) : more ->
    announce (" Checking " <> Rzk.printTree term <> " : " <> Rzk.printTree ty) $ atCommand command $ do
      result <- attempt $ do
        tyTerm <- elaborate ty
        ty' <- atSurface ty $ check tyTerm universe
        tyV <- evalM ty'
        termTerm <- elaborate term
        _ <- atSurface term $ check termTerm tyV
        pure ()
      case result of
        Left err -> skipping err path i total more k
        Right () -> checkCommands path (i + 1) total more k

  Rzk.CommandCompute loc term : more ->
    checkCommands path i total (Rzk.CommandComputeWHNF loc term : more) k

  command@(Rzk.CommandComputeNF _loc term) : more ->
    announce (" Computing NF for " <> Rzk.printTree term) $ atCommand command $ do
      result <- attempt $ do
        (term', _) <- atSurface term $ elaborate term >>= infer
        ctx <- askContext
        pure (ppCore (namingOfContext ctx) (nfCore (ctxScope ctx) ctx term'))
      case result of
        Left err -> skipping err path i total more k
        Right shown -> announce ("  " <> shown) $ checkCommands path (i + 1) total more k

  command@(Rzk.CommandComputeWHNF _loc term) : more ->
    announce (" Computing WHNF for " <> Rzk.printTree term) $ atCommand command $ do
      result <- attempt $ do
        (term', _) <- atSurface term $ elaborate term >>= infer
        ctx <- askContext
        pure (ppCore (namingOfContext ctx) (quoteWhnfCore (ctxScope ctx) ctx term'))
      case result of
        Left err -> skipping err path i total more k
        Right shown -> announce ("  " <> shown) $ checkCommands path (i + 1) total more k

  command@(Rzk.CommandAssume _loc names ty) : more ->
    announce (" Checking #assume " <> unwords (map Rzk.printTree names)) $ atCommand command $ do
      typeResult <- attempt $ do
        tyTerm <- elaborate ty
        ty' <- atSurface ty $ check tyTerm universe
        (ty',) <$> evalM ty'
      case typeResult of
        Left err -> skipping err path i total more k
        Right (tyC, tyV) ->
          assume (map (varIdentAt path) names) tyC tyV $ \assumed ->
            checkCommands path (i + 1) total more $ \decls errs ->
              k (sinkDecls assumed <> decls) errs

  -- A malformed section stops the module, as does an error inside a
  -- section: the declarations after it are not checked.
  command@(Rzk.CommandSection _loc name) : more -> do
    split <- attempt $ atCommand command $ splitSectionCommands name more
    case split of
      Left err -> k [] [err]
      Right (sectionCommands, more') ->
        withSection (Just (Rzk.printTree name)) path (i + 1) total sectionCommands $ \sectionDecls sectionErrs ->
          case sectionErrs of
            [] -> checkCommands path (i + 1 + genericLength sectionCommands) total more' $ \decls errs ->
                    k (sinkDecls sectionDecls <> decls) errs
            _  -> k sectionDecls sectionErrs

  command@(Rzk.CommandSectionEnd _loc endName) : _more -> do
    err <- captureError $ atCommand command $ issueTypeError $ TypeErrorOther $
      "unexpected #end " <> Rzk.printTree endName <> ", no section was declared!"
    k [] [err]

  Rzk.CommandData{} : more -> unsupported "#data" more
  where
    announce :: String -> m l a -> m l a
    announce what = traceNormal ("[ " <> show i <> " out of " <> show total <> " ]" <> what)

    unsupported :: String -> [Rzk.Command] -> m n r
    unsupported what more = do
      result <- attempt $ issueTypeError (TypeErrorUnsupported what)
      case result of
        Left err -> skipping err path i total more k
        Right () -> checkCommands path (i + 1) total more k

-- | The commands of a section up to its @#end@, and the commands after it.
-- A nested section is merged into the enclosing one, as in the old checker.
splitSectionCommands
  :: (Checking m, Distinct n)
  => Rzk.SectionName -> [Rzk.Command] -> m n ([Rzk.Command], [Rzk.Command])
splitSectionCommands name = \case
  [] -> issueTypeError $ TypeErrorOther $
    "Section " <> Rzk.printTree name <> " is not closed with an #end"
  Rzk.CommandSection _loc name' : more -> do
    (cs1, cs2) <- splitSectionCommands name' more
    (cs3, cs4) <- splitSectionCommands name cs2
    pure (cs1 <> cs3, cs4)
  Rzk.CommandSectionEnd _loc endName : more -> do
    when (Rzk.printTree name /= Rzk.printTree endName) $
      issueTypeError $ TypeErrorOther $
        "unexpected #end " <> Rzk.printTree endName <> ", expecting #end " <> Rzk.printTree name
    pure ([], more)
  command : more -> do
    (cs1, cs2) <- splitSectionCommands name more
    pure (command : cs1, cs2)

-- | An error raised by a judgement, captured rather than propagated.
captureError :: Checking m => m n () -> m n (FailureOf m)
captureError action = attempt action >>= \case
  Left err -> pure err
  Right () -> panicImpossible "captureError: the judgement did not fail"

-- | Check the commands of a section, then close it.
withSection
  :: forall m n r. (Checking m, Distinct n)
  => Maybe String -> Maybe FilePath -> Integer -> Integer -> [Rzk.Command]
  -> (forall l. (DExt n l, Distinct l) => [Decl l] -> [FailureOf m] -> m l r)
  -> m n r
withSection name path i total commands k =
  localContext (\ctx -> ctx { ctxSections = Section name [] : ctxSections ctx }) $
    checkCommands path i total commands $ \_decls errs -> do
      outer <- askContext
      result <- attempt $ performing (ActionCloseSection name) (endSection errs)
      case result of
        -- the section is closed even so, so that the enclosing one does not
        -- process its entries again
        Left err ->
          localContext (\c -> c { ctxSections = drop 1 (ctxSections c) }) $
            k [] (errs <> [err])
        -- the rest of the module is not checked under the closing action
        Right (decls, errs', ctx') ->
          inContext ctx' { ctxDiag = ctxDiag outer, ctxLocation = ctxLocation outer } (k decls errs')

-- * Closing a section

-- | Close the innermost section: abstract each of its assumptions out of
-- the definitions that use it, report the assumptions and the @uses@ that
-- went unused, and take the assumptions out of scope.
--
-- The definitions stay in scope with their entries replaced: the type and
-- the body are abstracted over the assumptions with Π and λ, and later
-- uses of an abstracted definition are rewritten to its application to the
-- assumption. The assumptions' names stay in the scope index, as a scope
-- only grows, but leave the surface names and the list of what is bound,
-- so they can no longer be referred to or shown.
endSection
  :: forall m n. (Checking m, Distinct n)
  => [FailureOf m]
  -> m n ([Decl n], [FailureOf m], Context (DiagOf m) n)
endSection errs = do
  ctx <- askContext
  let scope = ctxScope ctx
      entries = [ (name, lookupEntry name ctx)    -- newest first
                | name <- case ctxSections ctx of
                    []    -> []
                    s : _ -> sectionEntries s ]
      isAssumptionEntry entry = entryKind entry == EntryAssumption
      assumptions = IntSet.fromList [ Foil.nameId name | (name, entry) <- entries, isAssumptionEntry entry ]
      isAssumption name = Foil.nameId name `IntSet.member` assumptions

      -- In a sketch, a hole may still come to use an assumption, so the
      -- unused-variable errors are not reported in a section with a hole.
      tolerateUnused = optHoles (ctxOptions ctx) /= HolesAreErrors
        && any (maybe False containsHole . entryValueCore . snd) entries
      reportUnused = null errs && not tolerateUnused

      typeCoreOf entry = fromMaybe (quoteFolded scope ctx (entryType entry)) (entryTypeCore entry)
      namesOf = maybe [] freeNamesOf

      -- The names an entry depends on: those its type and body mention,
      -- and those the types of these mention, transitively. The types are
      -- the ones written, read off the context.
      dependencies entry =
        closeUnderTypes IntSet.empty (namesOf (entryTypeCore entry) <> namesOf (entryValueCore entry))
      closeUnderTypes seen = \case
        [] -> seen
        name : more
          | Foil.nameId name `IntSet.member` seen -> closeUnderTypes seen more
          | otherwise -> closeUnderTypes (IntSet.insert (Foil.nameId name) seen)
              (freeNamesOf (typeCoreOf (lookupEntry name ctx)) <> more)

      -- Abstract an assumption out of the definitions (oldest first) that
      -- depend on it.
      abstractAssumption a aEntry = go
        where
          go [] = pure (False, [])
          go ((x, xEntry, changed) : xs) = do
            let written = lookupEntry x ctx
                mentioned = any (sameName a) (namesOf (entryTypeCore written) <> namesOf (entryValueCore written))
                declared = any (sameName a) (entryUses xEntry)
            if Foil.nameId a `IntSet.member` dependencies xEntry
              then do
                -- used, but neither written in the type or the body nor
                -- declared with @uses@
                when (not mentioned && not declared) $
                  localContext (\c -> c { ctxLocation = entryLocation xEntry <|> ctxLocation c }) $
                    issueTypeError (TypeErrorImplicitAssumption (a, entryType aEntry) x)
                let xs' = [ (y, applyToAssumption a x yEntry, True) | (y, yEntry, _) <- xs ]
                (_, xs'') <- go xs'
                pure (True, (x, abstractOver a aEntry xEntry, True) : xs'')
              else do
                (used, xs'') <- go xs
                pure (used, (x, xEntry, changed) : xs'')

      -- Give an entry the assumption as an explicit parameter.
      abstractOver a aEntry entry =
        let aType = typeCoreOf aEntry
            cube = isCube scope ctx (entryType aEntry)
            mshape = if cube then Just (quoteFolded scope ctx (topShapeValue scope (entryType aEntry))) else Nothing
            family = cube && isTopeUniverse (entryType entry)
            newType = abstractName scope (entryName aEntry) a (typeCoreOf entry) $ \pat body ->
              if family
                then TypeShape Id aType mshape
                else TypeFun Id aType mshape (ScopedAST pat body)
            abstractBody body = abstractName scope (entryName aEntry) a body $ \pat body' ->
              Lambda (Just (Param Id aType mshape)) (ScopedAST pat body')
        in entry
             { entryTypeCore = Just newType
             , entryValueCore = abstractBody <$> entryValueCore entry
             , entryModality = Id
             , entryUses = filter (not . sameName a) (entryUses entry)
             }

      -- Rewrite every use of a definition into its application to the
      -- assumption it has just been abstracted over.
      applyToAssumption a x entry = entry
        { entryTypeCore = Just (rewrite (typeCoreOf entry))
        , entryValueCore = rewrite <$> entryValueCore entry
        }
        where rewrite = substituteName scope x (App (Var x) (Var a))

      -- The entries newest first; the definitions met so far, oldest first.
      collect recent [] = pure (recent, [])
      collect recent ((name, entry) : more)
        | isAssumptionEntry entry = do
            (used, recent') <- abstractAssumption name entry recent
            unused <- if used || not reportUnused
              then pure []
              else fmap pure $ captureError $
                localContext (\c -> c { ctxLocation = entryLocation entry <|> ctxLocation c }) $
                  issueTypeError (TypeErrorUnusedVariable name (entryType entry))
            (kept, unusedMore) <- collect recent' more
            pure (kept, unused <> unusedMore)
        | otherwise = collect ((name, entry, False) : recent) more

  (kept, unusedAssumptions) <- collect [] entries

  -- an abstracted entry is evaluated again, in the context it is now in
  let reevaluate entry =
        let ty = evalCore scope ctx' (typeCoreOf entry)
        in entry
             { entryType = ty
             , entryValue = evalCore scope ctx' <$> entryValueCore entry
             }
      final = [ (name, if changed then reevaluate entry else entry) | (name, entry, changed) <- kept ]
      ctx1 = ctx
        { ctxSections = closeInnermost (ctxSections ctx)
        , ctxBound = filter (not . isAssumption) (ctxBound ctx)
        , ctxNamed = Map.filter (not . isAssumption) (ctxNamed ctx)
        }
      ctx' = foldr (uncurry insertEntry) ctx1 final

      -- The section's definitions outlive it, so the enclosing section
      -- adopts them, to abstract them over its own assumptions in turn.
      closeInnermost sections = case drop 1 sections of
        []       -> []
        s : rest -> s { sectionEntries = reverse (map fst final) <> sectionEntries s } : rest

      decls =
        [ Decl
            { declName = fromMaybe "_" (entryName entry)
            , declNameOf = name
            , declType = entryType entry
            , declValue = entryValue entry
            , declLocation = entryLocation entry <|> ctxLocation ctx
            }
        | (name, entry) <- final ]

  -- the uses that name an assumption of this section which went unused
  unusedUses <- if not reportUnused then pure [] else
    fmap concat $ forM final $ \(name, entry) ->
      case filter isAssumption (entryUses entry) of
        [] -> pure []
        unused -> fmap pure $ captureError $
          localContext (\c -> c { ctxLocation = entryLocation entry <|> ctxLocation c }) $
            issueTypeError (TypeErrorUnusedUsedVariables unused name)

  pure (decls, errs <> unusedAssumptions <> unusedUses, ctx')
  where
    sameName a b = Foil.nameId a == Foil.nameId b
    isTopeUniverse ty = case NbE.force ty of
      NbE.VNode UniverseTopeF -> True
      _                       -> False

-- | Report a command's error and go on to the next one.
skipping
  :: forall m n r. (Checking m, Distinct n)
  => FailureOf m -> Maybe FilePath -> Integer -> Integer -> [Rzk.Command]
  -> (forall l. (DExt n l, Distinct l) => [Decl l] -> [FailureOf m] -> m l r)
  -> m n r
skipping err path i total more k =
  checkCommands path (i + 1) total more $ \decls errs -> k decls (err : errs)

-- | A trace at normal verbosity.
traceNormal :: Checking m => String -> m n a -> m n a
traceNormal msg action = do
  verbosity <- asksContext (optVerbosity . ctxOptions)
  if verbosity <= Normal then trace msg action else action

-- | Check one module.
checkModule
  :: forall m n r. (Checking m, Distinct n)
  => Maybe FilePath -> Rzk.Module
  -> (forall l. (DExt n l, Distinct l) => [Decl l] -> [FailureOf m] -> m l r)
  -> m n r
checkModule path (Rzk.Module _loc _lang commands) k =
  withSection Nothing path 1 (genericLength commands) commands k

-- | Check a list of modules, one after another, in a scope that grows as
-- it goes. Checking stops at the first module with an error.
checkModules
  :: forall m n r. (Checking m, Distinct n)
  => [(FilePath, Rzk.Module)]
  -> (forall l. (DExt n l, Distinct l) => [(FilePath, [Decl l])] -> [FailureOf m] -> m l r)
  -> m n r
checkModules [] k = k [] []
checkModules ((path, m) : ms) k =
  traceNormal ("Checking module from " <> path) $
    withLocation (LocationInfo (Just path) Nothing Nothing) $
      checkModule (Just path) m $ \decls errs ->
        case errs of
          _ : _ -> k [(path, decls)] errs
          []    -> checkModules ms $ \rest errors ->
            k ((path, sinkDecls decls) : rest) errors

-- | The weak head normal form of a term, read back ('quoteWhnfFolded').
quoteWhnfCore :: Distinct n => Foil.Scope n -> Context d n -> Core n -> Core n
quoteWhnfCore scope ctx = quoteWhnfFolded scope ctx . evalCore scope ctx

pattern VNodeUniverse :: Whnf n
pattern VNodeUniverse <- NbE.VNode UniverseF
