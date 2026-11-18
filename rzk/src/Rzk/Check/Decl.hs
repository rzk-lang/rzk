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
-- @#data@ declarations elaborate to opaque top-level entries, with computation
-- driven by their data roles. Modal eliminators are not covered yet.
module Rzk.Check.Decl
  ( Decl (..)
  , checkModules
  , resolveNames
  ) where

import           Control.Applicative      ((<|>))
import           Control.Monad            (forM, forM_, when)
import           Control.Monad.Foil       (DExt, Distinct)
import qualified Control.Monad.Foil       as Foil
import           Control.Monad.Free.Foil  (AST (..), ScopedAST (..))
import           Data.List                (genericLength, nub)
import qualified Data.Map                 as Map
import           Data.Maybe               (fromMaybe)
import qualified FreeFoil.NbE             as NbE
import           Debug.Trace              (trace)

import           Language.Rzk.Foil.Names  (TModality (..),
                                           VarIdent, markUnresolved, varIdentAt)
import qualified Language.Rzk.Syntax      as Rzk
import           Rzk.Check.Context
import           Rzk.Check.Convert
import           Rzk.Check.Conversion     (convertTypes)
import           Rzk.Check.Display
import           Rzk.Check.Error
import           Rzk.Check.Monad
import           Rzk.Check.Rules
import           Rzk.Check.Syntax
import           Rzk.Check.Value
import           Rzk.TypeCheck.Decl       (duplicateBinders)
import           Rzk.TypeCheck.Decl.Data

-- | A top-level declaration, as checked.
data Decl n = Decl
  { declName     :: VarIdent
    -- ^ the surface name of the declaration
  , declNameOf   :: Foil.Name n
    -- ^ its scoped name
  , declType     :: Value n
    -- ^ its checked type
  , declValue    :: Maybe (Value n)
    -- ^ its checked value, or 'Nothing' for an assumption or failed body
  , declLocation :: Maybe LocationInfo
    -- ^ where the declaration was written
  }

instance Foil.Sinkable Decl where
  sinkabilityProof rename decl = decl
    { declNameOf = rename (declNameOf decl)
    , declType = Foil.sinkabilityProof rename (declType decl)
    , declValue = Foil.sinkabilityProof rename <$> declValue decl
    }

sinkDecls :: DExt n l => [Decl n] -> [Decl l]
sinkDecls = Foil.sink1

-- | Resolve surface names and convert a term to raw syntax. A known identifier
-- resolves to its scoped name; an unknown one is marked for the type checker to
-- report where it is used. Type elaboration happens later in 'check' or 'infer'.
resolveNames :: Distinct n => Rzk.Term -> Check n (Raw n)
resolveNames term = do
  ctx <- askContext
  scope <- askScope
  forM_ (hintsOf term) $ \(original, suggestion) ->
    recordHint $ \c -> SimplificationHint
      (Rzk.printTree original) (Rzk.printTree suggestion) (surfaceLocation original c)
  let env name = case lookupNamed name ctx of
        Just v  -> Var v
        Nothing -> rawNode noPosition (HoleF (Just (markUnresolved name)))
  pure (toRaw scope env term)

-- | Bind a top-level entry and run the rest of the module under it. The
-- entry keeps its elaborated type and body, and is recorded in the
-- innermost open section.
withTopLevel
  :: (Distinct n)
  => VarIdent -> Core n -> Value n -> Maybe (Core n) -> EntryKind -> [Foil.Name n]
  -> Maybe (DataRole n)
  -> (forall l. (DExt n l, Distinct l) => Decl l -> Check l r)
  -> Check n r
withTopLevel name tyCore ty mval kind uses mrole k = do
  checkTopLevelDuplicate name
  ctx <- askContext
  scope <- askScope
  Foil.withFresh scope $ \binder -> do
    let pat = PatternVar (Just name) binder
        scope' = Foil.extendScopePattern pat scope
        value = fmap (evalCore scope ctx) mval
        entry = Entry
          { entryType = Foil.sink ty
          , entryValue = Foil.sink1 value
          , entryModality = Id
          , entryLocks = Id
          , entryKind = kind
          , entryName = Just name
          , entryLocation = ctxLocation ctx
          , entryUses = Foil.sink1 uses
          , entryTypeCore = Just (Foil.sink tyCore)
          , entryValueCore = Foil.sink1 mval
          , entryDataRole = Foil.sink1 mrole
          }
        ctx' = recordInSection (Foil.nameOf binder) (enterPattern pat [entry] ctx)
        decl = Decl
          { declName = name
          , declNameOf = Foil.nameOf binder
          , declType = Foil.sink ty
          , declValue = Foil.sink1 value
          , declLocation = ctxLocation ctx
          }
    inContext (ContextWithScope scope' ctx') (k decl)

-- | Record a new entry in the innermost open section.
recordInSection :: Foil.Name n -> Context n -> Context n
recordInSection name ctx = ctx
  { ctxSections = case ctxSections ctx of
      []       -> [Section Nothing [name]]
      s : rest -> s { sectionEntries = name : sectionEntries s } : rest
  }

-- | Assume names of one type, each a top-level entry.
assume
  :: (Distinct n)
  => [VarIdent] -> Core n -> Value n
  -> (forall l. (DExt n l, Distinct l) => [Decl l] -> Check l r)
  -> Check n r
assume [] _ _ k = k []
assume (name : names) tyCore ty k =
  withTopLevel name tyCore ty Nothing EntryAssumption [] Nothing $ \decl ->
    assume names (Foil.sink tyCore) (Foil.sink ty) $ \decls ->
      k (Foil.sink decl : decls)

-- | The names a declaration says it uses.
usedNames :: Distinct n => Maybe FilePath -> [Rzk.VarIdent] -> Check n [Foil.Name n]
usedNames path vars = forM vars $ \var -> do
  ctx <- askContext
  let name = varIdentAt path var
  maybe (issueTypeError (TypeErrorUndefined name)) pure (lookupNamed name ctx)

checkTopLevelDuplicate :: Distinct n => VarIdent -> Check n ()
checkTopLevelDuplicate name = do
  ctx <- askContext
  case lookupNamed name ctx of
    Nothing -> pure ()
    Just v -> case entryName (lookupEntry v ctx) of
      Just previous -> issueTypeError (TypeErrorDuplicateTopLevel [previous] name)
      Nothing       -> issueTypeError (TypeErrorDuplicateTopLevel [] name)

-- * Commands

addParams :: [Rzk.Param] -> Rzk.Term -> Rzk.Term
addParams []     = id
addParams params = Rzk.Lambda Nothing params

-- | The type of a declaration with its parameters, checked: the names it
-- says it uses, the type as elaborated, and as a value.
checkDeclType
  :: (Distinct n)
  => Maybe FilePath -> [Rzk.VarIdent] -> [Rzk.Param] -> Rzk.Term
  -> Check n ([Foil.Name n], Core n, Value n)
checkDeclType path vars params ty = do
  used <- usedNames path vars
  paramDecls <- either (issueTypeError . TypeErrorOther) (pure . concat) (mapM paramDeclsOf params)
  tyTerm <- resolveNames (addParamDecls paramDecls ty)
  ty' <- atSurface ty $ check tyTerm universe
  (used, ty',) <$> evalM ty'

-- * Inductive declarations

checkGeneratedType :: Distinct n => Rzk.Term -> Check n (Core n, Value n)
checkGeneratedType surface = do
  raw <- resolveNames surface
  core <- atSurface surface $ check raw universe
  (core,) <$> evalM core

assumptionDepsOf :: Distinct n => Core n -> Check n [Foil.Name n]
assumptionDepsOf ty = do
  ctx <- askContext
  let close seen [] = seen
      close seen (x : xs)
        | x `elem` seen = close seen xs
        | otherwise = close (x : seen)
            (maybe [] freeNamesOf (entryTypeCore (lookupEntry x ctx)) <> xs)
      deep = close [] (freeNamesOf ty)
  pure [ x | x <- deep, entryKind (lookupEntry x ctx) == EntryAssumption ]


checkDataSortLayers :: Distinct n => [Rzk.ParamDecl] -> [SortIndex] -> Value n -> Check n ()
checkDataSortLayers paramDecls sortIndices = go positions
  where
    positions =
      [ (True, spelledParam p) | p <- paramDecls ]
        <> [ (False, Rzk.printTree (sortIndexType si)) | si <- sortIndices ]
    spelledParam = \case
      Rzk.ParamTermType _ _ ty -> Rzk.printTree ty
      p                        -> Rzk.printTree p
    go :: Distinct m => [(Bool, String)] -> Value m -> Check m ()
    go [] _ = pure ()
    go ((isParam, spelled) : rest) ty = do
      ctx <- askContext
      scope <- askScope
      case piOf ty of
        Just (Pi env md dom mshape ret) -> do
          let position
                | isParam   = "a parameter of a #data must be a type, CUBE, TOPE, or a tope family, got "
                | otherwise = "an index of a #data sort must be a type, got "
              reject kind = issueTypeError $ TypeErrorOther $
                position <> kind <> ": " <> spelled
          case mshape of
            Just _  -> reject "a shape"
            Nothing
              | isCube scope ctx dom -> reject "a cube"
              | otherwise -> case classifyType scope ctx dom of
                  DomainType -> pure ()
                  DomainCubeUniverse | isParam -> pure ()
                  DomainTopeUniverse | isParam -> pure ()
                  DomainTopeFamily   | isParam -> pure ()
                  DomainModalTope    | isParam -> pure ()
                  kind -> reject (dataDomainKindName kind)
          underFreshVar scope ctx md dom mshape $ \scope' ctx' x ->
            inContext (ContextWithScope scope' ctx') $
              go rest (codomainAt scope' ctx' (Foil.sink env) ret (NbE.vvar x))
        Nothing -> panicImpossible "#data sort has fewer Π binders than parameters and indices"

checkDataConFieldLayers
  :: Distinct n
  => Rzk.VarIdent -> Int -> [Rzk.ParamDecl] -> Value n -> Check n ()
checkDataConFieldLayers cname numParams fieldDecls =
  go (replicate numParams Nothing <> map fieldInfo fieldDecls)
  where
    fieldInfo d = Just (isModalTypeSpelling d, spelledField d)
    isModalTypeSpelling = \case
      Rzk.ParamTermModalType{} -> True
      _                        -> False
    spelledField = \case
      Rzk.ParamTermType _ _ ty -> Rzk.printTree ty
      d                        -> Rzk.printTree d
    go :: Distinct m => [Maybe (Bool, String)] -> Value m -> Check m ()
    go [] _ = pure ()
    go (checkInfo : rest) ty = do
      ctx <- askContext
      scope <- askScope
      case piOf ty of
        Just (Pi env md dom mshape ret) -> do
          forM_ checkInfo $ \(modalSpelled, spelled) -> case mshape of
            Just _ -> pure ()
            Nothing
              | isCube scope ctx dom -> pure ()
              | otherwise -> case classifyType scope ctx dom of
                  DomainType
                    | modalSpelled -> issueTypeError $ TypeErrorOther $
                        "modal type fields are not supported yet in constructor " <> Rzk.printTree cname
                    | otherwise -> pure ()
                  kind -> issueTypeError $ TypeErrorOther $
                    "a field of constructor " <> Rzk.printTree cname
                      <> " must be a type, a cube point, or a shape, got "
                      <> dataDomainKindName kind <> ": " <> spelled
          underFreshVar scope ctx md dom mshape $ \scope' ctx' x ->
            inContext (ContextWithScope scope' ctx') $
              go rest (codomainAt scope' ctx' (Foil.sink env) ret (NbE.vvar x))
        Nothing -> panicImpossible "constructor type has fewer Π binders than parameters and fields"

dataDomainKindName :: DomainKind -> String
dataDomainKindName = \case
  DomainType            -> "a type"
  DomainCubeUniverse    -> "the cube universe"
  DomainCubeFamily      -> "a cube family"
  DomainTopeUniverse    -> "the tope universe"
  DomainTopeFamily      -> "a tope family"
  DomainOtherTopeFamily -> "a tope family over a non-cube domain"
  DomainModalTope       -> "a modal cube or tope kind"

withDataDecls
  :: forall n r. Distinct n
  => Maybe FilePath
  -> [Foil.Name n]
  -> Rzk.VarIdent
  -> [Rzk.VarIdent]
  -> [Rzk.ParamDecl]
  -> [SortIndex]
  -> [DataConSurface]
  -> [Rzk.DataElim]
  -> (forall l. (DExt n l, Distinct l) => [Decl l] -> Check l r)
  -> Check n r
withDataDecls path used name paramVars paramDecls sortIndices consData elims k = do
  let sortTerm = foldr wrapIndex (Rzk.Universe Nothing) sortIndices
      wrapIndex (SortIndex mv ty) body = case mv of
        Just v  -> surfacePi v ty body
        Nothing -> surfaceArrow ty body
  (dTyC, dTyV) <- checkGeneratedType (addParamDecls paramDecls sortTerm)
  checkDataSortLayers paramDecls sortIndices dTyV
  dDeps <- assumptionDepsOf dTyC
  withTopLevel (varIdentAt path name) dTyC dTyV Nothing EntryTopLevel
    (nub (used <> dDeps)) Nothing $ \dDecl ->
      bindCons (declNameOf dDecl) (Foil.sink1 used) 0 consData [Foil.sink dDecl] $
        \dName usedL declsAcc -> bindElims dName usedL declsAcc
  where
    numParams  = length paramDecls
    numMethods = length consData
    indexArity = length sortIndices

    bindCons
      :: forall m. DExt n m
      => Foil.Name m -> [Foil.Name m] -> Int -> [DataConSurface] -> [Decl m]
      -> (forall l. (DExt n l, Distinct l)
            => Foil.Name l -> [Foil.Name l] -> [Decl l] -> Check l r)
      -> Check m r
    bindCons dName usedHere _index [] acc kk = kk dName usedHere acc
    bindCons dName usedHere index (con : rest) acc kk = do
      probe <- stripPositions <$> resolveNames (dataConProbe con)
      let dOccursIn t = dName `elem` freeNamesOf t
      when (dOccursIn probe) $ do
        offending <- forM (dataConNonRec con) $ \ty -> do
          t <- stripPositions <$> resolveNames ty
          pure (ty, dOccursIn t)
        funRec <- case [ ty | (ty, True) <- offending ] of
          ty : _ -> do
            let (domains, codomain) = surfacePiSpine ty
            domainsClean <- mapM (fmap (not . dOccursIn . stripPositions) . resolveNames) domains
            pure (matchesDataApplied name paramVars indexArity codomain && and domainsClean)
          [] -> pure False
        issueTypeError $ TypeErrorOther $ if funRec
          then "constructor " <> Rzk.printTree (dataConName con)
            <> " has a function-typed recursive field; only directly recursive fields are supported for now"
          else "constructor " <> Rzk.printTree (dataConName con)
            <> " is not strictly positive: the declared type may occur in a field only as the type of the whole field"
      when (containsUniverse probe) $ recordWarning $ \ctx ->
        LargeInductiveTypeWarning
          (varIdentAt path name)
          (varIdentAt path (dataConName con))
          (ctxLocation ctx)
      (conTyC, conTyV) <- checkGeneratedType (dataConType con)
      checkDataConFieldLayers (dataConName con) numParams (dataConFields con) conTyV
      conDeps <- assumptionDepsOf conTyC
      let conSort = case dataConSort con of
            DataConPoint  -> PointCon
            DataConPath{} -> PathCon
          role = DataRole dName numParams
            (DataConKind conSort index (length (dataConFields con))
              (map fst (dataConRecursive con)))
      withTopLevel (varIdentAt path (dataConName con)) conTyC conTyV Nothing EntryTopLevel
        (nub (usedHere <> conDeps)) (Just role) $ \conDecl ->
          bindCons (Foil.sink dName) (Foil.sink1 usedHere) (index + 1) rest
            (sinkDecls acc <> [conDecl]) kk

    bindElims
      :: forall m. DExt n m
      => Foil.Name m -> [Foil.Name m] -> [Decl m] -> Check m r
    bindElims dName usedHere declsAcc = do
      ctx <- askContext
      let isBound cand = case lookupNamed (varIdentAt path cand) ctx of
            Nothing -> False
            Just _  -> True
          avoid = identTokenOf name
            : map identTokenOf paramVars
            <> [ identTokenOf v | SortIndex (Just v) _ <- sortIndices ]
            <> concatMap dataConLocalNames consData
      let freshIx = freshIdentsWith isBound (length [ () | SortIndex Nothing _ <- sortIndices ]) avoid "i"
      let indexVars = go sortIndices freshIx
            where
              go [] _ = []
              go (SortIndex (Just v) _ : rest) fresh = v : go rest fresh
              go (SortIndex Nothing _ : rest) (f : fresh) = f : go rest fresh
              go (SortIndex Nothing _ : _) [] =
                panicImpossible "not enough fresh index names"
          indexDecls =
            [ Rzk.ParamTermType Nothing (surfaceVar v) (sortIndexType si)
            | (v, si) <- zip indexVars sortIndices ]
          avoid' = map identTokenOf indexVars <> avoid
      let motiveV = freshIdentWith isBound avoid' "C"
          scrutV = freshIdentWith isBound (identTokenOf motiveV : avoid') "x"
      let maxRec = maximum (0 : map (length . dataConRecursive) consData)
          ihNames = freshIdentsWith isBound maxRec
            (identTokenOf motiveV : identTokenOf scrutV : avoid') "ih"
      let hasPaths = or [ True | DataConPath{} <- map dataConSort consData ]
          avoidM = map identTokenOf (motiveV : scrutV : ihNames) <> avoid'
          methodVars =
            if not hasPaths then Nothing else Just $
              let go _ [] = []
                  go av (con : rest) =
                    let Rzk.VarIdentToken t = identTokenOf (dataConName con)
                        m = freshIdentWith isBound av ("m-" <> t)
                     in m : go (identTokenOf m : av) rest
               in go avoidM consData
      let avoidM' = maybe [] (map identTokenOf) methodVars <> avoidM
      let endpointV = freshIdentWith isBound avoidM' "y"
          pathV = freshIdentWith isBound (identTokenOf endpointV : avoidM') "q"
          transportV = freshIdentWith isBound
            (identTokenOf endpointV : identTokenOf pathV : avoidM') "y"
      pathData <- either (issueTypeError . TypeErrorOther) pure $
        pathEndpointImages name paramVars consData ihNames methodVars
      let terms = elimTerms ElimSpec
            { esName = name, esModality = Id
            , esParamVars = paramVars, esParamDecls = paramDecls
            , esIndexVars = indexVars, esIndexDecls = indexDecls
            , esMotiveV = motiveV, esScrutV = scrutV, esIhNames = ihNames
            , esMethodVars = methodVars
            , esEndpointV = endpointV, esPathV = pathV, esTransportV = transportV
            , esConsData = consData, esPathData = pathData }
          generated =
            [ (ElimInd, elimIdent "ind" Id name, indTypeTerm terms)
            , (ElimRec, elimIdent "rec" Id name, recTypeTerm terms) ]
          computes = computeRules terms
      (elimReasc, computeReasc) <- either (issueTypeError . TypeErrorOther) pure $
        splitDataClauses name
        [ n | (_, n, _) <- generated ] (map fst computes) elims
      bindEliminators dName usedHere elimReasc computeReasc computes generated declsAcc

    bindEliminators
      :: forall m. DExt n m
      => Foil.Name m -> [Foil.Name m]
      -> [(Rzk.VarIdentToken, Rzk.Term)]
      -> [(Rzk.VarIdentToken, Rzk.Term)]
      -> [(Rzk.VarIdent, Rzk.Term)]
      -> [(ElimKind, Rzk.VarIdent, Rzk.Term)] -> [Decl m] -> Check m r
    bindEliminators _dName usedHere _elimReasc computeReasc computes [] acc =
      bindComputes usedHere computeReasc computes acc
    bindEliminators dName usedHere elimReasc computeReasc computes
        ((kind, elimName, elimTy) : rest) acc = do
      canonical <- checkGeneratedType elimTy
      (tyC, tyV) <- reascribe elimName canonical (lookup (identTokenOf elimName) elimReasc)
      deps <- assumptionDepsOf tyC
      withTopLevel (varIdentAt path elimName) tyC tyV Nothing EntryTopLevel
        (nub (usedHere <> deps))
        (Just (DataRole dName numParams (DataElimKind Id numMethods indexArity kind))) $ \decl ->
          bindEliminators (Foil.sink dName) (Foil.sink1 usedHere)
            elimReasc computeReasc computes rest (sinkDecls acc <> [decl])

    bindComputes
      :: forall m. DExt n m
      => [Foil.Name m] -> [(Rzk.VarIdentToken, Rzk.Term)]
      -> [(Rzk.VarIdent, Rzk.Term)] -> [Decl m] -> Check m r
    bindComputes _used _reasc [] acc = k acc
    bindComputes usedHere reasc ((cname, cty) : rest) acc = do
      canonical <- checkGeneratedType cty
      (tyC, tyV) <- reascribe cname canonical (lookup (identTokenOf cname) reasc)
      deps <- assumptionDepsOf tyC
      withTopLevel (varIdentAt path cname) tyC tyV Nothing EntryTopLevel
        (nub (usedHere <> deps)) Nothing $ \decl ->
          bindComputes (Foil.sink1 usedHere) reasc rest (sinkDecls acc <> [decl])

    reascribe
      :: Distinct m
      => Rzk.VarIdent -> (Core m, Value m) -> Maybe Rzk.Term -> Check m (Core m, Value m)
    reascribe _entryName canonical Nothing = pure canonical
    reascribe generatedName (_, canonicalV) (Just ty) = do
      actual@(_, actualV) <- checkGeneratedType ty
      result <- attempt (convertTypes canonicalV actualV)
      case result of
        Left _ -> issueTypeError $
          TypeErrorReascribedTypeMismatch (varIdentAt path generatedName) canonicalV actualV
        Right () -> pure actual

setOption :: Distinct n => String -> String -> Check n a -> Check n a
setOption name value = case name of
  "verbosity" -> case value of
    "debug"  -> withOptions (\o -> o { optVerbosity = Debug })
    "normal" -> withOptions (\o -> o { optVerbosity = Normal })
    "silent" -> withOptions (\o -> o { optVerbosity = Silent })
    _ -> const $ issueTypeError $ TypeErrorOther
      "unknown verbosity level (use \"debug\", \"normal\", or \"silent\")"
  "warn-overhang" -> yesNo $ \b o -> o { optWarnOverhang = b }
  "warn-tope-family-domain" -> yesNo $ \b o -> o { optWarnTopeFamilyDomain = b }
  "render" -> case value of
    "svg"   -> withOptions (\o -> o { optRender = Just "svg" })
    "latex" -> withOptions (\o -> o { optRender = Just "latex" })
    "none"  -> withOptions (\o -> o { optRender = Nothing })
    _ -> const $ issueTypeError $ TypeErrorOther
      "unknown render backend (use \"svg\", \"latex\", or \"none\")"
  _ | Just outcome <- ignoredFragmentOption name value -> case outcome of
      Left msg -> const $ issueTypeError $ TypeErrorOther msg
      Right warn -> if warn then warnIgnoredOption name else id
  _ | name `elem` ignoredOptions -> warnIgnoredOption name
  _ -> const $ issueTypeError $ TypeErrorOther ("unknown option " <> show name)
  where
    yesNo set = case value of
      "yes" -> withOptions (set True)
      "no"  -> withOptions (set False)
      _ -> const $ issueTypeError $ TypeErrorOther
        ("unknown value for " <> show name <> " (use \"yes\" or \"no\")")

unsetOption :: Distinct n => String -> Check n a -> Check n a
unsetOption name = case name of
  "verbosity" -> withOptions (\o -> o { optVerbosity = optVerbosity defaultOptions })
  "warn-overhang" -> withOptions (\o -> o { optWarnOverhang = optWarnOverhang defaultOptions })
  "warn-tope-family-domain" ->
    withOptions (\o -> o { optWarnTopeFamilyDomain = optWarnTopeFamilyDomain defaultOptions })
  "render" -> withOptions (\o -> o { optRender = Nothing })
  _ | name `elem` ignoredOptions -> warnIgnoredOption name
  _ -> const $ issueTypeError $ TypeErrorOther ("unknown option " <> show name)

-- | The options of checks the new checker does not run yet: accepted, and
-- without effect.
ignoredOptions :: [String]
ignoredOptions =
  [ "render-hide-term", "rstt-safe", "warn-meta-prefix"
  , "warn-free-standing-restriction", "warn-shape-dependency", "warn-meta-binder" ]

-- | Disabled fragment options need no warning: ignoring them has exactly the
-- requested effect. Enabled ones are accepted with a warning until the checks
-- are implemented. Invalid values are rejected as in the classic checker.
ignoredFragmentOption :: String -> String -> Maybe (Either String Bool)
ignoredFragmentOption "rstt-safe" = \case
  "off"   -> Just (Right False)
  "warn"  -> Just (Right True)
  "error" -> Just (Right True)
  _ -> Just (Left "unknown rstt-safe mode (use warn, error, or off)")
ignoredFragmentOption "warn-meta-prefix" = \case
  "off"        -> Just (Right False)
  "structural" -> Just (Right True)
  "strict"     -> Just (Right True)
  _ -> Just (Left "unknown value for \"warn-meta-prefix\" (use \"off\", \"structural\", or \"strict\")")
ignoredFragmentOption name = case name of
  "warn-meta-binder"              -> yesNo
  "warn-free-standing-restriction" -> yesNo
  "warn-shape-dependency"         -> yesNo
  _ -> const Nothing
  where
    yesNo = \case
      "no"  -> Just (Right False)
      "yes" -> Just (Right True)
      _ -> Just (Left ("unknown value for " <> show name <> " (use \"yes\" or \"no\")"))

-- | Record that an accepted option has no effect in the new checker.
warnIgnoredOption :: String -> Check n a -> Check n a
warnIgnoredOption name action = do
  recordWarning $ \ctx -> UnsupportedOptionWarning name (ctxLocation ctx)
  action

-- | Run a check at a command: the location points at the command, and a
-- judgement inside it narrows it to the sub-term it is about.
atCommand :: Rzk.Command -> Check n a -> Check n a
atCommand command = localContext $ \ctx -> ctx { ctxLocation = at <$> ctxLocation ctx }
  where
    at loc = loc { locationLine = fst <$> pos, locationColumn = snd <$> pos }
    pos = Rzk.hasPosition command

-- | Run a check with the location narrowed to where a surface term was
-- written.
atSurface :: (Rzk.HasPosition a) => a -> Check n b -> Check n b
atSurface x = localContext $ \ctx -> ctx { ctxLocation = surfaceLocation x ctx }

-- | The location of a surface term in the current file.
surfaceLocation :: (Rzk.HasPosition a) => a -> Context n -> Maybe LocationInfo
surfaceLocation x ctx = narrow <$> ctxLocation ctx
  where
    narrow loc = case Rzk.hasPosition x of
      Nothing          -> loc
      Just (line, col) -> loc { locationLine = Just line, locationColumn = Just col }

-- * The module driver

-- | Check a module's commands, extending the scope with each definition.
-- The continuation runs in the final scope with the declarations and the
-- errors found.
checkCommands
  :: forall n r. (Distinct n)
  => Maybe FilePath -> Integer -> Integer -> [Rzk.Command]
  -> (forall l. (DExt n l, Distinct l) => [Decl l] -> [ErrorInContext] -> Check l r)
  -> Check n r
checkCommands path i total commands k = case commands of
  [] -> k [] []
  command : more | (dup, previous) : _ <- duplicateBinders command -> do
    result <- attempt $ atCommand command $ issueTypeError (TypeErrorRepeatedBinder dup previous)
    case result of
      Left err -> skipping err path i total more k
      Right () -> checkCommands path (i + 1) total more k

  command@(Rzk.CommandUnsetOption _loc optionName) : more ->
    announce ("Unsetting option " <> optionName) $ do
      result <- attempt $ atCommand command $ unsetOption optionName (pure ())
      case result of
        Left err -> skipping err path i total more k
        Right () -> atCommand command $ unsetOption optionName $ checkCommands path (i + 1) total more k

  command@(Rzk.CommandSetOption _loc optionName optionValue) : more ->
    announce ("Setting option " <> optionName <> " = " <> optionValue) $ do
      result <- attempt $ atCommand command $ setOption optionName optionValue (pure ())
      case result of
        Left err -> skipping err path i total more k
        Right () -> atCommand command $ setOption optionName optionValue $ checkCommands path (i + 1) total more k

  command@(Rzk.CommandDefine _loc name (Rzk.DeclUsedVars _ vars) params ty term) : more ->
    announce (" Checking #define " <> Rzk.printTree name) $ atCommand command $ do
      typeResult <- attempt $ checkDeclType path vars params ty
      case typeResult of
        Left err -> skipping err path i total more k
        Right (used, tyC, tyV) -> do
          -- the body is checked on its own, so that a definition whose
          -- type is fine but whose proof is not still enters the scope
          result <- attempt $ do
            valTerm <- resolveNames (addParams params term)
            val' <- atSurface term $ check valTerm tyV
            case force tyV of
              VUniverse -> evalM val' >>= ensureTypeAtU "a definition at type U"
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
          withTopLevel (varIdentAt path name) tyC tyV mvalue EntryTopLevel used Nothing $ \decl ->
            checkCommands path (i + 1) total more $ \decls errs ->
              k (Foil.sink decl : decls) (bodyErrors <> renderErrors <> errs)

  command@(Rzk.CommandPostulate _loc name (Rzk.DeclUsedVars _ vars) params ty) : more ->
    announce (" Checking #postulate " <> Rzk.printTree name) $ atCommand command $ do
      typeResult <- attempt $ checkDeclType path vars params ty
      case typeResult of
        Left err -> skipping err path i total more k
        Right (used, tyC, tyV) ->
          withTopLevel (varIdentAt path name) tyC tyV Nothing EntryTopLevel used Nothing $ \decl ->
            checkCommands path (i + 1) total more $ \decls errs ->
              k (Foil.sink decl : decls) errs

  command@(Rzk.CommandCheck _loc term ty) : more ->
    announce (" Checking " <> Rzk.printTree term <> " : " <> Rzk.printTree ty) $ atCommand command $ do
      result <- attempt $ do
        tyTerm <- resolveNames ty
        ty' <- atSurface ty $ check tyTerm universe
        tyV <- evalM ty'
        termTerm <- resolveNames term
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
        (term', _) <- atSurface term $ resolveNames term >>= infer
        ctx <- askContext
        scope <- askScope
        pure (ppCore (namingOfContext ctx) (nfCore scope ctx term'))
      case result of
        Left err -> skipping err path i total more k
        Right shown -> announce ("  " <> shown) $ checkCommands path (i + 1) total more k

  command@(Rzk.CommandComputeWHNF _loc term) : more ->
    announce (" Computing WHNF for " <> Rzk.printTree term) $ atCommand command $ do
      result <- attempt $ do
        (term', _) <- atSurface term $ resolveNames term >>= infer
        ctx <- askContext
        scope <- askScope
        pure (ppCore (namingOfContext ctx) (quoteWhnfCore scope ctx term'))
      case result of
        Left err -> skipping err path i total more k
        Right shown -> announce ("  " <> shown) $ checkCommands path (i + 1) total more k

  command@(Rzk.CommandAssume _loc names ty) : more ->
    announce (" Checking #assume " <> unwords (map Rzk.printTree names)) $ atCommand command $ do
      typeResult <- attempt $ do
        tyTerm <- resolveNames ty
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

  command@(Rzk.CommandData _loc name (Rzk.DeclUsedVars _ vars) params sort body) : more ->
    announce (" Checking #data " <> Rzk.printTree name) $ atCommand command $ do
      result <- attempt $ do
        used <- usedNames path vars
        sortIndices <- either (issueTypeError . TypeErrorOther) pure (dataSortIndices sort)
        let (cons, elims) = dataBodyParts body
        paramVars <- either (issueTypeError . TypeErrorOther) pure (dataParamVars params)
        paramDecls <- either (issueTypeError . TypeErrorOther) (pure . concat)
          (mapM paramDeclsOf params)
        consData <- either (issueTypeError . TypeErrorOther) pure $
          mapM (dataConSurface name paramVars paramDecls (length sortIndices)) cons
        withDataDecls path used name paramVars paramDecls sortIndices consData elims $ \decls ->
          checkCommands path (i + 1) total more $ \moreDecls errs ->
            k (sinkDecls decls <> moreDecls) errs
      case result of
        Left err -> k [] [err]
        Right r  -> pure r
  where
    announce :: String -> Check l a -> Check l a
    announce what = traceNormal ("[ " <> show i <> " out of " <> show total <> " ]" <> what)

-- | The commands of a section up to its @#end@, and the commands after it.
-- A nested section is merged into the enclosing one, as in the old checker.
splitSectionCommands
  :: (Distinct n)
  => Rzk.SectionName -> [Rzk.Command] -> Check n ([Rzk.Command], [Rzk.Command])
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
captureError :: Check n () -> Check n (ErrorInContext)
captureError action = attempt action >>= \case
  Left err -> pure err
  Right () -> panicImpossible "captureError: the judgement did not fail"

-- | Check the commands of a section, then close it.
withSection
  :: forall n r. (Distinct n)
  => Maybe String -> Maybe FilePath -> Integer -> Integer -> [Rzk.Command]
  -> (forall l. (DExt n l, Distinct l) => [Decl l] -> [ErrorInContext] -> Check l r)
  -> Check n r
withSection name path i total commands k =
  localContext (\ctx -> ctx { ctxSections = Section name [] : ctxSections ctx }) $
    checkCommands path i total commands $ \_decls errs -> do
      outer <- askContext
      scope <- askScope
      result <- attempt $ performing (ActionCloseSection name) (endSection errs)
      case result of
        -- the section is closed even so, so that the enclosing one does not
        -- process its entries again
        Left err ->
          localContext (\c -> c { ctxSections = drop 1 (ctxSections c) }) $
            k [] (errs <> [err])
        -- the rest of the module is not checked under the closing action
        Right (decls, errs', ctx') ->
          inContext (ContextWithScope scope (ctx' { ctxDiag = ctxDiag outer, ctxLocation = ctxLocation outer })) (k decls errs')

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
  :: forall n. (Distinct n)
  => [ErrorInContext]
  -> Check n ([Decl n], [ErrorInContext], Context n)
endSection errs = do
  ctx <- askContext
  scope <- askScope
  let entries = [ (name, lookupEntry name ctx)    -- newest first
                | name <- case ctxSections ctx of
                    []    -> []
                    s : _ -> sectionEntries s ]
      isAssumptionEntry entry = entryKind entry == EntryAssumption
      assumptions = Foil.nameSetFromList [ name | (name, entry) <- entries, isAssumptionEntry entry ]
      isAssumption name = Foil.nameSetMember name assumptions

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
        closeUnderTypes Foil.emptyNameSet (namesOf (entryTypeCore entry) <> namesOf (entryValueCore entry))
      closeUnderTypes seen = \case
        [] -> seen
        name : more
          | Foil.nameSetMember name seen -> closeUnderTypes seen more
          | otherwise -> closeUnderTypes (Foil.nameSetInsert name seen)
              (freeNamesOf (typeCoreOf (lookupEntry name ctx)) <> more)

      -- Abstract an assumption out of the definitions (oldest first) that
      -- depend on it.
      abstractAssumption a aEntry = go
        where
          forcedDataTypes =
            [ dataRoleDataType role
            | (_, dataEntry) <- entries
            , Just role <- [entryDataRole dataEntry]
            , Foil.nameSetMember a (dependencies dataEntry) ]
          forcedFamily entry = maybe False
            ((`elem` forcedDataTypes) . dataRoleDataType) (entryDataRole entry)
          go [] = pure (False, [])
          go ((x, xEntry, changed) : xs) = do
            let written = lookupEntry x ctx
                mentioned = any (sameName a) (namesOf (entryTypeCore written) <> namesOf (entryValueCore written))
                declared = any (sameName a) (entryUses xEntry)
            if x `elem` forcedDataTypes || forcedFamily xEntry
              || Foil.nameSetMember a (dependencies xEntry)
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
             , entryDataRole = bumpDataRoleParams <$> entryDataRole entry
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
    sameName a b = a == b
    isTopeUniverse ty = case NbE.force ty of
      VUniverseTope -> True
      _                       -> False

-- | Report a command's error and go on to the next one.
skipping
  :: forall n r. (Distinct n)
  => ErrorInContext -> Maybe FilePath -> Integer -> Integer -> [Rzk.Command]
  -> (forall l. (DExt n l, Distinct l) => [Decl l] -> [ErrorInContext] -> Check l r)
  -> Check n r
skipping err path i total more k =
  checkCommands path (i + 1) total more $ \decls errs -> k decls (err : errs)

-- | A trace at normal verbosity.
traceNormal :: String -> Check n a -> Check n a
traceNormal msg action = do
  verbosity <- asksContext (optVerbosity . ctxOptions)
  if verbosity <= Normal then trace msg action else action

-- | Check one module.
checkModule
  :: forall n r. (Distinct n)
  => Maybe FilePath -> Rzk.Module
  -> (forall l. (DExt n l, Distinct l) => [Decl l] -> [ErrorInContext] -> Check l r)
  -> Check n r
checkModule path (Rzk.Module _loc _lang commands) k =
  withSection Nothing path 1 (genericLength commands) commands k

-- | Check a list of modules, one after another, in a scope that grows as
-- it goes. Checking stops at the first module with an error.
checkModules
  :: forall n r. (Distinct n)
  => [(FilePath, Rzk.Module)]
  -> (forall l. (DExt n l, Distinct l) => [(FilePath, [Decl l])] -> [ErrorInContext] -> Check l r)
  -> Check n r
checkModules [] k = k [] []
checkModules ((path, m) : ms) k =
  traceNormal ("Checking module from " <> path) $
    withLocation (LocationInfo (Just path) Nothing Nothing) $
      checkModule (Just path) m $ \decls errs ->
        case errs of
          _ : _ -> k [(path, decls)] errs
          []    -> checkModules ms $ \rest errors ->
            k ((path, sinkDecls decls) : rest) errors

