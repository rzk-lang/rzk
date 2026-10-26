{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Diagnostics for declarations outside the RSTT fragment.
--
-- For RSTT, see Riehl and Shulman,
-- <https://arxiv.org/abs/1705.07442 RS17> (2017), Figure 4 and Appendix A.2 for extension types.
-- For the fragment conditions, see Kudasov, Sim and Ahrens,
-- <https://arxiv.org/abs/2607.12207 KSA26> (2026), §5.
--
-- * Free-standing restrictions in assumptions, motives, and types passed as data.
-- * Cube domains, shape conditions, and assumed boundaries depending on outer points.
-- * Schematic variables bound inside terms below the parameter prefix.
-- * Modal constructs, auxiliary intervals, and involutions.
-- * Inductive constructors and eliminators.
-- * Any syntax outside the explicit RSTT allow-list.
--
-- Free-standing restrictions along concluded codomains are allowed.
-- Assumptions and postulates remain trusted; universe levels are not checked.
module Rzk.TypeCheck.Fragment.RSTT (
  fragmentHead,
  recordFragmentUses,
  recordSyntaxUses,
) where

import           Control.Applicative      ((<|>))
import           Control.Monad            (forM_, unless, when)
import           Control.Monad.Except     (catchError)
import           Control.Monad.Reader     (asks, local)
import           Data.Bifoldable          (bifoldMap)
import           Data.List                (intercalate)
import           Data.Maybe               (fromMaybe)
import qualified Data.IntSet              as IntSet
import qualified Control.Monad.Foil       as Foil

import           Control.Monad.Foil       (Distinct)
import           Control.Monad.Free.Foil  (AST (Node, Var), ScopedAST (..))

import           Control.Monad.Free.Foil.Annotated (AnnSig (..))
import           Language.Rzk.Foil.Names  (Binder, TModality (..),
                                           TypeInfo (..), VarIdent, binderName)
import           Language.Rzk.Foil.Syntax
import           Rzk.TypeCheck.Context
import           Rzk.TypeCheck.Display
import           Rzk.TypeCheck.Eval
import           Rzk.TypeCheck.Error
import           Rzk.TypeCheck.MetaPrefix (isMetaType)
import           Rzk.TypeCheck.Monad

-- Distinguish concluded types, assumed types, and terms (including type-valued data).
data CheckedPosition
  = InTail
  | InAssumption FragmentUse
  | InTerm PrefixPosition

-- A schematic argument may have its own parameter prefix.
data PrefixPosition = InParameterPrefix | OutsideParameterPrefix
  deriving (Eq)

-- | Report restriction and schematic-binder violations in a declaration.
-- Violations are errors when @rstt-safe = "error"@.
recordFragmentUses
  :: forall n. Distinct n
  => VarIdent          -- ^ the declaration
  -> TermT n           -- ^ its type
  -> Maybe (TermT n)   -- ^ its value, for a definition
  -> Bool              -- ^ does it supply an assumption rather than a proof?
  -> TypeCheck n ()
recordFragmentUses defName ty mval isAssumption = do
  restrictions <- asks ctxWarnFreeStandingRestriction
  binders <- asks ctxWarnMetaBinder
  shapes <- asks ctxWarnShapeDependency
  when (restrictions || binders || shapes) $
    localVerbosity Silent $
      forM_ [SourceTypes, ComputedTypes] $ \view ->
        checkFragmentUses view defName ty mval isAssumption `catchError`
          reportIncompleteRSTTCheck ("RSTT fragment check incomplete in " <> show defName)

-- Keep source checks even when reduction discards an argument or annotation.
data FragmentView = SourceTypes | ComputedTypes
  deriving (Eq)

checkFragmentUses
  :: forall n. Distinct n
  => FragmentView -> VarIdent -> TermT n -> Maybe (TermT n) -> Bool -> TypeCheck n ()
checkFragmentUses view defName ty mval isAssumption = do
  go (if isAssumption then InAssumption UseBinder else InTail) ty
  mapM_ (go (InTerm InParameterPrefix)) mval
  where
    go :: forall l. Distinct l => CheckedPosition -> TermT l -> TypeCheck l ()
    go (InTerm prefixPosition) t = goTerm prefixPosition t
    go InTail t = goTail t
    go (InAssumption use) t = goAssumed use t

    expose :: forall l. Distinct l => TermT l -> TypeCheck l (TermT l)
    expose = case view of
      SourceTypes -> pure
      ComputedTypes -> fragmentHead 256

    -- Allow restrictions along the spine of concluded codomains.
    goTail :: forall l. Distinct l => TermT l -> TypeCheck l ()
    goTail original = expose original >>= \t -> case t of
      TypeRestrictedT _ ty' rs -> do
        goTail ty'
        forM_ rs $ \(tope, term) -> do
          goTerm OutsideParameterPrefix tope
          goTerm OutsideParameterPrefix term
      TypeFunT _ orig md param mtope ret -> do
        checkShapeDependencies InTail t
        goAssumed UseBinder param
        checkDomain orig md param mtope
        inScope orig md param ret goTail
      RecOrT _ rs -> forM_ rs $ \(tope, term) -> do
        goTerm OutsideParameterPrefix tope
        goTail term
      _ -> goAssumed UseConcluded t

    -- Assumed restrictions must sit directly under a shape-Π.
    goAssumed :: forall l. Distinct l => FragmentUse -> TermT l -> TypeCheck l ()
    goAssumed = goAssumedAt OutsideParameterPrefix

    goAssumedAt
      :: forall l. Distinct l
      => PrefixPosition -> FragmentUse -> TermT l -> TypeCheck l ()
    goAssumedAt prefixPosition use original = expose original >>= \t -> case t of
      TypeRestrictedT _ ty' rs -> do
        reportFreeStanding use t
        goAssumed use ty'
        forM_ rs $ \(tope, term) -> do
          goTerm OutsideParameterPrefix tope
          goTerm OutsideParameterPrefix term
      TypeFunT _ orig md param mtope ret -> do
        checkShapeDependencies (InAssumption use) t
        goAssumed UseBinder param
        checkDomain orig md param mtope
        shape <- isShapeBinder (maybe False (const True) mtope) param
        inScope orig md param ret $
          if shape then goExtCodomain use else goAssumed use
      TypeSigmaT _ orig md a b -> do
        goAssumed UseBinder a
        inScope orig md a b (goAssumed use)
      -- Identity-type arguments must be ext-style; endpoints remain terms.
      TypeIdT _ a mtA b -> do
        goTerm OutsideParameterPrefix a
        mapM_ (goAssumed UseIdentity) mtA
        goTerm OutsideParameterPrefix b
      LambdaT info orig mparam body -> do
        md <- case mparam of
          Just (LambdaParam md param mtope) -> do
            goAssumed UseBinder param
            checkDomain orig md param mtope
            when (prefixPosition == OutsideParameterPrefix) $ reportMetaBinder orig param
            pure md
          Nothing -> pure Id
        dom <- binderType info mparam
        inScope orig md dom body (goAssumedAt prefixPosition use)
      -- recordSyntaxUses reports modal syntax; descend here for restriction checks.
      TypeModalT _ _ ty' -> goAssumed use ty'
      RecOrT _ rs -> forM_ rs $ \(tope, term) -> do
        goTerm OutsideParameterPrefix tope
        goAssumed use term
      Var{} -> pure ()
      _ -> goTerm OutsideParameterPrefix t

    -- Direct shape codomains admit boundary clipping to the binder's domain.
    goExtCodomain
      :: forall l. Distinct l => FragmentUse -> TermT l -> TypeCheck l ()
    goExtCodomain use original = expose original >>= \t -> case t of
      TypeRestrictedT _ ty' rs -> do
        goAssumed use ty'
        forM_ rs $ \(face, term) -> do
          goTerm OutsideParameterPrefix face
          goTerm OutsideParameterPrefix term
      _ -> goAssumed use t

    -- Topes may contain applications with their own binders and assumed types.
    checkDomain
      :: forall l. Distinct l
      => Binder -> TModality -> TermT l -> Maybe (ScopedTermT l) -> TypeCheck l ()
    checkDomain orig md param =
      mapM_ (\tope -> inScope orig md param tope (goTerm OutsideParameterPrefix))

    -- RS17 Appendix A.2 requires shapes independent of ambient cube points.
    -- Schematic families are judgements over cube contexts, not extension types.
    checkShapeDependencies :: forall l. Distinct l => CheckedPosition -> TermT l -> TypeCheck l ()
    checkShapeDependencies position t = do
      enabled <- asks ctxWarnShapeDependency
      when enabled $ case t of
        TypeFunT _ orig md param mtope ret -> do
          schematic <- isMetaType t
          shape <- isShapeBinder (maybe False (const True) mtope) param
          when shape $ do
            scope <- asks ctxScope
            let check :: forall k. Distinct k => Foil.Name k -> TermT k -> TermT k -> TypeCheck k ()
                check point cube original = do
                  reportShapeDependency "cube domain" point cube
                  unless schematic $ expose original >>= \body -> case (position, body) of
                    (InAssumption _, TypeRestrictedT _ _ rs) ->
                      forM_ rs $ \(face, _) -> reportShapeDependency "restriction boundary" point face
                    _ -> pure ()
            case mtope of
              Nothing -> withScopedT scope ret $ \binder body ->
                underBinder binder orig md param Nothing $ check (Foil.nameOf binder) (Foil.sink param) body
              Just tope -> withScopedT2 scope tope ret $ \binder domain body ->
                underBinder binder orig md param Nothing $ do
                  unless schematic $
                    reportShapeDependency "shape domain" (Foil.nameOf binder) domain
                  check (Foil.nameOf binder) (Foil.sink param) body
        _ -> pure ()

    reportShapeDependency
      :: forall l. Distinct l => String -> Foil.Name l -> TermT l -> TypeCheck l ()
    reportShapeDependency what point tope = do
      points <- outerPoints point tope
      unless (null points) $ do
        naming <- asks namingOfContext
        loc <- asks ctxLocation
        let render = ppTerm naming . untyped
        recordCheckWarning $ RSTTScopeWarning RSTTShapeDependency
          ("context-dependent " <> what <> " " <> render tope
            <> " (outer cube points: " <> intercalate ", " (map (render . Var) points)
            <> "; in " <> show defName <> ")") loc

    -- Follow aliases and variable types without reducing away dependencies.
    outerPoints :: forall l. Distinct l => Foil.Name l -> TermT l -> TypeCheck l [Foil.Name l]
    outerPoints point t = go (IntSet.singleton (Foil.nameId point)) (freeVarsOfTermT t)
      where
        go _ [] = pure []
        go seen (v : vs)
          | Foil.nameId v `IntSet.member` seen = go seen vs
          | otherwise = do
              ty <- typeOfVar v
              value <- valueOfVar v
              cube <- typeOfUncomputed ty >>= whnfT
              let here = case (value, cube) of
                    (Nothing, UniverseCubeT{}) -> [v]
                    _ -> []
                  dependencies = freeVarsOfTermT ty <> foldMap freeVarsOfTermT value
              rest <- go (IntSet.insert (Foil.nameId v) seen) (dependencies <> vs)
              pure (here <> rest)

    -- Check term binders and route type-valued bodies through goData.
    goTerm :: forall l. Distinct l => PrefixPosition -> TermT l -> TypeCheck l ()
    goTerm InParameterPrefix t | not (isLambda t) = goDataAt InParameterPrefix t
    goTerm prefixPosition t = case t of
      Var{} -> pure ()

      -- Function types returned by lets or branches still introduce shapes.
      TypeFunT{} -> goAssumed UseData t

      -- The stored type is synthesised by elaboration; it is not assumed.
      ReflT _ mx -> forM_ mx $ \(x, _mxty) -> goTerm OutsideParameterPrefix x

      TypeAscT _ term' ty' -> do
        goTerm OutsideParameterPrefix term'
        goTail ty'

      LambdaT info orig mparam body -> do
        checkShapeDependencies InTail (infoType info)
        md <- case mparam of
          Just (LambdaParam md param mtope) -> do
            goAssumed UseBinder param
            checkDomain orig md param mtope
            when (prefixPosition == OutsideParameterPrefix) $ reportMetaBinder orig param
            pure md
          Nothing -> pure Id
        dom <- binderType info mparam
        inScope orig md dom body (goTerm prefixPosition)

      AppT{} -> do
        let (h, args) = collectSpine t []
        goTerm OutsideParameterPrefix h
        forM_ args $ \(fnode, arg) -> do
          -- Substitution can create a forbidden type in a later argument domain.
          when (view == ComputedTypes) $
            typeOfUncomputed fnode >>= funDomain >>= mapM_ (goAssumed UseBinder)
          -- Schema arguments may bind their own schematic parameters.
          plumbing <- domainIsMeta fnode
          if plumbing then goTerm InParameterPrefix arg else goData arg

      -- The motive is assumed: the eliminator binds its variables at it.
      IdJT _ tA a tC d x p -> do
        goData tA
        goTerm OutsideParameterPrefix a
        goAssumed UseMotive tC
        goTerm OutsideParameterPrefix d
        goTerm OutsideParameterPrefix x
        goTerm OutsideParameterPrefix p

      -- Preserve prefix status for parameters bound after a schematic split.
      MatchT _ scrut mmotive branches -> do
        goTerm OutsideParameterPrefix scrut
        mapM_ (goAssumed UseMotive) mmotive
        forM_ branches $ \(_con, branch) -> goTerm prefixPosition branch

      PairT _ l r -> do
        goData l
        goData r

      -- The let annotation records the value's concluded type.
      LetT _ orig manno value body -> do
        mapM_ goTail manno
        goData value
        let dom = fromMaybe universeT (manno <|> valueType value)
        inScopeWith orig Id dom (Just value) body (goTerm OutsideParameterPrefix)

      LetModT _ orig _nu mu manno mmotive value body -> do
        mapM_ goTail manno
        mapM_ (goAssumed UseMotive) mmotive
        goData value
        inScope orig mu (fromMaybe universeT (manno <|> valueType value)) body
          (goTerm OutsideParameterPrefix)

      Node (AnnSig _ f) ->
        mapM_ (goTerm OutsideParameterPrefix) (bifoldMap (const []) (:[]) f)

    -- Type-valued data may later become binder or motive types. A proof
    -- quantified over a universe must still be walked as a term.
    goData :: forall l. Distinct l => TermT l -> TypeCheck l ()
    goData = goDataAt OutsideParameterPrefix

    goDataAt :: forall l. Distinct l => PrefixPosition -> TermT l -> TypeCheck l ()
    goDataAt prefixPosition t = do
      isType <- landsInUniverse t
      if isType then goAssumedAt prefixPosition UseData t else goTerm OutsideParameterPrefix t

    isLambda :: forall l. TermT l -> Bool
    isLambda LambdaT{} = True
    isLambda _         = False

    valueType :: forall l. TermT l -> Maybe (TermT l)
    valueType value = infoType <$> typeInfoOf value

    binderType
      :: forall l. Distinct l
      => TypeInfo (TermT l) -> Maybe (LambdaParam (ScopedTermT l) (TermT l))
      -> TypeCheck l (TermT l)
    binderType _info (Just (LambdaParam _ param _mtope)) = pure param
    binderType info Nothing = fromMaybe universeT <$> funDomain (infoType info)

    -- A cube domain without a tope denotes the unrestricted shape.
    isShapeBinder :: forall l. Distinct l => Bool -> TermT l -> TypeCheck l Bool
    isShapeBinder True _ = pure True
    isShapeBinder False param =
      typeOfUncomputed param >>= whnfT >>= \case
        UniverseCubeT{} -> pure True
        _               -> pure False

    -- Is the Π-domain of this function node schematic?
    domainIsMeta :: forall l. Distinct l => TermT l -> TypeCheck l Bool
    domainIsMeta f = do
      tf <- typeOfUncomputed f
      whnfT (stripTypeRestrictions tf) >>= \case
        TypeFunT _ _ _ dom _ _ -> isMetaType dom
        _                      -> pure False

    -- Recognise types and type families by their final codomain.
    landsInUniverse :: forall l. Distinct l => TermT l -> TypeCheck l Bool
    landsInUniverse t =
      typeOfUncomputed t >>= go
      where
        go :: forall k. Distinct k => TermT k -> TypeCheck k Bool
        go ty = whnfT (stripTypeRestrictions ty) >>= \case
          UniverseT{}     -> pure True
          UniverseCubeT{} -> pure True
          UniverseTopeT{} -> pure True
          TypeFunT _ orig md param _mtope ret -> inScope orig md param ret go
          _               -> pure False

    funDomain :: forall l. Distinct l => TermT l -> TypeCheck l (Maybe (TermT l))
    funDomain tf =
      whnfT (stripTypeRestrictions tf) >>= \case
        TypeFunT _ _ _ dom _ _ -> pure (Just dom)
        _                      -> pure Nothing

    reportMetaBinder :: forall l. Distinct l => Binder -> TermT l -> TypeCheck l ()
    reportMetaBinder orig param = do
      enabled <- asks ctxWarnMetaBinder
      when enabled $ do
        meta <- isMetaType param
        when meta $ do
          naming <- asks namingOfContext
          loc <- asks ctxLocation
          recordCheckWarning $ MetaBinderWarning
            defName
            (fromMaybe "_" (binderName orig))
            (ppTerm naming (untyped param))
            loc

    reportFreeStanding :: forall l. Distinct l => FragmentUse -> TermT l -> TypeCheck l ()
    reportFreeStanding use t = do
      enabled <- asks ctxWarnFreeStandingRestriction
      when enabled $ do
        naming <- asks namingOfContext
        loc <- asks ctxLocation
        recordCheckWarning $
          FreeStandingRestrictionWarning defName (ppTerm naming (untyped t)) use loc

-- Expose type constructors without consulting cached normal forms or simplifying
-- their guards. Keep this reducer in step with the object-level rules in Eval.
fragmentHead :: Distinct n => Int -> TermT n -> TypeCheck n (TermT n)
fragmentHead fuel t
  | fuel <= 0 = issueTypeError (TypeErrorOther "RSTT fragment reduction limit reached")
  | otherwise = case t of
      Var v -> valueOfVar v >>= maybe (neutral t) step
      AppT info f x -> step f >>= \case
        LambdaT _ _ _ body -> instantiate body x >>= step
        f' -> neutral (AppT info f' x)
      LetT _ _ _ value body -> instantiate body value >>= step
      TypeAscT _ value _ -> step value
      FirstT info pair -> step pair >>= \case
        PairT _ a _ -> step a
        pair' -> neutral (FirstT info pair')
      SecondT info pair -> step pair >>= \case
        PairT _ _ b -> step b
        pair' -> neutral (SecondT info pair')
      IdJT info a x motive d y proof -> step proof >>= \case
        ReflT{} -> step d
        proof' -> neutral (IdJT info a x motive d y proof')
      RecOrT _ rs -> firstMatching rs >>= maybe (pure t) step
      -- In particular, stop at TypeRestrictedT before reducing its topes.
      _ -> pure t
  where
    step = fragmentHead (fuel - 1)
    neutral term = do
      ty <- typeOfUncomputed term >>= step
      tryRestriction ty >>= maybe (pure term) step

-- TODO: Share this application-spine helper with MetaPrefix in the syntax layer.
collectSpine :: TermT n -> [(TermT n, TermT n)] -> (TermT n, [(TermT n, TermT n)])
collectSpine (AppT _ f x) acc = collectSpine f ((f, x) : acc)
collectSpine h acc            = (h, acc)

-- | Check explicit syntax before typechecking erases source positions and sugars.
-- Report each outermost forbidden node at its nearest available source position.
recordSyntaxUses :: forall n. Distinct n => Term n -> TypeCheck n ()
recordSyntaxUses term = do
  enabled <- asks rsttSafeEnabled
  when enabled $ do
    roles <- mapM (\v -> (,) (Foil.nameId v) <$> infoOfVar varDataRole v)
      (freeVarsOfTerm term)
    let inductive = IntSet.fromList [v | (v, Just _) <- roles]
    loc <- asks ctxLocation
    forM_ (go inductive loc term) $ \warning ->
      local (\ctx -> ctx { ctxLocation = warningLocation warning }) $
        recordCheckWarning warning
  where
    -- Free names retain their identities under binders; bound names are fresh.
    go :: forall l. IntSet.IntSet -> Maybe LocationInfo -> Term l -> [CheckWarning]
    go inductive loc (Var v)
      | Foil.nameId v `IntSet.member` inductive =
          [RSTTScopeWarning RSTTInductive "inductive constructor or eliminator" loc]
      | otherwise = []
    go inductive loc t@(Node (AnnSig _ sig)) =
      let here = case positionOfTerm t of
            Nothing -> loc
            Just pos -> atPosition pos <$> loc
      in if allowedNode sig
        then bifoldMap (\(ScopedAST _ body) -> go inductive here body)
                       (go inductive here) sig
        else let (extension, feature) = syntaxExtension sig
             in [RSTTScopeWarning extension feature here]

-- New constructors are forbidden until explicitly admitted here.
allowedNode :: TermSig scope term -> Bool
allowedNode = \case
  UniverseF -> True
  UniverseCubeF -> True
  UniverseTopeF -> True
  CubeUnitF -> True
  CubeUnitStarF -> True
  Cube2F -> True
  Cube2_0F -> True
  Cube2_1F -> True
  CubeProductF{} -> True
  TopeTopF -> True
  TopeBottomF -> True
  TopeEQF{} -> True
  TopeLEQF{} -> True
  TopeAndF{} -> True
  TopeOrF{} -> True
  RecBottomF -> True
  RecOrF{} -> True
  TypeFunF _ Id _ _ _ -> True
  TypeSigmaF _ Id _ _ -> True
  TypeIdF{} -> True
  AppF{} -> True
  LetF{} -> True
  LambdaF _ Nothing _ -> True
  LambdaF _ (Just (LambdaParam Id _ _)) _ -> True
  PairF{} -> True
  FirstF{} -> True
  SecondF{} -> True
  ReflF{} -> True
  IdJF{} -> True
  UnitF -> True
  TypeUnitF -> True
  TypeAscF{} -> True
  TypeRestrictedF{} -> True
  -- Hole checking also handles unresolved identifiers and unfinished obligations.
  HoleF{} -> True
  _ -> False

-- Classification improves diagnostics; it never grants admission.
syntaxExtension :: TermSig scope term -> (RSTTExtension, String)
syntaxExtension = \case
  TypeFunF{} -> modal "modal function binder"
  TypeSigmaF{} -> modal "modal pair binder"
  LambdaF{} -> modal "modal lambda binder"
  TypeModalF{} -> modal "modal type"
  ModAppF{} -> modal "modal introduction"
  ModExtractF{} -> modal "modal extraction"
  LetModF{} -> modal "modal let binding"
  CubeIF -> interval "auxiliary interval II"
  CubeI_0F -> interval "auxiliary interval endpoint 0_I"
  CubeI_1F -> interval "auxiliary interval endpoint 1_I"
  CubeFlipF{} -> interval "interval flip"
  CubeUnflipF{} -> interval "interval unflip"
  TopeInvF{} -> interval "tope involution"
  TopeUninvF{} -> interval "inverse tope involution"
  MatchF{} -> (RSTTInductive, "inductive match")
  MatchArmF{} -> (RSTTInductive, "inductive match arm")
  CubeSupF{} -> (RSTTUnsupported, "cube supremum (sup)")
  CubeInfF{} -> (RSTTUnsupported, "cube infimum (inf)")
  _ -> (RSTTUnsupported, "unrecognised syntax")
  where
    modal = (,) RSTTModal
    interval = (,) RSTTInterval
