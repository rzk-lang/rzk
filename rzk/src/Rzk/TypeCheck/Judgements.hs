{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The judgements — @typecheck@ and @infer@ — and the hole inventory.
--
-- The two belong together: checking a hole records its goal and context, and
-- recording a hole probes what could fill it, which typechecks and unifies
-- candidate terms.
module Rzk.TypeCheck.Judgements where

import           Control.Applicative      ((<|>))
import           Control.Monad            (forM, forM_, unless, when)
import           Control.Monad.Except     (catchError)
import           Control.Monad.Reader     (ask, asks, local)
import           Control.Monad.Writer.CPS (censor)
import           Data.List                (intercalate, tails)
import           Data.Maybe               (fromMaybe, isNothing)
import           Data.String              (fromString)

import           Control.Monad.Foil       (Distinct)
import qualified Control.Monad.Foil       as Foil
import           Control.Monad.Free.Foil  (AST (Var), ScopedAST (..))

import           Language.Rzk.Foil.Syntax
import           Language.Rzk.Foil.Names (Binder (..), TModality (..),
                                           TypeInfo (..), VarIdent,
                                           binderDisplayName, binderIsCompound,
                                           binderLeaves, binderName,
                                           freshenBinderLeaves, getVarIdent,
                                           refreshVar,
                                           ppVarIdentWithLocation,
                                           unmarkUnresolved)
import qualified Language.Rzk.Syntax      as Rzk
import           Rzk.TypeCheck.Context
import           Rzk.TypeCheck.Display
import           Rzk.TypeCheck.Error
import           Rzk.TypeCheck.Eval
import           Rzk.TypeCheck.Monad
import           Rzk.TypeCheck.Render
import           Rzk.TypeCheck.Unify

-- * Layers of a goal

isCubeType :: TermT n -> Bool
isCubeType = \case
  CubeUnitT{}     -> True
  Cube2T{}        -> True
  CubeIT{}        -> True
  CubeProductT{}  -> True
  UniverseCubeT{} -> True
  _               -> False

-- | Is a (WHNF) goal type in the cube or tope layer, so that a hole of this type
-- is a cube point or a tope rather than a term? Used to suppress the
-- type-layer-specific hole candidates (@recOR@, @recBOT@), which cannot inhabit a
-- cube or a tope.
isCubeOrTopeType :: TermT n -> Bool
isCubeOrTopeType t = isCubeType t || case t of
  UniverseTopeT{} -> True
  _               -> False

-- * Shadowing

-- | The names in scope a new one would shadow.
doesShadowName :: VarIdent -> TypeCheck n [VarIdent]
doesShadowName name = asks (shadowedBy name)

checkTopLevelDuplicate :: Distinct n => VarIdent -> TypeCheck n ()
checkTopLevelDuplicate name =
  doesShadowName name >>= \case
    []         -> return ()
    collisions -> issueTypeError $ TypeErrorDuplicateTopLevel collisions name

checkNameShadowing :: VarIdent -> TypeCheck n ()
checkNameShadowing name =
  doesShadowName name >>= \case
    [] -> return ()
    collisions -> issueWarning $
      Rzk.printTree (getVarIdent name) <> " shadows an existing definition:"
      <> unlines
        [ "  " <> ppVarIdentWithLocation name
        , "previous top-level definitions found at"
        , intercalate "\n"
          [ "  " <> ppVarIdentWithLocation prev | prev <- collisions ] ]

-- * The hole inventory

-- | A fresh hole of the given type.
mkHole :: TermT n -> TermT n
mkHole t = HoleT TypeInfo{ infoType = t, infoWHNF = Nothing, infoNF = Nothing } Nothing

-- | How many /branching/ eliminators 'allEliminationsInto' will chain.
--
-- A forced Π-application is free (see 'allEliminationsInto'), so this bounds only
-- the Σ/cube projections and @idJ@ steps, not the argument count of a spine. A
-- temporary fixed bound: branching is shallow in the goals seen so far (a few
-- projections), and a larger bound mostly adds self-referential spines (a built
-- result eliminated again).
maxEliminationDepth :: Int
maxEliminationDepth = 7

-- | Whether eliminating a value spends the search budget. A forced Π-application
-- is a 'SpineStep' — there is one way to fill the argument (with a hole), so
-- 'allEliminationsInto' applies it for free; a 'Branching' eliminator (a Σ/cube
-- projection or @idJ@) costs one against 'maxEliminationDepth'.
data ElimCost = SpineStep | Branching
  deriving (Eq, Show)

-- | All ways to eliminate a hypothesis into a value usable at a goal.
--
-- Given a @target@ type and a hypothesis /term/, return every elimination spine
-- over that term whose type fits the target (or a subtype of it). Arguments
-- introduced by application are left as holes for the caller to fill later. A
-- value that already fits is returned as-is; a function is applied to holes; a
-- Σ-type (or anything that unfolds to one, e.g. @is-contr@) is projected, possibly
-- repeatedly — so @first (first (is-segal-A ? ? ? ? ?))@ is discovered.
--
-- A Π-application is a forced spine step, so it extends the spine for free and
-- does not spend the budget. Only the genuinely branching eliminators count
-- against 'maxEliminationDepth', so the bound limits real search depth, not
-- argument count, and a lemma that must be applied to many holes is still reached.
allEliminationsInto
  :: Distinct n => TermT n -> [VarIdent] -> TermT n -> TypeCheck n [TermT n]
allEliminationsInto target inScopeNames = go maxEliminationDepth
  where
    go depth term = do
      ty    <- typeOf term
      fits  <- fitsInto term ty target
      elims <- eliminatorsOf inScopeNames ty
      let step (SpineStep, wrap) = go depth =<< wrap term
          step (Branching, wrap)
            | depth <= 0 = pure []
            | otherwise  = go (depth - 1) =<< wrap term
      deeper <- concat <$> mapM step elims
      pure ([term | fits] <> deeper)

-- | Whether a term of the given (whnf) type may stand where a value of the
-- @target@ type is expected: the two types unify under 'structuralHoleUnify', so a
-- hole acts as a wildcard leaf but a structural mismatch around it is still a
-- mismatch (an under-applied function does not match an extension-type goal, but a
-- partial application that genuinely fits an ordinary-function goal does).
--
-- Outer type restrictions are stripped from both sides first: an extension-type
-- boundary is satisfied by later refinement, not by the choice of spine, and
-- matching against the restricted goal would reject the very spine that introduces
-- the holes meant to satisfy it (@f ?@ at a boundary goal, say).
--
-- Holes or constraints recorded while probing are discarded, so this is a pure
-- yes\/no query.
fitsInto :: Distinct n => TermT n -> TermT n -> TermT n -> TypeCheck n Bool
fitsInto term ty target = do
  ty'     <- stripTypeRestrictions <$> whnfT ty
  target' <- stripTypeRestrictions <$> whnfT target
  censor (const mempty) $ local structuralHoleUnify
    ((unify (Just term) target' ty' >> pure True) `catchError` \_ -> pure False)

-- | The eliminators a value of the given (weak head normal) type admits, each as a
-- function wrapping the eliminated term, paired with its 'ElimCost'.
--
-- A Π-type is eliminated by application to a fresh hole (a spine step); a Σ-type by
-- either projection; an identity type by path induction (@idJ@), with the motive
-- and base case left as holes. The projections and @idJ@ branch. Anything else
-- admits no simple eliminator.
--
-- The names visible at the hole are passed in so that a binder an eliminator
-- introduces (the @idJ@ motive's @\\ b q → ?@) can be freshened against them.
eliminatorsOf
  :: Distinct n
  => [VarIdent] -> TermT n -> TypeCheck n [(ElimCost, TermT n -> TypeCheck n (TermT n))]
eliminatorsOf inScopeNames ty =
  case stripTypeRestrictions ty of
    TypeFunT _ty _orig _md param _mtope ret ->
      pure [ (SpineStep, \term -> do
                let h = mkHole param
                retAt <- instantiate ret h
                pure (appT retAt term h)) ]
    TypeSigmaT _ty _orig _md a b ->
      pure [ (Branching, \term -> pure (firstT a term))
           , (Branching, \term -> do
                bAt <- instantiate b (firstT a term)
                pure (secondT bAt term)) ]
    -- A cube point pair (a pattern-bound @(t , s) : 2 × 2@, say) projects to its
    -- coordinates; rzk renders those projections back as the pattern names.
    CubeProductT _ty a b ->
      pure [ (Branching, \term -> pure (firstT a term))
           , (Branching, \term -> pure (secondT b term)) ]
    -- A path @p : a =_A x@ is eliminated by path induction. The motive
    -- @C : (z : A) → (a =_A z) → U@ is always a function, so we introduce it
    -- straight away as @\\ b q → ?@ rather than leaving it a bare hole: the spine
    -- @idJ A a (\\ b q → ?) ? x p@ then has type @C x p@, which β-reduces to that
    -- inner hole — so J fits any goal (the player fills the motive and the base case
    -- @d : C a refl@). The two holes are the motive predicate and the base.
    TypeIdT _ty a mtA x -> do
      tA <- maybe (typeOf a) pure mtA
      scope <- asks ctxScope
      let c = motiveOf inScopeNames scope a tA
          dType = appT universeT
                    (appT (typeFunT (BinderVar Nothing) Id (typeIdT a (Just tA) a) Nothing
                            (closedScope scope universeT))
                      c a)
                    (reflT (typeIdT a (Just tA) a) Nothing)
          d = mkHole dType
          motiveAt y p = appT universeT
            (appT (typeFunT (BinderVar Nothing) Id (typeIdT a (Just tA) y) Nothing
                    (closedScope scope universeT))
              c y) p
      pure [ (Branching, \p -> pure (idJT (motiveAt x p) tA a c d x p)) ]
    _ -> pure []

-- | A scoped term that does not use its binder.
closedScope :: Distinct n => Foil.Scope n -> (forall l. TermT l) -> ScopedTermT n
closedScope scope t = Foil.withFresh scope $ \binder -> ScopedAST binder t

-- | The motive @\\ b q → ?@ of a path induction: a type in the two motive binders,
-- left as a hole.
motiveOf :: Distinct n => [VarIdent] -> Foil.Scope n -> TermT n -> TermT n -> TermT n
motiveOf inScopeNames scope a tA =
  Foil.withFresh scope $ \bBinder ->
    let scopeB = Foil.extendScope bBinder scope
        b = Var (Foil.nameOf bBinder)
        idTypeAtB = typeIdT (Foil.sink a) (Just (Foil.sink tA)) b
        -- the motive's own binders must not shadow anything visible at the
        -- hole (the move is inserted as source text)
        bName = refreshVar inScopeNames "b"
        qName = refreshVar (bName : inScopeNames) "q"
     in Foil.withFresh scopeB $ \qBinder ->
          let cBody = mkHole universeT
              cInner = lambdaT
                (typeFunT (BinderVar Nothing) Id idTypeAtB Nothing
                  (ScopedAST qBinder universeT))
                (BinderVar (Just qName)) Nothing
                (ScopedAST qBinder cBody)
              cType = typeFunT (BinderVar Nothing) Id tA Nothing
                (ScopedAST bBinder
                  (typeFunT (BinderVar Nothing) Id idTypeAtB Nothing
                    (ScopedAST qBinder universeT)))
           in lambdaT cType (BinderVar (Just bName)) Nothing (ScopedAST bBinder cInner)

-- | The binder for a λ introduced over a domain type.
--
-- A binder the type already gives as a pattern is kept as-is — it carries the
-- user's own names (@(t , s)@). Otherwise an /explicit/ (pre-whnf) Σ-type or
-- product domain is destructured into a fresh pair pattern, recursively for
-- products, so that a nameless @2 × 2 × 2@ parameter is introduced as
-- @((t1 , t2) , t3)@ rather than a single opaque variable. Any other domain keeps
-- its single binder.
--
-- Leaves are named by what they range over: a cube-product component is a point,
-- named @tN@; a Σ component is a term, named @xN@. The names are display-only (the
-- body is a hole that does not mention them) and carry a shared running index, so
-- every leaf in the pattern is distinct.
destructuringBinder :: Binder -> TermT n -> Binder
destructuringBinder orig param = case orig of
  BinderPair{} -> orig                 -- already a pattern: keep the user's names
  _ -> case param of
    CubeProductT{} -> fst (go (1 :: Int) param)
    TypeSigmaT{}   -> fst (go (1 :: Int) param)
    _              -> orig             -- not a product/Σ: leave the binder alone
  where
    -- a product/Σ becomes a pair; we recurse into a product's components (plain
    -- types) but not under a Σ's binder (a scope). A leaf is named by its enclosing
    -- constructor: @tN@ under a cube product, @xN@ under a Σ.
    go n = \case
      CubeProductT _ a b ->
        let (l, n')  = child "t" n  a
            (r, n'') = child "t" n' b
        in (BinderPair l r, n'')
      TypeSigmaT _ _ _md a _b ->
        let (l, n') = child "x" n a
        in (BinderPair l (BinderVar (Just (leaf "x" n'))), n' + 1)
      _ -> (BinderVar (Just (leaf "t" n)), n + 1)  -- unreached: go is called on products only
    child pfx n = \case
      c@CubeProductT{} -> go n c
      c@TypeSigmaT{}   -> go n c
      _                -> (BinderVar (Just (leaf pfx n)), n + 1)
    leaf pfx n = fromString (pfx <> show n :: String)

-- | All ways to introduce a value /of/ a goal type by its head constructor,
-- leaving the constituents as holes:
--
--   * a Π-type is introduced by a λ-abstraction over a hole body (@\\ x -> ?@); the
--     binder is taken from the type, so a pattern domain (a @Δ²@ point @(t , s)@,
--     say) is introduced as @\\ (t , s) -> ?@;
--   * a Σ-type or a cube product by a pair of holes (@(? , ?)@);
--   * an identity type by @refl@, but only when its two endpoints already agree
--     (otherwise @refl@ would not typecheck);
--   * the unit type by @unit@;
--   * the tope universe by each tope constructor — @TOP@, @BOT@, @? ≡ ?@, @? ≤ ?@,
--     @? ∧ ?@, @? ∨ ?@ — so a shape (a hole of type @TOPE@) can be built up by
--     tapping.
--
-- Unlike 'allEliminationsInto' this does not search: a type has at most one
-- introduction form (the tope universe is the one exception), read off its head
-- constructor. Outer restrictions are stripped first, so an extension type is
-- introduced by the form of its underlying type (its boundary is met by later
-- refinement of the holes, not by the choice of constructor).
--
-- The λ binder of a Π-introduction is freshened against the names already visible
-- at the hole, so introducing over a type whose own definition reuses an in-scope
-- name (@hom@, whose internal binder is @t@) yields @\\ t₁ -> ?@ rather than a @t@
-- that shadows the existing one.
allIntroductionsOf
  :: Distinct n => TermT n -> [VarIdent] -> TypeCheck n [TermT n]
allIntroductionsOf target inScopeNames = do
  target' <- stripTypeRestrictions <$> whnfT target
  scope <- asks ctxScope
  case target' of
    TypeFunT _ty orig _md param _mtope ret -> do
      -- An anonymous Π binder (a domain written @B → …@) must still be
      -- named in the offered move: the rendered λ shows its binder, and
      -- leaving the naming to the renderer would bypass the freshening
      -- below, shadowing an enclosing binder that already took the first
      -- default name. Name it here — the first default name, which the
      -- freshening bumps past everything visible at the hole.
      let named = case destructuringBinder orig param of
            BinderVar Nothing -> BinderVar (Just "x₁")
            b                 -> b
          binder = freshenBinderLeaves inScopeNames named
      pure $ Foil.withFresh scope $ \b ->
        let retAt = openWith (Foil.extendScope b scope) (Foil.nameOf b) ret
         in [ lambdaT target' binder Nothing (ScopedAST b (mkHole retAt)) ]
    TypeSigmaT _ty _orig _md a b -> do
      let h = mkHole a
      bAt <- instantiate b h
      pure [ pairT target' h (mkHole bAt) ]
    CubeProductT _ty a b ->
      pure [ pairT target' (mkHole a) (mkHole b) ]
    TypeIdT _ty a _tA b -> do
      agree <- endpointsAgree a b
      pure [ reflT target' Nothing | agree ]
    TypeUnitT{} -> pure [ unitT ]
    -- the tope universe: every tope constructor builds a tope, so all are
    -- introductions of a shape goal. Point arguments (of ≡, ≤) and tope arguments
    -- (of ∧, ∨) are left as holes.
    UniverseTopeT{} ->
      let point = mkHole (mkHole cubeT)  -- a point of an as-yet-unknown cube
          tope  = mkHole target'         -- a tope (its type is the tope universe)
       in pure [ topeTopT, topeBottomT
               , topeEQT  point point, topeLEQT point point
               , topeAndT tope  tope,  topeOrT  tope  tope ]
    _ -> pure []

-- | Whether the two endpoints of an identity type are definitionally equal, so that
-- @refl@ inhabits it. Like 'fitsInto', any holes or constraints recorded while
-- probing are discarded, leaving a pure yes\/no query.
endpointsAgree :: Distinct n => TermT n -> TermT n -> TypeCheck n Bool
endpointsAgree a b =
  censor (const mempty)
    ((unify Nothing a b >> pure True) `catchError` \_ -> pure False)

-- | Ex falso: in a contradictory tope context @recBOT@ inhabits any type, so it is a
-- candidate for every goal there (and only there — elsewhere it would not
-- typecheck). Independent of the goal and of the local hypotheses.
recBottomCandidates :: Distinct n => TypeCheck n [TermT n]
recBottomCandidates = do
  vacuous <- contextEntailsBottom
  pure [ recBottomT | vacuous ]

-- | Whether the local tope context is covered by the union of the given topes — the
-- coverage obligation of @recOR@, as a yes\/no query rather than a check that
-- issues an error.
coverageHolds :: Distinct n => [TermT n] -> TypeCheck n Bool
coverageHolds topes = do
  topesNF <- mapM nfTope topes
  entailContextM (foldr topeOrT topeBottomT topesNF)

-- | Tope case-split moves: ways to build a value of the goal by @recOR@, splitting
-- the proof over a cover of the local tope context. Three sources, offered together
-- (the UI ranks and filters):
--
--   * each disjunction @ψ ∨ φ@ already in the context becomes @recOR(ψ ↦ ?, φ ↦ ?)@
--     — its cover is immediate;
--   * when the goal is an extension type, its restriction faces are a cover
--     candidate, offered only when they actually cover the context (so the move
--     typechecks);
--   * a generic two-way split @recOR(? ↦ ?, ? ↦ ?)@ with the guards left as holes,
--     for an unusual split the player fills in by hand.
--
-- All three are offered only where a split makes sense — a cube variable is in
-- scope, the context has a non-trivial tope, or the goal is a restricted type — so
-- an ordinary (tope-free) goal is left alone.
recOrCandidates :: Distinct n => TermT n -> TypeCheck n [TermT n]
recOrCandidates goal = do
  goalW   <- whnfT goal
  topes   <- asks (filter (not . eqT topeTopT) . availableTopes)
  locals  <- asks localHypotheses
  hasCube <- or <$> mapM (fmap isCubeType . whnfT . varType . snd) locals
  let stripped     = stripTypeRestrictions goalW
      mkRecOr gs   = recOrT stripped [ (g, mkHole stripped) | g <- gs ]
      fromContext  = [ mkRecOr [l, r] | TopeOrT _ l r <- topes ]
      faces        = case goalW of
        TypeRestrictedT _ _ rs -> map fst rs
        _                      -> []
      isRestricted = case goalW of TypeRestrictedT{} -> True; _ -> False
      inShape      = hasCube || not (null topes) || isRestricted
      generic      = [ recOrT stripped [ (mkHole topeT, mkHole stripped)
                                       , (mkHole topeT, mkHole stripped) ]
                     | inShape ]
  fromFaces <- if length faces >= 2
    then do covered <- coverageHolds faces
            pure [ mkRecOr faces | covered ]
    else pure []
  pure (fromContext <> fromFaces <> generic)

-- | The local hypotheses: everything in scope that is not a top-level entry.
localHypotheses :: Context n -> [(Foil.Name n, VarInfo n)]
localHypotheses = filter (not . varIsTopLevel . snd) . varsInScope

-- | The allow-listed top-level lemmas a hole's candidate list may draw on.
lemmaHypotheses :: Context n -> [(Foil.Name n, VarInfo n)]
lemmaHypotheses ctx =
  [ entry
  | entry@(_, info) <- varsInScope ctx
  , varIsTopLevel info
  , Just name <- [binderName (varOrig info)]
  , name `elem` ctxHintLemmas ctx
  ]

-- | Record the goal and local context at a hole (lenient mode only).
recordHole :: Distinct n => Maybe VarIdent -> TermT n -> TypeCheck n ()
recordHole mname goalTy = recordHoleShape mname goalTy Nothing

-- | Record a hole. When the hole is the argument of a shape-restricted function its
-- goal is a /shape/: the cube @goalTy@ together with a membership tope, which is a
-- scope over the shape's bound variable. It is rendered under that binder, so the
-- goal reads @(binder : goalTy | tope)@.
recordHoleShape
  :: Distinct n
  => Maybe VarIdent
  -> TermT n
  -> Maybe (Binder, ScopedTermT n)
  -> TypeCheck n ()
recordHoleShape mname goalTy mshape = do
  goal'     <- whnfT goalTy
  locals    <- asks localHypotheses
  -- named top-level lemmas the caller allow-listed for hints. They feed the
  -- candidate-elimination loop only — not the local context shown to the user,
  -- since they are global definitions, not local hypotheses.
  lemmaVars <- asks lemmaHypotheses
  cubeFlags <- mapM (fmap isCubeType . whnfT . varType . snd) locals
  topes     <- asks (filter (not . eqT topeTopT) . availableTopes)
  loc       <- asks ctxLocation
  naming    <- asks namingOfContext

  -- names already visible at the hole, which a binder introduced by a move
  -- (a λ-introduction, or the idJ motive's own binders) must not shadow:
  -- each local hypothesis by its display name, or the leaves of a pattern
  -- hypothesis.
  let inScopeNames =
        [ nm
        | (v, _) <- locals
        , nm <- case displayOf naming v of
            (_, binder) | binderIsCompound binder -> binderLeaves binder
            (x, _)                                -> [x]
        ]

  -- for each local hypothesis (and allow-listed lemma), the elimination spines that
  -- land in the goal (arguments left as holes). Probing must not leak holes into the
  -- recorded output, hence the 'censor'.
  candidates <- censor (const mempty) $ do
    elims <- concat <$>
      mapM (\(v, _) -> allEliminationsInto goalTy inScopeNames (Var v)) (locals ++ lemmaVars)
    -- context-driven moves (independent of the goal's head and the hypotheses): ex
    -- falso in a contradictory context, and tope case-splits. recOR and recBOT are
    -- term-level eliminators, so they are offered only for a term goal — not when
    -- the hole is a cube point or a tope, where they cannot appear.
    let termLayer = not (isCubeOrTopeType goal')
    recbot <- if termLayer then recBottomCandidates    else pure []
    recor  <- if termLayer then recOrCandidates goalTy else pure []
    pure (elims <> recbot <> recor)

  -- A candidate is a move, inserted as source text, so every variable it
  -- mentions must be referable by the name it is shown under. A binder
  -- refreshed for display fails this (an inner @b@ shown as @b₁@ beside an
  -- outer @b@: writing @b₁@ is undefined), and so does the outer @b@ itself
  -- (writing @b@ resolves to the inner one). Until moves are rendered with
  -- source names, a variable is referable only if its source name is
  -- uniquely held in scope and its display name agrees with it; candidates
  -- mentioning any other named variable are dropped. Pattern and anonymous
  -- binders are left to the projection-folding rendering as before.
  ctx <- ask
  let namedSources =
        [ src
        | (_, info) <- varsInScope ctx
        , BinderVar (Just src) <- [varOrig info]
        ]
      referable v = case varOrig (lookupVarInfo v ctx) of
        BinderVar (Just src) ->
          fst (displayOf naming v) == src
            && length (filter (== src) namedSources) == 1
        _ -> True
      insertable = all referable . freeVarsOfTermT

  let render t = renderTerm naming (untyped t)
      -- a pattern binder is shown as its pattern, e.g. (t , s); others by name
      entryName v = case displayOf naming v of
        (_, binder) | binderIsCompound binder -> binderDisplayName binder
        (x, _)                                -> x
      entries = [ HoleEntry (entryName v) (render (varType info)) | (v, info) <- locals ]
      flagged = zip cubeFlags entries

  -- The goal shape, rendered under the shape's own binder. The name is read back
  -- from the naming rather than assumed: if the declared name is taken (the goal
  -- @(t : 2 × 2 | Δ¹×Δ¹ t)@ under an enclosing @t@), the binder is refreshed, and
  -- the tope must be shown under the /same/ name it is.
  goalShape <- forM mshape $ \(orig, tope) ->
    withBinder (BinderVar (binderName orig <|> Just "t")) Id goal' $ \binder -> do
      topeAt <- openScoped binder tope
      naming' <- asks namingOfContext
      let (shapeBinder, _) = displayOf naming' (Foil.nameOf binder)
      pure (shapeBinder, renderTerm naming' (untyped topeAt))

  -- the introduction forms for the goal itself (constituents left as holes); the Π
  -- binder is freshened against the names in scope so that it does not shadow.
  introductions <- censor (const mempty) (allIntroductionsOf goalTy inScopeNames)
  -- the goal cell: an SVG of the shape the hole must inhabit (an arrow, triangle or
  -- square), drawn from an abstract inhabitant with the proof term hidden. 'Nothing'
  -- when the goal is not a renderable shape.
  diagram <- censor (const mempty) (renderGoalCellSVG goal')

  recordHoleInfo HoleInfo
    { holeName          = mname
    , holeGoal          = render goal'
    , holeGoalShape     = goalShape
    , holeTermVars      = [ e | (False, e) <- flagged ]
    , holeCubeVars      = [ e | (True,  e) <- flagged ]
    , holeTopes         = map render topes
    , holeCandidates    = map render (filter insertable candidates)
    , holeIntroductions = map render introductions
    , holeDiagram       = diagram
    , holeLocation      = loc
    }

-- | Check a hole that appears as the argument of a shape-restricted function, whose
-- domain is the cube @cube@ restricted by @tope@ (a scope over the domain's bound
-- variable). Mirrors the hole case of 'typecheck', but records the shape as the
-- hole's goal so the diagnostic shows @(binder : cube | tope)@.
checkHoleAgainstShape
  :: Distinct n
  => Maybe VarIdent -> Binder -> TermT n -> ScopedTermT n
  -> TypeCheck n (TermT n)
checkHoleAgainstShape mname orig cube tope = do
  reject <- asks ctxHolesAreErrors
  if reject
    then issueTypeError (TypeErrorUnsolvedHole mname cube)
    else do
      recordHoleShape mname cube (Just (orig, tope))
      pure (holeT cube mname)


-- * Checking

-- | Check a @recOR@ against a known expected type: each branch is checked against
-- it under its own guard, the branches must agree on their overlaps, and together
-- they must cover the context.
checkRecOrAgainst
  :: Distinct n => TermT n -> [(Term n, Term n)] -> TypeCheck n (TermT n)
checkRecOrAgainst expected rs = do
  rs' <- forM rs $ \(tope, rterm) -> do
    tope' <- typecheck tope topeT
    checkTopeAgainstContext "recOR branch guard" tope'
    localTope tope' $ do
      expected' <- pruneVacuousFaces expected
      rterm' <- typecheck rterm expected'
      return (tope', rterm')
  sequence_ [ checkCoherence l r | l:rs'' <- tails rs', r <- rs'' ]
  contextEntailsUnion (map fst rs')
  return (recOrT expected rs')

-- | Drop the restriction faces of an extension type that are vacuous in the current
-- tope context (their overlap with the context is the empty tope ⊥). A face
-- mentioning an unfilled hole cannot be decided, so it is kept. Non-extension types
-- are returned unchanged. Used when descending into a recOR branch, where the
-- sibling branches' faces are disjoint from the branch guard.
pruneVacuousFaces :: Distinct n => TermT n -> TypeCheck n (TermT n)
pruneVacuousFaces (TypeRestrictedT _info ty rs) = do
  contextTopes <- asks ctxTopesNF
  kept <- fmap concat $ forM rs $ \face@(tope, _) -> do
    vacuous <- if containsHole tope
      then return False
      else (plainTope tope : contextTopes) `entailM` topeBottomT
    return [ face | not vacuous ]
  return $ case kept of
    [] -> ty
    _  -> typeRestrictedT ty kept
pruneVacuousFaces ty = return ty

typecheck :: Distinct n => Term n -> TermT n -> TypeCheck n (TermT n)
typecheck term ty = performing (ActionTypeCheck term ty) $ case term of
  -- An identifier that was not in scope: elaboration marked it, and this is where
  -- it is reported, under the binders and topes it was written beneath.
  Hole (Just name) | Just x <- unmarkUnresolved name ->
    issueTypeError (TypeErrorUndefined x)

  -- A hole is checked against a known type (this is checking position): in strict
  -- mode it is reported as an unsolved hole; in lenient mode its goal and context
  -- are recorded and it is treated as inhabiting the expected type.
  Hole mname -> do
    reject <- asks ctxHolesAreErrors
    if reject
      then issueTypeError (TypeErrorUnsolvedHole mname ty)
      else do
        recordHole mname ty
        return (holeT ty mname)

  _ -> whnfT ty >>= \case

    RecBottomT{} -> do
      -- Even under an absurd tope context (where the expected type collapses to
      -- recBOT), the term must still be well-formed in its own right, so that
      -- ill-typed bodies are not silently admitted under a false hypothesis. We
      -- synthesise its type, discard the result, and keep the recBOT elaboration.
      _ <- infer term
      return recBottomT

    tr@(TypeRestrictedT _ty ty' rs) -> case term of
      -- A recOR against a restricted type: push the restriction into each branch
      -- rather than stripping it first, so that a branch hole reports the boundary
      -- faces it must satisfy under its guard tope, and not the bare underlying
      -- type. Concrete branches still meet the faces, which are checked on each
      -- branch's overlap with them (see the general case below).
      RecOr branches -> checkRecOrAgainst tr branches
      _ -> do
        term' <- typecheck term ty'
        -- NOTE: restriction faces need not be contained in the local tope context.
        -- Each face is checked only on its overlap with the context, so an
        -- overhanging face is harmless (we only hint); a face disjoint from the
        -- context is vacuous, however, and is an error.
        forM_ rs $ \(tope, rterm) -> do
          checkTopeAgainstContext "restriction face" tope
          localTope tope $
            unifyTerms rterm term'
        return term'    -- FIXME: correct?

    ty' -> case term of
      Lambda orig mparam body ->
        case ty' of
          TypeFunT _ty _orig' md' param' mtope' ret -> do
            -- The λ's own domain annotation, if it wrote one, must agree with the
            -- domain of the type it is checked against.
            case mparam of
              Nothing -> return ()

              Just (LambdaParam md param Nothing) -> do
                when (md /= md') $
                  issueTypeError (TypeErrorModalityMismatch md' md term)
                paramType <- enterModality md $ infer param
                -- an argument can be a shape, which is a function into TOPE; the
                -- domain is then its cube, and its tope is the λ's shape tope.
                mcube <- typeOf paramType >>= \case
                  TypeFunT _ty _orig _md cube _mtope (ScopedAST _ UniverseTopeT{}) -> do
                    mapM_ checkNameShadowing (binderLeaves orig)
                    pure (Just cube)
                  _kind -> pure Nothing
                unifyTerms param' (fromMaybe paramType mcube)
                mapM_ checkNameShadowing (binderLeaves orig)
                case mcube of
                  Nothing -> return ()
                  Just _ ->
                    withBinder orig md param' $ \binder -> do
                      -- eta expand the shape into a tope over the bound variable
                      let etaTope = appT topeT (Foil.sink paramType) (Var (Foil.nameOf binder))
                      expected <- maybe (pure topeTopT) (openScoped binder) mtope'
                      unifyTerms expected etaTope

              Just (LambdaParam md param (Just tope)) -> do
                when (md /= md') $
                  issueTypeError (TypeErrorModalityMismatch md' md term)
                param'' <- enterModality md $ typecheck param =<< typeOf param'
                unifyTerms param' param''
                mapM_ checkNameShadowing (binderLeaves orig)
                checkUnder orig md param' tope $ \binder topeTerm -> do
                  tope'' <- typecheck topeTerm topeT
                  expected <- maybe (pure topeTopT) (openScoped binder) mtope'
                  unifyTerms expected tope''

            mapM_ checkNameShadowing (binderLeaves orig)
            body' <- elaborateUnder orig md' param' Nothing body $ \binder bodyTerm -> do
              mtopeIn <- traverse (openScoped binder) mtope'
              maybe id localTope mtopeIn $ do
                retIn <- openScoped binder ret
                typecheck bodyTerm retIn
            return (lambdaT ty' orig (Just (LambdaParam md' param' mtope')) body')

          _ -> issueTypeError $ TypeErrorUnexpectedLambda term ty

      Let orig annot val body -> do
        val' <- performing (ActionCheckLetValue (binderName orig)) $ case annot of
          Nothing -> infer val
          Just bindType -> do
            bindType' <- typecheck bindType universeT
            typecheck val bindType'
        bindTy <- typeOf val'
        body' <- elaborateUnder orig Id bindTy (Just val') body $ \_binder bodyTerm ->
          typecheck bodyTerm (Foil.sink ty')
        return (letT ty' orig (Just bindTy) val' body')

      LetMod orig app inn annot val body -> do
        val' <- performing (ActionCheckLetValue (binderName orig)) $ case annot of
          Nothing -> enterModality app $ infer val
          Just bindType -> do
            bindType' <- infer bindType
            bindUniv <- typeOf bindType'
            enterModality app $ typecheck val (typeModalT bindUniv inn bindType')
        bindTy <- typeOf val' >>= \case
          o@(TypeModalT _ty md t) ->
            if md == inn
              then return t
              else issueTypeError $ TypeErrorNotModal (untyped o) inn val'
          o -> issueTypeError $ TypeErrorNotModal (untyped o) inn val'
        bindVal <- whnfT val' >>= \case
          ModAppT _ty _m t -> pure (Just t)
          o | isRA inn -> pure (Just (modExtractT bindTy app inn o))
          _ -> pure Nothing
        body' <- elaborateUnder orig (comp app inn) bindTy bindVal body $ \_binder bodyTerm ->
          typecheck bodyTerm (Foil.sink ty')
        return (letModT ty' orig app inn (Just bindTy) val' body')

      Pair l r ->
        case ty' of
          CubeProductT _ty a b -> do
            l' <- typecheck l a
            r' <- typecheck r b
            return (pairT ty' l' r')
          TypeSigmaT _ty _orig md a b -> do
            l' <- enterModality md $ typecheck l a
            bAt <- instantiate b l'
            r' <- typecheck r bAt
            return (pairT ty' l' r')
          _ -> issueTypeError $ TypeErrorUnexpectedPair term ty

      Refl mx ->
        case ty' of
          TypeIdT _ty y _tA z -> do
            tA <- typeOf y
            forM_ mx $ \(x, mxty) -> do
              forM_ mxty $ \xty -> do
                xty' <- typecheck xty universeT
                unifyTerms tA xty'
              x' <- typecheck x tA
              unifyTerms x' y >> unifyTerms y x'
              unifyTerms x' z >> unifyTerms z x'
            when (isNothing mx) $
              unifyTerms y z >> unifyTerms z y
            return (reflT ty' (Just (y, Just tA)))
          _ -> issueTypeError $ TypeErrorUnexpectedRefl term ty

      ModExtract{} -> panicImpossible "extract is an internal term and cannot be typechecked"

      ModApp md body -> case ty' of
        TypeModalT _ty md' tpe -> do
          when (md /= md') $ issueTypeError $
            TypeErrorModalityMismatch md' md term
          body' <- enterModality md $ typecheck body tpe
          return $ modAppT ty' md body'
        _ -> issueTypeError $ TypeErrorNotModal term md ty'

      -- In checking position the common type is already known, so we push it into
      -- every branch instead of inferring each one and unifying. This is what lets a
      -- bare hole branch (recOR(φ ↦ ?, …)) be checked against the expected type and
      -- recorded, rather than hitting TypeErrorCannotInferHole via the inference
      -- rule. A recOR is a term-level eliminator, not a tope; rejecting it when it is
      -- checked against the tope universe keeps it out of the tope layer, where it
      -- would otherwise hit a panic.
      RecOr rs -> case ty' of
        UniverseTopeT{} -> issueTypeError $
          TypeErrorOther "a recOR cannot be used as a tope"
        _ -> checkRecOrAgainst ty' rs

      -- A neutral term is inferred, then its type unified with the expected one. In
      -- lenient (hole-checking) mode a term that still carries an unfilled hole is a
      -- work in progress, so a failure of that final unification is tolerated: the
      -- holes recorded while inferring the term stand, and we accept the term rather
      -- than rejecting the whole sketch. The mismatch is typically incidental to the
      -- missing pieces — an extension-type boundary face that only fails to line up
      -- because an argument hole sits in the wrong place (@f t@ vs @x@), where
      -- neither side is itself a hole, so the per-term deferral in unification cannot
      -- see it. Strict mode (the default, and CI) still rejects the mismatch.
      _ -> do
        term' <- infer term
        inferredType <- typeOf term'
        lenient <- not <$> asks ctxHolesAreErrors
        if lenient && containsHole term'
          then unifyTypes term' ty' inferredType `catchError` \_ -> return ()
          else unifyTypes term' ty' inferredType
        return term'

-- * Inference

inferAs :: Distinct n => TermT n -> Term n -> TypeCheck n (TermT n)
inferAs expectedKind term = do
  term' <- infer term
  ty <- typeOf term'
  kind <- typeOf ty
  unifyTypes ty expectedKind kind
  return term'

infer :: Distinct n => Term n -> TypeCheck n (TermT n)
infer tt = performing (ActionInfer tt) $ case tt of
  Hole (Just name) | Just x <- unmarkUnresolved name ->
    issueTypeError (TypeErrorUndefined x)
  Hole _mname -> issueTypeError (TypeErrorCannotInferHole tt)
  Var x -> do
    topLevel <- isTopLevelVar x
    unless topLevel $ do
      varMod <- modalityOfVar x
      locks <- locksOfVar x
      unless (coe varMod locks) $
        issueTypeError $ TypeErrorUnaccessibleVar x varMod locks
    pure (Var x)

  Universe     -> pure universeT
  UniverseCube -> pure cubeT
  UniverseTope -> pure topeT

  CubeUnit      -> pure cubeUnitT
  CubeUnitStar  -> pure cubeUnitStarT

  Cube2 -> pure cube2T
  Cube2_0 -> pure cube2_0T
  Cube2_1 -> pure cube2_1T

  CubeI -> pure cubeIT
  CubeI_0 -> pure cubeI_0T
  CubeI_1 -> pure cubeI_1T
  CubeProduct l r -> do
    l' <- typecheck l cubeT
    r' <- typecheck r cubeT
    return (cubeProductT l' r')

  CubeFlip t -> do
    t' <- infer t
    typeOf t' >>= \case
      CubeIT{} -> pure $ cubeFlipT cubeIT t'
      Cube2T{} -> pure $ cubeFlipT cube2T t'
      ty -> do
        tyStr <- ppInContext ty
        issueTypeError $ TypeErrorOther $
          "flip expects an interval cube (2 or 𝕀); got " <> tyStr
  CubeUnflip t -> do
    t' <- infer t
    typeOf t' >>= \case
      TypeModalT _ Op CubeIT{} -> pure $ cubeUnflipT cubeIT t'
      TypeModalT _ Op Cube2T{} -> pure $ cubeUnflipT cube2T t'
      ty -> do
        tyStr <- ppInContext ty
        issueTypeError $ TypeErrorOther $
          "unflip expects an interval cube (2 or 𝕀) under _op; got " <> tyStr

  CubeSup l r -> do
    l' <- inferAs cubeT l
    r' <- inferAs cubeT r
    lTy <- typeOf l'
    rTy <- typeOf r'
    case (lTy, rTy) of
      (Cube2T{}, Cube2T{}) -> return (cubeSupT cube2T l' r')
      (CubeIT{}, CubeIT{}) -> return (cubeSupT cubeIT l' r')
      -- Mixed 2/𝕀 lands in 𝕀 (the join of a 2-point and an 𝕀-point), coercing
      -- the 2 side up via 2 <: 𝕀, as the adjacent TopeLEQ does.
      (CubeIT{}, Cube2T{}) -> do
        r'' <- typecheck r cubeIT
        return (cubeSupT cubeIT l' r'')
      (Cube2T{}, CubeIT{}) -> do
        l'' <- typecheck l cubeIT
        return (cubeSupT cubeIT l'' r')
      _ -> issueTypeError $ TypeErrorNotIntervalCube "sup" lTy rTy

  CubeInf l r -> do
    l' <- inferAs cubeT l
    r' <- inferAs cubeT r
    lTy <- typeOf l'
    rTy <- typeOf r'
    case (lTy, rTy) of
      (Cube2T{}, Cube2T{}) -> return (cubeInfT cube2T l' r')
      (CubeIT{}, CubeIT{}) -> return (cubeInfT cubeIT l' r')
      -- Mixed 2/𝕀 lands in 𝕀 (the meet of a 2-point and an 𝕀-point), coercing
      -- the 2 side up via 2 <: 𝕀, as the adjacent TopeLEQ does.
      (CubeIT{}, Cube2T{}) -> do
        r'' <- typecheck r cubeIT
        return (cubeInfT cubeIT l' r'')
      (Cube2T{}, CubeIT{}) -> do
        l'' <- typecheck l cubeIT
        return (cubeInfT cubeIT l'' r')
      _ -> issueTypeError $ TypeErrorNotIntervalCube "inf" lTy rTy

  Pair l r -> do
    l' <- infer l
    r' <- infer r
    lt <- typeOf l'
    rt <- typeOf r'
    typeOf lt >>= \case
      --    Γ ⊢ l ⇒ (I : CUBE)
      --    Γ ⊢ r ⇒ (J : CUBE)
      -- ———————————————————————————
      -- Γ ⊢ (l, r) ⇒ (I × J : CUBE)
      UniverseCubeT{} -> return (pairT (cubeProductT lt rt) l' r')
      --    Γ ⊢ l ⇒ (A : U)
      --    Γ ⊢ r ⇒ (B : U)
      -- ———————————————————————————
      -- Γ ⊢ (l, r) ⇒ (A × B : U)             where A × B = Σ (_ : A), B
      _ -> do
        -- NOTE: infer as a non-dependent pair!
        rtScope <- constScope rt
        return (pairT (typeSigmaT (BinderVar Nothing) Id lt rtScope) l' r')

  First t -> do
    t' <- infer t
    fmap stripTypeRestrictions (typeOf t') >>= \case
      RecBottomT{} -> pure recBottomT -- FIXME: is this ok?
      TypeSigmaT _ty _orig _md lt _rt ->
        return (firstT lt t')
      CubeProductT _ty l _r ->
        return (firstT l t')
      ty -> issueTypeError $ TypeErrorNotPair t' ty

  Second t -> do
    t' <- infer t
    fmap stripTypeRestrictions (typeOf t') >>= \case
      RecBottomT{} -> pure recBottomT -- FIXME: is this ok?
      TypeSigmaT _ty _orig _md lt rt -> do
        rtAt <- instantiate rt (firstT lt t')
        return (secondT rtAt t')
      CubeProductT _ty _l r ->
        return (secondT r t')
      ty -> issueTypeError $ TypeErrorNotPair t' ty

  TypeUnit -> pure typeUnitT
  Unit -> pure unitT

  TopeTop -> pure topeTopT
  TopeBottom -> pure topeBottomT

  TopeEQ l r -> do
    l' <- inferAs cubeT l
    lt <- typeOf l'
    r' <- typecheck r lt
    return (topeEQT l' r')

  TopeLEQ l r -> do
    l' <- inferAs cubeT l
    r' <- inferAs cubeT r
    lTy <- typeOf l'
    rTy <- typeOf r'
    case (lTy, rTy) of
      (Cube2T{}, Cube2T{}) -> return (topeLEQT l' r')
      (CubeIT{}, CubeIT{}) -> return (topeLEQT l' r')
      (CubeIT{}, Cube2T{}) -> do
        r'' <- typecheck r cubeIT
        return (topeLEQT l' r'')
      (Cube2T{}, CubeIT{}) -> do
        l'' <- typecheck l cubeIT
        return (topeLEQT l'' r')
      _ -> do
        lStr <- ppInContext lTy
        rStr <- ppInContext rTy
        issueTypeError $ TypeErrorOther $
          "the (t ≤ s) tope expects points in interval cubes (2 or 𝕀); got "
            <> lStr <> " and " <> rStr

  TopeAnd l r -> do
    l' <- typecheck l topeT
    r' <- typecheck r topeT
    return (topeAndT l' r')

  TopeOr l r -> do
    l' <- typecheck l topeT
    r' <- typecheck r topeT
    return (topeOrT l' r')

  TopeInv t -> do
    t' <- typecheck t topeT
    return (topeInvT t')

  TopeUninv t -> do
    t' <- typecheck t (typeModalT universeT Op topeT)
    return (topeUninvT t')

  RecBottom -> do
    contextEntails topeBottomT
    return recBottomT

  RecOr rs -> do
    ttts <- forM rs $ \(tope, term) -> do
      tope' <- typecheck tope topeT
      -- NOTE: branch guards need not be contained in the context. recOR requires
      -- only coverage (context |- OR(guards)); a guard may overhang the context (when
      -- splitting with a named shape, say). checkTopeAgainstContext warns on overhang
      -- and errors only if the guard is disjoint from the context (a vacuous branch).
      checkTopeAgainstContext "recOR branch guard" tope'
      localTope tope' $ do
        term' <- inferAs universeT term
        ty <- typeOf term'
        return (tope', (term', ty))
    let rs' = map (fmap fst) ttts
        ts  = map (fmap snd) ttts
    sequence_ [ checkCoherence l r | l:rs'' <- tails rs', r <- rs'' ]
    contextEntailsUnion (map fst ttts)
    return (recOrT (recOrT universeT ts) rs')

  TypeFun orig md a Nothing b -> do
    a' <- enterModality md $ infer a
    typeOf a' >>= \case
      -- an argument can be a type
      UniverseT{} ->
        case a' of
          -- except if it is the TOPE universe
          UniverseTopeT{} ->
            issueTypeError $ TypeErrorOther "tope params are illegal"
          _ -> do
            mapM_ checkNameShadowing (binderLeaves orig)
            b' <- elaborateUnder orig md a' Nothing b $ \_binder bTerm ->
              typecheck bTerm universeT
            return (typeFunT orig md a' Nothing b')
      -- an argument can be a cube
      UniverseCubeT{} -> do
        mapM_ checkNameShadowing (binderLeaves orig)
        b' <- elaborateUnder orig md a' Nothing b $ \_binder bTerm ->
          typecheck bTerm universeT
        return (typeFunT orig md a' Nothing b')
      -- an argument can be a shape
      TypeFunT _ty _orig _md cube mtope (ScopedAST _ UniverseTopeT{}) -> do
        mapM_ checkNameShadowing (binderLeaves orig)
        (tope', b') <- checkUnder orig md cube b $ \binder bTerm -> do
          -- eta expand a' into a tope over the bound variable
          let etaTope = appT topeT (Foil.sink a') (Var (Foil.nameOf binder))
          tope' <- case mtope of
            Nothing     -> pure etaTope
            Just tope'' -> do
              inner <- openScoped binder tope''
              pure (topeAndT inner etaTope)
          bTyped <- localTope etaTope $ typecheck bTerm universeT
          pure (ScopedAST binder tope', ScopedAST binder bTyped)
        return (typeFunT orig md cube (Just tope') b')
      ty -> issueTypeError $ TypeErrorInvalidArgumentType a ty

  TypeFun orig md cube (Just tope) ret -> do
    cube' <- enterModality md $ typecheck cube cubeT
    mapM_ checkNameShadowing (binderLeaves orig)
    (tope', ret') <- checkUnder orig md cube' tope $ \binder topeTerm -> do
      topeTyped <- typecheck topeTerm topeT
      retTerm <- openScoped binder ret
      retTyped <- localTope topeTyped $ typecheck retTerm universeT
      pure (ScopedAST binder topeTyped, ScopedAST binder retTyped)
    return (typeFunT orig md cube' (Just tope') ret')

  TypeSigma orig md a b -> do
    a' <- enterModality md $ typecheck a universeT
    mapM_ checkNameShadowing (binderLeaves orig)
    b' <- elaborateUnder orig md a' Nothing b $ \_binder bTerm ->
      typecheck bTerm universeT
    return (typeSigmaT orig md a' b')

  TypeId x (Just tA) y -> do
    tA' <- typecheck tA universeT
    x' <- typecheck x tA'
    y' <- typecheck y tA'
    return (typeIdT x' (Just tA') y')

  TypeId x Nothing y -> do
    x' <- inferAs universeT x
    tA <- typeOf x'
    y' <- typecheck y tA
    return (typeIdT x' (Just tA) y')

  App f x -> do
    f' <- inferAs universeT f
    fmap stripTypeRestrictions (typeOf f') >>= \case
      TypeFunT _ty orig md a mtope b -> do
        -- A hole argument to a shape-restricted function carries the shape as its
        -- goal: record (binder : a | tope) rather than just the cube a.
        x' <- enterModality md $ case (x, mtope) of
          (Hole mname, Just tope) -> checkHoleAgainstShape mname orig a tope
          _                       -> typecheck x a
        bAt <- instantiate b x'
        let result = appT bAt f' x'
        case b of
          ScopedAST _ UniverseTopeT{} ->
            case mtope of
              Nothing -> return result
              Just tope -> do
                topeAt <- instantiate tope x'
                return (topeAndT topeAt result)
          _ -> do
            -- FIXME: need to check?
            forM_ mtope $ \tope -> do
              topeAt <- instantiate tope x'
              contextEntails topeAt
            return result
      ty -> issueTypeError $ TypeErrorNotFunction f' ty

  Lambda _orig Nothing _body ->
    issueTypeError $ TypeErrorCannotInferBareLambda tt

  Lambda orig (Just (LambdaParam md ty Nothing)) body -> do
    ty' <- enterModality md $ infer ty
    mcube <- typeOf ty' >>= \case
      -- an argument can be a type
      UniverseT{} ->
        case ty' of
          -- except if it is the TOPE universe
          UniverseTopeT{} ->
            issueTypeError $ TypeErrorOther "tope params are illegal"
          _ -> return Nothing
      -- an argument can be a cube
      UniverseCubeT{} -> return Nothing
      -- an argument can be a shape
      TypeFunT _ty _orig _md cube _mtope (ScopedAST _ UniverseTopeT{}) -> do
        mapM_ checkNameShadowing (binderLeaves orig)
        return (Just cube)
      kind -> issueTypeError $ TypeErrorInvalidArgumentType ty kind
    mapM_ checkNameShadowing (binderLeaves orig)
    let param = fromMaybe ty' mcube
    case mcube of
      Nothing -> do
        (body', ret) <- checkUnder orig md param body $ \binder bodyTerm -> do
          body' <- infer bodyTerm
          ret <- typeOf body'
          pure (ScopedAST binder body', ScopedAST binder ret)
        return (lambdaT (typeFunT orig md param Nothing ret) orig
                  (Just (LambdaParam md param Nothing)) body')
      Just _ -> do
        (tope', body', ret) <- checkUnder orig md param body $ \binder bodyTerm -> do
          -- eta expand the shape into a tope over the bound variable
          let etaTope = appT topeT (Foil.sink ty') (Var (Foil.nameOf binder))
          body' <- localTope etaTope $ infer bodyTerm
          ret <- typeOf body'
          pure (ScopedAST binder etaTope, ScopedAST binder body', ScopedAST binder ret)
        return (lambdaT (typeFunT orig md param (Just tope') ret) orig
                  (Just (LambdaParam md param (Just tope'))) body')

  Lambda orig (Just (LambdaParam md cube (Just tope))) body -> do
    cube' <- enterModality md $ typecheck cube cubeT
    mapM_ checkNameShadowing (binderLeaves orig)
    (tope', body', ret) <- checkUnder orig md cube' tope $ \binder topeTerm -> do
      topeTyped <- infer topeTerm
      bodyTerm <- openScoped binder body
      body' <- localTope topeTyped $ infer bodyTerm
      ret <- typeOf body'
      pure (ScopedAST binder topeTyped, ScopedAST binder body', ScopedAST binder ret)
    return (lambdaT (typeFunT orig md cube' (Just tope') ret) orig
              (Just (LambdaParam md cube' (Just tope'))) body')

  Let orig annot val body -> do
    val' <- performing (ActionCheckLetValue (binderName orig)) $ case annot of
      Nothing -> infer val
      Just ty -> do
        bindTy <- typecheck ty universeT
        typecheck val bindTy
    bindTy <- typeOf val'
    (body', ret) <- checkUnderWith orig Id bindTy (Just val') body $ \binder bodyTerm -> do
      body' <- infer bodyTerm
      ret <- typeOf body'
      pure (ScopedAST binder body', ScopedAST binder ret)
    retAt <- instantiate ret val'
    return (letT retAt orig (Just bindTy) val' body')

  LetMod orig app inn annot val body -> do
    val' <- performing (ActionCheckLetValue (binderName orig)) $ case annot of
      Nothing -> enterModality app $ infer val
      Just bindType -> do
        bindType' <- infer bindType
        bindUniv <- typeOf bindType'
        enterModality app $ typecheck val (typeModalT bindUniv inn bindType')
    bindTy <- typeOf val' >>= \case
      o@(TypeModalT _ty md t) ->
        if md == inn
          then return t
          else issueTypeError $ TypeErrorNotModal (untyped o) inn val'
      o -> issueTypeError $ TypeErrorNotModal (untyped o) inn val'
    bindVal <- whnfT val' >>= \case
      ModAppT _ty _m t -> pure (Just t)
      o | isRA inn -> pure (Just (modExtractT bindTy app inn o))
      _ -> pure Nothing
    (body', ret) <- checkUnderWith orig (comp app inn) bindTy bindVal body $ \binder bodyTerm -> do
      body' <- infer bodyTerm
      ret <- typeOf body'
      pure (ScopedAST binder body', ScopedAST binder ret)
    retAt <- instantiate ret val'
    return (letModT retAt orig app inn (Just bindTy) val' body')

  Refl Nothing -> issueTypeError $ TypeErrorCannotInferBareRefl tt
  Refl (Just (x, Nothing)) -> do
    x' <- inferAs universeT x
    ty <- typeOf x'
    return (reflT (typeIdT x' (Just ty) x') (Just (x', Just ty)))
  Refl (Just (x, Just ty)) -> do
    ty' <- typecheck ty universeT
    x' <- typecheck x ty'
    return (reflT (typeIdT x' (Just ty') x') (Just (x', Just ty')))

  IdJ tA a tC d x p -> do
    tA' <- typecheck tA universeT
    a' <- typecheck a tA'
    typeOf_C <- motiveType tA' a'
    tC' <- typecheck tC typeOf_C
    univScope <- constScope universeT
    let typeOf_d =
          appT universeT
            (appT (typeFunT (BinderVar Nothing) Id (typeIdT a' (Just tA') a') Nothing univScope)
              tC' a')
            (reflT (typeIdT a' (Just tA') a') Nothing)
    d' <- typecheck d typeOf_d
    x' <- typecheck x tA'
    p' <- typecheck p (typeIdT a' (Just tA') x')
    let ret =
          appT universeT
            (appT (typeFunT (BinderVar Nothing) Id (typeIdT a' (Just tA') x') Nothing univScope)
              tC' x')
            p'
    return (idJT ret tA' a' tC' d' x' p')

  TypeAsc term ty -> do
    ty' <- inferAs universeT ty -- this works on types AND cubes
    term' <- typecheck term ty'
    return (typeAscT term' ty')

  TypeRestricted ty rs -> do
    ty' <- typecheck ty universeT
    rs' <- forM rs $ \(tope, term) -> do
      tope' <- typecheck tope topeT
      term' <- localTope tope' $ typecheck term ty'
      return (tope', term')
    sequence_ [ checkCoherence l r | l:rs'' <- tails rs', r <- rs'' ]
    return (typeRestrictedT ty' rs')

  TypeModal md ty -> do
    ty' <- enterModality md $ infer ty
    universeTy <- typeOf ty'
    _ <- case universeTy of
      UniverseT{}     -> pure universeTy
      UniverseCubeT{} -> pure universeTy
      UniverseTopeT{} -> pure universeTy
      _               -> issueTypeError $ TypeErrorNotTypeInModal universeTy
    return (typeModalT universeTy md ty')

  ModApp md term -> do
    term' <- enterModality md $ infer term
    ty <- typeOf term'
    tyUniv <- typeOf ty
    return $ modAppT (typeModalT tyUniv md ty) md term'

  ModExtract{} -> panicImpossible "extract is an internal term and cannot be inferred"

-- | The type of the motive of a path induction: @(z : A) → (a =_A z) → U@.
motiveType :: Distinct n => TermT n -> TermT n -> TypeCheck n (TermT n)
motiveType tA a = do
  scope <- asks ctxScope
  pure $ Foil.withFresh scope $ \zBinder ->
    let scopeZ = Foil.extendScope zBinder scope
        idType = typeIdT (Foil.sink a) (Just (Foil.sink tA)) (Var (Foil.nameOf zBinder))
     in Foil.withFresh scopeZ $ \pBinder ->
          typeFunT (BinderVar Nothing) Id tA Nothing
            (ScopedAST zBinder
              (typeFunT (BinderVar Nothing) Id idType Nothing
                (ScopedAST pBinder universeT)))
