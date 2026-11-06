{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- The 'HasHeads' instance of the signature is an orphan: the class is
-- free-foil-NBE's and the signature is "Language.Rzk.Foil.Syntax"'s, and it
-- belongs with the rules here.
{-# OPTIONS_GHC -Wno-orphans #-}
-- | A conversion fast path on free-foil-NBE: Rzk's elimination rules as an
-- 'Eval' instance on its typed signature, with the type checker's 'Context'
-- as the evaluation context. 'Convertible' accepts the comparison;
-- 'DontKnow' leaves it to ordinary unification.
--
-- The fragment the check decides is βδ with the one-step η of
-- 'Rzk.TypeCheck.Eval.etaMatch' for lambdas and pairs, @recOR@ whose guards
-- the tope context decides, and the subtyping of the ordinary unification
-- (shape domains of Π and the faces of extension types, see 'relateWith').
-- Holes and the modal constructs never match, so a comparison that meets
-- one answers 'DontKnow'. See the soundness and attribution notes below.
module Rzk.TypeCheck.NbE
  ( Conversion (..)
  , nbeConvertible
  , nbeDifferential
    -- * The evaluation context
  , Ctx (..)
  , Approximation (..)
    -- * Soundness
    -- $soundness
    -- * Attribution
    -- $attribution
  ) where

import           Control.Monad.Reader              (ask, asks)
import           Data.Bifunctor                    (bimap)

import qualified Control.Monad.Foil                as Foil
import           Control.Monad.Free.Foil           (AST (..), ScopedAST (..))
import           Control.Monad.Free.Foil.Annotated (AnnSig (..))
import           FreeFoil.NbE                      (EtaScoped (..),
                                                    EtaTerm (..), Eval (..),
                                                    Extend (..),
                                                    GlueSpeculation (..),
                                                    HasDefinitions (..),
                                                    HasHeads (..))
import qualified FreeFoil.NbE                      as NbE
import qualified FreeFoil.NbE.Conversion           as Conv

import           Language.Rzk.Foil.Names           (Binder (..),
                                                    TModality (..),
                                                    TypeInfo (..))
import           Language.Rzk.Foil.Syntax
import           Rzk.TypeCheck.Context
import           Rzk.TypeCheck.Display             (namingOfContext, ppTerm)
import           Rzk.TypeCheck.Eval                (checkTope, entailM,
                                                    localTopeLazily, nfTope,
                                                    underBinder)
import           Rzk.TypeCheck.Monad
import qualified Rzk.TypeCheck.NbE.Spine           as Spine

-- $soundness
-- Soundness is a subset argument, as for the hand-written fast path this
-- module replaced ("Rzk.TypeCheck.NbE.Spine"): 'Convertible' is answered
-- only for comparisons the ordinary unification accepts. Every rule here
-- mirrors a case of 'Rzk.TypeCheck.Unify.unifyInCurrentContext' or of
-- 'Rzk.TypeCheck.Eval.whnfT', and where a case of the unification cannot be
-- mirrored (a restricted type against an unrestricted one, a Π with a tope
-- on one side only, a recOR against a term that is not one) the nodes do
-- not match and the answer is 'DontKnow'. Tope entailment is the checker's
-- own solver, run purely on the context ('entails'), so a guard reduces
-- here exactly when 'whnfT' reduces it.
--
-- The variance of the comparison is the context's 'ctxCovariance'. The
-- positions that keep it are those where the unification keeps it; every
-- other position is compared invariantly, which is stricter than the
-- unification and therefore sound.
--
-- $attribution
-- The algorithm is free-foil-NBE's (Renata Shakirova and Nikolai Kudasov):
-- normalisation by evaluation generic in the signature, with conversion
-- checking after Coquand, /An algorithm for type-checking dependent types/
-- (1996), the untyped η for surjective pairs of Abel and Coquand (2007),
-- glued evaluation and approximate conversion checking after Kovács'
-- smalltt, and subtyping as conversion with the direction in the context,
-- as in Rocq's kernel and in Rzk's own unification. The states of the
-- approximate check ('Approximation') follow smalltt's @unify@ through
-- free-foil-NBE's "FreeFoil.NbE.Definitions". The precedent for the gating
-- ('Convertible' or 'DontKnow', never a refutation) and for the fragment is
-- "Rzk.TypeCheck.NbE.Spine", whose header cites the sources in detail. The
-- template for deciding tope guards during evaluation is Sterling and
-- Angiuli, /Normalization for Cubical Type Theory/ (LICS 2021).

-- * Terms and values

type Sig = AnnSig TypeInfo TermSig

type Value n = NbE.Value Foil.NameBinder Sig n

type Env i n = Foil.Substitution (NbE.Value Foil.NameBinder Sig) i n

-- | The annotation of a value node: the type as a value, and the memoised
-- forms, which evaluation never consults.
type Info n = TypeInfo (Value n)

-- * The evaluation context

-- | The type checker's context, with the state of the approximate
-- conversion check and the evidence that the scope is distinct, which the
-- lazy evaluation of a definition needs.
data Ctx n = Ctx
  { ctxContext       :: Context n
  , ctxApproximation :: Approximation
  , ctxDistinct      :: Foil.DistinctEvidence n
  , ctxEtaLambda     :: Maybe (Value n)
    -- ^ Under the binder of a λ compared by η: the λ's type, for the
    -- observer to compare with the other value's type ('appliedTo').
  , ctxPiDomainsOnly :: Bool
    -- ^ Compare two Π types by their domains and topes alone, as the
    -- unification compares the types of two lambdas.
  }

-- | The context a comparison starts in.
rootCtx :: Foil.Distinct n => Context n -> Ctx n
rootCtx context = Ctx context Rigid Foil.Distinct Nothing False

-- | The state of a conversion check, after smalltt's approximate conversion
-- checking. A check starts 'Rigid'. Two glued values of one defined name
-- compare their folded sides 'Flex', which unfolds nothing and fails where
-- it would have to. If that fails, they compare their unfolded sides
-- 'Full', which unfolds every glued value at once.
data Approximation = Rigid | Flex | Full

variance :: Ctx n -> Covariance
variance = ctxCovariance . ctxContext

withVariance :: Covariance -> Ctx n -> Ctx n
withVariance v ctx = ctx { ctxContext = (ctxContext ctx) { ctxCovariance = v } }

withContext :: Context n -> Ctx n -> Ctx n
withContext context ctx = ctx { ctxContext = context }

-- | The context for comparing two Π types by their domains and topes alone,
-- invariantly.
piDomainsOnly :: Ctx n -> Ctx n
piDomainsOnly ctx = (withVariance Invariant ctx) { ctxPiDomainsOnly = True }

-- | A value that matches nothing: what an observer answers when the types
-- of the values it compares rule the comparison out.
unmatched :: Value n
unmatched = NbE.vnode (AnnSig (infoOf universeValue) (HoleF Nothing))

opposite :: Covariance -> Covariance
opposite = \case
  Covariant     -> Contravariant
  Contravariant -> Covariant
  Invariant     -> Invariant

-- | A defined name is glued to its value, evaluated in the ambient scope
-- when first needed; any other name is a neutral.
instance HasDefinitions Ctx Foil.NameBinder Sig where
  {-# INLINE contextDefinition #-}
  contextDefinition scope ctx x =
    case varValue (lookupVarInfo x (ctxContext ctx)) of
      Nothing   -> NbE.vvar x
      Just body -> case ctxDistinct ctx of
        Foil.Distinct -> NbE.vglued x (NbE.eval scope ctx Foil.identitySubst body)
  {-# INLINE contextGlueSpeculation #-}
  contextGlueSpeculation _ ctx x _ y _ = case ctxApproximation ctx of
    Rigid
      | x == y    -> TryFolded ctx { ctxApproximation = Flex } (Just ctx { ctxApproximation = Full })
      | otherwise -> CompareUnfolded ctx
    Flex
      | x == y    -> TryFolded ctx Nothing
      | otherwise -> Unrelated
    Full -> CompareUnfolded ctx
  {-# INLINE contextUnfoldGlued #-}
  contextUnfoldGlued _ ctx _ _ = case ctxApproximation ctx of
    Flex -> Nothing
    _    -> Just ctx

-- | The heads are the function of an application and the pair of a
-- projection.
instance HasHeads TermSig where
  {-# INLINE headsWith #-}
  headsWith hd other = \case
    AppF f x  -> AppF (hd f) (other x)
    FirstF p  -> FirstF (hd p)
    SecondF p -> SecondF (hd p)
    node      -> bimap id other node

-- * The checker's judgements, run purely

-- | Run a judgement on the context. An error is no answer.
purely :: Context n -> TypeCheck n a -> Maybe a
purely ctx tc = either (const Nothing) Just (runTypeCheckIn ctx tc)

-- | Does the tope context entail the tope?
entails :: Foil.Distinct n => Context n -> TermT n -> Bool
entails ctx tope = purely ctx (checkTope tope) == Just True

-- | Are the two topes equivalent, on their own (as
-- 'Rzk.TypeCheck.Unify.unifyTopes' decides two topes at a term position)?
topesEquivalent :: Foil.Distinct n => Context n -> TermT n -> TermT n -> Bool
topesEquivalent ctx l r = eqT l r || purely ctx check == Just True
  where
    check = do
      l' <- nfTope l
      r' <- nfTope r
      (&&) <$> [plainTope l'] `entailM` r' <*> [plainTope r'] `entailM` l'

-- | The context with a tope assumed. Whether the context became
-- contradictory is decided only if a judgement under it asks.
assumeTope :: Foil.Distinct n => TermT n -> Context n -> Maybe (Context n)
assumeTope tope ctx = purely ctx (localTopeLazily tope ask)

-- | The context under a binder, as the checker enters one.
enterContext
  :: (Foil.Distinct n, Foil.DExt n l)
  => Foil.NameBinder n l -> Binder -> TModality -> TermT n -> Maybe (TermT n)
  -> Ctx n -> Ctx l
enterContext binder orig md ty mval ctx =
  case runTypeCheckIn (ctxContext ctx) (underBinder binder orig md ty mval ask) of
    Right context -> Ctx context (ctxApproximation ctx) Foil.Distinct Nothing False
    Left _        -> error "Rzk.TypeCheck.NbE: impossible: entering a binder failed"

-- | A node matches on a condition: 'mempty' if it holds, no match otherwise.
guarded :: Monoid m => Bool -> Maybe m
guarded holds = if holds then Just mempty else Nothing

-- | The conjunction of matches.
allOf :: Monoid m => [Maybe m] -> Maybe m
allOf = fmap mconcat . sequence

-- * Values and their types

-- | The type of a value: of a variable from the context, of a node from its
-- annotation. A glued value has the type of its unfolded head.
typeOfValue :: Foil.Distinct n => Foil.Scope n -> Ctx n -> Value n -> Value n
typeOfValue scope ctx v = case NbE.force v of
  NbE.VVar x -> NbE.eval scope ctx Foil.identitySubst (varType (lookupVarInfo x (ctxContext ctx)))
  NbE.VNode (AnnSig info _) -> infoType info
  NbE.VSuspended _ (AnnSig info _) -> infoType info

-- | A type value with its restrictions peeled off.
stripRestrictions :: Value n -> Value n
stripRestrictions v = case NbE.force v of
  NbE.VNode (AnnSig _ (TypeRestrictedF ty _)) -> stripRestrictions ty
  _                                           -> v

-- | A Π type value, opened: the environment its scopes are suspended under,
-- its modality, its domain, its tope scope and its codomain scope.
data Pi n where
  Pi :: Foil.Distinct i
     => Env i n -> TModality -> Value n
     -> Maybe (ScopedAST Foil.NameBinder Sig i) -> ScopedAST Foil.NameBinder Sig i
     -> Pi n

piOf :: Value n -> Maybe (Pi n)
piOf v = case NbE.force (stripRestrictions v) of
  NbE.VSuspended env (AnnSig _ (TypeFunF _ md param mtope ret)) -> Just (Pi env md param mtope ret)
  _ -> Nothing

-- | A scoped subterm of a suspended node, opened at a binder of the outer
-- scope: the body under the environment extended with the binder's name.
openScoped
  :: (Foil.DExt n l, Foil.Distinct i)
  => Foil.Scope l -> Ctx l -> Env i n -> Foil.NameBinder n l
  -> ScopedAST Foil.NameBinder Sig i -> Value l
openScoped scope ctx env binder (ScopedAST b body) =
  case Foil.assertDistinct b of
    Foil.Distinct ->
      NbE.eval scope ctx (Foil.addSubst (Foil.sink env) b (NbE.vvar (Foil.nameOf binder))) body

-- | The codomain of a Π at a value.
codomainAt
  :: (Foil.Distinct n, Foil.Distinct i)
  => Foil.Scope n -> Ctx n -> Env i n -> ScopedAST Foil.NameBinder Sig i -> Value n -> Value n
codomainAt scope ctx env (ScopedAST b ret) arg = case Foil.assertDistinct b of
  Foil.Distinct -> NbE.eval scope ctx (Foil.addSubst env b arg) ret

-- | An annotation for a node the rules build, with the given type.
infoOf :: Value n -> Info n
infoOf ty = TypeInfo { infoType = ty, infoWHNF = Nothing, infoNF = Nothing }

-- | The universe, as a value: the type of the binders the rules invent for
-- nodes that never match.
universeValue :: Value n
universeValue = NbE.vnode (AnnSig (infoOf universeValue) UniverseF)

-- | The universe of topes, as a value.
topeUniverseValue :: Value n
topeUniverseValue = NbE.vnode (AnnSig (infoOf universeValue) UniverseTopeF)

-- | The identity type of a @refl@, from its annotation: the endpoints.
endpointsOf :: Info n -> Maybe (Value n, Value n)
endpointsOf info = case NbE.force (stripRestrictions (infoType info)) of
  NbE.VNode (AnnSig _ (TypeIdF x _ y)) -> Just (x, y)
  _                                    -> Nothing

-- * Rules

{-# SPECIALIZE NbE.nf :: Foil.Distinct n => Foil.Scope n -> Ctx n -> TermT n -> TermT n #-}
{-# SPECIALIZE NbE.eval ::
      (Foil.Distinct o, Foil.Distinct i) =>
      Foil.Scope o -> Ctx o -> Env i o -> TermT i -> Value o #-}
{-# SPECIALIZE NbE.quote :: Foil.Distinct n => Foil.Scope n -> Ctx n -> Value n -> TermT n #-}
{-# SPECIALIZE Conv.conv :: Foil.Distinct n => Foil.Scope n -> Ctx n -> Value n -> Value n -> Bool #-}
{-# SPECIALIZE Conv.convertible :: Foil.Distinct n => Foil.Scope n -> Ctx n -> TermT n -> TermT n -> Bool #-}

-- | β, δ through 'definition', let, the projections, path induction on
-- @refl@, and @recOR@ by the tope context; everything else is a constructor.
instance Eval Ctx Foil.NameBinder Sig where
  {-# INLINABLE evalSig #-}
  evalSig scope ctx env (AnnSig info node) = case node of
    AppF fun arg   -> appValue info' scope ctx (ev fun) (ev arg)
    FirstF p       -> firstValue info' (ev p)
    SecondF p      -> secondValue info' (ev p)
    LetF _orig _mparam val (ScopedAST x body) -> case Foil.assertDistinct x of
      Foil.Distinct -> NbE.eval scope ctx (Foil.addSubst env x (ev val)) body
    TypeAscF term _ty -> ev term
    IdJF tA a tC d x p -> idJValue info' (ev tA) (ev a) (ev tC) (ev d) (ev x) (ev p)
    -- The branch whose guard the tope context entails, as 'whnfT' chooses
    -- it; otherwise the one branch if they are all the same term; otherwise
    -- the node stays stuck.
    RecOrF rs ->
      case [ branch | (guard, branch) <- rs, entails (ctxContext ctx) (NbE.quote scope ctx (ev guard)) ] of
        branch : _ -> ev branch
        [] -> case nubT (map snd rs) of
          [branch] -> ev branch
          _        -> NbE.evalNode scope ctx env (AnnSig info node)
    -- The domain annotation of a λ is not part of its value: the unification
    -- compares two lambdas by their types, and η cannot observe it.
    LambdaF orig _mparam body -> NbE.evalNode scope ctx env (AnnSig info (LambdaF orig Nothing body))
    _ -> NbE.evalNode scope ctx env (AnnSig info node)
    where
      ev = NbE.eval scope ctx env
      info' = fmap ev info

  -- The observers of a λ apply the other value to the bound name, those of
  -- a pair project it.
  {-# INLINE etaMatchWith #-}
  etaMatchWith scoped term (AnnSig info node) = case node of
    LambdaF orig mparam body -> Just $ AnnSig (unobserved info) $
      LambdaF orig (paramObserved <$> mparam) (scoped body (EtaScoped appliedTo))
    PairF a b -> Just $ AnnSig (unobserved info) $
      PairF (term a (EtaTerm firstOf)) (term b (EtaTerm secondOf))
    _ -> Nothing
    where
      -- Conversion never compares the annotation, nor the domain annotation
      -- of a λ, which 'evalSig' drops from values.
      unobserved = fmap (`term` EtaTerm (\_ _ other -> other))
      paramObserved (LambdaParam md ty mtope) =
        LambdaParam md (term ty (EtaTerm (\_ _ other -> other)))
          (fmap (`scoped` EtaScoped (\_ _ _ other -> other)) mtope)

  -- What a binder adds to the context: its type and modality, and for the
  -- body of a λ over a shape its tope, from the λ's type.
  {-# INLINE extendScopedWith #-}
  extendScopedWith scoped term (AnnSig info node) = AnnSig (fmap term info) $ case node of
    LambdaF orig mparam body ->
      LambdaF orig (paramExtended <$> mparam) (scoped body (lambdaBinder orig (infoType info)))
    TypeFunF orig md param mtope ret ->
      TypeFunF orig md (term param) (fmap (`scoped` assumeVar orig md param) mtope) (scoped ret (assumeVar orig md param))
    TypeSigmaF orig md a b -> TypeSigmaF orig md (term a) (scoped b (assumeVar orig md a))
    LetF orig mparam val body ->
      LetF orig (fmap term mparam) (term val) (scoped body (assumeDefined orig val))
    -- Modal lets and match arms never match (the former are left to the
    -- unification, the latter never survive elaboration), so the binder's
    -- type is never consulted.
    LetModF orig app inn mparam mmotive val body ->
      LetModF orig app inn (fmap term mparam) (fmap term mmotive) (term val)
        (scoped body (assumeVar orig Id universeValue))
    MatchArmF orig body -> MatchArmF orig (scoped body (assumeVar orig Id universeValue))
    -- No other constructor has a scoped position, so the function on them
    -- is never called.
    _ -> bimap (const noScopedPosition) term node
    where
      paramExtended (LambdaParam md ty mtope) =
        LambdaParam md (term ty) (fmap (`scoped` assumeVar (BinderVar Nothing) md ty) mtope)
      noScopedPosition = error "Rzk.TypeCheck.NbE: impossible: a scoped position of a node without scoped positions"

  -- The cases of 'Rzk.TypeCheck.Unify.unifyInCurrentContext' that are not
  -- a structural comparison of the children in the ambient variance. Every
  -- other node is compared invariantly, position by position.
  {-# INLINE relateWith #-}
  relateWith scope ctx scoped term hd l@(AnnSig infoL nodeL) r@(AnnSig infoR nodeR) = case (nodeL, nodeR) of
    -- the context-sensitive fragment never matches
    (HoleF{}, _) -> Nothing
    (_, HoleF{}) -> Nothing
    (TypeModalF{}, _) -> Nothing
    (_, TypeModalF{}) -> Nothing
    (ModAppF{}, _) -> Nothing
    (_, ModAppF{}) -> Nothing
    (ModExtractF{}, _) -> Nothing
    (_, ModExtractF{}) -> Nothing
    (LetModF{}, _) -> Nothing
    (_, LetModF{}) -> Nothing

    -- recBOT unifies with anything, and so does unit on the expected side
    (RecBottomF, _) -> Just mempty
    (_, RecBottomF) -> Just mempty
    (UnitF, _) -> Just mempty

    -- two recORs are coherent branch by branch: under each overlap of their
    -- guards, the branches' types and the branches agree; a recOR against
    -- anything else is left to the unification
    (RecOrF rsL, RecOrF rsR) -> allOf
      [ case assumeTope (topeAndT (quoteIn tL) (quoteIn tR)) (ctxContext ctx) of
          Nothing -> Nothing
          Just overlap ->
            let ctx' = withContext overlap ctx
            in Just $ term (stripRestrictions (typeOfValue scope ctx bL)) (stripRestrictions (typeOfValue scope ctx bR)) ctx'
                      <> term bL bR ctx'
      | (tL, bL) <- rsL, (tR, bR) <- rsR ]
    (RecOrF{}, _) -> Nothing
    (_, RecOrF{}) -> Nothing

    -- two topes are compared by entailment, each way
    _ | isTope nodeL, isTope nodeR ->
      guarded (topesEquivalent (ctxContext ctx) (quoteNode scope ctx l) (quoteNode scope ctx r))

    -- the cube 2 is a subcube of I
    (CubeIF, Cube2F) | Covariant <- variance ctx     -> Just mempty
    (Cube2F, CubeIF) | Contravariant <- variance ctx -> Just mempty

    -- refl is compared by the endpoints of its type
    (ReflF{}, ReflF{}) -> case (endpointsOf infoL, endpointsOf infoR) of
      (Just (xL, yL), Just (xR, yR)) -> Just (term xL xR ctx <> term yL yR ctx)
      _                              -> Nothing

    -- two lambdas: their types by domain and tope (the modality, the
    -- parameter types, and the topes, as the unification compares them),
    -- and the bodies where both are defined
    (LambdaF _ _ bodyL, LambdaF _ _ bodyR) -> case (piOf (infoType infoL), piOf (infoType infoR)) of
      (Just piL, Just piR) -> Just $
        term (stripRestrictions (infoType infoL)) (stripRestrictions (infoType infoR)) (piDomainsOnly ctx)
          <> scoped bodyL bodyR (lambdasBinder piL piR)
      _ -> Nothing

    -- Π: the domain in the opposite variance, the topes as topes, the
    -- codomain under the binder
    (TypeFunF _ md cubeL mtopeL retL, TypeFunF _ md' cubeR mtopeR retR)
      | md == md' ->
          let under = assumeVar (BinderVar Nothing) md cubeR
              domain = term cubeL cubeR (withVariance (opposite (variance ctx)) ctx { ctxPiDomainsOnly = False })
              codomain
                | ctxPiDomainsOnly ctx = mempty
                | otherwise = scoped retL retR under
          in case (mtopeL, mtopeR) of
               (Nothing, Nothing) -> Just (domain <> codomain)
               (Just tL, Just tR) -> Just (domain <> scoped tL tR under <> codomain)
               _ -> Nothing
    (TypeSigmaF _ md aL bL, TypeSigmaF _ md' aR bR)
      | md == md' -> Just (term aL aR ctx <> scoped bL bR (assumeVar (BinderVar Nothing) md aR))
    (TypeIdF xL tAL yL, TypeIdF xR tAR yR) -> Just $
      (case (tAL, tAR) of
         (Just tL, Just tR) -> term tL tR (withVariance Invariant ctx)
         _                  -> mempty)
        <> term xL xR ctx <> term yL yR ctx
    (AppF fL xL, AppF fR xR) -> Just (hd fL fR ctx <> term xL xR (withVariance Invariant ctx))

    -- extension types: the underlying types, and the faces of the supertype
    -- covered by the faces of the subtype, agreeing where they overlap.
    -- Two face lists with the same topes, written the same, cover each
    -- other, so only the boundary terms are compared, without assuming the
    -- faces (which is stricter than comparing them under the faces).
    (TypeRestrictedF tyL rsL, TypeRestrictedF tyR rsR)
      | length rsL == length rsR
      , and (zipWith (\(tL, _) (tR, _) -> eqT (quoteIn tL) (quoteIn tR)) rsL rsR)
      , Just same <- allOf (zipWith (\(_, bL) (_, bR) -> Just (term bL bR ctx)) rsL rsR) ->
          Just (term tyL tyR ctx <> same)
    (TypeRestrictedF tyL rsL, TypeRestrictedF tyR rsR) -> allOf $
      Just (term tyL tyR ctx) : case variance ctx of
        Covariant     -> [facesCovered rsL rsR]
        Contravariant -> [facesCovered rsR rsL]
        Invariant     -> [facesCovered rsL rsR, facesCovered rsR rsL]

    _ -> NbE.relateNodes scope (withVariance Invariant ctx)
           (\a a' extend -> scoped a a' (invariantly extend)) term hd l r
    where
      quoteIn = NbE.quote scope ctx

      -- The faces of the supertype must be covered by the faces of the
      -- subtype: under each face of the supertype, the subtype's face topes
      -- cover the context, and the boundary terms agree under each overlap.
      -- The comparisons keep the sides of the nodes, so the terms are
      -- passed as (left, right) whichever side is the supertype.
      facesCovered superRs subRs = allOf
        [ case assumeTope (quoteIn tope) (ctxContext ctx) of
            Nothing -> Nothing
            Just face -> allOf $
              guarded (entails face (foldr (topeOrT . quoteIn . fst) topeBottomT subRs))
                : [ case assumeTope (quoteIn tope') face of
                      Nothing -> Nothing
                      Just overlap -> Just (sided term' (withContext overlap ctx))
                  | (tope', term') <- subRs ]
        | (tope, term0) <- superRs
        , let sided term' ctx' = case variance ctx of
                Contravariant -> term term' term0 ctx'
                _             -> term term0 term' ctx'
        ]

-- | A node without scoped positions (a tope, say) read back as a term.
quoteNode :: Foil.Distinct n => Foil.Scope n -> Ctx n -> Sig s (Value n) -> TermT n
quoteNode scope ctx (AnnSig info node) =
  Node (AnnSig (fmap quoteIn info) (bimap (const noScope) quoteIn node))
  where
    quoteIn = NbE.quote scope ctx
    noScope = error "Rzk.TypeCheck.NbE: impossible: a scoped position of a node without scoped positions"

-- | Whether a node belongs to the tope layer.
isTope :: TermSig a b -> Bool
isTope = \case
  TopeTopF     -> True
  TopeBottomF  -> True
  TopeEQF{}    -> True
  TopeLEQF{}   -> True
  TopeAndF{}   -> True
  TopeOrF{}    -> True
  TopeInvF{}   -> True
  TopeUninvF{} -> True
  _            -> False

-- | Apply a function value to an argument: a suspended λ is entered under
-- its environment extended with the argument, any other function is a stuck
-- application with the given annotation.
appValue :: Foil.Distinct n => Info n -> Foil.Scope n -> Ctx n -> Value n -> Value n -> Value n
{-# INLINE appValue #-}
appValue info scope ctx fun arg = NbE.eliminate fun $ \case
  NbE.VSuspended env (AnnSig _ (LambdaF _ _ (ScopedAST x body))) ->
    case Foil.assertDistinct x of
      Foil.Distinct -> NbE.eval scope ctx (Foil.addSubst env x arg) body
  NbE.Stuck fun' -> NbE.vnode (AnnSig info (AppF fun' arg))

firstValue :: Info n -> Value n -> Value n
{-# INLINE firstValue #-}
firstValue info p = NbE.eliminate p $ \case
  NbE.VNode (AnnSig _ (PairF a _)) -> a
  NbE.Stuck p' -> NbE.vnode (AnnSig info (FirstF p'))

secondValue :: Info n -> Value n -> Value n
{-# INLINE secondValue #-}
secondValue info p = NbE.eliminate p $ \case
  NbE.VNode (AnnSig _ (PairF _ b)) -> b
  NbE.Stuck p' -> NbE.vnode (AnnSig info (SecondF p'))

-- | Path induction: on @refl@ the base case, otherwise stuck on the path.
idJValue :: Info n -> Value n -> Value n -> Value n -> Value n -> Value n -> Value n -> Value n
{-# INLINE idJValue #-}
idJValue info tA a tC d x p = NbE.eliminate p $ \case
  NbE.VNode (AnnSig _ (ReflF _)) -> d
  NbE.Stuck p' -> NbE.vnode (AnnSig info (IdJF tA a tC d x p'))

-- * η

-- | The projections for η of pairs, annotated with the components of the
-- pair's Σ type.
firstOf :: Foil.Distinct n => Foil.Scope n -> Ctx n -> Value n -> Value n
{-# INLINE firstOf #-}
firstOf scope ctx p = firstValue (infoOf (sigmaFirst scope ctx p)) p

secondOf :: Foil.Distinct n => Foil.Scope n -> Ctx n -> Value n -> Value n
{-# INLINE secondOf #-}
secondOf scope ctx p = secondValue (infoOf (sigmaSecond scope ctx p)) p

sigmaFirst :: Foil.Distinct n => Foil.Scope n -> Ctx n -> Value n -> Value n
sigmaFirst scope ctx p = case NbE.force (stripRestrictions (typeOfValue scope ctx p)) of
  NbE.VSuspended _ (AnnSig _ (TypeSigmaF _ _ a _)) -> a
  NbE.VNode (AnnSig _ (CubeProductF a _))          -> a
  _ -> error "Rzk.TypeCheck.NbE: impossible: η for a pair against a value whose type is not a Σ"

sigmaSecond :: Foil.Distinct n => Foil.Scope n -> Ctx n -> Value n -> Value n
sigmaSecond scope ctx p = case NbE.force (stripRestrictions (typeOfValue scope ctx p)) of
  NbE.VSuspended env (AnnSig _ (TypeSigmaF _ _ _ b)) -> codomainAt scope ctx env b (firstOf scope ctx p)
  NbE.VNode (AnnSig _ (CubeProductF _ b))            -> b
  _ -> error "Rzk.TypeCheck.NbE: impossible: η for a pair against a value whose type is not a Σ"

-- | The observer of a λ's body: the other value applied to the bound name,
-- annotated with the codomain of its type there. First the λ's type (left
-- in the context by 'lambdaBinder') and the other value's are compared by
-- domain and tope, as the unification compares the types of the λ and of
-- the η-expansion of the other value; if they differ, the observation
-- matches nothing.
appliedTo :: Foil.Distinct l => Foil.Scope l -> Ctx l -> Foil.NameBinder n l -> Value l -> Value l
{-# INLINE appliedTo #-}
appliedTo scope ctx binder f
  | typesAgree = appValue (infoOf codomain) scope ctx f x
  | otherwise  = unmatched
  where
    x = NbE.vvar (Foil.nameOf binder)
    fType = typeOfValue scope ctx f
    typesAgree = case ctxEtaLambda ctx of
      Just lambdaType -> Conv.conv scope (piDomainsOnly ctx) (stripRestrictions lambdaType) (stripRestrictions fType)
      Nothing -> error "Rzk.TypeCheck.NbE: impossible: η under a binder that is not a λ's"
    codomain = case piOf fType of
      Just (Pi env _ _ _ ret) -> codomainAt scope ctx env ret x
      Nothing -> error "Rzk.TypeCheck.NbE: impossible: η for a λ against a value whose type is not a Π"

-- * Binders

-- | The binder of one name of a type, at a modality.
assumeVar :: Binder -> TModality -> Value n -> Extend Ctx Foil.NameBinder Sig n
{-# INLINE assumeVar #-}
assumeVar orig md ty = Extend $ \scope binder _ ctx ->
  enterContext binder orig md (NbE.quote scope ctx ty) Nothing ctx

-- | The binder of a let: its name has the value.
assumeDefined :: Binder -> Value n -> Extend Ctx Foil.NameBinder Sig n
{-# INLINE assumeDefined #-}
assumeDefined orig val = Extend $ \scope binder _ ctx ->
  enterContext binder orig Id (NbE.quote scope ctx (typeOfValue scope ctx val)) (Just (NbE.quote scope ctx val)) ctx

-- | The binder of a λ, from the λ's type: its domain and modality, and its
-- tope, assumed for the body.
lambdaBinder :: Binder -> Value n -> Extend Ctx Foil.NameBinder Sig n
{-# INLINE lambdaBinder #-}
lambdaBinder orig ty = Extend $ \scope binder scope' ctx -> case piOf ty of
  Just (Pi env md param mtope _) ->
    let ctx' = (enterContext binder orig md (NbE.quote scope ctx param) Nothing ctx)
                 { ctxEtaLambda = Just (Foil.sink ty) }
    in case mtope of
         Nothing   -> ctx'
         Just tope -> underTope scope' ctx' (openScoped scope' ctx' env binder tope)
  Nothing -> error ("Rzk.TypeCheck.NbE: impossible: a λ whose type is not a Π: "
    <> ppTerm (namingOfContext (ctxContext ctx)) (untyped (NbE.quote scope ctx ty)))

-- | The binder two lambdas are compared under: the right one's domain, and
-- both topes, since the bodies agree only where both are defined.
lambdasBinder :: Pi n -> Pi n -> Extend Ctx Foil.NameBinder Sig n
{-# INLINE lambdasBinder #-}
lambdasBinder (Pi envL _ _ mtopeL _) (Pi envR md paramR mtopeR _) = Extend $ \scope binder scope' ctx ->
  let ctx' = enterContext binder (BinderVar Nothing) md (NbE.quote scope ctx paramR) Nothing ctx
  in case (mtopeL, mtopeR) of
       (Just tL, Just tR) ->
         underTope scope' ctx' $ NbE.vnode $ AnnSig (infoOf topeUniverseValue) $
           TopeAndF (openScoped scope' ctx' envL binder tL) (openScoped scope' ctx' envR binder tR)
       _ -> ctx'

-- | The context with a tope value assumed. A tope the solver cannot take
-- (one with a recOR in it, say) is not assumed, which only keeps a guard
-- from reducing.
underTope :: Foil.Distinct n => Foil.Scope n -> Ctx n -> Value n -> Ctx n
underTope scope ctx tope = case assumeTope (NbE.quote scope ctx tope) (ctxContext ctx) of
  Just context -> withContext context ctx
  Nothing      -> ctx

-- | An extension of the context for invariant comparison of the positions
-- under it.
invariantly :: Extend Ctx Foil.NameBinder Sig n -> Extend Ctx Foil.NameBinder Sig n
{-# INLINE invariantly #-}
invariantly (Extend extend) = Extend $ \scope binder scope' ctx ->
  withVariance Invariant (extend scope binder scope' ctx)

-- * Entry point

-- | The answer of the fast path.
--
-- Note that the two cases are not opposites, so this is deliberately not a
-- 'Bool': 'DontKnow' is never a refutation, and a caller may not read it as
-- one.
data Conversion
  = Convertible
    -- ^ Definitely convertible, by the module's soundness argument.
  | DontKnow
    -- ^ No answer. The two terms may well be convertible, and the caller
    -- falls back to the ordinary unification to find out.
  deriving (Eq, Show)

-- | Are the two terms definitely convertible (or, in the context's
-- variance, in the subtyping relation)? Which fast path answers is the
-- context's 'ctxFastPath'; under 'FastPathCheck' the answer is 'DontKnow',
-- since the unification decides every query and 'nbeDifferential' records
-- the fast paths' answers.
nbeConvertible :: Foil.Distinct n => TermT n -> TermT n -> TypeCheck n Conversion
nbeConvertible t1 t2 = asks $ \ctx -> case ctxFastPath ctx of
  FastPathOff     -> DontKnow
  FastPathSpine   -> spine ctx
  FastPathGeneric -> generic ctx
  FastPathCheck   -> DontKnow
  where
    generic ctx
      | Conv.convertible (ctxScope ctx) (rootCtx ctx) t1 t2 = Convertible
      | otherwise = DontKnow
    spine ctx = case Spine.nbeConvertible' ctx t1 t2 of
      Spine.Convertible -> Convertible
      Spine.DontKnow    -> DontKnow

-- | The answers of the generic and of the hand-written fast path, tallied
-- in the check log (the differential test, 'FastPathCheck').
nbeDifferential :: Foil.Distinct n => TermT n -> TermT n -> TypeCheck n (Conversion, Conversion)
nbeDifferential t1 t2 = do
  ctx <- ask
  let generic
        | Conv.convertible (ctxScope ctx) (rootCtx ctx) t1 t2 = Convertible
        | otherwise = DontKnow
      spine = case Spine.nbeConvertible' ctx t1 t2 of
        Spine.Convertible -> Convertible
        Spine.DontKnow    -> DontKnow
      hit answer = if answer == Convertible then 1 else 0
  modifyLog $ \l -> l { logNbEStats = (logNbEStats l)
    { nbeQueries = nbeQueries (logNbEStats l) + 1
    , nbeGenericHits = nbeGenericHits (logNbEStats l) + hit generic
    , nbeSpineHits = nbeSpineHits (logNbEStats l) + hit spine
    , nbeGenericOnly = nbeGenericOnly (logNbEStats l) + hit generic * (1 - hit spine)
    , nbeSpineOnly = nbeSpineOnly (logNbEStats l) + hit spine * (1 - hit generic)
    } }
  pure (generic, spine)
