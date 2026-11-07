{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
-- The 'HasHeads' instance of the signature is an orphan: the class is
-- free-foil-NBE's and the signature is "Rzk.Check.Syntax"'s, and it
-- belongs with the rules here.
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Values: Rzk's rules as an 'Eval' instance of free-foil-NBE on the
-- checker's context.
--
-- Evaluation is the library's; this module gives it the elimination rules
-- (β, the projections, path induction, @let@, a @recOR@ whose guard the tope
-- context entails, the lattice and flip operations of the cube layer, the
-- connectives of the tope layer), the η laws of λ and pairs, what each
-- binder adds to the context, how two nodes compare (subtyping in the
-- context's variance, holes, topes by entailment), and the refinement of a
-- neutral by a restriction of its type.
--
-- The algorithm is free-foil-NBE's (Renata Shakirova and Nikolai Kudasov):
-- normalisation by evaluation generic in the signature, conversion checking
-- after Coquand (/An algorithm for type-checking dependent types/, 1996),
-- the untyped η for surjective pairs of Abel and Coquand (2007), glued
-- evaluation and approximate conversion checking after Kovács' smalltt, and
-- subtyping as conversion with the direction in the context, as in Rocq's
-- kernel and in Rzk's own unification. The rules follow the evaluator and
-- the unification of "Rzk.TypeCheck", and the fast path on free-foil-NBE
-- that preceded this checker.
module Rzk.Check.Value
  ( -- * Values
    Value
  , Whnf
  , Env
  , TypeValue (..)
  , TopeValue (..)
  , CubeValue (..)
    -- * Evaluation and readback
  , evalIn
  , evalCore
  , evalScope
  , quoteIn
  , quoteFolded
  , nfCore
  , force
  , conv
  , convertible
    -- * Values the rules build
  , vnode
  , universe
  , cubeUniverse
  , topeUniverse
  , vcube2
  , vcubeI
  , vtopeTop
  , vtopeBottom
  , vunit
  , vtypeUnit
  , vhole
  , vrecBottom
  , vlambda
    -- * Elimination
  , appValue
  , firstValue
  , secondValue
  , codomainAt
  , Pi (..)
  , piOf
  , Sigma (..)
  , sigmaOf
  , Family (..)
  , familyOf
  , isTopShape
  , stripRestrictions
  , restrictionsOf
    -- * Patterns
  , patternValue
  , bindPattern
  , patternEntries
  , letEntries
  , singlePattern
  , underPattern
  , underFreshVar
  , isCube
  , topShapeValue
    -- * The tope layer
  , topeOf
  , pointOf
  , cubeOfType
  , entailsValue
  , assumeValue
  , topeAndValue
    -- * Types of neutrals
  , neutralType
  , flagsOfType
    -- * Holes
  , valueHasHole
  ) where

import           Control.Applicative         ((<|>))
import           Control.Monad.Foil          (DExt, Distinct, NameBinder, Scope)
import qualified Control.Monad.Foil          as Foil
import           Control.Monad.Free.Foil     (AST (..), ScopedAST (..))
import           Data.Bifoldable             (bifoldMap)
import           Data.Bifunctor              (bimap)
import           Data.List                   (intercalate)
import           Data.Maybe                  (fromMaybe, listToMaybe)
import           Data.Monoid                 (Any (..))
import           Data.Void                   (Void)
import           Unsafe.Coerce               (unsafeCoerce)

import           FreeFoil.NbE                (EtaScoped (..), EtaTerm (..),
                                              Eval (..), Extend (..),
                                              GlueSpeculation (..),
                                              HasDefinitions (..),
                                              HasHeads (..))
import qualified FreeFoil.NbE                as NbE
import qualified FreeFoil.NbE.Conversion     as Conv

import           Language.Rzk.Foil.Names     (TModality (..), VarIdent)
import           Rzk.Check.Context
import           Rzk.Check.Syntax
import           Rzk.Check.Topes

-- * Values

type Whnf = NbE.Whnf Pattern Sig

type Env i n = Foil.Substitution (NbE.Value Pattern Sig) i n

-- | A value standing for a type.
newtype TypeValue n = TypeValue { typeValue :: Value n }

-- | A value standing for a tope.
newtype TopeValue n = TopeValue { topeValue :: Value n }

-- | A value standing for a cube.
newtype CubeValue n = CubeValue { cubeValue :: Value n }

instance Foil.Sinkable TypeValue where
  sinkabilityProof rename (TypeValue v) = TypeValue (Foil.sinkabilityProof rename v)

instance Foil.Sinkable TopeValue where
  sinkabilityProof rename (TopeValue v) = TopeValue (Foil.sinkabilityProof rename v)

-- A node's constructor, for fingerprints.
deriving instance Show (Sig () ())

-- * The library's hooks

-- | A defined name is glued to its value; any other name is a neutral.
instance HasDefinitions (Context d) Pattern Sig where
  {-# INLINE contextDefinition #-}
  contextDefinition _ ctx x = case entryValue (lookupEntry x ctx) of
    Just value -> NbE.vglued x value
    Nothing    -> NbE.vvar x
  {-# INLINE contextUnfolding #-}
  contextUnfolding _ ctx folded unfolded = case ctxUnfolding ctx of
    Folded   -> folded
    Unfolded -> unfolded
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

-- | The heads: the function of an application, the pair of a projection,
-- the path of an induction.
instance HasHeads Sig where
  {-# INLINE headsWith #-}
  headsWith hd other = \case
    AppF f x           -> AppF (hd f) (other x)
    FirstF p           -> FirstF (hd p)
    SecondF p          -> SecondF (hd p)
    IdJF tA a tC d x p -> IdJF (other tA) (other a) (other tC) (other d) (other x) (hd p)
    node               -> bimap id other node

{-# SPECIALIZE NbE.eval ::
      (Distinct o, Distinct i) => Scope o -> Context d o -> Env i o -> Core i -> Value o #-}
{-# SPECIALIZE NbE.quote :: Distinct n => Scope n -> Context d n -> Value n -> Core n #-}
{-# SPECIALIZE NbE.nf :: Distinct n => Scope n -> Context d n -> Core n -> Core n #-}
{-# SPECIALIZE Conv.conv :: Distinct n => Scope n -> Context d n -> Value n -> Value n -> Bool #-}
{-# SPECIALIZE Conv.convertible :: Distinct n => Scope n -> Context d n -> Core n -> Core n -> Bool #-}

instance Eval (Context d) Pattern Sig where
  {-# INLINABLE evalSig #-}
  evalSig scope ctx env = \case
    AppF fun arg -> appValue scope ctx (ev fun) (ev arg)
    FirstF p -> firstValue (ev p)
    SecondF p -> secondValue (ev p)
    LetF _mty val (ScopedAST pat body) -> case Foil.assertDistinct pat of
      Foil.Distinct ->
        NbE.eval scope ctx (bindPattern env pat (ev val)) body
    TypeAscF term _ty -> ev term
    IdJF tA a tC d x p -> idJValue (ev tA) (ev a) (ev tC) (ev d) (ev x) (ev p)
    -- A face whose tope is ⊥ restricts nothing, and a type with no face
    -- left is its underlying type, as the old checker's weak head normal
    -- form dropped them. Such a face arises by instantiation, as with the
    -- subshape ⊥ of a shape.
    TypeRestrictedF ty rs ->
      case [ (tope', face) | (tope, face) <- rs, let tope' = ev tope, not (isBottomTope tope') ] of
        []  -> ev ty
        rs' -> NbE.vnode (TypeRestrictedF (ev ty) [ (tope', ev face) | (tope', face) <- rs' ])
    -- The branch whose guard the tope context entails; otherwise the one
    -- branch if they are all the same term; otherwise stuck.
    RecOrF rs ->
      let rs' = [ (ev guard, branch) | (guard, branch) <- rs ]
      in case [ branch | (guard, branch) <- rs', entailsValue scope ctx guard ] of
           branch : _ -> ev branch
           [] -> case map snd rs of
             branch : branches
               | all (eqCore branch) branches -> ev branch
             _ -> NbE.vnode (RecOrF [ (guard, ev branch) | (guard, branch) <- rs' ])
    -- the cube layer
    CubeFlipF t -> case NbE.force (ev t) of
      NbE.VNode (CubeUnflipF t') -> t'
      NbE.VNode Cube2_0F -> opp (NbE.vnode Cube2_1F)
      NbE.VNode Cube2_1F -> opp (NbE.vnode Cube2_0F)
      NbE.VNode CubeI_0F -> opp (NbE.vnode CubeI_1F)
      NbE.VNode CubeI_1F -> opp (NbE.vnode CubeI_0F)
      NbE.Stuck t' -> NbE.vnode (CubeFlipF t')
    CubeUnflipF t -> case NbE.force (ev t) of
      NbE.VNode (CubeFlipF t') -> t'
      NbE.VNode (ModAppF Op p) | NbE.VNode Cube2_0F <- NbE.force p -> NbE.vnode Cube2_1F
      NbE.VNode (ModAppF Op p) | NbE.VNode Cube2_1F <- NbE.force p -> NbE.vnode Cube2_0F
      NbE.VNode (ModAppF Op p) | NbE.VNode CubeI_0F <- NbE.force p -> NbE.vnode CubeI_1F
      NbE.VNode (ModAppF Op p) | NbE.VNode CubeI_1F <- NbE.force p -> NbE.vnode CubeI_0F
      NbE.Stuck t' -> NbE.vnode (CubeUnflipF t')
    CubeSupF l r -> supValue (ev l) (ev r)
    CubeInfF l r -> infValue (ev l) (ev r)
    -- the tope layer
    TopeAndF l r -> case NbE.force (ev l) of
      NbE.VNode TopeBottomF -> vtopeBottom
      NbE.Stuck l' -> case NbE.force (ev r) of
        NbE.VNode TopeBottomF -> vtopeBottom
        NbE.Stuck r' -> NbE.vnode (TopeAndF l' r')
    TopeOrF l r -> case (NbE.force (ev l), NbE.force (ev r)) of
      (NbE.VNode TopeBottomF, r') -> NbE.fromWhnf r'
      (l', NbE.VNode TopeBottomF) -> NbE.fromWhnf l'
      (l', r') -> NbE.vnode (TopeOrF (NbE.fromWhnf l') (NbE.fromWhnf r'))
    node -> NbE.evalNode scope ctx env node
    where
      ev = NbE.eval scope ctx env
      opp = NbE.vnode . ModAppF Op

  -- The observers of a λ apply the other value to the pattern; those of a
  -- pair project it.
  {-# INLINE etaMatchWith #-}
  etaMatchWith scoped term = \case
    -- the domain annotation is not compared: both sides have the same type
    LambdaF _ body -> Just (LambdaF Nothing (scoped body (EtaScoped appliedToPattern)))
    PairF a b -> Just (PairF (term a (EtaTerm (\_ _ -> firstValue))) (term b (EtaTerm (\_ _ -> secondValue))))
    _ -> Nothing

  -- What a binder adds to the context.
  {-# INLINE extendScopedWith #-}
  extendScopedWith scoped term = \case
    LambdaF mparam body ->
      LambdaF (fmap (fmap term) mparam) (scoped body (lambdaBinder mparam))
    TypeFunF md dom mshape ret ->
      TypeFunF md (term dom) (fmap term mshape) (scoped ret (piBinder md dom mshape))
    TypeSigmaF md a b ->
      TypeSigmaF md (term a) (scoped b (sigmaBinder md a))
    LetF mty val body ->
      LetF (fmap term mty) (term val) (scoped body (letBinder mty val))
    LetModF app inn mty mmotive val body ->
      LetModF app inn (fmap term mty) (fmap term mmotive) (term val)
        (scoped body opaqueBinder)
    MatchArmF body -> MatchArmF (scoped body opaqueBinder)
    node -> bimap (const noScopedPosition) term node
    where
      noScopedPosition = error "Rzk.Check.Value: impossible: a scoped position of a node without scoped positions"

  -- Two points that differ as values are equal if the tope context
  -- entails their equation, as the old unification compared every two
  -- terms whose type is a cube.
  {-# INLINE relateMismatch #-}
  relateMismatch scope ctx l r =
    isPointValue scope ctx l && isPointValue scope ctx r
      && entails (ctxTopes ctx) (TEq (pointOf scope ctx l) (pointOf scope ctx r))

  -- Subtyping in the context's variance; the cases of the old unification
  -- that are not a structural comparison of the children.
  {-# INLINE relateWith #-}
  relateWith scope ctx scoped term hd l r = case (l, r) of
    -- a hole stands for a term of the expected type
    (HoleF _, _) -> Just mempty
    (_, HoleF _) -> Just mempty
    -- recBOT inhabits every type, unit is the only point of Unit
    (RecBottomF, _) -> Just mempty
    (_, RecBottomF) -> Just mempty
    (UnitF, _) -> Just mempty
    (_, UnitF) -> Just mempty
    -- the modal fragment is not compared yet
    (TypeModalF{}, _) -> Nothing
    (_, TypeModalF{}) -> Nothing
    (ModAppF{}, _) -> Nothing
    (_, ModAppF{}) -> Nothing
    (ModExtractF{}, _) -> Nothing
    (_, ModExtractF{}) -> Nothing
    (LetModF{}, _) -> Nothing
    (_, LetModF{}) -> Nothing

    -- two recORs are coherent branch by branch: under each overlap of their
    -- guards the branches agree; a recOR against another term agrees with
    -- it under each guard
    (RecOrF rsL, RecOrF rsR) -> Just $ mconcat
      [ underOverlap (topeAndValue tL tR) (term bL bR)
      | (tL, bL) <- rsL, (tR, bR) <- rsR ]
    (RecOrF rsL, _) | Just rv <- nodeValue r -> Just $ mconcat
      [ underOverlap tL (term bL rv) | (tL, bL) <- rsL ]
    (_, RecOrF rsR) | Just lv <- nodeValue l -> Just $ mconcat
      [ underOverlap tR (term lv bR) | (tR, bR) <- rsR ]

    -- two topes are compared by entailment in the context's variance: the
    -- subtype's tope entails the supertype's
    _ | isTopeNode l, isTopeNode r, Just lv <- nodeValue l, Just rv <- nodeValue r ->
      let tl = topeOf scope ctx lv
          tr = topeOf scope ctx rv
          topes = ctxTopes ctx
      in guarded $ case ctxVariance ctx of
           Covariant     -> topesEntail topes [tr] tl
           Contravariant -> topesEntail topes [tl] tr
           Invariant     -> equivalent topes tl tr

    -- the cube 2 is a subcube of 𝕀
    (CubeIF, Cube2F) | Covariant <- ctxVariance ctx     -> Just mempty
    (Cube2F, CubeIF) | Contravariant <- ctxVariance ctx -> Just mempty

    -- refl is refl: its endpoints are those of its type
    (ReflF _, ReflF _) -> Just mempty

    -- two lambdas: their domains, and the bodies where both shapes hold
    (LambdaF mpL bodyL, LambdaF mpR bodyR) -> Just $
      (case (mpL, mpR) of
         (Just pL, Just pR) -> term (paramDomain pL) (paramDomain pR) (withVariance Invariant ctx)
         _ -> mempty)
      <> scoped bodyL bodyR ctx (lambdasBinder mpL mpR)

    -- a family type: the cube in the opposite variance, the shape in the
    -- context's variance (the family's subtyping is covariant in it, and
    -- the tope context plays no part)
    (TypeShapeF md cubeL mshapeL, TypeShapeF md' cubeR mshapeR)
      | md == md' ->
          let domain = term cubeL cubeR (withVariance (opposite (ctxVariance ctx)) ctx)
          in case (mshapeL, mshapeR) of
               (Nothing, Nothing) -> Just domain
               (Just sL, Just sR) -> Just (domain <> term sL sR ctx)
               _ -> Nothing

    -- Π: the domain in the opposite variance, the shapes as shapes, the
    -- codomain under the binder
    (TypeFunF md domL mshapeL retL, TypeFunF md' domR mshapeR retR)
      | md == md' ->
          let ctx' = withVariance (opposite (ctxVariance ctx)) ctx
              domain = term domL domR ctx'
              shapes = case (mshapeL, mshapeR) of
                (Nothing, Nothing) -> Just mempty
                (Just sL, Just sR) -> Just (term sL sR ctx')
                _ -> Nothing
          in fmap (\s -> domain <> s <> scoped retL retR ctx (piBinder md domR Nothing)) shapes
    (TypeSigmaF md aL bL, TypeSigmaF md' aR bR)
      | md == md' -> Just (term aL aR ctx <> scoped bL bR ctx (sigmaBinder md aR))
    (TypeIdF xL tAL yL, TypeIdF xR tAR yR) -> Just $
      (case (tAL, tAR) of
         (Just tL, Just tR) -> term tL tR (withVariance Invariant ctx)
         _ -> mempty)
        <> term xL xR ctx <> term yL yR ctx
    (AppF fL xL, AppF fR xR) -> Just (hd fL fR ctx <> term xL xR (withVariance Invariant ctx))

    -- extension types: the underlying types, and the faces of the supertype
    -- covered by the faces of the subtype, agreeing where they overlap
    (TypeRestrictedF tyL rsL, TypeRestrictedF tyR rsR)
      | length rsL == length rsR
      , and (zipWith (\(tL, _) (tR, _) -> topeOf scope ctx tL == topeOf scope ctx tR) rsL rsR) ->
          Just (term tyL tyR ctx <> mconcat (zipWith (\(_, bL) (_, bR) -> term bL bR ctx) rsL rsR))
    (TypeRestrictedF tyL rsL, TypeRestrictedF tyR rsR) -> fmap (term tyL tyR ctx <>) $
      case ctxVariance ctx of
        Covariant     -> facesCovered rsL rsR
        Contravariant -> facesCovered rsR rsL
        Invariant     -> (<>) <$> facesCovered rsL rsR <*> facesCovered rsR rsL
    -- a restricted type against an unrestricted one is left to the
    -- checker's subtyping, which has the term the faces are checked on

    _ -> NbE.relateNodes scope (withVariance Invariant ctx) scoped term hd l r
    where
      guarded holds = if holds then Just mempty else Nothing

      -- A comparison under a tope, which holds trivially when the tope
      -- makes the context contradictory.
      underOverlap tope compare' =
        let ctx' = assumeValue scope ctx tope
        in if entailsBottom (ctxTopes ctx') then mempty else compare' ctx'

      -- The faces of the supertype are covered by the faces of the subtype:
      -- under each face of the supertype, the subtype's topes cover the
      -- context, and the boundary terms agree under each overlap. The sides
      -- of the comparison are kept whichever side is the supertype.
      facesCovered superRs subRs = fmap mconcat $ sequence
        [ let face = assumeValue scope ctx tope
          in if entailsBottom (ctxTopes face) then Just mempty
             else if entailsUnion (ctxTopes face) (map (topeOf scope face . fst) subRs)
               then Just $ mconcat
                 [ let overlap = assumeValue scope face tope'
                   in if entailsBottom (ctxTopes overlap) then mempty else sided term' overlap
                 | (tope', term') <- subRs ]
               else Nothing
        | (tope, term0) <- superRs
        , let sided term' ctx' = case ctxVariance ctx of
                Contravariant -> term term' term0 ctx'
                _             -> term term0 term' ctx'
        ]

  -- A neutral reduces by a restriction of its type whose tope holds, and
  -- a point of the unit type is unit.
  {-# INLINE refineVar #-}
  refineVar scope ctx x
    | flagRefines (entryFlags (lookupEntry x ctx)) = refineNeutral scope ctx (NbE.vvar x)
    | otherwise = NbE.vvar x

  {-# INLINE refineSig #-}
  refineSig scope ctx value node = case node of
    AppF{}    -> refineSpine
    FirstF{}  -> refineSpine
    SecondF{} -> refineSpine
    IdJF{}    -> refineSpine
    _         -> value
    where
      refineSpine = case spineHead value of
        Just x | flagRefines (entryFlags (lookupEntry x ctx)) -> refineNeutral scope ctx value
        _ -> value

  -- A pattern stands for its names paired; bound to a value, its leaves
  -- get the projections.
  {-# INLINE binderValue #-}
  binderValue _ _ pat = Just (patternValue pat)

  {-# INLINE bindValue #-}
  bindValue _ _ env pat v = Just (bindPattern env pat v)

-- | Is a value a point of a cube: an endpoint, an operation on points, or
-- a neutral whose type is a cube? A pair of points is not asked about:
-- conversion compares its components, which are.
isPointValue :: Distinct n => Scope n -> Context d n -> Value n -> Bool
isPointValue scope ctx v = case NbE.force v of
  NbE.VNode node -> case node of
    CubeUnitStarF -> True
    Cube2_0F      -> True
    Cube2_1F      -> True
    CubeI_0F      -> True
    CubeI_1F      -> True
    CubeSupF{}    -> True
    CubeInfF{}    -> True
    CubeFlipF{}   -> True
    CubeUnflipF{} -> True
    PairF{}       -> False
    _             -> neutralOfCube
  NbE.VVar _ -> neutralOfCube
  _ -> False
  where
    neutralOfCube = maybe False (isCube scope ctx) (neutralType scope ctx v)

-- | Is a tope value ⊥?
isBottomTope :: Value n -> Bool
isBottomTope v = case NbE.force v of
  NbE.VNode TopeBottomF -> True
  _                     -> False

-- | A node without scoped positions, as a value.
nodeValue :: Sig a (Value n) -> Maybe (Value n)
nodeValue node
  | getAny (bifoldMap (const (Any True)) (const mempty) node) = Nothing
  | otherwise = Just (NbE.vnode (bimap (const noScoped) id node))
  where
    noScoped = error "Rzk.Check.Value: impossible: a scoped position of a node without scoped positions"

-- | The head variable of a neutral spine, if it is one.
spineHead :: Value n -> Maybe (Foil.Name n)
spineHead v = case NbE.force v of
  NbE.VVar x -> Just x
  NbE.VNode (AppF f _) -> spineHead f
  NbE.VNode (FirstF p) -> spineHead p
  NbE.VNode (SecondF p) -> spineHead p
  NbE.VNode (IdJF _ _ _ _ _ p) -> spineHead p
  _ -> Nothing

-- | Refine a neutral by its type: a restriction whose tope the context
-- entails gives its term; the unit type gives unit. The spine is walked
-- from its head, since a restriction may be on the type of a prefix: in
-- @g t x@ with @g : (t : ψ) → (X → A) [ϕ t ↦ f t]@, it is @g t@ that is
-- @f t@ under @ϕ t@. A refined prefix gets the rest of the spine again by
-- evaluation. The result, like the term of a restriction, is refined in
-- turn, since it may be a neutral restricted further (a component of a
-- pair restricted under a smaller tope).
refineNeutral :: Distinct n => Scope n -> Context d n -> Value n -> Value n
refineNeutral scope ctx value = fromMaybe value (refinedNeutral scope ctx value)

-- | A neutral refined by its type or the type of a prefix of its spine, or
-- 'Nothing' if neither has a restriction the context entails.
refinedNeutral :: Distinct n => Scope n -> Context d n -> Value n -> Maybe (Value n)
refinedNeutral scope ctx value = viaPrefix <|> byOwnType
  where
    viaPrefix = case NbE.forceFolded value of
      NbE.VNode (AppF f a)  -> again (\f' -> appValue scope ctx f' a) f
      NbE.VNode (FirstF p)  -> again firstValue p
      NbE.VNode (SecondF p) -> again secondValue p
      _                     -> Nothing
    again eliminate prefix =
      refineNeutral scope ctx . eliminate <$> refinedNeutral scope ctx prefix
    byOwnType = do
      ty <- neutralType scope ctx value
      case NbE.force ty of
        NbE.VNode TypeUnitF -> Just vunit
        _ -> refineNeutral scope ctx <$>
               listToMaybe [ t | (tope, t) <- restrictionsOf ty, entailsValue scope ctx tope ]

-- * Evaluation and readback

evalIn :: (Distinct o, Distinct i) => Scope o -> Context d o -> Env i o -> Core i -> Value o
{-# INLINE evalIn #-}
evalIn = NbE.eval

-- | A core term as a value, in the scope it was checked in.
evalCore :: Distinct n => Scope n -> Context d n -> Core n -> Value n
{-# INLINE evalCore #-}
evalCore scope ctx = NbE.eval scope ctx Foil.identitySubst

-- | A scoped term at a value for its pattern.
evalScope :: (Distinct n, Distinct i) => Scope n -> Context d n -> Env i n -> ScopedCore i -> Value n -> Value n
{-# INLINE evalScope #-}
evalScope scope ctx env (ScopedAST pat body) arg = case Foil.assertDistinct pat of
  Foil.Distinct -> NbE.eval scope ctx (bindPattern env pat arg) body

quoteIn :: Distinct n => Scope n -> Context d n -> Value n -> Core n
{-# INLINE quoteIn #-}
quoteIn = NbE.quote

-- | Read a value back with defined names folded, as the checker's own terms
-- spell them.
quoteFolded :: Distinct n => Scope n -> Context d n -> Value n -> Core n
quoteFolded scope ctx = NbE.quote scope ctx { ctxUnfolding = Folded }

nfCore :: Distinct n => Scope n -> Context d n -> Core n -> Core n
{-# INLINE nfCore #-}
nfCore = NbE.nf

force :: Value n -> Whnf n
{-# INLINE force #-}
force = NbE.force

-- | Two values compared in the context's variance.
conv :: Distinct n => Scope n -> Context d n -> Value n -> Value n -> Bool
{-# INLINE conv #-}
conv = Conv.conv

convertible :: Distinct n => Scope n -> Context d n -> Core n -> Core n -> Bool
{-# INLINE convertible #-}
convertible = Conv.convertible

-- * Values the rules build

vnode :: Sig Void (Value n) -> Value n
{-# INLINE vnode #-}
vnode = NbE.vnode

universe, cubeUniverse, topeUniverse, vcube2, vcubeI, vtopeTop, vtopeBottom, vunit, vtypeUnit, vrecBottom :: Value n
universe = NbE.vnode UniverseF
cubeUniverse = NbE.vnode UniverseCubeF
topeUniverse = NbE.vnode UniverseTopeF
vcube2 = NbE.vnode Cube2F
vcubeI = NbE.vnode CubeIF
vtopeTop = NbE.vnode TopeTopF
vtopeBottom = NbE.vnode TopeBottomF
vunit = NbE.vnode UnitF
vtypeUnit = NbE.vnode TypeUnitF
vrecBottom = NbE.vnode RecBottomF

vhole :: Maybe VarIdent -> Value n
vhole = NbE.vnode . HoleF

-- | A λ value over a core scope under an environment.
vlambda :: Distinct i => Env i n -> Maybe (Param (Value n)) -> ScopedCore i -> Value n
vlambda env mparam body = NbE.vsuspended env (LambdaF mparam body)

-- * Elimination

-- | Apply a function value: a λ enters its body under the pattern bound to
-- the argument; a neutral stays stuck, with the shape of a tope family it
-- is an application of as a side condition.
appValue :: Distinct n => Scope n -> Context d n -> Value n -> Value n -> Value n
{-# INLINE appValue #-}
appValue scope ctx fun arg = NbE.eliminate fun $ \case
  NbE.VSuspended env (LambdaF _ (ScopedAST pat body)) ->
    case Foil.assertDistinct pat of
      Foil.Distinct -> NbE.eval scope ctx (bindPattern env pat arg) body
  NbE.Stuck fun' -> case spineHead fun' of
    Just x | flagTopeFamily (entryFlags (lookupEntry x ctx))
           , Just ty <- neutralType scope ctx fun'
           , Just (Family _ _ (Just shape)) <- familyOf ty
           , not (isTopShape shape) ->
        topeAndValue (appValue scope ctx shape arg) (NbE.vnode (AppF fun' arg))
    _ -> NbE.vnode (AppF fun' arg)

firstValue :: Value n -> Value n
{-# INLINE firstValue #-}
firstValue p = NbE.eliminate p $ \case
  NbE.VNode (PairF a _) -> a
  NbE.Stuck p' -> NbE.vnode (FirstF p')

secondValue :: Value n -> Value n
{-# INLINE secondValue #-}
secondValue p = NbE.eliminate p $ \case
  NbE.VNode (PairF _ b) -> b
  NbE.Stuck p' -> NbE.vnode (SecondF p')

-- | Path induction: on @refl@ the base case, otherwise stuck on the path.
idJValue :: Value n -> Value n -> Value n -> Value n -> Value n -> Value n -> Value n
{-# INLINE idJValue #-}
idJValue tA a tC d x p = NbE.eliminate p $ \case
  NbE.VNode (ReflF _) -> d
  NbE.Stuck p' -> NbE.vnode (IdJF tA a tC d x p')

-- | The join of two points, normalised at the endpoints.
supValue :: Value n -> Value n -> Value n
supValue l r = case (NbE.force l, NbE.force r) of
  (NbE.VNode Cube2_0F, _) -> r
  (_, NbE.VNode Cube2_0F) -> l
  (NbE.VNode Cube2_1F, _) -> l
  (_, NbE.VNode Cube2_1F) -> r
  (NbE.VNode CubeI_0F, _) -> r
  (_, NbE.VNode CubeI_0F) -> l
  (NbE.VNode CubeI_1F, _) -> l
  (_, NbE.VNode CubeI_1F) -> r
  _ -> NbE.vnode (CubeSupF l r)

-- | The meet of two points, normalised at the endpoints and distributed
-- over a join.
infValue :: Value n -> Value n -> Value n
infValue l r = case (NbE.force l, NbE.force r) of
  (NbE.VNode Cube2_0F, _) -> l
  (_, NbE.VNode Cube2_0F) -> r
  (NbE.VNode Cube2_1F, _) -> r
  (_, NbE.VNode Cube2_1F) -> l
  (NbE.VNode CubeI_0F, _) -> l
  (_, NbE.VNode CubeI_0F) -> r
  (NbE.VNode CubeI_1F, _) -> r
  (_, NbE.VNode CubeI_1F) -> l
  (NbE.VNode (CubeSupF a b), _) -> supValue (infValue a r) (infValue b r)
  (_, NbE.VNode (CubeSupF a b)) -> supValue (infValue l a) (infValue l b)
  _ -> NbE.vnode (CubeInfF l r)

-- | The codomain of a scope at a value for its pattern.
codomainAt :: (Distinct n, Distinct i) => Scope n -> Context d n -> Env i n -> ScopedCore i -> Value n -> Value n
{-# INLINE codomainAt #-}
codomainAt = evalScope

-- | A Π type value, opened: the environment its codomain is suspended
-- under, its modality, its domain, its shape and its codomain.
data Pi n where
  Pi :: Distinct i => Env i n -> TModality -> Value n -> Maybe (Value n) -> ScopedCore i -> Pi n

piOf :: Value n -> Maybe (Pi n)
piOf v = case NbE.force (stripRestrictions v) of
  NbE.VSuspended env (TypeFunF md dom mshape ret) -> Just (Pi env md dom mshape ret)
  _ -> Nothing

-- | The type of a tope family: its modality, its cube and its shape.
data Family n = Family TModality (Value n) (Maybe (Value n))

familyOf :: Value n -> Maybe (Family n)
familyOf v = case NbE.force (stripRestrictions v) of
  NbE.VNode (TypeShapeF md cube mshape) -> Just (Family md cube mshape)
  _ -> Nothing

-- | Is a shape the one that is ⊤ everywhere?
isTopShape :: Value n -> Bool
isTopShape v = case NbE.force v of
  NbE.VSuspended _ (LambdaF _ (ScopedAST _ TopeTop)) -> True
  _ -> False

-- | A Σ type value, opened.
data Sigma n where
  Sigma :: Distinct i => Env i n -> TModality -> Value n -> ScopedCore i -> Sigma n

sigmaOf :: Value n -> Maybe (Sigma n)
sigmaOf v = case NbE.force (stripRestrictions v) of
  NbE.VSuspended env (TypeSigmaF md a b) -> Just (Sigma env md a b)
  _ -> Nothing

-- | A type value with its restrictions peeled off.
stripRestrictions :: Value n -> Value n
stripRestrictions v = case NbE.force v of
  NbE.VNode (TypeRestrictedF ty _) -> stripRestrictions ty
  _                                -> v

-- | The restrictions of a type value, outermost first.
restrictionsOf :: Value n -> [(Value n, Value n)]
restrictionsOf v = case NbE.force v of
  NbE.VNode (TypeRestrictedF ty rs) -> rs <> restrictionsOf ty
  _                                 -> []

-- * Patterns

-- | A pattern as a value of its own scope: its names, paired.
patternValue :: Pattern n l -> Value l
patternValue = \case
  PatternVar _ b  -> NbE.vvar (Foil.nameOf b)
  PatternUnit b   -> NbE.vvar (Foil.nameOf b)
  PatternPair l r -> NbE.vnode (PairF (unsafeCoerce (patternValue l)) (patternValue r))

-- | An environment extended with a pattern bound to a value: each leaf
-- gets the projection of the value at its position, computed when the
-- name is looked up.
bindPattern :: Env i n -> Pattern i l -> Value n -> Env l n
bindPattern env pat v = case pat of
  PatternVar _ b  -> Foil.addSubst env b v
  PatternUnit b   -> Foil.addSubst env b v
  PatternPair l r -> bindPattern (bindPattern env l (firstValue v)) r (secondValue v)

-- | The pattern binding one name.
singlePattern :: NameBinder n l -> Pattern n l
singlePattern = PatternVar Nothing

-- | The entries of a pattern's names, in binding order, from the type of
-- the value it is bound to: a pair pattern destructures a Σ type or a cube
-- product. A leaf whose type cannot be read gets a hole for a type.
--
-- The context is the one under the pattern, without the pattern's own
-- entries yet: the codomain of a Σ is evaluated at the left component's
-- names, which the environment binds, so the entries are never consulted
-- while they are built.
patternEntries
  :: forall d n l. Distinct l
  => Scope l -> Context d l -> TModality -> Maybe LocationInfo -> Value l -> Pattern n l -> [Entry l]
patternEntries scope ctx md loc = go
  where
    go :: Value l -> Pattern x y -> [Entry l]
    go ty = \case
      PatternVar x _ -> [entryOf x ty]
      PatternUnit _  -> [entryOf Nothing ty]
      PatternPair l r -> case NbE.force (stripRestrictions ty) of
        NbE.VSuspended env (TypeSigmaF _ a b) ->
          go a l <> go (evalScope scope ctx env b (patternValueIn l)) r
        NbE.VNode (CubeProductF a b) -> go a l <> go b r
        _ -> go (vhole Nothing) l <> go (vhole Nothing) r
    -- a sub-pattern's names are names of the whole pattern's scope
    patternValueIn :: Pattern x y -> Value l
    patternValueIn = unsafeCoerce . patternValue
    entryOf mname t = localEntry md mname loc (flagsOfType scope ctx t) t

-- | The entries of a let's pattern: each name has its component of the
-- value.
letEntries
  :: forall d n l. Distinct l
  => Scope l -> Context d l -> Maybe LocationInfo -> Value l -> Value l -> Pattern n l -> [Entry l]
letEntries scope ctx loc ty val pat =
  zipWith (\entry v -> entry { entryValue = Just v })
    (patternEntries scope ctx Id loc ty pat)
    (leafValues pat val)
  where
    leafValues :: Pattern x y -> Value l -> [Value l]
    leafValues p v = case p of
      PatternVar _ _  -> [v]
      PatternUnit _   -> [v]
      PatternPair l r -> leafValues l (firstValue v) <> leafValues r (secondValue v)

-- * Binders

-- | The context under a pattern bound at a type, with a shape assumed for
-- it.
underPattern
  :: (Distinct n, DExt n l)
  => Scope l -> Context d n -> TModality -> Value n -> Maybe (Value n) -> Pattern n l -> Context d l
underPattern scope' ctx md ty mshape pat =
  let ctx0 = sinkContextUnchecked ctx
      entries = patternEntries scope' ctx0 md (ctxLocation ctx) (Foil.sink ty) pat
      ctx' = enterPattern pat entries ctx
  in case mshape of
       Nothing    -> ctx'
       Just shape -> assumeValue scope' ctx' (appValue scope' ctx' (Foil.sink shape) (patternValue pat))

-- | The context under one fresh name of a type: the scope, the context
-- with the name's entry, and the name as a value.
underFreshVar
  :: Distinct n
  => Scope n -> Context d n -> TModality -> Value n -> Maybe (Value n)
  -> (forall l. DExt n l => Scope l -> Context d l -> Value l -> r) -> r
underFreshVar scope ctx md ty mshape k = Foil.withFresh scope $ \b ->
  let pat = singlePattern b
      scope' = Foil.extendScopePattern pat scope
  in k scope' (underPattern scope' ctx md ty mshape pat) (NbE.vvar (Foil.nameOf b))

-- | Does a value stand for a cube: by its head, or by the sort of the
-- entry at its head?
isCube :: Distinct n => Scope n -> Context d n -> Value n -> Bool
isCube scope ctx v = cubeOfType v /= KOther || case neutralType scope ctx v of
  Just s | NbE.VNode UniverseCubeF <- NbE.force s -> True
  _ -> False

-- | The shape of a binder over a cube with no restriction: the family
-- that is ⊤ everywhere.
topShapeValue :: Distinct n => Scope n -> Value n -> Value n
topShapeValue scope cube = Foil.withFresh scope $ \b ->
  NbE.vsuspended Foil.identitySubst
    (LambdaF (Just (Param Id cube Nothing)) (ScopedAST (singlePattern b) TopeTop))

-- | The binder of a λ, from its parameter: its domain and modality, and its
-- shape, assumed for the body. A λ without a parameter binds names of an
-- unknown type.
lambdaBinder :: Maybe (Param (Value n)) -> Extend (Context d) Pattern Sig n
{-# INLINE lambdaBinder #-}
lambdaBinder mparam = Extend $ \_ pat scope' ctx -> case mparam of
  Just (Param md dom mshape) -> underPattern scope' ctx md dom mshape pat
  Nothing -> underPattern scope' ctx Id (vhole Nothing) Nothing pat

-- | The binder of a Π: its domain, and its shape assumed for the codomain.
piBinder :: TModality -> Value n -> Maybe (Value n) -> Extend (Context d) Pattern Sig n
{-# INLINE piBinder #-}
piBinder md dom mshape = Extend $ \_ pat scope' ctx -> underPattern scope' ctx md dom mshape pat

sigmaBinder :: TModality -> Value n -> Extend (Context d) Pattern Sig n
{-# INLINE sigmaBinder #-}
sigmaBinder md a = Extend $ \_ pat scope' ctx -> underPattern scope' ctx md a Nothing pat

-- | The binder of a let: its names have the components of the value.
letBinder :: Maybe (Value n) -> Value n -> Extend (Context d) Pattern Sig n
{-# INLINE letBinder #-}
letBinder mty val = Extend $ \_ pat scope' ctx ->
  let ctx0 = sinkContextUnchecked ctx
      ty = maybe (vhole Nothing) id mty
      entries = letEntries scope' ctx0 (ctxLocation ctx) (Foil.sink ty) (Foil.sink val) pat
  in enterPattern pat entries ctx

-- | A binder whose names have no known type (a construct not yet checked
-- on values).
opaqueBinder :: Extend (Context d) Pattern Sig n
{-# INLINE opaqueBinder #-}
opaqueBinder = Extend $ \_ pat scope' ctx -> underPattern scope' ctx Id (vhole Nothing) Nothing pat

-- | The binder two lambdas are compared under: the right one's domain, and
-- both shapes, since the bodies agree only where both are defined.
lambdasBinder :: Maybe (Param (Value n)) -> Maybe (Param (Value n)) -> Extend (Context d) Pattern Sig n
{-# INLINE lambdasBinder #-}
lambdasBinder mpL mpR = Extend $ \_ pat scope' ctx ->
  let (md, dom) = case (mpR, mpL) of
        (Just p, _) -> (paramModality p, paramDomain p)
        (_, Just p) -> (paramModality p, paramDomain p)
        _           -> (Id, vhole Nothing)
      ctx' = underPattern scope' ctx md dom Nothing pat
      assumeShape c mp = case mp >>= paramShape of
        Nothing    -> c
        Just shape -> assumeValue scope' c (appValue scope' c (Foil.sink shape) (patternValue pat))
  in assumeShape (assumeShape ctx' mpL) mpR

-- | The observer of a λ's body: the other value applied to the pattern.
appliedToPattern :: Distinct l => Scope l -> Context d l -> Pattern n l -> Value l -> Value l
{-# INLINE appliedToPattern #-}
appliedToPattern scope ctx pat other = appValue scope ctx other (patternValue pat)

-- * The tope layer

-- | Assume a tope value.
assumeValue :: Distinct n => Scope n -> Context d n -> Value n -> Context d n
assumeValue scope ctx tope = ctx { ctxTopes = assume (topeOf scope ctx tope) (ctxTopes ctx) }

-- | Does the context entail a tope value?
entailsValue :: Distinct n => Scope n -> Context d n -> Value n -> Bool
entailsValue scope ctx tope = entails (ctxTopes ctx) (topeOf scope ctx tope)

-- | The conjunction of two tope values, with ⊥ absorbing.
topeAndValue :: Value n -> Value n -> Value n
topeAndValue l r = case (NbE.force l, NbE.force r) of
  (NbE.VNode TopeBottomF, _) -> vtopeBottom
  (_, NbE.VNode TopeBottomF) -> vtopeBottom
  (NbE.VNode TopeTopF, _) -> r
  (_, NbE.VNode TopeTopF) -> l
  _ -> NbE.vnode (TopeAndF l r)

isTopeNode :: Sig a b -> Bool
isTopeNode = \case
  TopeTopF     -> True
  TopeBottomF  -> True
  TopeEQF{}    -> True
  TopeLEQF{}   -> True
  TopeAndF{}   -> True
  TopeOrF{}    -> True
  TopeInvF{}   -> True
  TopeUninvF{} -> True
  _            -> False

-- | A tope value read into the solver's syntax.
topeOf :: Distinct n => Scope n -> Context d n -> Value n -> Tope
topeOf scope ctx v = case NbE.force v of
  NbE.VNode TopeTopF -> TTop
  NbE.VNode TopeBottomF -> TBot
  NbE.VNode (TopeEQF x y) -> TEq (pointOf scope ctx x) (pointOf scope ctx y)
  NbE.VNode (TopeLEQF x y) -> TLeq (pointOf scope ctx x) (pointOf scope ctx y)
  NbE.VNode (TopeAndF l r) -> TAnd (topeOf scope ctx l) (topeOf scope ctx r)
  NbE.VNode (TopeOrF l r) -> TOr (topeOf scope ctx l) (topeOf scope ctx r)
  NbE.VNode (TopeInvF t) -> TInv (topeOf scope ctx t)
  NbE.VNode (TopeUninvF t) -> TUninv (topeOf scope ctx t)
  NbE.VNode (TypeModalF md t) -> TModal md (topeOf scope ctx t)
  NbE.VNode (ModAppF md t) -> TModApp md (topeOf scope ctx t)
  NbE.VNode (HoleF m) -> THole m
  _ -> TNeutral (pointOf scope ctx v)

-- | A point value read into the solver's syntax. A variable carries its
-- cube; a defined point is unfolded.
pointOf :: Distinct n => Scope n -> Context d n -> Value n -> Point
pointOf scope ctx v = case NbE.force v of
  NbE.VVar x -> PVar (Foil.nameId x) (cubeOfType (entryType (lookupEntry x ctx)))
  NbE.VNode CubeUnitStarF -> PStar
  NbE.VNode Cube2_0F -> P2_0
  NbE.VNode Cube2_1F -> P2_1
  NbE.VNode CubeI_0F -> PI_0
  NbE.VNode CubeI_1F -> PI_1
  NbE.VNode (PairF a b) -> PPair (pointOf scope ctx a) (pointOf scope ctx b)
  NbE.VNode (FirstF p) -> PFst (pointOf scope ctx p)
  NbE.VNode (SecondF p) -> PSnd (pointOf scope ctx p)
  NbE.VNode (CubeSupF l r) -> PSup (pointOf scope ctx l) (pointOf scope ctx r)
  NbE.VNode (CubeInfF l r) -> PInf (pointOf scope ctx l) (pointOf scope ctx r)
  NbE.VNode (CubeFlipF p) -> PFlip (pointOf scope ctx p)
  NbE.VNode (CubeUnflipF p) -> PUnflip (pointOf scope ctx p)
  NbE.VNode (ModAppF md p) -> PModApp md (pointOf scope ctx p)
  NbE.VNode (ModExtractF app inn p) -> PModExtract app inn (pointOf scope ctx p)
  NbE.VNode (HoleF m) -> PHole m
  NbE.VNode (AppF f a) ->
    PApp (maybe KOther cubeOfType (neutralType scope ctx v)) (pointOf scope ctx f) (pointOf scope ctx a)
  _ -> POpaque (fingerprint (quoteIn scope ctx v))

-- | A cube value, as far as the solver tells cubes apart.
cubeOfType :: Value n -> Cube
cubeOfType v = case NbE.force v of
  NbE.VNode CubeUnitF -> KUnit
  NbE.VNode Cube2F -> K2
  NbE.VNode CubeIF -> KI
  NbE.VNode (CubeProductF a b) -> KProd (cubeOfType a) (cubeOfType b)
  _ -> KOther

-- | A term as a string that equal terms share: the constructors and the
-- names.
fingerprint :: Core n -> String
fingerprint (Var x) = "v" <> show (Foil.nameId x)
fingerprint (Node sig) =
  show (bimap (const ()) (const ()) sig)
    <> "(" <> intercalate "," (bifoldMap scoped (pure . fingerprint) sig) <> ")"
  where
    scoped (ScopedAST pat body) =
      [ "\\" <> intercalate "," [ show (Foil.nameId x) | (_, x) <- patternNames pat ] <> "." <> fingerprint body ]

-- * Types of neutrals

-- | The type of a neutral, read off its head and its eliminations: an
-- application has the codomain of its function's type at the argument, a
-- projection the component of the pair's Σ type, a path induction the
-- motive at its endpoints. A glued head keeps its declared type.
neutralType :: Distinct n => Scope n -> Context d n -> Value n -> Maybe (Value n)
neutralType scope ctx v = case NbE.forceFolded v of
  NbE.VVar x -> Just (entryType (lookupEntry x ctx))
  NbE.VNode (AppF f a) -> do
    fTy <- neutralType scope ctx f
    case familyOf fTy of
      Just _ -> Just topeUniverse
      Nothing -> do
        Pi env _ _ _ ret <- piOf fTy
        Just (codomainAt scope ctx env ret a)
  NbE.VNode (FirstF p) -> do
    ty <- neutralType scope ctx p
    case NbE.force (stripRestrictions ty) of
      NbE.VSuspended _ (TypeSigmaF _ a _) -> Just a
      NbE.VNode (CubeProductF a _) -> Just a
      _ -> Nothing
  NbE.VNode (SecondF p) -> do
    ty <- neutralType scope ctx p
    case NbE.force (stripRestrictions ty) of
      NbE.VSuspended env (TypeSigmaF _ _ b) -> Just (codomainAt scope ctx env b (firstValue p))
      NbE.VNode (CubeProductF _ b) -> Just b
      _ -> Nothing
  NbE.VNode (IdJF _ _ tC _ x p) -> Just (appValue scope ctx (appValue scope ctx tC x) p)
  _ -> Nothing

-- | What evaluation needs to know about a type: whether a neutral of it may
-- reduce by a restriction (of the type, or of a codomain or a component of
-- it), or is unit, and whether it is a tope family over a shape (anywhere
-- along its codomains and components). A defined type is unfolded.
flagsOfType :: forall d n. Distinct n => Scope n -> Context d n -> Value n -> EntryFlags
flagsOfType scope0 ctx0 = go scope0 ctx0 noFlags
  where
    go :: forall l. Distinct l => Scope l -> Context d l -> EntryFlags -> Value l -> EntryFlags
    go scope ctx flags ty = case NbE.force ty of
      NbE.VNode (TypeRestrictedF inner _) -> go scope ctx flags { flagRefines = True } inner
      NbE.VNode TypeUnitF -> flags { flagRefines = True }
      NbE.VNode (TypeShapeF _ _ (Just shape))
        | not (isTopShape shape) -> flags { flagTopeFamily = True }
      NbE.VSuspended env (TypeFunF md dom mshape ret) ->
        underFreshVar scope ctx md dom mshape $ \scope' ctx' x ->
          go scope' ctx' flags (evalScope scope' ctx' (Foil.sink env) ret x)
      -- a projection of a pair reaches either component
      NbE.VSuspended env (TypeSigmaF md a b) ->
        let flags' = go scope ctx flags a
        in underFreshVar scope ctx md a Nothing $ \scope' ctx' x ->
             go scope' ctx' flags' (evalScope scope' ctx' (Foil.sink env) b x)
      _ -> flags

-- * Holes

-- | Does a value contain a hole, as far as its evaluated parts go: a hole
-- under a binder counts when its scope is read syntactically.
valueHasHole :: Value n -> Bool
valueHasHole v = case NbE.force v of
  NbE.VVar _ -> False
  NbE.VNode (HoleF _) -> True
  NbE.VNode node -> getAny (bifoldMap (const mempty) (Any . valueHasHole) node)
  NbE.VSuspended _ node ->
    getAny (bifoldMap (\(ScopedAST _ body) -> Any (containsHole body)) (Any . valueHasHole) node)
