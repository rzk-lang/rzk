{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Normalisation by evaluation, used as an all-or-nothing fast path for
-- conversion checking.
--
-- 'nbeConvertible' evaluates both sides into a value domain with closures
-- (sharing by construction: a definition's value is evaluated once per
-- occurrence, not once per copy, and Haskell's laziness makes the evaluation
-- call-by-need) and compares the values structurally, with one-step η for
-- lambdas and pairs mirroring 'Rzk.TypeCheck.Eval.etaMatch'. Evaluation is
-- \emph{glued}: a spine over a definition keeps the spine as well as its
-- unfolding, and the unfolding is lazy. Two spines over the same definition
-- are then compared argument by argument, without unfolding it. Only where
-- the spines disagree is an unfolding forced.
--
-- It answers only 'True' ("definitely convertible") or 'False' ("do not
-- know"), never a definite inequality, so a caller falls back to the
-- ordinary unification on 'False'. The fast path can therefore accept more
-- than the unification below it, but never less. Note that it does accept
-- more. The ordinary path decomposes an application pairwise, which invents
-- subgoals that a βδ-equal but structurally different pair need not meet
-- (see 'Rzk.TypeCheck.Unify.unifyViaDecompose').
--
-- Soundness is a subset argument: 'True' is answered only for terms that are
-- βδ-convertible up to α and the one-step η above, with every construct whose
-- /reduction/ consults the context — @recOR@ guard selection, holes, the
-- modal constructs — evaluating to an opaque 'VAbort' that poisons the
-- comparison into 'False'. Extension types and @recBOT@ are compared
-- structurally (see the note at their 'eval' case): a structurally identical
-- pair of restricted types is also accepted by the ordinary unification,
-- through reflexive coverage and the cross-face coherences already proved at
-- formation. Every 'True' is therefore also a success of the old unification.
--
-- The evaluator is a pure function of the 'Context': 'valueOfVar' is a plain
-- reader-only lookup, and fresh variables for comparing closures are de
-- Bruijn levels ('NLevel'), so the foil scope machinery is never extended and
-- no quote function is needed.
--
-- == Attribution
--
-- None of the underlying techniques are ours. Semantic conversion checking —
-- evaluate both sides into a value domain with closures and compare the
-- values, applying functions to fresh generic values — is the algorithm of
-- Coquand, /An algorithm for type-checking dependent types/ (Science of
-- Computer Programming 26, 1996), the implementation-level core of
-- normalisation by evaluation (Berger and Schwichtenberg, LICS 1991).
--
-- Comparing two stuck terms by their spines, head first and arguments
-- pairwise, rather than by normalising them, is the algorithmic equality of
-- Abel and Coquand, /Untyped Algorithmic Equality for Martin-Löf's Logical
-- Framework with Surjective Pairs/ (Fundamenta Informaticae 77(4):345–395,
-- 2007; TLCA 2005), see <https://www2.tcs.ifi.lmu.de/~abel/lfsigma.pdf>.
--
-- The representation we use for it is the \emph{glued evaluation} of András
-- Kovács' <https://github.com/AndrasKovacs/smalltt smalltt>. Its README
-- states the tension as "in basic conversion checking, we want to evaluate
-- as efficiently as possible; in quoting, we want to output terms which are
-- as small as possible". It resolves the tension by evaluating a top-level
-- variable to "values which represent lazy ('non-deterministic') choice
-- between unfolding the definition, and not unfolding it". Following
-- smalltt, 'conv' also speculates: "whenever we have the same top-level head
-- symbol on both sides, we try to unify the spines", and unfolds only when
-- that fails. smalltt separates its modes and uses gluing for quoting small
-- terms as well. This module has no quote function, so it needs neither.
-- The general implementation shape, environment machines with closures and
-- de Bruijn levels for fresh variables, follows the same author's
-- <https://github.com/AndrasKovacs/elaboration-zoo elaboration-zoo>.
--
-- For the fragment this module deliberately aborts on (tope-indexed
-- reduction such as @recOR@), the template is cubical: Sterling and Angiuli,
-- /Normalization for Cubical Type Theory/ (LICS 2021), and its
-- implementation lineage in @cooltt@.
--
-- What is specific to rzk is only the packaging: the all-or-nothing gating
-- ('True' or do-not-know, never refute), and 'VAbort' poisoning of the
-- context-sensitive fragment so that the fast path stays sound by a subset
-- argument.
module Rzk.TypeCheck.NbE (Conversion (..), nbeConvertible) where

import           Control.Monad.Reader              (asks)
import           Data.Bifoldable                   (bifoldMap)
import           Data.Bifunctor                    (bimap)
import           Data.Maybe                        (isJust)
import           Data.Monoid                       (All (..))
import           Data.ZipMatchK                    (zipMatch2)

import           Control.Monad.Foil                (NameBinder)
import qualified Control.Monad.Foil                as Foil
import           Control.Monad.Free.Foil           (AST (Node, Var),
                                                    ScopedAST (..))
import           Control.Monad.Free.Foil.Annotated (AnnSig (..))

import           Language.Rzk.Foil.Syntax
import           Rzk.TypeCheck.Context
import           Rzk.TypeCheck.Monad

-- * The value domain

data Val n
  = VLam (Closure n)
    -- ^ A lambda: its body under the environment it was evaluated in.
  | VNeutral (Neu n) (Maybe (Val n))
    -- ^ A \emph{glued} elimination spine over a variable. The first field is
    -- the spine. The second is the value that spine unfolds to, when its head
    -- has a definition, and 'Nothing' for a rigid head (a local, an
    -- assumption, a comparison level). The unfolding is a lazy thunk, so we
    -- compute it at most once, and only on demand.
    --
    -- We keep both so that 'conv' can answer @f a =? f a@ from the spines,
    -- without unfolding @f@. Keeping only the unfolding, as this evaluator
    -- did before, normalises a repeated lemma once per occurrence, and again
    -- per nesting level.
  | VCon (TermSig (Closure n) (Val n))
    -- ^ Any other node, its term fields evaluated and its scoped fields
    -- closed over the environment. Covers constructors (pairs, @refl@),
    -- types (Π, Σ, identity, universes) and the cube/tope operators, which
    -- are compared structurally only (reflexivity is entailment).
  | VAbort AbortReason
    -- ^ A construct outside the context-insensitive fragment. Poisons the
    -- comparison: 'conv' answers 'False' the moment it meets one. The
    -- reason records which construct, for diagnostics.

-- | Why evaluation gave up: which context-sensitive construct was met.
data AbortReason
  = AbortHole
  | AbortRecOr
  | AbortModal
  | AbortStuckElim
    -- ^ An elimination of a non-canonical, non-neutral value (an abort
    -- propagating through an application, projection or @idJ@).
  deriving (Eq, Ord, Show, Enum, Bounded)

data Neu n
  = NVar (Foil.Name n)
    -- ^ An ambient variable. Its definition, if it has one, sits beside the
    -- spine in the 'VNeutral' that carries it.
  | NLevel Int
    -- ^ A fresh variable minted while comparing closures.
  | NApp (Neu n) (Val n)
  | NFirst (Neu n)
  | NSecond (Neu n)
  | NIdJ (Val n) (Val n) (Val n) (Val n) (Val n) (Neu n)

-- | A scoped term closed over its environment. The term lives in the scope
-- @i@ the environment is defined on, and the values it evaluates to live in
-- the ambient scope @n@.
--
-- One binder, not @NameBinders i l@: every scope field of every 'TermSig'
-- constructor is a unary 'ScopedAST' (even a pair-pattern lambda binds one
-- variable operationally), so a multi-binder closure would have nothing to
-- be built from without peeling syntactic lambda chains in 'eval'. Peeling
-- (the eval/apply arity optimisation of Marlow and Peyton Jones' fast
-- curry) does not pay here the way spine-batching paid at the term level:
-- an intermediate 'VLam' costs one closure and one persistent environment
-- insert, not a 'substituteT' traversal, and η-comparison and partial
-- application want one-argument-at-a-time semantics anyway.
data Closure n where
  Closure :: Env i n -> NameBinder i l -> TermT l -> Closure n

-- | An environment for evaluating a term of scope @i@ into values of the
-- ambient scope @n@. It maps the binders passed on the way down to the values
-- they were bound to; the entries are lazy on purpose, since forcing one would
-- evaluate it and evaluation is call-by-need.
--
-- This is 'Foil.Substitution', which is exactly that: a map from the names of
-- @i@ to something in @n@, storing only the names it moves. A name it does not
-- carry stands for itself, and 'Foil.lookupSubst' produces that itself through
-- 'Foil.injectName' (see 'Foil.addRename', which deletes an identity rename
-- rather than storing it). Every 'Foil.addSubst' advances @i@ by the binder it
-- inserts, so the scope index tracks which binders the environment accounts
-- for, and the identity environment for the ambient scope is the empty one.
--
-- This is the same invariant 'peelLambdas' relies on for its substitution.
type Env i n = Foil.Substitution Val i n

-- | A name stands for the rigid spine on itself. 'Foil.lookupSubst' uses this
-- for a name the environment does not carry, which is also how the name
-- arrives at the ambient scope @n@.
instance Foil.InjectName Val where
  injectName x = VNeutral (NVar x) Nothing

-- * Evaluation

eval :: forall i n. Context n -> Env i n -> TermT i -> Val n
eval ctx env = \case
  -- An ambient name stays a spine, glued to the value of its definition when
  -- it has one. We evaluate that value only on demand, and in the ambient
  -- scope, whose identity environment is the empty one.
  --
  -- The environment carries no ambient name, so a lookup that lands here is
  -- either a miss, or a hit on a value that is itself a rigid spine on an
  -- ambient name. Every value the environment holds is an output of 'eval' or
  -- a fresh 'NLevel', so in the second case the head was already glued and
  -- carries no definition, and gluing it again changes nothing.
  Var x -> case Foil.lookupSubst env x of
    VNeutral (NVar x') Nothing ->
      VNeutral (NVar x')
        (eval ctx Foil.identitySubst <$> varValue (lookupVarInfo x' ctx))
    v -> v

  AppT _ty f x -> applyVal ctx (eval ctx env f) (eval ctx env x)
  LambdaT _ty _orig _mparam (ScopedAST binder body) ->
    VLam (Closure env binder body)
  LetT _ty _orig _mparam val (ScopedAST binder body) ->
    eval ctx (Foil.addSubst env binder (eval ctx env val)) body
  FirstT _ty t  -> projVal ProjFirst  (eval ctx env t)
  SecondT _ty t -> projVal ProjSecond (eval ctx env t)
  TypeAscT _ty term _ty' -> eval ctx env term
  IdJT _ty tA a tC d x p ->
    let vd = eval ctx env d
        stuck np = NIdJ (eval ctx env tA) (eval ctx env a) (eval ctx env tC)
                        vd (eval ctx env x) np
        -- As in 'projVal'. The induction stays stuck on the spine, and is
        -- glued to the induction over the unfolded path.
        elim = \case
          VCon ReflF{}      -> vd
          VNeutral np munf  -> VNeutral (stuck np) (elim <$> munf)
          VAbort r          -> VAbort r
          _                 -> VAbort AbortStuckElim
    in elim (eval ctx env p)

  -- The context-sensitive fragment. A @recOR@ reduces by deciding its guards
  -- against the tope context, and a hole defers by design, so both abort. The
  -- modal constructs reduce under 'enterModality', which eval does not track.
  --
  -- An extension type ('TypeRestrictedT') and @recBOT@ do /not/ abort: they
  -- fall through to the generic constructor case below and are compared
  -- structurally. For two restricted types with structurally identical
  -- underlying types and face lists this is sound: the ordinary unification
  -- proves the same pair by reflexive coverage (each face entails the
  -- disjunction containing itself) and by re-checking the cross-face
  -- coherences that were already proved when the type was formed (entailment
  -- is monotone in the tope context). Two @recBOT@s unify unconditionally.
  HoleT{} -> VAbort AbortHole
  RecOrT{} -> VAbort AbortRecOr
  TypeModalT{} -> VAbort AbortModal
  ModAppT{} -> VAbort AbortModal
  ModExtractT{} -> VAbort AbortModal
  LetModT{} -> VAbort AbortModal

  -- everything else is a plain constructor: evaluate the fields
  Node (AnnSig _info sig) ->
    VCon (bimap (\(ScopedAST binder body) -> Closure env binder body) (eval ctx env) sig)

-- | β, and the same application pushed into the glued unfolding. Applying a
-- spine grows the spine rather than forcing its head. We carry the unfolded
-- side along lazily, so we normalise nothing unless a comparison asks for it,
-- and the shared thunk keeps us from normalising it twice.
applyVal :: Context n -> Val n -> Val n -> Val n
applyVal ctx f v = case f of
  VLam closure -> applyClosure ctx closure v
  -- We match on the unfolding rather than use 'fmap'. A rigid spine then
  -- costs one allocation, and not also the closure that would push the
  -- application into an unfolding it does not have.
  VNeutral neu Nothing -> VNeutral (NApp neu v) Nothing
  VNeutral neu (Just u) -> VNeutral (NApp neu v) (Just (applyVal ctx u v))
  VAbort r -> VAbort r
  _ -> VAbort AbortStuckElim

applyClosure :: Context n -> Closure n -> Val n -> Val n
applyClosure ctx (Closure env binder body) v =
  eval ctx (Foil.addSubst env binder v) body

-- | Which component of a pair a projection takes. One value decides both the
-- spine 'projVal' builds and the component it picks, so the two cannot
-- disagree.
data Proj = ProjFirst | ProjSecond

-- | Project from a pair value, or stay neutral. For a glued spine the
-- projection stays stuck on the spine, and is glued to the projection out of
-- the unfolding. Thus 'conv' can still compare @π₁ (f a)@ rigidly.
projVal :: Proj -> Val n -> Val n
projVal proj = go
  where
    go = \case
      VCon (PairF l r) -> case proj of
        ProjFirst  -> l
        ProjSecond -> r
      VNeutral n munf  -> VNeutral (neu n) (go <$> munf)
      VAbort r         -> VAbort r
      _                -> VAbort AbortStuckElim
    neu = case proj of
      ProjFirst  -> NFirst
      ProjSecond -> NSecond

-- * Conversion

-- | 'False' means "do not know", never a definite inequality.
--
-- We compare two spines standing on the same definition argument by argument,
-- with neither side unfolded. Congruence makes that answer definite. Forcing
-- both sides to a weak head normal form first, which is what this module did
-- before, normalises a repeated lemma once per occurrence, and again per
-- nesting level.
conv :: Context n -> Int -> Val n -> Val n -> Bool
conv ctx lvl l r
  -- Look for a nearby state in which the two sides stand on the same
  -- definition, and answer from their spines if there is one.
  | align l r = True
  -- Without an alignment we force both sides to a weak head normal form and
  -- compare them structurally, as before.
  | otherwise = case (forceVal l, forceVal r) of
      (VAbort _, _) -> False
      (_, VAbort _) -> False

      (VLam c1, VLam c2) ->
        conv ctx (lvl + 1) (applyClosure ctx c1 (freshV lvl)) (applyClosure ctx c2 (freshV lvl))
      -- one-step η for lambdas, as in 'etaMatch'
      (VLam c1, v2) ->
        conv ctx (lvl + 1) (applyClosure ctx c1 (freshV lvl)) (applyVal ctx v2 (freshV lvl))
      (v1, VLam c2) ->
        conv ctx (lvl + 1) (applyVal ctx v1 (freshV lvl)) (applyClosure ctx c2 (freshV lvl))

      -- one-step η for pairs. The spine is rigid here, so the projections off
      -- it are rigid too.
      (VCon (PairF a b), VNeutral n _) ->
        conv ctx lvl a (rigid (NFirst n)) && conv ctx lvl b (rigid (NSecond n))
      (VNeutral n _, VCon (PairF a b)) ->
        conv ctx lvl (rigid (NFirst n)) a && conv ctx lvl (rigid (NSecond n)) b

      (VCon s1, VCon s2) -> case zipMatch2 s1 s2 of
        Nothing -> False
        Just s  -> getAll (bifoldMap
          (All . uncurry (convClosure ctx lvl))
          (All . uncurry (conv ctx lvl))
          s)

      (VNeutral n1 _, VNeutral n2 _) -> convNeu ctx lvl n1 n2
      _ -> False
  where
    rigid n = VNeutral n Nothing
    freshV = rigid . NLevel

    -- Two spines on the same definition are convertible when their arguments
    -- are. This is congruence, and it needs no unfolding.
    aligned (VNeutral n1 munf1) (VNeutral n2 _) =
      -- A rigid head reaches the structural case below anyway. Attempting it
      -- here would only repeat a failure.
      isJust munf1 && sameHead n1 n2 && convNeu ctx lvl n1 n2
    aligned _ _ = False

    unfolded (VNeutral _ munf) = munf
    unfolded _                 = Nothing

    -- The two sides need not stand on the same definition to begin with.
    -- One is often the other under a thin wrapper, such as
    -- @transport … refl t@ for @t@, an alias, or an accessor, and such a
    -- wrapper is a δ-step or two away. We therefore search the unfoldings of
    -- both sides, nearest first, and take the first shared head.
    --
    -- The search is breadth-first over both chains at once. Walking one chain
    -- to its end before starting the other normalises that side completely
    -- whenever the wrapper is on the other side. For example,
    -- @d20 A f x@ against @wrap A (d20 A f x)@ aligns after one step on the
    -- right, but costs 21 million evaluation steps if we walk the left chain
    -- first.
    align a b = or
      [ aligned a' b'
      | k <- [0 .. maxAlignOffset]
      , (i, a') <- zip [0 :: Int ..] (chainOf a)
      , (j, b') <- zip [0 :: Int ..] (chainOf b)
      , max i j == k ]

    -- Lazy, and cut off at the window. An element is forced only when the
    -- search reaches it.
    chainOf v = take (maxAlignOffset + 1) (v : maybe [] chainOf (unfolded v))

    -- Unfold at the head until the value is canonical or truly stuck.
    forceVal v = maybe v forceVal (unfolded v)

-- | How far to look for a shared head before giving up and comparing weak
-- head normal forms instead. A wrapper sits one or two unfoldings from what
-- it wraps, and beyond that a shared head is unlikely. Every step inside the
-- window forces an unfolding that the fallback would force anyway, so the
-- window costs nothing extra. We keep it small.
maxAlignOffset :: Int
maxAlignOffset = 4

-- | Do both spines stand on the same variable? Only their heads are compared;
-- the arguments are left to 'convNeu'.
sameHead :: Neu n -> Neu n -> Bool
sameHead n1 n2 = case (neuHead n1, neuHead n2) of
  (Just x, Just y) -> Foil.nameId x == Foil.nameId y
  _                -> False

-- | The variable a spine is stuck on, unless it is a fresh comparison level.
neuHead :: Neu n -> Maybe (Foil.Name n)
neuHead = \case
  NVar x           -> Just x
  NLevel _         -> Nothing
  NApp n _         -> neuHead n
  NFirst n         -> neuHead n
  NSecond n        -> neuHead n
  NIdJ _ _ _ _ _ n -> neuHead n

convClosure :: Context n -> Int -> Closure n -> Closure n -> Bool
convClosure ctx lvl c1 c2 =
  conv ctx (lvl + 1) (applyClosure ctx c1 fresh) (applyClosure ctx c2 fresh)
  where
    fresh = VNeutral (NLevel lvl) Nothing

convNeu :: Context n -> Int -> Neu n -> Neu n -> Bool
convNeu ctx lvl = go
  where
    go (NVar x) (NVar y) = Foil.nameId x == Foil.nameId y
    go (NLevel i) (NLevel j) = i == j
    go (NApp n v) (NApp n' v') = go n n' && conv ctx lvl v v'
    go (NFirst n) (NFirst n') = go n n'
    go (NSecond n) (NSecond n') = go n n'
    go (NIdJ tA a tC d x n) (NIdJ tA' a' tC' d' x' n') =
      go n n'
        && conv ctx lvl tA tA' && conv ctx lvl a a' && conv ctx lvl tC tC'
        && conv ctx lvl d d' && conv ctx lvl x x'
    go _ _ = False

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

-- | Are the two terms definitely convertible?
nbeConvertible :: TermT n -> TermT n -> TypeCheck n Conversion
nbeConvertible t1 t2 = asks $ \ctx ->
  if conv ctx 0 (eval ctx Foil.identitySubst t1) (eval ctx Foil.identitySubst t2)
    then Convertible
    else DontKnow

