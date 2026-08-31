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
-- It answers only 'Convertible' or 'DontKnow', never a definite inequality,
-- so a caller falls back to the ordinary unification on 'DontKnow'. The two
-- cases are not opposites, which is why the answer is not a 'Bool'. The fast
-- path can therefore accept more
-- than the unification below it, but never less. Note that it does accept
-- more. The ordinary path decomposes an application pairwise, which invents
-- subgoals that a βδ-equal but structurally different pair need not meet
-- (see 'Rzk.TypeCheck.Unify.unifyViaDecompose').
--
-- Soundness is a subset argument: 'Convertible' is answered only for terms that are
-- βδ-convertible up to α and the one-step η above, with every construct whose
-- /reduction/ consults the context — @recOR@ guard selection, holes, the
-- modal constructs — evaluating to an opaque 'VAbort' that poisons the
-- comparison into 'DontKnow'. Extension types and @recBOT@ are compared
-- structurally (see the note at their 'eval' case): a structurally identical
-- pair of restricted types is also accepted by the ordinary unification,
-- through reflexive coverage and the cross-face coherences already proved at
-- formation. Every 'Convertible' is therefore also a success of the old
-- unification.
--
-- The evaluator is a pure function of the 'Context': 'valueOfVar' is a plain
-- reader-only lookup, and fresh variables for comparing closures are de
-- Bruijn levels ('HFresh'), so the foil scope machinery is never extended and
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
-- ('Convertible' or 'DontKnow', never refute), and 'VAbort' poisoning of the
-- context-sensitive fragment so that the fast path stays sound by a subset
-- argument.
module Rzk.TypeCheck.NbE (Conversion (..), nbeConvertible) where

import           Control.Monad.Reader              (asks)
import           Data.Bifoldable                   (bifoldMap)
import           Data.Bifunctor                    (bimap)
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
  | VNeutral (Neu n)
    -- ^ An elimination spine, \emph{glued} to its unfolding when it has one.
  | VCon (TermSig (Closure n) (Val n))
    -- ^ Any other node, its term fields evaluated and its scoped fields
    -- closed over the environment. Covers constructors (pairs, @refl@),
    -- types (Π, Σ, identity, universes) and the cube/tope operators, which
    -- are compared structurally only (reflexivity is entailment).
  | VAbort
    -- ^ A construct outside the context-insensitive fragment, or an
    -- elimination of a value that is neither canonical nor neutral. Poisons
    -- the comparison: 'conv' gives up the moment it meets one.
    --
    -- Carries no reason. Nothing can observe one: the module exports
    -- 'nbeConvertible' alone, whose 'DontKnow' says only that the fast path
    -- declined. The eval cases below say which construct each abort is for.

-- | An elimination spine: what it stands on, and what is eliminated off it.
--
-- The two constructors carry the gluing invariant. Only a spine on a name with
-- a definition unfolds, so only 'NGlued' has an unfolding, and a spine on a
-- comparison level cannot be given one. This was a 'Maybe' beside the spine
-- and a comment saying when it was 'Just'.
data Neu n
  = NRigid (Head n) [Elim n]
    -- ^ A spine that does not unfold: on a local, an assumption, or a fresh
    -- comparison level.
  | NGlued (Foil.Name n) [Elim n] (Val n)
    -- ^ A spine on a name with a definition, beside the value the /whole
    -- spine/ unfolds to. That value is a lazy thunk maintained as the spine
    -- grows, so we compute it at most once and only on demand.
    --
    -- We keep both so that 'conv' can answer @f a =? f a@ from the two
    -- spines, without unfolding @f@. Keeping only the unfolding, as this
    -- evaluator did before, normalises a repeated lemma once per occurrence,
    -- and again per nesting level.

-- | What a spine stands on.
data Head n
  = HVar (Foil.Name n)
    -- ^ An ambient name with no definition.
  | HFresh DeBruijnLevel
    -- ^ A fresh variable minted while comparing two closures.

-- | One elimination off a spine.
--
-- A spine holds its eliminations \emph{outermost first}, so @f a b@ is
-- @'NRigid' ('HVar' f) ['EApp' b, 'EApp' a]@. Growing a spine is then a cons
-- rather than an append, which keeps 'applyVal' constant-time in the length
-- of the spine.
data Elim n
  = EApp (Val n)
  | EFirst
  | ESecond
  | EIdJ (Val n) (Val n) (Val n) (Val n) (Val n)
    -- ^ Path induction over the spine: the type, the base point, the motive,
    -- the base case, and the endpoint. The path is the spine itself.

-- | The supply of fresh variables minted while comparing two closures: a de
-- Bruijn level, counting binders from the outside in.
--
-- Note that such a variable is not a name of the ambient scope @n@, so a
-- @'Val' n@ is really a value in @n@ extended by whichever of these are live.
-- We do not index that: extending the foil scope per comparison is the cost
-- this module exists to avoid, and it would need a quote function to get back
-- out. The newtype buys only that the supply cannot be confused with another
-- 'Int'.
--
-- The discipline it rests on is that every level reachable from a value being
-- compared at @lvl@ was minted below @lvl@. That holds because a closure
-- captures only levels minted before it, and because sibling fields of a
-- 'VCon' are compared independently, so one field's levels never reach
-- another. It is checked by reading, not by the types.
newtype DeBruijnLevel = DeBruijnLevel Int
  deriving (Eq)

-- | The next level to mint, once @lvl@ has been used.
nextLevel :: DeBruijnLevel -> DeBruijnLevel
nextLevel (DeBruijnLevel i) = DeBruijnLevel (i + 1)

-- | Grow a spine by one elimination, pushing the same elimination into the
-- unfolding when there is one.
--
-- We match on the spine rather than use 'fmap' over a 'Maybe'. A rigid spine
-- then costs one allocation, and not also the closure that would push the
-- elimination into an unfolding it does not have.
elimNeu :: Elim n -> (Val n -> Val n) -> Neu n -> Neu n
elimNeu e onUnfolding = \case
  NRigid h es   -> NRigid h (e : es)
  NGlued x es u -> NGlued x (e : es) (onUnfolding u)

-- | What a spine stands on, always.
headOf :: Neu n -> Head n
headOf (NRigid h _)   = h
headOf (NGlued x _ _) = HVar x

-- | What is eliminated off a spine, outermost first.
elimsOf :: Neu n -> [Elim n]
elimsOf (NRigid _ es)   = es
elimsOf (NGlued _ es _) = es

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

-- | A name stands for the empty rigid spine on itself. 'Foil.lookupSubst' uses
-- this for a name the environment does not carry, which is also how the name
-- arrives at the ambient scope @n@.
instance Foil.InjectName Val where
  injectName x = VNeutral (NRigid (HVar x) [])

-- * Evaluation

eval :: forall i n. Context n -> Env i n -> TermT i -> Val n
eval ctx env = \case
  -- An ambient name stays a spine, glued to the value of its definition when
  -- it has one. We evaluate that value only on demand, and in the ambient
  -- scope, whose identity environment is the empty one.
  --
  -- The environment carries no ambient name, so a lookup that lands here is
  -- either a miss, or a hit on a value that is itself a rigid spine on an
  -- ambient name with nothing eliminated off it. Every value the environment
  -- holds is an output of 'eval' or a fresh 'HFresh' variable, so in the
  -- second case the head was already glued and carries no definition, and
  -- gluing it again changes nothing.
  Var x -> case Foil.lookupSubst env x of
    v@(VNeutral (NRigid (HVar x') [])) ->
      case varValue (lookupVarInfo x' ctx) of
        Nothing   -> v
        Just body -> VNeutral (NGlued x' [] (eval ctx Foil.identitySubst body))
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
        e = EIdJ (eval ctx env tA) (eval ctx env a) (eval ctx env tC)
                 vd (eval ctx env x)
        -- As in 'projVal'. The induction stays stuck on the spine, and is
        -- glued to the induction over the unfolded path.
        elim = \case
          VCon ReflF{} -> vd
          VNeutral neu -> VNeutral (elimNeu e elim neu)
          -- an abort propagating through, or a stuck induction
          _            -> VAbort
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
  HoleT{} -> VAbort
  RecOrT{} -> VAbort
  TypeModalT{} -> VAbort
  ModAppT{} -> VAbort
  ModExtractT{} -> VAbort
  LetModT{} -> VAbort

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
  VNeutral neu -> VNeutral (elimNeu (EApp v) (\u -> applyVal ctx u v) neu)
  -- an abort propagating through, or a stuck application
  _ -> VAbort

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
      VNeutral neu -> VNeutral (elimNeu e go neu)
      -- an abort propagating through, or a stuck projection
      _            -> VAbort
    e = case proj of
      ProjFirst  -> EFirst
      ProjSecond -> ESecond

-- * Conversion

-- | 'False' means "do not know", never a definite inequality. The exported
-- 'nbeConvertible' names the two cases; inside, 'Bool' is the conjunction
-- monoid this folds with.
--
-- We compare two spines standing on the same definition argument by argument,
-- with neither side unfolded. Congruence makes that answer definite. Forcing
-- both sides to a weak head normal form first, which is what this module did
-- before, normalises a repeated lemma once per occurrence, and again per
-- nesting level.
conv :: Context n -> DeBruijnLevel -> Val n -> Val n -> Bool
conv ctx lvl l r
  -- Look for a nearby state in which the two sides stand on the same
  -- definition, and answer from their spines if there is one.
  | align l r = True
  -- Without an alignment we force both sides to a weak head normal form and
  -- compare them structurally, as before.
  | otherwise = case (forceVal l, forceVal r) of
      (VAbort, _) -> False
      (_, VAbort) -> False

      (VLam c1, VLam c2) ->
        conv ctx (nextLevel lvl) (applyClosure ctx c1 (freshV lvl)) (applyClosure ctx c2 (freshV lvl))
      -- one-step η for lambdas, as in 'etaMatch'
      (VLam c1, v2) ->
        conv ctx (nextLevel lvl) (applyClosure ctx c1 (freshV lvl)) (applyVal ctx v2 (freshV lvl))
      (v1, VLam c2) ->
        conv ctx (nextLevel lvl) (applyVal ctx v1 (freshV lvl)) (applyClosure ctx c2 (freshV lvl))

      -- one-step η for pairs. Both sides are forced here, so the spine is
      -- rigid and 'projVal' grows it without unfolding anything.
      (VCon (PairF a b), n@(VNeutral _)) ->
        conv ctx lvl a (projVal ProjFirst n) && conv ctx lvl b (projVal ProjSecond n)
      (n@(VNeutral _), VCon (PairF a b)) ->
        conv ctx lvl (projVal ProjFirst n) a && conv ctx lvl (projVal ProjSecond n) b

      (VCon s1, VCon s2) -> case zipMatch2 s1 s2 of
        Nothing -> False
        Just s  -> getAll (bifoldMap
          (All . uncurry (convClosure ctx lvl))
          (All . uncurry (conv ctx lvl))
          s)

      (VNeutral n1, VNeutral n2) -> convNeu ctx lvl n1 n2
      _ -> False
  where
    freshV k = VNeutral (NRigid (HFresh k) [])

    -- Two spines on the same definition are convertible when their
    -- eliminations are. This is congruence, and it needs no unfolding.
    --
    -- Only a glued spine is worth trying. A rigid one reaches the structural
    -- case below anyway, and attempting it here would only repeat a failure.
    -- We need no separate head test: 'convNeu' compares the two heads first.
    aligned (VNeutral n1@NGlued{}) (VNeutral n2) = convNeu ctx lvl n1 n2
    aligned _ _ = False

    unfolded (VNeutral (NGlued _ _ u)) = Just u
    unfolded _                         = Nothing

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

convClosure :: Context n -> DeBruijnLevel -> Closure n -> Closure n -> Bool
convClosure ctx lvl c1 c2 =
  conv ctx (nextLevel lvl) (applyClosure ctx c1 fresh) (applyClosure ctx c2 fresh)
  where
    fresh = VNeutral (NRigid (HFresh lvl) [])

-- | Two spines are convertible when they stand on the same head and their
-- eliminations agree pairwise. Neither side is unfolded.
convNeu :: Context n -> DeBruijnLevel -> Neu n -> Neu n -> Bool
convNeu ctx lvl n1 n2 =
  convHead (headOf n1) (headOf n2) && convElims (elimsOf n1) (elimsOf n2)
  where
    convHead (HVar x) (HVar y)     = Foil.nameId x == Foil.nameId y
    convHead (HFresh i) (HFresh j) = i == j
    convHead _ _                   = False

    -- The recursive call comes first so that a difference in spine length,
    -- and the innermost eliminations, are settled before we compare the
    -- arguments of the outermost one.
    convElims [] []           = True
    convElims (e:es) (e':es') = convElims es es' && convElim e e'
    convElims _ _             = False

    convElim (EApp v) (EApp v') = conv ctx lvl v v'
    convElim EFirst EFirst      = True
    convElim ESecond ESecond    = True
    convElim (EIdJ tA a tC d x) (EIdJ tA' a' tC' d' x') =
      conv ctx lvl tA tA' && conv ctx lvl a a' && conv ctx lvl tC tC'
        && conv ctx lvl d d' && conv ctx lvl x x'
    convElim _ _                = False

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
  if conv ctx (DeBruijnLevel 0) (eval ctx Foil.identitySubst t1) (eval ctx Foil.identitySubst t2)
    then Convertible
    else DontKnow

