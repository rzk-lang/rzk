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
-- lambdas and pairs mirroring 'Rzk.TypeCheck.Eval.etaMatch'. It answers only
-- 'True' ("definitely convertible") or 'False' ("do not know") — never a
-- definite inequality — so a caller falls back to the ordinary unification on
-- 'False' and the fast path cannot change what typechecks, only how fast.
--
-- Soundness is a subset argument: 'True' is answered only for terms that are
-- βδ-convertible up to α and the one-step η above, entirely inside the
-- context-insensitive fragment of the theory. Everything context-sensitive —
-- topes and extension types ('tryRestriction', @recOR@, @recBOT@), holes,
-- modalities — evaluates to an opaque 'VAbort' that poisons the comparison
-- into 'False'. Every 'True' is therefore also a success of the old
-- unification (its α-fast-path and structural cases, with reflexive tope
-- entailment); see the NbE spike notes for the argument in full.
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
-- normalisation by evaluation (Berger and Schwichtenberg, LICS 1991). The
-- concrete implementation shape follows the style popularised by András
-- Kovács' <https://github.com/AndrasKovacs/smalltt smalltt> and
-- <https://github.com/AndrasKovacs/elaboration-zoo elaboration-zoo>:
-- environment machines with closures, de Bruijn levels for fresh variables,
-- and demand-driven definition unfolding (our 'unfoldNeu' — unfold at an
-- elimination or on a comparison mismatch, comparing neutral spines first —
-- is a simplified form of smalltt's glued evaluation). For the fragment this
-- module deliberately aborts on (tope-indexed reduction, extension types),
-- the template is cubical: Sterling and Angiuli, /Normalization for Cubical
-- Type Theory/ (LICS 2021), and its implementation lineage in @cooltt@.
-- What is specific to rzk is only the packaging: the all-or-nothing
-- gating ('True' or do-not-know, never refute), and 'VAbort' poisoning of
-- the context-sensitive fragment so that the fast path stays sound by a
-- subset argument.
module Rzk.TypeCheck.NbE (nbeConvertible) where

import           Control.Monad.Reader              (asks)
import           Data.Bifoldable                   (bifoldMap)
import           Data.Bifunctor                    (bimap)
import qualified Data.IntMap                       as IntMap
import           Data.Monoid                       (All (..))
import           Data.ZipMatchK                    (zipMatch2)
import           Unsafe.Coerce                     (unsafeCoerce)

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
    -- ^ A stuck elimination spine over a variable.
  | VCon (TermSig (Closure n) (Val n))
    -- ^ Any other node, its term fields evaluated and its scoped fields
    -- closed over the environment. Covers constructors (pairs, @refl@),
    -- types (Π, Σ, identity, universes) and the cube/tope operators, which
    -- are compared structurally only (reflexivity is entailment).
  | VAbort
    -- ^ A construct outside the context-insensitive fragment. Poisons the
    -- comparison: 'conv' answers 'False' the moment it meets one.

data Neu n
  = NVar (Foil.Name n)
    -- ^ An ambient variable (its definition, if any, is unfolded on demand
    -- by 'unfoldNeu').
  | NLevel Int
    -- ^ A fresh variable minted while comparing closures.
  | NApp (Neu n) (Val n)
  | NFirst (Neu n)
  | NSecond (Neu n)
  | NIdJ (Val n) (Val n) (Val n) (Val n) (Val n) (Neu n)

-- | A scoped term closed over its environment. The environment maps the raw
-- ids of the binders passed on the way down to values; a missing id belongs
-- to the ambient scope @n@ (the same invariant 'peelLambdas' relies on for
-- its substitution).
data Closure n where
  Closure :: Env n -> NameBinder i l -> TermT l -> Closure n

-- | Lazy on purpose: forcing an entry would evaluate it, and evaluation is
-- call-by-need.
type Env n = IntMap.IntMap (Val n)

-- * Evaluation

eval :: forall i n. Context n -> Env n -> TermT i -> Val n
eval ctx env = \case
  Var x -> case IntMap.lookup (Foil.nameId x) env of
    Just v  -> v
    -- An env miss is an ambient name; kept neutral here, unfolded on demand.
    Nothing -> VNeutral (NVar (unsafeCoerce x))

  AppT _ty f x -> applyVal ctx (eval ctx env f) (eval ctx env x)
  LambdaT _ty _orig _mparam (ScopedAST binder body) ->
    VLam (Closure env binder body)
  LetT _ty _orig _mparam val (ScopedAST binder body) ->
    eval ctx (IntMap.insert (Foil.nameId (Foil.nameOf binder)) (eval ctx env val) env) body
  FirstT _ty t  -> projVal ctx NFirst  fstOf (eval ctx env t)
  SecondT _ty t -> projVal ctx NSecond sndOf (eval ctx env t)
  TypeAscT _ty term _ty' -> eval ctx env term
  IdJT _ty tA a tC d x p ->
    let vd = eval ctx env d
    in case forceVal ctx (eval ctx env p) of
         VCon ReflF{} -> vd
         VNeutral np  -> VNeutral
           (NIdJ (eval ctx env tA) (eval ctx env a) (eval ctx env tC)
                 vd (eval ctx env x) np)
         _ -> VAbort

  -- the context-sensitive fragment
  HoleT{} -> VAbort
  RecOrT{} -> VAbort
  RecBottomT{} -> VAbort
  TypeRestrictedT{} -> VAbort
  TypeModalT{} -> VAbort
  ModAppT{} -> VAbort
  ModExtractT{} -> VAbort
  LetModT{} -> VAbort

  -- everything else is a plain constructor: evaluate the fields
  Node (AnnSig _info sig) ->
    VCon (bimap (\(ScopedAST binder body) -> Closure env binder body) (eval ctx env) sig)
  where
    fstOf l _r = l
    sndOf _l r = r

-- | β, and δ at the head of an elimination, exactly where 'whnfT' unfolds.
applyVal :: Context n -> Val n -> Val n -> Val n
applyVal ctx f v = case f of
  VLam closure -> applyClosure ctx closure v
  VNeutral neu -> case unfoldNeu ctx neu of
    Just f' -> applyVal ctx f' v
    Nothing -> VNeutral (NApp neu v)
  _ -> VAbort

applyClosure :: Context n -> Closure n -> Val n -> Val n
applyClosure ctx (Closure env binder body) v =
  eval ctx (IntMap.insert (Foil.nameId (Foil.nameOf binder)) v env) body

-- | Project from a pair value, or stay neutral.
projVal
  :: Context n
  -> (Neu n -> Neu n) -> (Val n -> Val n -> Val n)
  -> Val n -> Val n
projVal ctx neu pick v = case forceVal ctx v of
  VCon (PairF l r) -> pick l r
  VNeutral n       -> VNeutral (neu n)
  _                -> VAbort

-- | Unfold a neutral's head definition once (replaying the spine), if it has one.
unfoldNeu :: Context n -> Neu n -> Maybe (Val n)
unfoldNeu ctx = \case
  NVar x   -> eval ctx IntMap.empty <$> varValue (lookupVarInfo x ctx)
  NLevel _ -> Nothing
  NApp n v -> (\f -> applyVal ctx f v) <$> unfoldNeu ctx n
  NFirst n  -> projVal ctx NFirst  (\l _ -> l) <$> unfoldNeu ctx n
  NSecond n -> projVal ctx NSecond (\_ r -> r) <$> unfoldNeu ctx n
  NIdJ tA a tC d x n ->
    (\vp -> case forceVal ctx vp of
        VCon ReflF{} -> d
        VNeutral np  -> VNeutral (NIdJ tA a tC d x np)
        _            -> VAbort)
    <$> unfoldNeu ctx n

-- | Unfold definitions at the head until the value is canonical or truly stuck.
forceVal :: Context n -> Val n -> Val n
forceVal ctx (VNeutral n) | Just v <- unfoldNeu ctx n = forceVal ctx v
forceVal _ v = v

-- * Conversion

-- | 'False' means "do not know", never a definite inequality.
conv :: Context n -> Int -> Val n -> Val n -> Bool
conv ctx lvl l r = case (forceVal ctx l, forceVal ctx r) of
  (VAbort, _) -> False
  (_, VAbort) -> False

  (VLam c1, VLam c2) ->
    conv ctx (lvl + 1) (applyClosure ctx c1 (freshV lvl)) (applyClosure ctx c2 (freshV lvl))
  -- one-step η for lambdas, as in 'etaMatch'
  (VLam c1, v2) ->
    conv ctx (lvl + 1) (applyClosure ctx c1 (freshV lvl)) (applyVal ctx v2 (freshV lvl))
  (v1, VLam c2) ->
    conv ctx (lvl + 1) (applyVal ctx v1 (freshV lvl)) (applyClosure ctx c2 (freshV lvl))

  -- one-step η for pairs
  (VCon (PairF a b), VNeutral n) ->
    conv ctx lvl a (VNeutral (NFirst n)) && conv ctx lvl b (VNeutral (NSecond n))
  (VNeutral n, VCon (PairF a b)) ->
    conv ctx lvl (VNeutral (NFirst n)) a && conv ctx lvl (VNeutral (NSecond n)) b

  (VCon s1, VCon s2) -> case zipMatch2 s1 s2 of
    Nothing -> False
    Just s  -> getAll (bifoldMap
      (All . uncurry (convClosure ctx lvl))
      (All . uncurry (conv ctx lvl))
      s)

  (VNeutral n1, VNeutral n2) -> convNeu ctx lvl n1 n2
  _ -> False
  where
    freshV = VNeutral . NLevel

convClosure :: Context n -> Int -> Closure n -> Closure n -> Bool
convClosure ctx lvl c1 c2 =
  conv ctx (lvl + 1) (applyClosure ctx c1 fresh) (applyClosure ctx c2 fresh)
  where
    fresh = VNeutral (NLevel lvl)

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

-- | Are the two terms definitely convertible? 'False' means "do not know" —
-- fall back to the ordinary unification.
nbeConvertible :: TermT n -> TermT n -> TypeCheck n Bool
nbeConvertible t1 t2 = asks $ \ctx ->
  conv ctx 0 (eval ctx IntMap.empty t1) (eval ctx IntMap.empty t2)
