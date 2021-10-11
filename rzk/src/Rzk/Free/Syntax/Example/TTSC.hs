{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Rzk.Free.Syntax.Example.TTSC where

import qualified Bound.Scope                             as Scope
import qualified Bound.Var                               as Bound
import           Data.Bifunctor
import           Data.Bifunctor.TH
import           Data.Char                               (chr, ord)
import           Data.Maybe                              (fromMaybe)
import           Data.String                             (IsString (..))
import           Data.Text.Prettyprint.Doc               as Doc
import qualified Rzk.Free.Bound.Name                     as Name

import           Rzk.Free.Syntax.FreeScoped
import qualified Rzk.Free.Syntax.FreeScoped.TypeCheck    as TypeCheck
import           Rzk.Free.Syntax.FreeScoped.Unification2 (HigherOrderUnifiable (..),
                                                          Unifiable (..))
import qualified Rzk.Free.Syntax.FreeScoped.Unification2 as Unification2
import qualified Rzk.Syntax.Var                          as Rzk

data CubeTermF scope term
  = CubeF
  | CubeUnitF
  | CubeUnitPointF

  | CubeProdF term term
  | CubeFirstF term
  | CubeSecondF term
  | CubePairF term term

  | Cube2F
  | Cube2_0F
  | Cube2_1F
  deriving (Eq, Functor, Foldable)

data TopeTermF scope term
  = TopeF

  | TopeTopF
--  | TopeTopIntroF

  | TopeBottomF
--  | TopeBottomElimF (Maybe term)

  | TopeOrF term term
--  | TopeOrInLF (Maybe term) term
--  | TopeOrInRF (Maybe term) term
--  | TopeOrCaseF term scope scope

  | TopeAndF term term
--  | TopeAndFirstF term
--  | TopeAndSecondF term
--  | TopeAndPairF term term

  | TopeEqF term term
--  | TopeEqReflF term
--  | TopeEqSymF term
--  | TopeEqTransF term term
--  | TopeEqSubstF term scope term

--  | TopeUniqUnitPointF term
--  | TopeUniqFirstF term term
--  | TopeUniqSecondF term term
--  | TopeUniqPairF term

  | TopeLeqF term term
--  | TopeLeqReflF term
--  | TopeLeqTransF term term
--  | TopeLeqAntiSymF term term
--  | TopeLeqLEMF term term
--  | TopeLeq0F term
--  | TopeLeq1F term
--  | TopeLeqDistinctF
  deriving (Eq, Functor, Foldable)

data TypeTermF scope term
  = UniverseF

  | PiF term scope
  | LamF (Maybe term) scope
  | AppF term term

  | SigmaF term scope
  | PairF term term
  | FirstF term
  | SecondF term

  | IdTypeF (Maybe term) term term
  | ReflF (Maybe term) term
  | JF term term term term term term

  | RecBottomF (Maybe term)
  | RecOrF term term term term

  | ExtPiF term scope scope scope scope
  | ExtLamF (Maybe (term, Maybe scope)) scope
  | ExtAppF term term

  deriving (Eq, Functor, Foldable)

type TopeF = CubeTermF :+: TopeTermF
type TermF = CubeTermF :+: TopeTermF :+: TypeTermF

type Term b = TypeCheck.Term TermF b
type Term' = Term Rzk.Var Rzk.Var

type ScopedTerm b = TypeCheck.ScopedTerm TermF b
type ScopedTerm' = ScopedTerm Rzk.Var Rzk.Var

type TypedTerm b = TypeCheck.TypedTerm TermF b
type TypedTerm' = TypedTerm Rzk.Var Rzk.Var

type ScopedTypedTerm b = TypeCheck.ScopedTypedTerm TermF b
type ScopedTypedTerm' = ScopedTypedTerm Rzk.Var Rzk.Var

type UTypedTerm b a v = TypeCheck.UTypedTerm TermF b a v
type UTypedTerm' = UTypedTerm Rzk.Var Rzk.Var Rzk.Var

-- *** For typechecking

type TypeError' = TypeCheck.TypeError UTypedTerm'
type TypeInfo' = TypeCheck.TypeInfo Rzk.Var UTypedTerm' Rzk.Var
type TypeCheck' = TypeCheck.TypeCheck UTypedTerm' Rzk.Var Rzk.Var

-- ** Pattern synonyms (should be generated with TH)

-- *** Untyped

-- | A variable.
pattern Var :: a -> Term b a
pattern Var x = PureScoped x

pattern Cube :: Term b a
pattern Cube = FreeScoped (InL (InL CubeF))

pattern CubeUnit :: Term b a
pattern CubeUnit = FreeScoped (InL (InL CubeUnitF))

pattern CubeUnitPoint :: Term b a
pattern CubeUnitPoint = FreeScoped (InL (InL CubeUnitPointF))

pattern CubeProd :: Term b a -> Term b a -> Term b a
pattern CubeProd i j = FreeScoped (InL (InL (CubeProdF i j)))

pattern CubePair :: Term b a -> Term b a -> Term b a
pattern CubePair i j = FreeScoped (InL (InL (CubePairF i j)))

pattern CubeFirst :: Term b a -> Term b a
pattern CubeFirst c = FreeScoped (InL (InL (CubeFirstF c)))

pattern CubeSecond :: Term b a -> Term b a
pattern CubeSecond c = FreeScoped (InL (InL (CubeSecondF c)))

pattern Cube2 :: Term b a
pattern Cube2 = FreeScoped (InL (InL Cube2F))

pattern Cube2_0 :: Term b a
pattern Cube2_0 = FreeScoped (InL (InL Cube2_0F))

pattern Cube2_1 :: Term b a
pattern Cube2_1 = FreeScoped (InL (InL Cube2_1F))

pattern Tope :: Term b a
pattern Tope = FreeScoped (InL (InR TopeF))

pattern TopeBottom :: Term b a
pattern TopeBottom = FreeScoped (InL (InR TopeBottomF))

pattern TopeTop :: Term b a
pattern TopeTop = FreeScoped (InL (InR TopeTopF))

pattern TopeOr :: Term b a -> Term b a -> Term b a
pattern TopeOr x y = FreeScoped (InL (InR (TopeOrF x y)))

pattern TopeAnd :: Term b a -> Term b a -> Term b a
pattern TopeAnd x y = FreeScoped (InL (InR (TopeAndF x y)))

pattern TopeEq :: Term b a -> Term b a -> Term b a
pattern TopeEq x y = FreeScoped (InL (InR (TopeEqF x y)))

pattern TopeLeq :: Term b a -> Term b a -> Term b a
pattern TopeLeq x y = FreeScoped (InL (InR (TopeLeqF x y)))

-- | Universe type \(\mathcal{U}_i\)
pattern Universe :: Term b a
pattern Universe = FreeScoped (InR UniverseF)

-- | A dependent product type (\(\Pi\)-type): \(\prod_{x : A} B(x)).
pattern Pi :: Term b a -> ScopedTerm b a -> Term b a
pattern Pi a b = FreeScoped (InR (PiF a b))

mkFun :: Term b a -> Term b a -> Term b a
mkFun a b = Pi a (Scope.toScope (Bound.F <$> b))

-- | A \(\lambda\)-abstraction.
pattern Lam :: Maybe (Term b a) -> ScopedTerm b a -> Term b a
pattern Lam ty body = FreeScoped (InR (LamF ty body))

-- | An application of one term to another.
pattern App :: Term b a -> Term b a -> Term b a
pattern App t1 t2 = FreeScoped (InR (AppF t1 t2))

pattern Sigma :: Term b a -> ScopedTerm b a -> Term b a
pattern Sigma a b = FreeScoped (InR (SigmaF a b))

pattern Pair :: Term b a -> Term b a -> Term b a
pattern Pair t1 t2 = FreeScoped (InR (PairF t1 t2))

pattern First :: Term b a -> Term b a
pattern First t = FreeScoped (InR (FirstF t))

pattern Second :: Term b a -> Term b a
pattern Second t = FreeScoped (InR (SecondF t))

pattern IdType :: Maybe (Term b a) -> Term b a -> Term b a -> Term b a
pattern IdType t x y = FreeScoped (InR (IdTypeF t x y))

pattern Refl :: Maybe (Term b a) -> Term b a -> Term b a
pattern Refl t x = FreeScoped (InR (ReflF t x))

pattern J
  :: Term b a
  -> Term b a
  -> Term b a
  -> Term b a
  -> Term b a
  -> Term b a
  -> Term b a
pattern J tA a tC d x p = FreeScoped (InR (JF tA a tC d x p))

pattern ExtPi
  :: Term b a
  -> ScopedTerm b a
  -> ScopedTerm b a
  -> ScopedTerm b a
  -> ScopedTerm b a
  -> Term b a
pattern ExtPi i psi tA phi a = FreeScoped (InR (ExtPiF i psi tA phi a))

pattern ExtLam :: Maybe (Term b a, Maybe (ScopedTerm b a)) -> ScopedTerm b a -> Term b a
pattern ExtLam ty body = FreeScoped (InR (ExtLamF ty body))

pattern ExtApp :: Term b a -> Term b a -> Term b a
pattern ExtApp t1 t2 = FreeScoped (InR (ExtAppF t1 t2))

pattern RecBottom :: Maybe (Term b a) -> Term b a
pattern RecBottom phi = FreeScoped (InR (RecBottomF phi))

pattern RecOr :: Term b a -> Term b a -> Term b a -> Term b a -> Term b a
pattern RecOr psi phi a b = FreeScoped (InR (RecOrF psi phi a b))

{-# COMPLETE
   Var,

   Cube,
   CubeUnit, CubeUnitPoint,
   CubeProd, CubePair, CubeFirst, CubeSecond,
   Cube2, Cube2_0, Cube2_1,

   Tope,
   TopeBottom, TopeTop, TopeOr, TopeAnd, TopeEq,
   TopeLeq,

   Universe,
   Pi, Lam, App,
   Sigma, Pair, First, Second,
   IdType, Refl, J,

   RecBottom, RecOr,
   ExtPi, ExtLam, ExtApp
   #-}

-- *** Typed

-- | A variable.
pattern VarT :: a -> TypedTerm b a
pattern VarT x = PureScoped x

pattern CubeT :: Maybe (TypedTerm b a) -> TypedTerm b a
pattern CubeT ty = TypeCheck.TypedT ty (InL (InL CubeF))

pattern CubeUnitT :: Maybe (TypedTerm b a) -> TypedTerm b a
pattern CubeUnitT ty = TypeCheck.TypedT ty (InL (InL CubeUnitF))

pattern CubeUnitPointT :: Maybe (TypedTerm b a) -> TypedTerm b a
pattern CubeUnitPointT ty = TypeCheck.TypedT ty (InL (InL CubeUnitPointF))

pattern CubeProdT :: Maybe (TypedTerm b a) -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a
pattern CubeProdT ty i j = TypeCheck.TypedT ty (InL (InL (CubeProdF i j)))

pattern CubePairT :: Maybe (TypedTerm b a) -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a
pattern CubePairT ty i j = TypeCheck.TypedT ty (InL (InL (CubePairF i j)))

pattern CubeFirstT :: Maybe (TypedTerm b a) -> TypedTerm b a -> TypedTerm b a
pattern CubeFirstT ty c = TypeCheck.TypedT ty (InL (InL (CubeFirstF c)))

pattern CubeSecondT :: Maybe (TypedTerm b a) -> TypedTerm b a -> TypedTerm b a
pattern CubeSecondT ty c = TypeCheck.TypedT ty (InL (InL (CubeSecondF c)))

pattern Cube2T :: Maybe (TypedTerm b a) -> TypedTerm b a
pattern Cube2T ty = TypeCheck.TypedT ty (InL (InL Cube2F))

pattern Cube2_0T :: Maybe (TypedTerm b a) -> TypedTerm b a
pattern Cube2_0T ty = TypeCheck.TypedT ty (InL (InL Cube2_0F))

pattern Cube2_1T :: Maybe (TypedTerm b a) -> TypedTerm b a
pattern Cube2_1T ty = TypeCheck.TypedT ty (InL (InL Cube2_1F))

pattern TopeT :: Maybe (TypedTerm b a) -> TypedTerm b a
pattern TopeT ty = TypeCheck.TypedT ty (InL (InR TopeF))

pattern TopeBottomT :: Maybe (TypedTerm b a) -> TypedTerm b a
pattern TopeBottomT ty = TypeCheck.TypedT ty (InL (InR TopeBottomF))

pattern TopeTopT :: Maybe (TypedTerm b a) -> TypedTerm b a
pattern TopeTopT ty = TypeCheck.TypedT ty (InL (InR TopeTopF))

pattern TopeOrT :: Maybe (TypedTerm b a) -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a
pattern TopeOrT ty x y = TypeCheck.TypedT ty (InL (InR (TopeOrF x y)))

pattern TopeAndT :: Maybe (TypedTerm b a) -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a
pattern TopeAndT ty x y = TypeCheck.TypedT ty (InL (InR (TopeAndF x y)))

pattern TopeEqT :: Maybe (TypedTerm b a) -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a
pattern TopeEqT ty x y = TypeCheck.TypedT ty (InL (InR (TopeEqF x y)))

pattern TopeLeqT :: Maybe (TypedTerm b a) -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a
pattern TopeLeqT ty x y = TypeCheck.TypedT ty (InL (InR (TopeLeqF x y)))

-- | Universe type \(\mathcal{U}_i\)
pattern UniverseT :: Maybe (TypedTerm b a) -> TypedTerm b a
pattern UniverseT ty = TypeCheck.TypedT ty (InR UniverseF)

-- | A dependent product type (\(\Pi\)-type): \(\prod_{x : A} B(x)).
pattern PiT
  :: Maybe (TypedTerm b a)
  -> TypedTerm b a
  -> ScopedTypedTerm b a
  -> TypedTerm b a
pattern PiT ty a b = TypeCheck.TypedT ty (InR (PiF a b))

mkFunT :: TypedTerm b a -> TypedTerm b a -> TypedTerm b a
mkFunT a b = PiT (Just universeT) a (Scope.toScope (Bound.F <$> b))

-- | A \(\lambda\)-abstraction.
pattern LamT :: Maybe (TypedTerm b a) -> Maybe (TypedTerm b a) -> ScopedTypedTerm b a -> TypedTerm b a
pattern LamT ty argType body = TypeCheck.TypedT ty (InR (LamF argType body))

-- | An application of one term to another.
pattern AppT :: Maybe (TypedTerm b a) -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a
pattern AppT ty t1 t2 = TypeCheck.TypedT ty (InR (AppF t1 t2))

pattern SigmaT :: Maybe (TypedTerm b a) -> TypedTerm b a -> ScopedTypedTerm b a -> TypedTerm b a
pattern SigmaT ty a b = TypeCheck.TypedT ty (InR (SigmaF a b))

pattern PairT :: Maybe (TypedTerm b a) -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a
pattern PairT ty t1 t2 = TypeCheck.TypedT ty (InR (PairF t1 t2))

pattern FirstT :: Maybe (TypedTerm b a) -> TypedTerm b a -> TypedTerm b a
pattern FirstT ty t = TypeCheck.TypedT ty (InR (FirstF t))

pattern SecondT :: Maybe (TypedTerm b a) -> TypedTerm b a -> TypedTerm b a
pattern SecondT ty t = TypeCheck.TypedT ty (InR (SecondF t))

pattern IdTypeT :: Maybe (TypedTerm b a) -> Maybe (TypedTerm b a) -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a
pattern IdTypeT ty t x y = TypeCheck.TypedT ty (InR (IdTypeF t x y))

pattern ReflT :: Maybe (TypedTerm b a) -> Maybe (TypedTerm b a) -> TypedTerm b a -> TypedTerm b a
pattern ReflT ty t x = TypeCheck.TypedT ty (InR (ReflF t x))

pattern JT
  :: Maybe (TypedTerm b a)
  -> TypedTerm b a
  -> TypedTerm b a
  -> TypedTerm b a
  -> TypedTerm b a
  -> TypedTerm b a
  -> TypedTerm b a
  -> TypedTerm b a
pattern JT ty tA a tC d x p = TypeCheck.TypedT ty (InR (JF tA a tC d x p))

pattern ExtPiT
  :: Maybe (TypedTerm b a) -> TypedTerm b a
  -> ScopedTypedTerm b a
  -> ScopedTypedTerm b a
  -> ScopedTypedTerm b a
  -> ScopedTypedTerm b a
  -> TypedTerm b a
pattern ExtPiT ty i psi tA phi a = TypeCheck.TypedT ty (InR (ExtPiF i psi tA phi a))

pattern ExtLamT :: Maybe (TypedTerm b a) -> Maybe (TypedTerm b a, Maybe (ScopedTypedTerm b a)) -> ScopedTypedTerm b a -> TypedTerm b a
pattern ExtLamT ty argType body = TypeCheck.TypedT ty (InR (ExtLamF argType body))

pattern ExtAppT :: Maybe (TypedTerm b a) -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a
pattern ExtAppT ty t1 t2 = TypeCheck.TypedT ty (InR (ExtAppF t1 t2))

pattern RecBottomT :: Maybe (TypedTerm b a) -> Maybe (TypedTerm b a) -> TypedTerm b a
pattern RecBottomT ty phi = TypeCheck.TypedT ty (InR (RecBottomF phi))

pattern RecOrT :: Maybe (TypedTerm b a) -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a
pattern RecOrT ty psi phi a b = TypeCheck.TypedT ty (InR (RecOrF psi phi a b))

{-# COMPLETE
   VarT,

   CubeT,
   CubeUnitT, CubeUnitPointT,
   CubeProdT, CubePairT, CubeFirstT, CubeSecondT,
   Cube2T, Cube2_0T, Cube2_1T,

   TopeT,
   TopeBottomT, TopeTopT, TopeOrT, TopeAndT, TopeEqT,
   TopeLeqT,

   UniverseT,
   PiT, LamT, AppT,
   SigmaT, PairT, FirstT, SecondT,
   IdTypeT, ReflT, JT,

   RecBottomT, RecOrT,
   ExtPiT, ExtLamT, ExtAppT
   #-}

-- ** Smart constructors

-- | Universe (type of types).
--
-- >>> universeT :: TypedTerm'
-- U : U
universeT :: TypedTerm b a
universeT = TypeCheck.TypedT Nothing (InR UniverseF)

-- | Abstract over one variable in a term.
--
-- >>> lam Nothing "x" (App (Var "f") (Var "x")) :: Term'
-- λx₁ → f x₁
-- >>> lam Nothing "f" (App (Var "f") (Var "x")) :: Term'
-- λx₁ → x₁ x
-- >>> lam (Just (Var "A")) "x" (App (Var "f") (Var "x")) :: Term'
-- λ(x₁ : A) → f x₁
-- >>> lam (Just (Fun (Var "A") (Var "B"))) "f" (App (Var "f") (Var "x")) :: Term'
-- λ(x₁ : A → B) → x₁ x
lam :: Eq a => Maybe (Term a a) -> a -> Term a a -> Term a a
lam ty x body = Lam ty (Name.abstract1Name x body)

-- | Abstract over one variable in a term (without type).
--
-- >>> lam_ "x" (App (Var "f") (Var "x")) :: Term'
-- λx₁ → f x₁
lam_ :: Eq a => a -> Term a a -> Term a a
lam_ x body = Lam Nothing (Name.abstract1Name x body)

-- ** Evaluation

whnfUntyped :: Term b a -> Term b a
whnfUntyped = TypeCheck.untyped . whnf . TypeCheck.pseudoTyped

nfUntyped :: Term b a -> Term b a
nfUntyped = TypeCheck.untyped . nf . TypeCheck.pseudoTyped

-- | Evaluate a term to its weak head normal form (WHNF).
whnf :: TypedTerm b a -> TypedTerm b a
whnf = \case
  CubeFirstT ty t ->
    case whnf t of
      CubePairT _ty f _s -> whnf f
      t'                 -> CubeFirstT ty t'

  CubeSecondT ty t ->
    case whnf t of
      CubePairT _ty _f s -> whnf s
      t'                 -> CubeSecondT ty t'

  RecOrT ty psi phi a b ->
    error "here ty should not only have the type of RecOr, but it's entire context, so that we can extract tope constraints and act on them"

  AppT ty f x ->
    case whnf f of
      LamT _ty _typeOfArg body ->
        whnf (Scope.instantiate1 x body)
      f' -> AppT ty f' x

  FirstT ty t ->
    case whnf t of
      PairT _ty f _s -> whnf f
      t'             -> FirstT ty t'

  SecondT ty t ->
    case whnf t of
      PairT _ty _f s -> whnf s
      t'             -> SecondT ty t'

  JT ty tA a tC d x p ->
    case whnf p of
      ReflT _ _ _ -> whnf d
      p'          -> JT ty tA a tC d x p'

  t@LamT{} -> t
  t@PairT{} -> t
  t@ReflT{} -> t

  t@UniverseT{} -> t
  t@PiT{} -> t
  t@SigmaT{} -> t
  t@IdTypeT{} -> t

  t@VarT{} -> t

nf :: TypedTerm b a -> TypedTerm b a
nf = \case
  AppT ty f x ->
    case whnf f of
      LamT _ty _typeOfArg body ->
        nf (Scope.instantiate1 x body)
      f' -> AppT (nf <$> ty) (nf f') (nf x)

  FirstT ty t ->
    case whnf t of
      PairT _ty f _s -> nf f
      t'             -> FirstT (nf <$> ty) (nf t')

  SecondT ty t ->
    case whnf t of
      PairT _ty _f s -> nf s
      t'             -> SecondT (nf <$> ty) (nf t')

  JT ty tA a tC d x p ->
    case whnf p of
      ReflT _ _ _ -> nf d
      p'          -> JT (nf <$> ty) (nf tA) (nf a) (nf tC) (nf d) (nf x) (nf p')

  LamT ty typeOfArg body -> LamT (nf <$> ty) (nf <$> typeOfArg) (nfScope body)
  PairT ty t1 t2 -> PairT (nf <$> ty) (nf t1) (nf t2)
  ReflT ty a x -> ReflT (nf <$> ty) (nf <$> a) (nf x)

  UniverseT ty -> UniverseT (nf <$> ty)
  PiT ty a b -> PiT (nf <$> ty) (nf a) (nfScope b)
  SigmaT ty a b -> SigmaT (nf <$> ty) (nf a) (nfScope b)
  IdTypeT ty a x y -> IdTypeT (nf <$> ty) (nf <$> a) (nf x) (nf y)

  t@VarT{} -> t
  where
    nfScope = Scope.toScope . nf . Scope.fromScope

-- ** Unification

instance Unifiable CubeTermF where

instance Unifiable TopeTermF where

-- | Should be derived with TH or Generics.
instance Unifiable TypeTermF where
  zipMatch (AppF f1 x1) (AppF f2 x2)
    = Just (AppF (Right (f1, f2)) (Right (x1, x2)))

  zipMatch (LamF argTy1 body1) (LamF argTy2 body2)
    = Just (LamF argTy (Right (body1, body2)))
    where
      argTy =
        case (argTy1, argTy2) of
          (Nothing, _)     -> Left <$> argTy2
          (_, Nothing)     -> Left <$> argTy1
          (Just x, Just y) -> Just (Right (x, y))

  zipMatch (PiF arg1 body1) (PiF arg2 body2)
    = Just (PiF (Right (arg1, arg2)) (Right (body1, body2)))

  zipMatch (SigmaF arg1 body1) (SigmaF arg2 body2)
    = Just (SigmaF (Right (arg1, arg2)) (Right (body1, body2)))
  zipMatch (PairF f1 x1) (PairF f2 x2)
    = Just (PairF (Right (f1, f2)) (Right (x1, x2)))
  zipMatch (FirstF t1) (FirstF t2)
    = Just (FirstF (Right (t1, t2)))
  zipMatch (SecondF t1) (SecondF t2)
    = Just (SecondF (Right (t1, t2)))

  zipMatch (IdTypeF a1 x1 y1) (IdTypeF a2 x2 y2)
    = Just (IdTypeF a (Right (x1, x2)) (Right (y1, y2)))
    where
      a =
        case (a1, a2) of
          (Nothing, _)     -> Left <$> a2
          (_, Nothing)     -> Left <$> a1
          (Just x, Just y) -> Just (Right (x, y))
  zipMatch (ReflF a1 x1) (ReflF a2 x2)
    = Just (ReflF a (Right (x1, x2)))
    where
      a =
        case (a1, a2) of
          (Nothing, _)     -> Left <$> a2
          (_, Nothing)     -> Left <$> a1
          (Just x, Just y) -> Just (Right (x, y))
  zipMatch (JF tA1 a1 tC1 d1 x1 p1) (JF tA2 a2 tC2 d2 x2 p2)
    = Just (JF (Right (tA1, tA2)) (Right (a1, a2)) (Right (tC1, tC2)) (Right (d1, d2)) (Right (x1, x2)) (Right (p1, p2)))

  zipMatch UniverseF UniverseF = Just UniverseF

  zipMatch PiF{} _ = Nothing
  zipMatch LamF{} _ = Nothing
  zipMatch AppF{} _ = Nothing

  zipMatch SigmaF{} _ = Nothing
  zipMatch PairF{} _ = Nothing
  zipMatch FirstF{} _ = Nothing
  zipMatch SecondF{} _ = Nothing

  zipMatch IdTypeF{} _ = Nothing
  zipMatch ReflF{} _ = Nothing
  zipMatch JF{} _ = Nothing

  zipMatch UniverseF{} _ = Nothing

instance HigherOrderUnifiable TypeTermF where
  appSome _ []     = error "cannot apply to zero arguments"
  appSome f (x:xs) = (AppF f x, xs)

  unAppSome (AppF f x) = Just (f, [x])
  unAppSome _          = Nothing

  abstract = LamF Nothing

unifyTerms
  :: (Eq v, Eq a)
  => [v]
  -> UTypedTerm b a v
  -> UTypedTerm b a v
  -> [([(v, UTypedTerm b a v)], [(UTypedTerm b a v, UTypedTerm b a v)])]
unifyTerms mvars t1 t2 = Unification2.driver mvars whnf (t1, t2)

unifyTerms_
  :: (Eq v, Eq a)
  => [v]
  -> UTypedTerm b a v
  -> UTypedTerm b a v
  -> [(v, UTypedTerm b a v)]
unifyTerms_ mvars t1 t2 = fst (head (unifyTerms mvars t1 t2))

unifyTerms'
  :: UTypedTerm'
  -> UTypedTerm'
  -> [([(Rzk.Var, UTypedTerm')], [(UTypedTerm', UTypedTerm')])]
unifyTerms' = unifyTerms (iterate succ "?")

-- | Unify two typed terms with meta-variables.
unifyTerms'_
  :: UTypedTerm'
  -> UTypedTerm'
  -> [(Rzk.Var, UTypedTerm')]
unifyTerms'_ t1 t2 = fst (head (unifyTerms' t1 t2))

-- ** Pretty-printing

instance Pretty Rzk.Var where
  pretty (Rzk.Var x) = pretty x

instance (Pretty n, Pretty b) => Pretty (Name.Name n b) where
  pretty (Name.Name Nothing b)     = pretty b
  pretty (Name.Name (Just name) b) = "<" <> pretty name <> " " <> pretty b <> ">"

instance (Pretty b, Pretty a) => Pretty (Bound.Var b a) where
  pretty (Bound.B b) = "<bound " <> pretty b <> ">"
  pretty (Bound.F x) = "<free " <> pretty x <> ">"

instance IsString a => IsString (Bound.Var b a) where
  fromString = Bound.F . fromString

-- | Uses 'Pretty' instance.
instance (Pretty a, Pretty b, IsString a) => Show (Term b a) where
  show = show . pretty

-- | Uses default names (@x@ with a positive integer subscript) for bound variables:
instance (Pretty a, Pretty b, IsString a) => Pretty (Term b a) where
  pretty = ppTerm defaultFreshVars

defaultFreshVars :: IsString a => [a]
defaultFreshVars = mkDefaultFreshVars "x"

defaultFreshMetaVars :: IsString a => [a]
defaultFreshMetaVars = mkDefaultFreshVars "M"

mkDefaultFreshVars :: IsString a => String -> [a]
mkDefaultFreshVars prefix = [ fromString (prefix <> toIndex i) | i <- [1..] ]
  where
    toIndex n = index
      where
        digitToSub c = chr ((ord c - ord '0') + ord '₀')
        index = map digitToSub (show n)

instance (Pretty a, Pretty b, IsString a) => Show (TypedTerm b a) where
  show = \case
    FreeScoped (TypeCheck.TypedF term ty) -> show (FreeScoped (bimap TypeCheck.untypedScoped TypeCheck.untyped term)) <> " : " <> show (TypeCheck.untyped (fromMaybe universeT ty))
    t -> show (TypeCheck.untyped t)

ppTypedTerm :: (Pretty a, Pretty b) => [a] -> TypedTerm b a -> Doc ann
ppTypedTerm vars = ppTerm vars . TypeCheck.untyped

-- | Pretty-print an untyped term.
ppTerm :: (Pretty a, Pretty b) => [a] -> Term b a -> Doc ann
ppTerm vars = \case
  Var x -> pretty x

  Cube -> "CUBE"
  CubeUnit -> "1"
  CubeUnitPoint -> "★"
  CubeProd i j -> ppTermArg vars i <+> "×" <+> ppTermArg vars j
  CubeFirst t  -> "π₁" <+> ppTermArg vars t
  CubeSecond t -> "π₂" <+> ppTermArg vars t
  CubePair f s -> tupled (map (ppTerm vars) [f, s])

  Cube2 -> "𝟚"
  Cube2_0 -> "0"
  Cube2_1 -> "1"

  Tope -> "TOPE"
  TopeTop -> "⊤"
  TopeBottom -> "⊥"
  TopeOr psi phi -> ppTermArg vars psi <+> "∨" <+> ppTermArg vars phi
  TopeAnd psi phi -> ppTermArg vars psi <+> "∧" <+> ppTermArg vars phi
  TopeEq x y -> ppTermArg vars x <+> "≡" <+> ppTermArg vars y
  TopeLeq x y -> ppTermArg vars x <+> "≤" <+> ppTermArg vars y

  Universe -> "U"

  Pi a b -> ppScopedTerm vars b $ \x b' ->
    if withoutBoundVars b
       then ppTermArg vars a <+> "→" <+> b'
       else parens (pretty x <+> ":" <+> ppTerm vars a) <+> "→" <+> b'
  Lam Nothing body -> ppScopedTerm vars body $ \x body' ->
    "λ" <> pretty x <+> "→" <+> body'
  Lam (Just ty) body -> ppScopedTerm vars body $ \x body' ->
    "λ" <> parens (pretty x <+> ":" <+> ppTerm vars ty) <+> "→" <+> body'
  App f x -> ppTermFun vars f <+> ppTermArg vars x

  Sigma a b -> ppScopedTerm vars b $ \x b' ->
    if withoutBoundVars b
       then ppTermArg vars a <+> "×" <+> b'
       else parens (pretty x <+> ":" <+> ppTerm vars a) <+> "×" <+> b'
  Pair f s -> tupled (map (ppTerm vars) [f, s])
  First t  -> "π₁" <+> ppTermArg vars t
  Second t -> "π₂" <+> ppTermArg vars t

  IdType Nothing x y -> ppTermFun vars x <+> "=" <+> ppTermFun vars y
  IdType (Just a) x y -> ppTermFun vars x <+> "=_{" <> ppTerm vars a <> "}" <+> ppTermFun vars y
  Refl Nothing x -> "refl" <+> ppTermArg vars x
  Refl (Just a) x -> "refl_{" <> ppTerm vars a <> "}" <+> ppTermArg vars x
  J tA a tC d x p -> ppElimWithArgs vars "J" [tA, a, tC, d, x, p]

  ExtPi i psi tA phi a -> ppScopedTerms vars [psi, tA, phi, a] $ \x [psi', tA', phi', a'] ->
     "<{" <+> pretty x <+> ":" <+> ppTerm vars i <+> "|" <+> psi'
     <+> "}" <+> "→" <+> tA' <+> "|" <+> phi' <+> "↦" <+> a' <+> ">"
  ExtLam Nothing body -> ppScopedTerm vars body $ \x body' ->
    "λ" <> pretty x <+> "→" <+> body'
  ExtLam (Just (i, Nothing)) body -> ppScopedTerm vars body $ \x body' ->
    "λ" <> parens (pretty x <+> ":" <+> ppTerm vars i) <+> "→" <+> body'
  ExtLam (Just (i, (Just psi))) body -> ppScopedTerms vars [psi, body] $ \x [psi', body'] ->
    "λ" <> "{" <+> pretty x <+> ":" <+> ppTerm vars i <+> "|" <+> psi' <+> "}" <+> "→" <+> body'
  ExtApp f x -> ppTermFun vars f <+> ppTermArg vars x

  RecBottom Nothing -> "rec⊥"
  RecBottom (Just psi) -> "rec⊥_{" <+> ppTerm vars psi <+> "}"

  RecOr psi phi a b -> ppElimWithArgs vars "rec∨" [psi, phi, a, b]
  where
    withoutBoundVars = null . Scope.bindings

ppElimWithArgs :: (Pretty a, Pretty b) => [a] -> Doc ann -> [Term b a] -> Doc ann
ppElimWithArgs vars name args = name <> tupled (map (ppTermFun vars) args)

-- | Pretty-print an untyped in a head position.
ppTermFun :: (Pretty a, Pretty b) => [a] -> Term b a -> Doc ann
ppTermFun vars = \case
  t@Var{} -> ppTerm vars t

  t@Cube{} -> ppTerm vars t
  t@CubeUnit{} -> ppTerm vars t
  t@CubeUnitPoint{} -> ppTerm vars t
  t@CubeProd{} -> ppTerm vars t
  t@CubePair{} -> ppTerm vars t
  t@CubeFirst{} -> ppTerm vars t
  t@CubeSecond{} -> ppTerm vars t
  t@Cube2{} -> ppTerm vars t
  t@Cube2_0{} -> ppTerm vars t
  t@Cube2_1{} -> ppTerm vars t

  t@Tope{} -> ppTerm vars t
  t@TopeBottom{} -> ppTerm vars t
  t@TopeTop{} -> ppTerm vars t

  t@RecOr{} -> ppTerm vars t
  t@RecBottom{} -> ppTerm vars t

  t@App{} -> ppTerm vars t
  t@ExtApp{} -> ppTerm vars t
  t@First{} -> ppTerm vars t
  t@Second{} -> ppTerm vars t
  t@Pair{} -> ppTerm vars t
  t@Refl{} -> ppTerm vars t
  t@J{} -> ppTerm vars t
  t@Universe{} -> ppTerm vars t

  t@TopeLeq{} -> Doc.parens (ppTerm vars t)
  t@TopeEq{} -> Doc.parens (ppTerm vars t)
  t@TopeOr{} -> Doc.parens (ppTerm vars t)
  t@TopeAnd{} -> Doc.parens (ppTerm vars t)

  t@Sigma{} -> Doc.parens (ppTerm vars t)
  t@IdType{} -> Doc.parens (ppTerm vars t)
  t@Pi{} -> Doc.parens (ppTerm vars t)
  t@Lam{} -> Doc.parens (ppTerm vars t)
  t@ExtPi{} -> Doc.parens (ppTerm vars t)
  t@ExtLam{} -> Doc.parens (ppTerm vars t)

-- | Pretty-print an untyped in an argument position.
ppTermArg :: (Pretty a, Pretty b) => [a] -> Term b a -> Doc ann
ppTermArg vars = \case
  t@Var{} -> ppTerm vars t
  t@Universe{} -> ppTerm vars t
  t@Pair{} -> ppTerm vars t

  t@Cube{} -> ppTerm vars t
  t@CubeUnit{} -> ppTerm vars t
  t@CubeUnitPoint{} -> ppTerm vars t
  t@CubeProd{} -> ppTerm vars t
  t@CubePair{} -> ppTerm vars t
  t@Cube2{} -> ppTerm vars t
  t@Cube2_0{} -> ppTerm vars t
  t@Cube2_1{} -> ppTerm vars t

  t@Tope{} -> ppTerm vars t
  t@TopeBottom{} -> ppTerm vars t
  t@TopeTop{} -> ppTerm vars t

  t@TopeLeq{} -> Doc.parens (ppTerm vars t)
  t@TopeEq{} -> Doc.parens (ppTerm vars t)
  t@TopeOr{} -> Doc.parens (ppTerm vars t)
  t@TopeAnd{} -> Doc.parens (ppTerm vars t)

  t@CubeFirst{} -> Doc.parens (ppTerm vars t)
  t@CubeSecond{} -> Doc.parens (ppTerm vars t)
  t@RecOr{} -> Doc.parens (ppTerm vars t)
  t@RecBottom{} -> Doc.parens (ppTerm vars t)
  t@ExtApp{} -> Doc.parens (ppTerm vars t)
  t@ExtLam{} -> Doc.parens (ppTerm vars t)
  t@ExtPi{} -> Doc.parens (ppTerm vars t)

  t@App{} -> Doc.parens (ppTerm vars t)
  t@First{} -> Doc.parens (ppTerm vars t)
  t@Second{} -> Doc.parens (ppTerm vars t)
  t@Refl{} -> Doc.parens (ppTerm vars t)
  t@J{} -> Doc.parens (ppTerm vars t)
  t@Lam{} -> Doc.parens (ppTerm vars t)
  t@Pi{} -> Doc.parens (ppTerm vars t)
  t@Sigma{} -> Doc.parens (ppTerm vars t)
  t@IdType{} -> Doc.parens (ppTerm vars t)

ppScopedTerm
  :: (Pretty a, Pretty b)
  => [a] -> ScopedTerm b a -> (a -> Doc ann -> Doc ann) -> Doc ann
ppScopedTerm [] _ _            = error "not enough fresh names"
ppScopedTerm (x:xs) t withScope = withScope x (ppTerm xs (Scope.instantiate1 (Var x) t))

ppScopedTerms
  :: (Pretty a, Pretty b)
  => [a] -> [ScopedTerm b a] -> (a -> [Doc ann] -> Doc ann) -> Doc ann
ppScopedTerms [] _ _            = error "not enough fresh names"
ppScopedTerms (x:xs) ts withScope = withScope x (ppTerm xs . Scope.instantiate1 (Var x) <$> ts)

-- Template Haskell derivations
deriveBifunctor ''CubeTermF
deriveBifoldable ''CubeTermF
deriveBitraversable ''CubeTermF

deriveBifunctor ''TopeTermF
deriveBifoldable ''TopeTermF
deriveBitraversable ''TopeTermF

deriveBifunctor ''TypeTermF
deriveBifoldable ''TypeTermF
deriveBitraversable ''TypeTermF
