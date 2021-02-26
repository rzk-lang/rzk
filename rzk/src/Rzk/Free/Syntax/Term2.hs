{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
module Rzk.Free.Syntax.Term2 where

import           Bound
import           Bound.Name
import           Control.Monad          (ap, liftM)
import           Control.Monad.Identity (Identity (..))
import           Control.Monad.Trans    (lift)
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bifunctor.TH
import           Data.Bitraversable

import           Unsafe.Coerce          (unsafeCoerce)

-- * Free monad transformer with scoping

data FreeScopedF term scope subterm a
  = PureScopedF a
  | FreeScopedF (term scope subterm)
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype FreeScopedT b term m a = FreeScopedT
  { runFreeScopedT :: m (FreeScopedF term (Scope b (FreeScopedT b term m) a) (FreeScopedT b term m a) a)
  }

instance (Bifoldable term, Foldable m) => Foldable (FreeScopedT b term m) where
  foldMap f (FreeScopedT m) = foldMap f' m
    where
      f' (PureScopedF x) = f x
      f' (FreeScopedF t) = bifoldMap (foldMap f) (foldMap f) t

instance (Bitraversable term, Traversable m, Monad m)
  => Traversable (FreeScopedT b term m) where
  traverse f (FreeScopedT m) = FreeScopedT <$> traverse f' m
    where
      f' (PureScopedF x) = PureScopedF <$> f x
      f' (FreeScopedF t) = FreeScopedF <$> bitraverse (traverse f) (traverse f) t

instance (Bifunctor term, Monad m) => Functor (FreeScopedT b term m) where
  fmap f (FreeScopedT m) = FreeScopedT (liftM f' m)
    where
      f' (PureScopedF x) = PureScopedF (f x)
      f' (FreeScopedF t) = FreeScopedF (bimap (fmap f) (fmap f) t)

instance (Bifunctor term, Monad m) => Applicative (FreeScopedT b term m) where
  pure = return
  (<*>) = ap

instance (Bifunctor term, Monad m) => Monad (FreeScopedT b term m) where
  return = FreeScopedT . return . PureScopedF
  FreeScopedT m >>= f = FreeScopedT (m >>= f')
    where
      f' (PureScopedF x) = runFreeScopedT (f x)
      f' (FreeScopedF t) = return (FreeScopedF (bimap (>>>= f) (>>= f) t))

hoistFreeScopedT
  :: (Monad m, Bifunctor term)
  => (forall x. m x -> n x) -> FreeScopedT b term m a -> FreeScopedT b term n a
hoistFreeScopedT phi (FreeScopedT m) = FreeScopedT (phi (liftM f' m))
  where
    f' (PureScopedF x) = PureScopedF x
    f' (FreeScopedF t) = FreeScopedF (bimap hoistFreeScopedT' (hoistFreeScopedT phi) t)

    hoistFreeScopedT' = Scope . hoistFreeScopedT phi . fmap (fmap (hoistFreeScopedT phi)) . unscope

transFreeScopedT
  :: (Monad m, Bifunctor term)
  => (forall x y. term x y -> term' x y)
  -> FreeScopedT b term m a -> FreeScopedT b term' m a
transFreeScopedT phi (FreeScopedT m) = FreeScopedT (liftM f' m)
  where
    f' (PureScopedF x) = PureScopedF x
    f' (FreeScopedF t) = FreeScopedF (phi (bimap transFreeScopedT' (transFreeScopedT phi) t))

    transFreeScopedT' = Scope . transFreeScopedT phi . fmap (fmap (transFreeScopedT phi)) . unscope

wrapFreeScopedT
  :: Monad m
  => term (Scope b (FreeScopedT b term m) a) (FreeScopedT b term m a)
  -> FreeScopedT b term m a
wrapFreeScopedT = FreeScopedT . return . FreeScopedF

-- * Free monad with scoping

type FreeScoped b term = FreeScopedT b term Identity

pattern PureScoped :: a -> FreeScoped b term a
pattern PureScoped x = FreeScopedT (Identity (PureScopedF x))

pattern FreeScoped
  :: term (Scope b (FreeScoped b term) a) (FreeScoped b term a)
  -> FreeScoped b term a
pattern FreeScoped t = FreeScopedT (Identity (FreeScopedF t))

-- data FreeScoped b term a
--   = PureScoped a
--   | FreeScoped (term (Scope b (FreeScoped b term) a) (FreeScoped b term a))
--
-- instance Bifunctor term => Functor (FreeScoped b term) where
--   fmap f (PureScoped x) = PureScoped (f x)
--   fmap f (FreeScoped t) = FreeScoped (bimap (fmap f) (fmap f) t)
--
-- instance Bifunctor term => Applicative (FreeScoped b term) where
--   pure = return
--   (<*>) = ap
--
-- instance Bifunctor term => Monad (FreeScoped b term) where
--   return = PureScoped
--   PureScoped x >>= f = f x
--   FreeScoped t >>= f = FreeScoped (bimap (>>>= f) (>>= f) t)

-- * Term bifunctor to provide nodes

data TermF bound scope term
  = LambdaF scope
  | AppF term term
  | UniverseF
  | PiF term scope
  | UnitTypeF
  | UnitF
  deriving (Show, Functor, Foldable, Traversable)

deriveBifunctor ''TermF
deriveBifoldable ''TermF
deriveBitraversable ''TermF

data TypedF term scope typedTerm = TypedF
  { typeF :: typedTerm
  , termF :: term scope typedTerm
  } deriving (Show, Functor, Foldable, Traversable)

instance Bifunctor term => Bifunctor (TypedF term) where
  bimap f g (TypedF t x) = TypedF (g t) (bimap f g x)

instance Bifoldable term => Bifoldable (TypedF term) where
  bifoldMap f g (TypedF t x) = g t <> bifoldMap f g x

instance Bitraversable term => Bitraversable (TypedF term) where
  bitraverse f g (TypedF t x) = TypedF <$> g t <*> bitraverse f g x

type TypedTermF bound = TypedF (TermF bound)

-- * Annotations

data Annotated ann a = Annotated
  { annotation :: ann
  , value      :: a
  } deriving (Show, Functor, Foldable, Traversable)

deriveBifunctor ''Annotated
deriveBifoldable ''Annotated
deriveBitraversable ''Annotated

instance Monoid ann => Applicative (Annotated ann) where
  pure = return
  (<*>) = ap

instance Monoid ann => Monad (Annotated ann) where
  return = Annotated mempty
  Annotated ann x >>= f = reAnnotate (ann <>) (f x)

reAnnotate :: (a -> b) -> Annotated a x -> Annotated b x
reAnnotate = first

-- * Term

-- | Generic term.
type GTerm bound = FreeScopedT (Name bound ())

-- | Term without annotations.
type Term bound = GTerm bound (TermF bound) Identity

-- | A typed term without annotations.
type TypedTerm bound = GTerm bound (TypedTermF bound) Identity

-- | Term with annotations at every node.
type ATerm ann bound = GTerm bound (TermF bound) (Annotated ann)

-- | Typed term with annotations at every node.
type ATypedTerm ann bound = GTerm bound (TypedTermF bound) (Annotated ann)

-- | Scoped term.
type Scope1Term bound = Scope (Name bound ()) (Term bound)

-- | Scoped typed term.
type Scope1TypedTerm bound = Scope (Name bound ()) (TypedTerm bound)

-- | Scoped 'ATerm'.
type Scope1ATerm ann bound = Scope (Name bound ()) (ATerm ann bound)

-- | Scoped annotated typed term.
type Scope1ATypedTerm ann bound = Scope (Name bound ()) (ATypedTerm ann bound)

type ATermF ann b a = FreeScopedF (TermF b) (Scope1ATerm ann b a) (ATerm ann b a) a

type ATermF' ann b a = TermF (Scope1ATerm ann b a) (ATerm ann b a) a

-- ** Patterns

pattern Variable :: a -> Term b a
pattern Variable x = PureScoped x

pattern App :: Term b a -> Term b a -> Term b a
pattern App t1 t2 = FreeScoped (AppF t1 t2)

pattern Lambda :: Scope1Term b a -> Term b a
pattern Lambda body = FreeScoped (LambdaF body)

pattern Universe :: Term b a
pattern Universe = FreeScoped UniverseF

pattern Pi :: Term b a -> Scope1Term b a -> Term b a
pattern Pi a b = FreeScoped (PiF a b)

pattern Unit :: Term b a
pattern Unit = FreeScoped UnitF

pattern UnitType :: Term b a
pattern UnitType = FreeScoped UnitTypeF

{-# COMPLETE Variable, App, Lambda, Universe, Pi, Unit, UnitType #-}

-- *** Patterns for typed terms

pattern VariableT :: a -> TypedTerm b a
pattern VariableT x = PureScoped x

pattern Typed :: TypedTerm b a -> TermF b (Scope1TypedTerm b a) (TypedTerm b a) -> TypedTerm b a
pattern Typed t term = FreeScoped (TypedF t term)

pattern AppT :: TypedTerm b a -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a
pattern AppT t t1 t2 = Typed t (AppF t1 t2)

pattern LambdaT :: TypedTerm b a -> Scope1TypedTerm b a -> TypedTerm b a
pattern LambdaT t body = Typed t (LambdaF body)

pattern UniverseT :: TypedTerm b a -> TypedTerm b a
pattern UniverseT t = Typed t UniverseF

pattern PiT :: TypedTerm b a -> TypedTerm b a -> Scope1TypedTerm b a -> TypedTerm b a
pattern PiT t a b = Typed t (PiF a b)

pattern UnitT :: TypedTerm b a -> TypedTerm b a
pattern UnitT t = Typed t UnitF

pattern UnitTypeT :: TypedTerm b a -> TypedTerm b a
pattern UnitTypeT t = Typed t UnitTypeF

{-# COMPLETE VariableT, AppT, LambdaT, UniverseT, PiT, UnitT, UnitTypeT #-}
{-# COMPLETE VariableT, Typed #-}

-- ** With annotations

pattern ATerm :: ann -> ATermF ann b a -> ATerm ann b a
pattern ATerm ann f = FreeScopedT (Annotated ann f)

pattern AppA :: ann -> ATerm ann b a -> ATerm ann b a -> ATerm ann b a
pattern AppA ann t1 t2 = FreeScopedT (Annotated ann (FreeScopedF (AppF t1 t2)))

pattern LambdaA :: ann -> Scope1ATerm ann b a -> ATerm ann b a
pattern LambdaA ann body = FreeScopedT (Annotated ann (FreeScopedF (LambdaF body)))

pattern UniverseA :: ann -> ATerm ann b a
pattern UniverseA ann = FreeScopedT (Annotated ann (FreeScopedF UniverseF))

pattern PiA :: ann -> ATerm ann b a -> Scope1ATerm ann b a -> ATerm ann b a
pattern PiA ann a b = FreeScopedT (Annotated ann (FreeScopedF (PiF a b)))

{-# COMPLETE Variable, AppA, LambdaA, UniverseA, PiA #-}

-- ** Smart binding constructors

lam :: Eq a => a -> Term a a -> Term a a
lam x body = Lambda (abstract1Name x body)

piType :: Eq a => a -> Term a a -> Term a a -> Term a a
piType x a b = Pi a (abstract1Name x b)

-- * Evaluation

whnf :: Term b a -> Term b a
whnf = \case
  App t1 t2 ->
    case whnf t1 of
      Lambda body -> instantiate1 t2 body
      t1'         -> App t1' t2
  t@Variable{} -> t
  t@Lambda{} -> t
  t@Universe{} -> t
  t@Pi{} -> t
  t@Unit{} -> t
  t@UnitType{} -> t

nf :: Term b a -> Term b a
nf = \case
  App t1 t2 ->
    case whnf t1 of
      Lambda body -> nf (instantiate1 t2 body)
      t1'         -> App (nf t1') (nf t2)
  Lambda body -> Lambda (nfScope body)
  Pi a b -> Pi (nf a) (nfScope b)
  t@Variable{} -> t
  t@Universe{} -> t
  t@Unit{} -> t
  t@UnitType{} -> t
  where
    nfScope = toScope . nf . fromScope

whnfT :: (a -> TypedTerm b a) -> TypedTerm b a -> TypedTerm b a
whnfT typeOfFreeVar = \case
  AppT t t1 t2 ->
    case whnfT typeOfFreeVar t1 of
      _ | PiT _ _ b <- whnfT typeOfFreeVar (typeOf typeOfFreeVar t1), UnitTypeT _ <- fromScope b
        -> UnitT (UnitTypeT universeT)
      LambdaT _ body -> instantiate1 t2 body
      t1'            -> AppT t t1' t2
  t@VariableT{} -> t
  t@LambdaT{} -> t
  t@UniverseT{} -> t
  t@PiT{} -> t
  t@UnitT{} -> t
  t@UnitTypeT{} -> t

whnfTClosed :: TypedTerm b a -> TypedTerm b a
whnfTClosed = whnfT (error "a free variable in a closed term!")

nfT :: (a -> TypedTerm b a) -> TypedTerm b a -> TypedTerm b a
nfT typeOfFreeVar = nfT'
  where
    nfT' = \case
      AppT t t1 t2 ->
        case whnfT typeOfFreeVar t1 of
          _ | PiT _ _ b <- whnfT typeOfFreeVar (typeOf typeOfFreeVar t1), UnitTypeT _ <- fromScope b
            -> UnitT (UnitTypeT universeT)
          LambdaT _ body -> nfT' (instantiate1 t2 body)
          t1'            -> AppT (nfT' t) (nfT' t1') (nfT' t2)
      LambdaT t@(PiT _ a _) body -> LambdaT (nfT' t) (nfScopeT a body)
      LambdaT _ _ -> error "impossible type of Lambda"
      PiT _ a b      -> PiT universeT (nfT' a) (nfScopeT a b)
      t@VariableT{} -> t
      t@UniverseT{} -> t
      t@UnitT{} -> t
      t@UnitTypeT{} -> t
      where
        nfScopeT typeOfBoundVar = toScope . nfT typeOfVar . fromScope
          where
            typeOfVar (B (Name _ ())) = F <$> typeOfBoundVar
            typeOfVar (F x)           = F <$> typeOfFreeVar x

nfTClosed :: TypedTerm b a -> TypedTerm b a
nfTClosed = nfT (error "a free variable in a closed term!")

-- * Pretty-printing

parens :: String -> String
parens s = "(" <> s <> ")"

ppTerm :: [String] -> Term b String -> String
ppTerm vars = \case
  Universe -> "U"
  Variable x -> x
  App t1 t2 -> ppTermFun vars t1 <> " " <> ppTermArg vars t2
  Lambda body ->
    let z:zs = vars
     in "\\" <> z <> "." <> ppTerm zs (instantiate1 (Variable z) body)
  Pi a b ->
    let z:zs = vars
     in parens (z <> " : " <> ppTerm vars a) <> " -> " <> ppTerm zs (instantiate1 (Variable z) b)
  Unit -> "unit"
  UnitType -> "UNIT"

ppTermFun :: [String] -> Term b String -> String
ppTermFun vars = \case
  t@Lambda{} -> parens (ppTerm vars t)
  t -> ppTerm vars t

ppTermArg :: [String] -> Term b String -> String
ppTermArg vars = \case
  t@Variable{} -> ppTerm vars t
  t -> parens (ppTerm vars t)

ppTypedTermWithSig :: [String] -> TypedTerm b String -> String
ppTypedTermWithSig vars t
  = ppTypedTerm vars t <> " : " <> ppTypedTerm vars (typeOf (error "don't know types of free vars") t)

ppTypedTerm :: [String] -> TypedTerm b String -> String
ppTypedTerm vars = \case
  VariableT x -> x
  AppT _ t1 t2 -> parens (ppTypedTerm vars t1) <> " " <> parens (ppTypedTerm vars t2)
  LambdaT (PiT _ a _) body ->
    let z:zs = vars
     in "\\(" <> z <> " : " <> ppTypedTerm vars a <> ")." <> ppTypedTerm zs (instantiate1 (VariableT z) body)
  LambdaT _ body ->
    let z:zs = vars
     in "\\" <> z <> "." <> ppTypedTerm zs (instantiate1 (VariableT z) body)
  UniverseT _ -> "U"
  PiT _ a b ->
    let z:zs = vars
     in parens (z <> " : " <> ppTypedTerm vars a) <> " -> " <> ppTypedTerm zs (instantiate1 (VariableT z) b)
  UnitT _ -> "unit"
  UnitTypeT _ -> "UNIT"

-- * Typecheck

universeT :: TypedTerm b a
universeT = UniverseT universeT

untyped :: TypedTerm b a -> Term b a
untyped = transFreeScopedT termF

typecheckScope
  :: Eq a
  => TypedTerm b a -> (a -> TypedTerm b a) -> Scope1Term b a -> Scope1TypedTerm b a -> Scope1TypedTerm b a
typecheckScope typeOfBoundVar typeOfFreeVar term expectedType
  = toScope (typecheck typeOfVar (fromScope term) (fromScope expectedType))
    where
      typeOfVar (B (Name _ ())) = F <$> typeOfBoundVar
      typeOfVar (F x)           = F <$> typeOfFreeVar x

typeOfScoped :: Eq a => TypedTerm b a -> (a -> TypedTerm b a) -> Scope1TypedTerm b a -> Scope1TypedTerm b a
typeOfScoped typeOfBoundVar typeOfFreeVar = toScope . typeOf typeOfVar . fromScope
    where
      typeOfVar (B (Name _ ())) = F <$> typeOfBoundVar
      typeOfVar (F x)           = F <$> typeOfFreeVar x

typeOf :: (a -> TypedTerm b a) -> TypedTerm b a -> TypedTerm b a
typeOf _ (Typed t _)               = t
typeOf typeOfFreeVar (VariableT x) = typeOfFreeVar x

inferScoped :: Eq a => TypedTerm b a -> (a -> TypedTerm b a) -> Scope1Term b a -> Scope1TypedTerm b a
inferScoped typeOfBoundVar typeOfFreeVar = toScope . infer typeOfVar . fromScope
    where
      typeOfVar (B (Name _ ())) = F <$> typeOfBoundVar
      typeOfVar (F x)           = F <$> typeOfFreeVar x

infer :: Eq a => (a -> TypedTerm b a) -> Term b a -> TypedTerm b a
infer typeOfFreeVar = \case
  Universe -> universeT
  Pi a b ->
    let a' = typecheck typeOfFreeVar a universeT
        b' = typecheckScope a' typeOfFreeVar b (toScope universeT)
     in PiT universeT a' b'
  App (Lambda body) arg ->
    case infer typeOfFreeVar arg of
      arg' ->
        let typeOfArg = typeOf typeOfFreeVar arg'
            body' = inferScoped typeOfArg typeOfFreeVar body
            typeOfBody = typeOfScoped arg' typeOfFreeVar body'
            typeOfResult = instantiate1 arg' typeOfBody
        in AppT typeOfResult (LambdaT (PiT universeT typeOfArg typeOfBody) body') arg'
  App t1 t2 ->
    case infer typeOfFreeVar t1 of
      t1'@(Typed (PiT _ a b) _) ->
        let t2' = typecheck typeOfFreeVar t2 a
         in AppT (instantiate1 t2' b) t1' t2'
      t1'@(VariableT x) ->
        case typeOfFreeVar x of
          PiT _ a b ->
            let t2' = typecheck typeOfFreeVar t2 a
             in AppT (instantiate1 t2' b) t1' t2'
          _ -> error "not a function!"
      _ -> error "not a function!"
  term@(Lambda _body) -> error $ "can't infer Lambda: " <> ppTerm ["x", "y", "z"] (unsafeCoerce term)
  Unit -> UnitT (UnitTypeT universeT)
  UnitType -> UnitTypeT universeT
  Variable x -> VariableT x

typecheck :: Eq a => (a -> TypedTerm b a) -> Term b a -> TypedTerm b a -> TypedTerm b a
typecheck typeOfFreeVar term expectedType = case (term, expectedType) of
  (Lambda body, PiT _ a b) ->
     LambdaT expectedType (typecheckScope a typeOfFreeVar body b)
  (Lambda _, _) -> error "lambda is expected to be a non-function type?!"
  (Variable x, _) -> VariableT x
  _ ->
    case infer typeOfFreeVar term of
      Typed ty x          -> Typed (unify typeOfFreeVar ty expectedType) x
      term'@(VariableT x) -> unify typeOfFreeVar (typeOfFreeVar x) expectedType `seq` term'

unifyScoped
  :: Eq a
  => TypedTerm b a -> (a -> TypedTerm b a) -> Scope1TypedTerm b a -> Scope1TypedTerm b a -> Scope1TypedTerm b a
unifyScoped typeOfBoundVar typeOfFreeVar l r
  = toScope (unify typeOfVar (fromScope l) (fromScope r))
    where
      typeOfVar (B (Name _ ())) = F <$> typeOfBoundVar
      typeOfVar (F x)           = F <$> typeOfFreeVar x

unify :: Eq a => (a -> TypedTerm b a) -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a
unify typeOfFreeVar = go
  where
    go l r = go' (whnfT typeOfFreeVar l) (whnfT typeOfFreeVar r)

    go' (UniverseT _) (UniverseT _) = universeT
    go' (PiT _ a b) (PiT _ c d)     =
      let ac = go a c
       in PiT universeT ac (unifyScoped ac typeOfFreeVar b d)
    go' (AppT t a b) (AppT t' c d)  = AppT (go t t') (go a c) (go b d)
    go' (LambdaT (PiT _ a b) x) (LambdaT (PiT _ c d) y) =
      let ac = go a c
       in LambdaT (PiT universeT ac (unifyScoped ac typeOfFreeVar b d)) (unifyScoped ac typeOfFreeVar x y)
    go' (VariableT x) (VariableT y)
      | x == y = VariableT x
      | otherwise = error "can't unify different variables"
    go' l r = error ("can't unify terms:\n" <> ppTypedTerm ["x", "y", "z"] (unsafeCoerce l) <> "\nand\n" <> ppTypedTerm ["x", "y", "z"] (unsafeCoerce r))

typecheckClosed :: Eq a => Term b a -> TypedTerm b a -> TypedTerm b a
typecheckClosed = typecheck (error "expected closed term, but free vars found!")

inferClosed :: Eq a => Term b a -> TypedTerm b a
inferClosed = infer (error "expected closed term, but free vars found!")

zero :: Term String String
zero = lam "s" (lam "z" (Variable "z"))

nat :: Int -> Term String String
nat n = lam "s" (lam "z" (iterate (App (Variable "s")) (Variable "z") !! n))

(-->) :: Term b a -> Term b a -> Term b a
a --> b = Pi a (lift b)

natT :: Eq a => TypedTerm b a
natT = mkType $ (Universe --> Universe) --> (Universe --> Universe)

mkType :: Eq a => Term b a -> TypedTerm b a
mkType t = typecheckClosed t universeT

ex1 :: Term String String
ex1 = lam "f" (lam "x" (App (Variable "f") (Variable "x")))

ex1Type :: TypedTerm String String
ex1Type = mkType $ piType "f" (Universe --> UnitType) (Universe --> UnitType)

idfun :: Term String String
idfun = lam "x" (Variable "x")

-- |
-- >>> putStrLn $ ppTypedTermWithSig  ["x", "y", "z"] (F.typecheckClosed idfun idfunT)
-- \(x : U).x : (x : U) -> (\(y : U).y) (U)
idfunT :: TypedTerm String String
idfunT = mkType $ Universe --> App idfun Universe
