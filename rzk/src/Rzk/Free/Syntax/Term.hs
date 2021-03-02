{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Rzk.Free.Syntax.Term where

import           Bound
import           Control.Monad              (ap)
import           Control.Monad.Identity     (Identity (..))
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bifunctor.TH
import           Data.Bitraversable

import           Rzk.Free.Bound.Name
import           Rzk.Free.Syntax.FreeScoped

-- * Term bifunctor to provide nodes

data TermF bound scope term
  = LambdaF scope
  | AppF term term
  | UniverseF
  | PiF term scope
  | UnitTypeF
  | UnitF
  | IdTypeF term term term
  | ReflF term term
  | IdJF term term term term term term
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

pattern IdType :: Term b a -> Term b a -> Term b a -> Term b a
pattern IdType t x y = FreeScoped (IdTypeF t x y)

pattern Refl :: Term b a -> Term b a -> Term b a
pattern Refl t x = FreeScoped (ReflF t x)

pattern IdJ :: Term b a -> Term b a -> Term b a -> Term b a -> Term b a -> Term b a -> Term b a
pattern IdJ tA a tC d x p = FreeScoped (IdJF tA a tC d x p)

{-# COMPLETE Variable, App, Lambda, Universe, Pi, Unit, UnitType, IdType, Refl, IdJ #-}

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

pattern IdTypeT :: TypedTerm b a -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a
pattern IdTypeT tt t x y = Typed tt (IdTypeF t x y)

pattern ReflT :: TypedTerm b a -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a
pattern ReflT tt t x = Typed tt (ReflF t x)

pattern IdJT :: TypedTerm b a -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a
pattern IdJT tt tA a tC d x p = Typed tt (IdJF tA a tC d x p)

{-# COMPLETE VariableT, AppT, LambdaT, UniverseT, PiT, UnitT, UnitTypeT, IdTypeT, ReflT, IdJT #-}
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

lam_ :: Term b (Maybe a) -> Term b a
lam_ body = Lambda (abstract1Unnamed body)

piType :: Eq a => a -> Term a a -> Term a a -> Term a a
piType x a b = Pi a (abstract1Name x b)

-- ** ???

typeOfScoped :: Eq a => TypedTerm b a -> (a -> TypedTerm b a) -> Scope1TypedTerm b a -> Scope1TypedTerm b a
typeOfScoped typeOfBoundVar typeOfFreeVar = toScope . typeOf typeOfVar . fromScope
    where
      typeOfVar (B (Name _ ())) = F <$> typeOfBoundVar
      typeOfVar (F x)           = F <$> typeOfFreeVar x

typeOf :: (a -> TypedTerm b a) -> TypedTerm b a -> TypedTerm b a
typeOf _ (Typed t _)               = t
typeOf typeOfFreeVar (VariableT x) = typeOfFreeVar x

untyped :: TypedTerm b a -> Term b a
untyped = transFreeScopedT termF

universeT :: TypedTerm b a
universeT = UniverseT universeT

piT :: TypedTerm b a -> Scope1TypedTerm b a -> TypedTerm b a
piT = PiT universeT

idTypeT :: TypedTerm b a -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a
idTypeT = IdTypeT universeT

idJ :: Term b a
idJ = lam_ (lam_ (lam_ (lam_ (lam_ (lam_ (IdJ arg1 arg2 arg3 arg4 arg5 arg6))))))
  where
    arg1 = Variable (Just (Just (Just (Just (Just Nothing)))))
    arg2 = Variable (Just (Just (Just (Just Nothing))))
    arg3 = Variable (Just (Just (Just Nothing)))
    arg4 = Variable (Just (Just Nothing))
    arg5 = Variable (Just Nothing)
    arg6 = Variable (Nothing)
