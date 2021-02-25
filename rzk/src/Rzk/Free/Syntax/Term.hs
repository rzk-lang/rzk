{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}
module Rzk.Free.Syntax.Term where

import           Bound
import           Bound.Name
import           Control.Monad (ap)
import           Data.Void

newtype Scope' term a b = Scope' { unscope' :: Scope b term a }

unscopeVoid :: Monad term => Scope' term a Void -> term a
unscopeVoid = instantiate absurd . unscope'

abstractVoid :: Monad term => term a -> Scope' term a Void
abstractVoid = Scope' . abstract (const Nothing)

class Functor1 term where
  fmap1 :: (forall x. scope x -> scope' x) -> term scope -> term scope'

data FreeScoped term a
  = PureScoped a
  | FreeScoped (term (Scope' (FreeScoped term) a))

deriving instance
  ( Show a
  , Show (term (Scope' (FreeScoped term) a))
  ) => Show (FreeScoped term a)

instance Functor1 term => Functor (FreeScoped term) where
  fmap f (PureScoped x) = PureScoped (f x)
  fmap f (FreeScoped t) = FreeScoped (fmap1 (Scope' . fmap f . unscope') t)

instance Functor1 term => Applicative (FreeScoped term) where
  pure = return
  (<*>) = ap

instance Functor1 term => Monad (FreeScoped term) where
  return = PureScoped
  PureScoped x >>= f = f x
  FreeScoped t >>= f = FreeScoped (fmap1 (Scope' . (>>>= f) . unscope') t)

data TermF bound scope
  = LambdaF (scope (Name bound ()))
  | AppF (scope Void) (scope Void)

deriving instance
  ( Show (scope (Name bound ()))
  , Show (scope Void)
  ) => Show (TermF bound scope)

instance Functor1 (TermF bound) where
  fmap1 k (LambdaF body) = LambdaF (k body)
  fmap1 k (AppF t1 t2)   = AppF (k t1) (k t2)

type Term bound = FreeScoped (TermF bound)

type Scope1Term bound = Scope (Name bound ()) (Term bound)

-- * Pattern synonyms for 'Term'

pattern Variable :: a -> Term b a
pattern Variable x = PureScoped x

isApp :: Term b a -> Maybe (Term b a, Term b a)
isApp (FreeScoped (AppF t1 t2)) = Just (unscopeVoid t1, unscopeVoid t2)
isApp _ = Nothing

pattern App :: Term b a -> Term b a -> Term b a
pattern App t1 t2 <- (isApp -> Just (t1, t2)) where
  App t1 t2 = FreeScoped (AppF (abstractVoid t1) (abstractVoid t2))

pattern Lambda :: Scope1Term b a -> Term b a
pattern Lambda body = FreeScoped (LambdaF (Scope' body))

{-# COMPLETE Variable, App, Lambda #-}

-- * Smart constructors for binders

lam :: Eq a => a -> Term a a -> Term a a
lam x body = Lambda (abstract1Name x body)

-- * Evaluation

whnf :: Term b a -> Term b a
whnf = \case
  App t1 t2 ->
    case whnf t1 of
      Lambda body -> instantiate1 t2 body
      t1'         -> App t1' t2
  t@Variable{} -> t
  t@Lambda{} -> t

nf :: Term b a -> Term b a
nf = \case
  App t1 t2 ->
    case whnf t1 of
      Lambda body -> nf (instantiate1 t2 body)
      t1' -> App (nf t1') (nf t2)
  Lambda body -> Lambda (nfScope body)
  t@Variable{} -> t
  where
    nfScope = toScope . nf . fromScope

-- * Pretty-printing

parens :: String -> String
parens s = "(" <> s <> ")"

ppTerm :: [String] -> Term b String -> String
ppTerm vars = \case
  Variable x -> x
  App t1 t2 -> ppTermFun vars t1 <> " " <> ppTermArg vars t2
  Lambda body ->
    let z:zs = vars
     in "\\" <> z <> "." <> ppTerm zs (instantiate1 (Variable z) body)

ppTermFun :: [String] -> Term b String -> String
ppTermFun vars = \case
  t@Lambda{} -> parens (ppTerm vars t)
  t -> ppTerm vars t

ppTermArg :: [String] -> Term b String -> String
ppTermArg vars = \case
  t@Variable{} -> ppTerm vars t
  t -> parens (ppTerm vars t)

