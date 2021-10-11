{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
module Rzk.Free.Syntax.FreeScoped.ScopedUnification where

import qualified Bound.Scope                as Bound
import qualified Bound.Scope.Simple         as Bound.Simple
import           Control.Applicative        (Alternative)
import           Control.Monad.State
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bifunctor.TH
import           Data.Bitraversable
import           Data.Functor.Identity      (Identity (..))
import           Data.Monoid                (Any (..))

import           Rzk.Free.Syntax.FreeScoped

-- | A variable in a unifiable term.
data UVarG term a v
  = UFreeVar a
  -- ^ A free variable.
  | UMetaVar v [term (UVarG term a v)]
  -- ^ A meta variable fully applied to some terms, possibly with meta-variables.
  deriving (Functor)

type UVar b t = UVarG (FreeScoped b t)

-- | A free scoped monad for higher-order unification.
type UFreeScoped b t a v =
  FreeScoped b t (UVar b t a v)

class Bitraversable t => Unifiable t where
  zipMatch
    :: t scope term
    -> t scope term
    -> Maybe (t (Either scope (scope, scope)) (Either term (term, term)))

instance (Unifiable f, Unifiable g) => Unifiable (f :+: g) where
  zipMatch (InL f1) (InL f2) = fmap InL (zipMatch f1 f2)
  zipMatch (InR g1) (InR g2) = fmap InR (zipMatch g1 g2)
  zipMatch _ _               = Nothing

class Unifiable t => HigherOrderUnifiable t where
  -- | Assign a list of valid shape guesses to each subterm (and subscope).
  --
  --     bimap fst fst . shapeGuesses = id
  shapeGuesses
    :: t scope term
    -> t (scope, [t () ()]) (term, [t () ()])

instance (HigherOrderUnifiable f, HigherOrderUnifiable g)
  => HigherOrderUnifiable (f :+: g) where

  shapeGuesses (InL f) = InL (bimap (second (map InL)) (second (map InL)) (shapeGuesses f))
  shapeGuesses (InR g) = InR (bimap (second (map InR)) (second (map InR)) (shapeGuesses g))

class Reducable t where
  reduce :: FreeScoped b t a -> FreeScoped b t a

data Constraint b t a
  = FreeScoped b t a :~: FreeScoped b t a
  | ForAll (Bound.Simple.Scope b (Constraint b t) a)
  deriving (Functor)

type UConstraint b t a v = Constraint b t (UVar b t a v)

class Monad m => MonadFresh v m | m -> v where
  freshMeta :: m v

newtype FreshT v m a = FreshT { unFreshT :: StateT [v] m a }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

instance Monad m => MonadFresh v (FreshT v m) where
  freshMeta = do
    vs <- FreshT get
    case vs of
      v:vs' -> do
        FreshT (put vs')
        return v
      _ -> error "not enough fresh variables"

type Fresh v = FreshT v Identity

runFreshT :: Monad m => FreshT v m a -> [v] -> m a
runFreshT m vs = evalStateT (unFreshT m) vs

runFresh :: Fresh v a -> [v] -> a
runFresh m = runIdentity . runFreshT m

runFreshIntegers :: Fresh Integer a -> a
runFreshIntegers m = runFresh m [0..]

data SimplifyResult a
  = CannotSimplify
  | Simplified [a]
  deriving (Show, Functor, Foldable, Traversable)

type Subst b t a = (a, Bound.Scope Int (FreeScoped b t) a)

isStuck :: HigherOrderUnifiable t => UFreeScoped b t a v -> Bool
isStuck = isStuck' isUMetaVar
  where
    isUMetaVar UMetaVar{} = True
    isUMetaVar _          = False

substituteGuesses
  :: forall a b t m.
    (HigherOrderUnifiable t, MonadFresh a m, MonadPlus m)
  => (a -> Bool)        -- ^ A meta variable?
  -> FreeScoped b t a   -- ^ Term (with meta variables).
  -> m [Subst b t a]
substituteGuesses isMeta term = go (term, [])
  where
    go :: forall x. (FreeScoped b t x, [t () ()]) -> m [Subst b t x]
    go (term, guesses) = case term of
      t@(PureScoped x)
        | isMeta x -> do
            t' <- msum (map (fmap wrapFreeScopedT . bitraverse mkMetaScoped mkMeta) guesses)
            return [(x, Bound.abstract (const Nothing) t')]
        | otherwise -> return []
      FreeScoped t -> do
        t' <- bitraverse goScoped go (shapeGuesses t)
        return (bifold t')

    goScoped (scope, guesses) = go (Bound.fromScope scope, guesses)

    mkMeta _ = PureScoped <$> freshMeta
    mkMetaScoped _ = Bound.abstract (const Nothing) . PureScoped <$> freshMeta

isStuck'
  :: HigherOrderUnifiable t
  => (a -> Bool) -> FreeScoped b t a -> Bool
isStuck' p = \case
  PureScoped x -> p x
  FreeScoped t -> getAny $ bifoldMap
    (Any . isStuckSubScope)
    (Any . isStuckSubTerm)
    (shapeGuesses t)
  where
    isStuckSubTerm (t, guesses)
      = not (null guesses) && isStuck' p t
    isStuckSubScope (s, guesses)
      = not (null guesses) && isStuck' (any p) (Bound.fromScope s)

simplify
  :: ( HigherOrderUnifiable t, Reducable t
     , MonadPlus m, MonadFresh v m
     , Eq a, Eq b, Eq v )
  => (a -> Bool)
  -> Constraint b t a
  -> m (SimplifyResult (Constraint b t a))
simplify isMeta (ForAll c) = fmap (ForAll . Bound.Simple.toScope) <$>
  simplify (any isMeta) (Bound.Simple.fromScope c)
simplify isMeta (t1 :~: t2) =
  case (reduce t1, reduce t2) of

    (t1', t2')
      | isStuck' isMeta t1' || isStuck' isMeta t2' -> return CannotSimplify

    (FreeScoped t1', FreeScoped t2') ->
      case zipMatch t1' t2' of
        Nothing -> mzero
        Just t  -> do
          let go (Left _)         = return []
              go (Right (e1, e2)) = return [e1 :~: e2]

              goScope (Left _)  = return []
              goScope (Right (s1, s2)) = return
                [ForAll $ Bound.Simple.toScope $
                  Bound.fromScope s1 :~: Bound.fromScope s2]
          Simplified . bifold <$> bitraverse goScope go t

    _ -> return CannotSimplify

-- * Example: untyped lambda calculus

data TermF scope term
  = App term term
  | Lam scope
  deriving (Functor, Foldable, Traversable)

type Term b a = FreeScoped b TermF a
type UTerm b a v = UFreeScoped b TermF a v

type Term' = Term String String
type UTerm' = UTerm String String String

-- TH derived instances
deriveBifunctor ''UVarG
deriveBifoldable ''UVarG
deriveBitraversable ''UVarG
