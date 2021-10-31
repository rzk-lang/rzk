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
import qualified Bound.Var                  as Bound
import           Control.Applicative        (Alternative)
import           Control.Monad.State
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bifunctor.TH
import           Data.Bitraversable
import           Data.Functor.Identity      (Identity (..))
import           Data.List                  (partition)
import           Data.Monoid                (Any (..))
import           Data.Void                  (absurd)

import           Rzk.Free.Syntax.FreeScoped

-- | A variable in a unifiable term.
data UVar a v
  = UFreeVar a
  -- ^ A free variable.
  | UMetaVar v
  -- ^ A meta variable.
  deriving (Functor)

data MetaAppF scope term
  = MetaAppF term [term]

instance Unifiable MetaAppF where
  zipMatch (MetaAppF x xs) (MetaAppF y ys)
    | length xs /= length ys = Nothing
    | otherwise = Just
        (MetaAppF (Right (x, y)) (Right <$> zip xs ys))

instance HigherOrderUnifiable MetaAppF where
  shapeGuesses (MetaAppF x xs) = MetaAppF (x, []) (zip xs (repeat []))

instance Reducible MetaAppF where
  reduceInL = id
  reduceInR = id
  reduce    = id

-- | A free scoped monad for higher-order unification.
type UFreeScoped b t a v =
  FreeScoped b (t :+: MetaAppF) (UVar a v)

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

class Bifunctor t => Reducible t where
  {-# MINIMAL reduceInL | reduceInR #-}
  reduceInL
    :: Bifunctor ext
    => FreeScoped b (t :+: ext) a
    -> FreeScoped b (t :+: ext) a
  reduceInL = transFreeScopedT swap . reduceInR . transFreeScopedT swap
    where
      swap (InL x) = InR x
      swap (InR y) = InL y

  reduce :: FreeScoped b t a -> FreeScoped b t a
  reduce = transFreeScopedT extract . reduceInL . transFreeScopedT InL
    where
      extract (InL x)         = x
      extract (InR (Const y)) = absurd y

  reduceInR
    :: Bifunctor ext
    => FreeScoped b (ext :+: t) a
    -> FreeScoped b (ext :+: t) a
  reduceInR = transFreeScopedT swap . reduceInL . transFreeScopedT swap
    where
      swap (InL x) = InR x
      swap (InR y) = InL y

instance (Reducible f, Reducible g) => Reducible (f :+: g) where
  -- FIXME: recursively reduce until we cannot reduce anymore?
  -- or maybe ask for step-by-step reduction to improve
  reduceInL
    = transFreeScopedT (swap . assoc . swap)
    . reduceInL
    . transFreeScopedT (assoc . swap)
    . reduceInL
    . transFreeScopedT assoc
    where
      assoc (InL (InL x)) = InL x
      assoc (InL (InR y)) = InR (InL y)
      assoc (InR z)       = InR (InR z)

      swap (InL x) = InR x
      swap (InR y) = InL y

data Constraint b t a
  = FreeScoped b t a :~: FreeScoped b t a
  | ForAll (Bound.Simple.Scope b (Constraint b t) a)
  deriving (Functor)

type UConstraint b t a v = Constraint b (t :+: MetaAppF) (UVar a v)

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

type Subst b t a = (a, Bound.Scope Int (FreeScoped b t) a)
type USubst b t v = Subst b (t :+: MetaAppF) v

applyUSubsts
  :: (Eq v, Bifunctor t)
  => [USubst b t v]
  -> UFreeScoped b t a v
  -> UFreeScoped b t a v
applyUSubsts substs = \case
  t@(PureScoped UFreeVar{}) -> t
  t@(PureScoped (UMetaVar v)) ->
    case lookup v substs of
      Just s -> Bound.instantiate (error "impossible: no arguments") (UMetaVar <$> s)
      Nothing -> t
  FreeScoped t -> FreeScoped (bimap (Bound.toScope . fmap dist' . applyUSubsts substs . fmap dist . Bound.fromScope) (applyUSubsts substs) t)
  where
    dist (Bound.B b)            = UFreeVar (Bound.B b)
    dist (Bound.F (UFreeVar x)) = UFreeVar (Bound.F x)
    dist (Bound.F (UMetaVar v)) = UMetaVar v

    dist' (UFreeVar (Bound.B b)) = Bound.B b
    dist' (UFreeVar (Bound.F x)) = Bound.F (UFreeVar x)
    dist' (UMetaVar v)           = Bound.F (UMetaVar v)

isStuck :: HigherOrderUnifiable t => UFreeScoped b t a v -> Bool
isStuck = isStuck' isUMetaVar
  where
    isUMetaVar UMetaVar{} = True
    isUMetaVar _          = False

substituteGuesses
  :: forall a b v t m.
    (HigherOrderUnifiable t, MonadFresh v m, MonadPlus m)
  => b -- default scope var
  -> UFreeScoped b t a v   -- ^ Term (with meta variables).
  -> m [USubst b t v]
substituteGuesses defaultBoundVar = \case
  PureScoped _ -> pure []
  FreeScoped (InL t) -> bifold <$> bitraverse goScoped go (shapeGuesses t)
  FreeScoped (InR _) -> pure []
  where
    go :: forall x.
      (FreeScoped b (t :+: MetaAppF) (UVar x v), [t () ()])
      -> m [USubst b t v]
    go = \case
      (_t, []) -> pure []
      (FreeScoped (InR (MetaAppF (PureScoped (UMetaVar v)) args)), guesses) -> do
        msum $ flip map guesses $ \guess -> do
          guess' <- bitraverse
            (const $ Bound.toScope . FreeScoped . InR . flip MetaAppF (pure (Bound.B defaultBoundVar) : (zipWith const (pure . Bound.F . UFreeVar <$> [0..]) args)) . pure . Bound.F . UMetaVar <$> freshMeta)
            (const $ FreeScoped . InR . flip MetaAppF (zipWith const (pure . UFreeVar <$> [0..]) args) . pure . UMetaVar <$> freshMeta)
            guess
          return [(v, Bound.abstractEither (\case {UFreeVar i -> Left i; UMetaVar v' -> Right v'}) (FreeScoped (InL guess')))]
      (_, _guesses) -> pure [] -- FIXME: go recursively?

    -- TODO: allow inner meta variables to access all bound variables by default?
    goScoped
      :: forall x.
        (Bound.Scope b (FreeScoped b (t :+: MetaAppF)) (UVar x v), [t () ()])
      -> m [USubst b t v]
    goScoped (s, guesses) = go (dist <$> Bound.fromScope s, guesses)
      where
        dist (Bound.B b)            = UFreeVar (Bound.B b)
        dist (Bound.F (UFreeVar x)) = UFreeVar (Bound.F x)
        dist (Bound.F (UMetaVar v)) = UMetaVar v

guessAndSubstitute
  :: (Eq v, HigherOrderUnifiable t, MonadFresh v m, MonadPlus m)
  => b -- ^ Default bound variable.
  -> UFreeScoped b t a v
  -> m (Maybe (UFreeScoped b t a v, [USubst b t v]))
guessAndSubstitute defaultBoundVar term = do
  substs <- substituteGuesses defaultBoundVar term
  return $ if null substs
    then Nothing
    else Just (applyUSubsts substs term, substs)

reduceAndGuess
  :: (Eq v, HigherOrderUnifiable t, Reducible t, MonadFresh v m, MonadPlus m)
  => b -- ^ Default bound variable.
  -> UFreeScoped b t a v
  -> m (UFreeScoped b t a v, [USubst b t v])
reduceAndGuess defaultBoundVar t = guessAndSubstitute defaultBoundVar t' >>= \case
    Nothing  -> return (t', [])
    Just (t'', substs) -> do
      (t''', moreSubsts) <- reduceAndGuess defaultBoundVar t''
      return (t''', substs ++ moreSubsts)
  where
    t' = reduce t

data SimplifyResult a
  = CannotSimplify
  | Simplified [a]
  deriving (Show, Functor, Foldable, Traversable)

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

isStuckU
  :: HigherOrderUnifiable t
  => UFreeScoped b t a v -> Bool
isStuckU = \case
  PureScoped UMetaVar{} -> True
  PureScoped UFreeVar{} -> False
  FreeScoped t -> getAny $ bifoldMap
    (Any . isStuckSubScope)
    (Any . isStuckSubTerm)
    (shapeGuesses t)
  where
    isStuckSubTerm (t, guesses)
      = not (null guesses) && isStuckU t

    isStuckSubScope (s, guesses)
      = not (null guesses) && isStuckU (dist <$> Bound.fromScope s)

    dist (Bound.B b)            = UFreeVar (Bound.B b)
    dist (Bound.F (UFreeVar x)) = UFreeVar (Bound.F x)
    dist (Bound.F (UMetaVar v)) = UMetaVar v

simplify
  :: ( HigherOrderUnifiable t, Reducible t
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

simplifyU
  :: ( HigherOrderUnifiable t, Reducible t
     , MonadPlus m, MonadFresh v m
     , Eq a, Eq b, Eq v )
  => UConstraint b t a v
  -> m (SimplifyResult (UConstraint b t a v))
simplifyU (ForAll c) = fmap (ForAll . Bound.Simple.toScope . fmap dist') <$>
  simplifyU (dist <$> Bound.Simple.fromScope c)
  where
    dist (Bound.B b)            = UFreeVar (Bound.B b)
    dist (Bound.F (UFreeVar x)) = UFreeVar (Bound.F x)
    dist (Bound.F (UMetaVar v)) = UMetaVar v

    dist' (UFreeVar (Bound.B b)) = Bound.B b
    dist' (UFreeVar (Bound.F x)) = Bound.F (UFreeVar x)
    dist' (UMetaVar v)           = Bound.F (UMetaVar v)

simplifyU (t1 :~: t2) =
  -- FIXME: reduceAndGuess
  case (reduce t1, reduce t2) of

    (t1', t2')
      | isStuckU t1' || isStuckU t2' -> return CannotSimplify

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

repeatedlySimplifyU
  :: ( HigherOrderUnifiable t, Reducible t
     , MonadPlus m, MonadFresh v m
     , Eq a, Eq b, Eq v )
  => [UConstraint b t a v]
  -> m [UConstraint b t a v]
repeatedlySimplifyU = go
  where
    go [] = return []
    go (c:cs) = do
      simplifyU c >>= \case
        CannotSimplify -> do
          cs' <- go cs
          return (c:cs')
        Simplified c' -> do
          go (c' <> cs)

tryFlexRigid
  :: ( HigherOrderUnifiable t, Reducible t
     , MonadPlus m, MonadFresh v m
     , Eq a, Eq b, Eq v )
  => UConstraint b t a v
  -> [m [[USubst b t v]]]
tryFlexRigid (ForAll c)  = tryFlexRigid (dist <$> Bound.Simple.fromScope c)
  where
    dist (Bound.B b)            = UFreeVar (Bound.B b)
    dist (Bound.F (UFreeVar x)) = UFreeVar (Bound.F x)
    dist (Bound.F (UMetaVar v)) = UMetaVar v
tryFlexRigid (t1 :~: t2)
  | isStuckU t1 = error "not implemented yet"
  | otherwise = error "not implemented yet"

unify
  :: ( HigherOrderUnifiable t, Reducible t
     , MonadPlus m, MonadFresh v m
     , Eq a, Eq b, Eq v )
  => [USubst b t v]
  -> [UConstraint b t a v]
  -> m ([UConstraint b t a v], [USubst b t v])
unify substs constraints = do
  constraints' <- repeatedlySimplifyU constraints
  let (flexflex, flexrigid) = partition isFlexFlex constraints'
  case flexrigid of
    [] -> return (flexflex, substs)
    fr : _ -> do
      let psubsts = tryFlexRigid fr
      trySubsts psubsts (flexrigid <> flexflex)
  where
    trySubsts [] _cs = mzero
    trySubsts (mss : psubsts) cs'' = do
      ss <- mss
      let these = foldr mplus mzero [unify newS cs'' | newS <- ss]
      let those = trySubsts psubsts cs''
      these `mplus` those

isFlexFlex
  :: (HigherOrderUnifiable t)
  => UConstraint b t a v
  -> Bool
isFlexFlex (t1 :~: t2) = isStuckU t1 && isStuckU t2
isFlexFlex (ForAll c)  = isFlexFlex (dist <$> Bound.Simple.fromScope c)
  where
    dist (Bound.B b)            = UFreeVar (Bound.B b)
    dist (Bound.F (UFreeVar x)) = UFreeVar (Bound.F x)
    dist (Bound.F (UMetaVar v)) = UMetaVar v

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
deriveBifunctor ''UVar
deriveBifoldable ''UVar
deriveBitraversable ''UVar

deriveBifunctor ''MetaAppF
deriveBifoldable ''MetaAppF
deriveBitraversable ''MetaAppF
