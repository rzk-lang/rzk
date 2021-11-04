{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Rzk.Free.Syntax.FreeScoped.ScopedUnification where

import qualified Bound.Scope                as Bound
import qualified Bound.Scope.Simple         as Bound.Simple
import qualified Bound.Term                 as Bound
import qualified Bound.Var                  as Bound
import           Control.Applicative        (Alternative)
import           Control.Monad.Logic
import           Control.Monad.State
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bifunctor.TH
import           Data.Bitraversable
import           Data.Functor.Identity      (Identity (..))
import           Data.List                  (partition)
import           Data.Maybe                 (listToMaybe)
import           Data.Monoid                (Any (..))
import           Data.Void                  (absurd)

import           Data.Char                  (chr, ord)
import           Data.String                (IsString (..))
import           Data.Text.Prettyprint.Doc  as Doc
import           Rzk.Free.Bound.Name
import qualified Rzk.Syntax.Var             as Rzk

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

instance Reducible (Const a) where
  reduceInL term =
    case term of
      t@PureScoped{}             -> t
      FreeScoped (InL (Const _)) -> term
      _                          -> reduceInR term

class Bifunctor t => Reducible t where
  {-# MINIMAL reduceInL | reduceInR #-}
  reduceInL
    :: Reducible ext
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
    :: Reducible ext
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
    = transFreeScopedT assoc'
    . reduceInL
    . transFreeScopedT assoc
    where
      assoc (InL (InL x)) = InL x
      assoc (InL (InR y)) = InR (InL y)
      assoc (InR z)       = InR (InR z)

      assoc' (InL x)       = InL (InL x)
      assoc' (InR (InL y)) = InL (InR y)
      assoc' (InR (InR z)) = InR z

data Constraint b t a
  = FreeScoped b t a :~: FreeScoped b t a
  | ForAll (Bound.Simple.Scope b (Constraint b t) a)
  deriving (Functor)

substInConstraint
  :: (Eq a, Eq b, Bifunctor t)
  => (a, FreeScoped b t a)
  -> Constraint b t a
  -> Constraint b t a
substInConstraint (x, t) = \case
  t1 :~: t2 -> Bound.substitute x t t1 :~: Bound.substitute x t t2
  ForAll c -> ForAll $ Bound.Simple.toScope $
    substInConstraint (Bound.F x, Bound.F <$> t) (Bound.Simple.fromScope c)

substInConstraintWith
  :: (Bifunctor t)
  => (a -> FreeScoped b t x)
  -> Constraint b t a
  -> Constraint b t x
substInConstraintWith f = \case
  t1 :~: t2 -> (t1 >>= f) :~: (t2 >>= f)
  ForAll c -> ForAll $ Bound.Simple.toScope $
    substInConstraintWith withF (Bound.Simple.fromScope c)
  where
    withF (Bound.B b) = pure (Bound.B b)
    withF (Bound.F x) = Bound.F <$> f x

instantiateC
  :: (Bifunctor t)
  => FreeScoped b t a
  -> Bound.Simple.Scope b (Constraint b t) a
  -> Constraint b t a
instantiateC t = substInConstraintWith withB . Bound.Simple.fromScope
  where
    withB (Bound.B _) = t
    withB (Bound.F x) = pure x

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

instance MonadFresh v m => MonadFresh v (LogicT m) where
  freshMeta = lift freshMeta

type Fresh v = FreshT v Identity

runFreshT :: Monad m => FreshT v m a -> [v] -> m a
runFreshT m vs = evalStateT (unFreshT m) vs

runFresh :: Fresh v a -> [v] -> a
runFresh m = runIdentity . runFreshT m

runFreshIntegers :: Fresh Integer a -> a
runFreshIntegers m = runFresh m [0..]

data MetaAbs b t a = MetaAbs
  { metaAbsArity :: !Int
  , metaAbsBody  :: Bound.Scope Int (FreeScoped b t) a
  }

type Subst b t a = (a, MetaAbs b t a)
type USubst b t v = Subst b (t :+: MetaAppF) v

newtype Substs b t a = Substs { getSubsts :: [Subst b t a] }
  deriving (Semigroup, Monoid)

type USubsts b t v = Substs b (t :+: MetaAppF) v

applyUSubsts
  :: (Eq v, Bifunctor t)
  => USubsts b t v
  -> UFreeScoped b t a v
  -> UFreeScoped b t a v
applyUSubsts substs = \case
  t@(PureScoped UFreeVar{}) -> t
  t@(PureScoped (UMetaVar v)) ->
    case lookup v (getSubsts substs) of
      Just (MetaAbs _arity s) ->
        Bound.instantiate (error "impossible: no arguments") (UMetaVar <$> s)
      Nothing -> t
  t@(FreeScoped (InR (MetaAppF (PureScoped (UMetaVar v)) args))) ->
    case lookup v (getSubsts substs) of
      Just (MetaAbs _arity s) ->
        Bound.instantiate (args !!) (UMetaVar <$> s)
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
  -> m (USubsts b t v)
substituteGuesses defaultBoundVar = fmap Substs . \case
  PureScoped _ -> pure []
  FreeScoped (InL t) -> bifold <$> bitraverse goScoped go (shapeGuesses t)
  FreeScoped (InR _) -> pure []
  where
    go :: forall x.
      (FreeScoped b (t :+: MetaAppF) (UVar x v), [t () ()])
      -> m [USubst b t v]
    go = \case
      (FreeScoped (InL t), _guesses) ->
        bifold <$> bitraverse goScoped go (shapeGuesses t)
      (PureScoped (UMetaVar v), guesses) ->
        go (FreeScoped (InR (MetaAppF (PureScoped (UMetaVar v)) [])), guesses)
      (_t, []) -> pure []
      (FreeScoped (InR (MetaAppF (PureScoped (UMetaVar v)) args)), guesses) -> do
        msum $ flip map guesses $ \guess -> do
          guess' <- bitraverse
            (const $ Bound.toScope . FreeScoped . InR . flip MetaAppF (pure (Bound.B defaultBoundVar) : (zipWith const (pure . Bound.F . UFreeVar <$> [0..]) args)) . pure . Bound.F . UMetaVar <$> freshMeta)
            (const $ FreeScoped . InR . flip MetaAppF (zipWith const (pure . UFreeVar <$> [0..]) args) . pure . UMetaVar <$> freshMeta)
            guess
          let arity = length args
          return [(v, MetaAbs arity $ Bound.abstractEither (\case {UFreeVar i -> Left i; UMetaVar v' -> Right v'}) (FreeScoped (InL guess')))]
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
  -> m (Maybe (UFreeScoped b t a v, USubsts b t v))
guessAndSubstitute defaultBoundVar term = do
  substs <- substituteGuesses defaultBoundVar term
  return $ if null (getSubsts substs)
    then Nothing
    else Just (applyUSubsts substs term, substs)

reduceAndGuess
  :: (Eq v, HigherOrderUnifiable t, Reducible t, MonadFresh v m, MonadPlus m)
  => b -- ^ Default bound variable.
  -> UFreeScoped b t a v
  -> m (UFreeScoped b t a v, USubsts b t v)
reduceAndGuess defaultBoundVar t = guessAndSubstitute defaultBoundVar t' >>= \case
    Nothing  -> return (t', Substs [])
    Just (t'', substs) -> do
      (t''', moreSubsts) <- reduceAndGuess defaultBoundVar t''
      return (t''', substs <> moreSubsts)
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
  = AppF term term
  | LamF scope
  deriving (Functor, Foldable, Traversable)

instance Unifiable TermF where
  zipMatch (AppF f1 x1) (AppF f2 x2)
    = Just (AppF (Right (f1, f2)) (Right (x1, x2)))
  zipMatch (LamF body1) (LamF body2)
    = Just (LamF (Right (body1, body2)))
  zipMatch _ _ = Nothing

instance HigherOrderUnifiable TermF where
  shapeGuesses (AppF f x)  = AppF (f, [LamF ()]) (x, [])
  shapeGuesses (LamF body) = LamF (body, [])

instance Reducible TermF where
  reduceInL = \case
    t@VarE{} -> t

    AppE f x ->
      case reduce f of
        LamE body -> reduce (Bound.instantiate1 x body)
        f'        -> AppE f' x
    t@LamE{} -> t

    t -> reduceInR t

type TermE ext b = FreeScoped b (TermF :+: ext)
type ScopedTermE ext b a = Bound.Scope b (TermE ext b) a

type Term b = FreeScoped (Name b ()) TermF
type ScopedTerm b a = Bound.Scope (Name b ()) (Term b) a
type UTerm b a v = UFreeScoped (Name b ()) TermF a v
type ScopedUTerm b a v
  = Bound.Scope (Name b ()) (FreeScoped (Name b ()) (TermF :+: MetaAppF)) (UVar a v)

type Term'  = Term  Rzk.Var Rzk.Var
type UTerm' = UTerm Rzk.Var Rzk.Var Rzk.Var

type Constraint' = UConstraint (Name Rzk.Var ()) TermF Rzk.Var Rzk.Var
type UConstraint' = UConstraint (Name Rzk.Var ()) (TermF :+: MetaAppF) Rzk.Var Rzk.Var

-- ** Testing

type UnifyM' = LogicT (Fresh Rzk.Var)

-- |
-- >>> t1 = (AppE (AppE (pure (UMetaVar "f")) (pure (UFreeVar "x"))) (pure (UFreeVar "z"))) :: UTerm'
-- >>> runUnifyM' $ reduceAndGuess (Rzk.Name Nothing ()) t1
-- [(?M₂[z,x],[(f,λx₁ → ?M₁[x₁]),(M₁,λx₁. λx₂ → ?M₂[x₂,x₁])])]
--
-- >>> t = lamU "z" (AppE (VarE (UMetaVar "f")) (VarE (UFreeVar "x"))) :: UTerm'
-- >>> t
-- λx₁ → ?f x
-- >>> runUnifyM' $ reduceAndGuess (Rzk.Name Nothing ()) t
-- [(λx₁ → (λx₂ → ?M₁[x₂]) x,[(f,λx₁ → ?M₁[x₁])])]
runUnifyM' :: UnifyM' a -> [a]
runUnifyM' m = runFresh (observeAllT m) defaultFreshMetaVars
  where
    defaultFreshMetaVars = [ fromString ("M" <> toIndex i) | i <- [1..] ]

    toIndex n = index
      where
        digitToSub c = chr ((ord c - ord '0') + ord '₀')
        index = map digitToSub (show n)

-- >>> t1 = lamU "x" (lamU "y" (AppE (AppE (VarE (UMetaVar "f")) (VarE (UFreeVar "x"))) (VarE (UFreeVar "y"))))
-- >>> t2 = lamU "x" (lamU "y" (AppE (VarE (UFreeVar "y")) (AppE (VarE (UFreeVar "x")) (VarE (UFreeVar "y"))))) :: UTerm'
-- >>> t1
-- λx₁ → λx₂ → ?f x₁ x₂
-- >>> t2
-- λx₁ → λx₂ → x₂ (x₁ x₂)
-- >>> >>> unifyUTerms' t1 t2
-- *** Exception: not implemented yet
unifyUTerms' :: UTerm' -> UTerm' -> Maybe [USubst (Name Rzk.Var ()) TermF Rzk.Var]
unifyUTerms' t1 t2 = listToMaybe . runUnifyM' $ do
  (flexflex, substs) <- unify [] [t1 :~: t2]
  return substs

-- ** Simple pattern synonyms

-- | A variable.
pattern Var :: a -> Term b a
pattern Var x = PureScoped x

-- | A \(\lambda\)-abstraction.
pattern Lam :: ScopedTerm b a -> Term b a
pattern Lam body = FreeScoped (LamF body)

-- | An application of one term to another.
pattern App :: Term b a -> Term b a -> Term b a
pattern App t1 t2 = FreeScoped (AppF t1 t2)

{-# COMPLETE Var, Lam, App #-}

-- | A variable.
pattern VarE :: a -> TermE ext b a
pattern VarE x = PureScoped x

-- | A \(\lambda\)-abstraction.
pattern LamE :: ScopedTermE ext b a -> TermE ext b a
pattern LamE body = FreeScoped (InL (LamF body))

-- | An application of one term to another.
pattern AppE :: TermE ext b a -> TermE ext b a -> TermE ext b a
pattern AppE t1 t2 = FreeScoped (InL (AppF t1 t2))

pattern ExtE :: ext (ScopedTermE ext b a) (TermE ext b a) -> TermE ext b a
pattern ExtE ext = FreeScoped (InR ext)

{-# COMPLETE VarE, LamE, AppE, ExtE #-}

-- | Abstract over one variable in a term.
--
-- >>> lam "x" (App (Var "f") (Var "x")) :: Term String String
-- λx₁ → f x₁
-- >>> lam "f" (App (Var "f") (Var "x")) :: Term String String
-- λx₁ → x₁ x
lam :: Eq a => a -> Term a a -> Term a a
lam x body = Lam (abstract1Name x body)

-- | Abstract over one variable in a term with metavariables.
--
-- >>> lamU "x" (AppE (VarE (UFreeVar "f")) (VarE (UFreeVar "x"))) :: UTerm'
-- λx₁ → f x₁
-- >>> lamU "x" (AppE (VarE (UMetaVar "f")) (VarE (UFreeVar "x"))) :: UTerm'
-- λx₁ → ?f x₁
-- >>> lamU "x" (AppE (VarE (UMetaVar "f")) (VarE (UMetaVar "x"))) :: UTerm'
-- λx₁ → ?f ?x
lamU :: Eq a => a -> UTerm a a v -> UTerm a a v
lamU x body = LamE (abstractName' f body)
  where
    f (UFreeVar y) | x == y = Just (Name (Just x) ())
    f _            = Nothing


-- * Pretty-printing

instance Pretty Rzk.Var where
  pretty (Rzk.Var x) = pretty x

-- | Uses 'Pretty' instance.
instance (Pretty a, Pretty b, IsString a) => Show (Term b a) where
  show = show . pretty

-- | Uses default names (@x@ with a positive integer subscript) for bound variables:
instance (Pretty a, Pretty b, IsString a) => Pretty (Term b a) where
  pretty = ppTerm defaultFreshVars
    where
      defaultFreshVars = [ fromString ("x" <> toIndex i) | i <- [1..] ]

      toIndex n = index
        where
          digitToSub c = chr ((ord c - ord '0') + ord '₀')
          index = map digitToSub (show n)

-- | Pretty-print an untyped term.
ppTerm :: (Pretty a, Pretty b) => [a] -> Term b a -> Doc ann
ppTerm vars = \case
  Var x -> pretty x
  App f x -> ppTermFun vars f <+> ppTermArg vars x
  Lam body -> ppScopedTerm vars body $ \x body' ->
    "λ" <> pretty x <+> "→" <+> body'

-- | Pretty-print an untyped in a head position.
ppTermFun :: (Pretty a, Pretty b) => [a] -> Term b a -> Doc ann
ppTermFun vars = \case
  t@Var{} -> ppTerm vars t
  t@App{} -> ppTerm vars t

  t@Lam{} -> Doc.parens (ppTerm vars t)

-- | Pretty-print an untyped in an argument position.
ppTermArg :: (Pretty a, Pretty b) => [a] -> Term b a -> Doc ann
ppTermArg vars = \case
  t@Var{} -> ppTerm vars t

  t@App{} -> Doc.parens (ppTerm vars t)
  t@Lam{} -> Doc.parens (ppTerm vars t)

ppScopedTerm
  :: (Pretty a, Pretty b)
  => [a] -> ScopedTerm b a -> (a -> Doc ann -> Doc ann) -> Doc ann
ppScopedTerm [] _ _            = error "not enough fresh names"
ppScopedTerm (x:xs) t withScope = withScope x (ppTerm xs (Bound.instantiate1 (Var x) t))

ppConstraint :: [Rzk.Var] -> Constraint' -> Doc ann
ppConstraint (x:xs) = \case
  ForAll c -> "forall" <+> pretty x <> dot
    <+> ppConstraint xs (instantiateC (pure (UFreeVar x)) c)

-- ** Pretty-printing terms and constraints with meta variables

instance (Pretty a, Pretty v) => Pretty (UVar a v) where
  pretty (UFreeVar x) = pretty x
  pretty (UMetaVar v) = "?" <> pretty v

instance (Pretty b, Pretty v, Show v) => Show (Substs (Name b ()) (TermF :+: MetaAppF) v) where
  show (Substs substs) = show substs

instance (Pretty b, Pretty v) => Show (MetaAbs (Name b ()) (TermF :+: MetaAppF) v) where
  show = show . pretty

instance (Pretty b, Pretty v) => Pretty (MetaAbs (Name b ()) (TermF :+: MetaAppF) v) where
  pretty = ppMetaAbs defaultFreshVars
    where
      defaultFreshVars = [ fromString ("x" <> toIndex i) | i <- [1..] ]

      toIndex n = index
        where
          digitToSub c = chr ((ord c - ord '0') + ord '₀')
          index = map digitToSub (show n)


-- | Uses 'Pretty' instance.
instance (Pretty a, Pretty b, Pretty v, IsString a) => Show (UTerm b a v) where
  show = show . pretty

-- | Uses default names (@x@ with a positive integer subscript) for bound variables:
instance (Pretty a, Pretty b, Pretty v, IsString a) => Pretty (UTerm b a v) where
  pretty = ppUTerm defaultFreshVars
    where
      defaultFreshVars = [ fromString ("x" <> toIndex i) | i <- [1..] ]

      toIndex n = index
        where
          digitToSub c = chr ((ord c - ord '0') + ord '₀')
          index = map digitToSub (show n)

-- | Pretty-print an untyped term.
ppUTerm :: (Pretty a, Pretty b, Pretty v) => [a] -> UTerm b a v -> Doc ann
ppUTerm vars = \case
  VarE x -> pretty x
  AppE f x -> ppUTermFun vars f <+> ppUTermArg vars x
  LamE body -> ppScopedUTerm vars body $ \x body' ->
    "λ" <> pretty x <+> "→" <+> body'
  ExtE (MetaAppF m args) -> ppUTermFun vars m <>
    encloseSep "[" "]" comma (ppUTerm vars <$> args)

-- | Pretty-print an untyped in a head position.
ppUTermFun :: (Pretty a, Pretty b, Pretty v) => [a] -> UTerm b a v -> Doc ann
ppUTermFun vars = \case
  t@VarE{} -> ppUTerm vars t
  t@AppE{} -> ppUTerm vars t
  t@ExtE{} -> ppUTerm vars t

  t@LamE{} -> Doc.parens (ppUTerm vars t)

-- | Pretty-print an untyped in an argument position.
ppUTermArg :: (Pretty a, Pretty b, Pretty v) => [a] -> UTerm b a v -> Doc ann
ppUTermArg vars = \case
  t@VarE{} -> ppUTerm vars t
  t@ExtE{} -> ppUTerm vars t

  t@AppE{} -> Doc.parens (ppUTerm vars t)
  t@LamE{} -> Doc.parens (ppUTerm vars t)

ppScopedUTerm
  :: (Pretty a, Pretty b, Pretty v)
  => [a] -> ScopedUTerm b a v -> (a -> Doc ann -> Doc ann) -> Doc ann
ppScopedUTerm [] _ _             = error "not enough fresh names"
ppScopedUTerm (x:xs) t withScope = withScope x $
  ppUTerm xs (Bound.instantiate1 (VarE (UFreeVar x)) t)

ppUConstraint :: [Rzk.Var] -> UConstraint' -> Doc ann
ppUConstraint (x:xs) = \case
  ForAll c -> "forall" <+> pretty x <> dot
    <+> ppUConstraint xs (instantiateC (pure (UFreeVar x)) c)

ppMetaAbs :: (Pretty b, Pretty v) => [Rzk.Var] -> MetaAbs (Name b ()) (TermF :+: MetaAppF) v -> Doc ann
ppMetaAbs vars (MetaAbs arity body) =
  (if null args then ("" <>) else (("λ" <> hsep (map pretty args) <> ".") <+>))
  (ppUTerm vars' (Bound.instantiate ((VarE . UFreeVar <$> args) !!) (UMetaVar <$> body)))
  where
    (args, vars') = splitAt arity vars

-- TH derived instances
deriveBifunctor ''UVar
deriveBifoldable ''UVar
deriveBitraversable ''UVar

deriveBifunctor ''MetaAppF
deriveBifoldable ''MetaAppF
deriveBitraversable ''MetaAppF

deriveBifunctor ''TermF
deriveBifoldable ''TermF
deriveBitraversable ''TermF
