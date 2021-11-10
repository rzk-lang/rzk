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
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Rzk.Free.Syntax.FreeScoped.ScopedUnification where

import qualified Bound.Scope                as Bound
import qualified Bound.Scope.Simple         as Bound.Simple
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
import           Data.Void

import           Data.Char                  (chr, ord)
import           Data.String                (IsString (..))
import           Data.Text.Prettyprint.Doc  as Doc
import           Rzk.Free.Bound.Name
import qualified Rzk.Syntax.Var             as Rzk

import           Rzk.Free.Syntax.FreeScoped

data MetaAppF v scope term
  = MetaAppF v [term]
  deriving (Functor, Foldable, Traversable)

deriveBifunctor ''MetaAppF
deriveBifoldable ''MetaAppF
deriveBitraversable ''MetaAppF

instance Unifiable (MetaAppF v) where
  zipMatch _ _ = Nothing

instance HigherOrderUnifiable (MetaAppF v) where
  shapeGuesses (MetaAppF x xs) = MetaAppF x (zip xs (repeat []))

instance Reducible (MetaAppF v) where
  reduceInL = id
  reduceInR = id
  reduce    = id

-- | A free scoped monad for higher-order unification.
type UFreeScoped b t a v =
  FreeScoped b (t :+: MetaAppF v) a

class Bitraversable t => Unifiable t where
  zipMatch
    :: t scope term
    -> t scope term
    -> Maybe (t (Either scope (scope, scope)) (Either term (term, term)))

instance (Unifiable f, Unifiable g) => Unifiable (f :+: g) where
  zipMatch (InL f1) (InL f2) = fmap InL (zipMatch f1 f2)
  zipMatch (InR g1) (InR g2) = fmap InR (zipMatch g1 g2)
  zipMatch _ _               = Nothing

data IsHead
  = HasHead
  | NoHead
  deriving (Eq, Show)

class Unifiable t => HigherOrderUnifiable t where
  -- | Assign a list of valid shape guesses to each subterm (and subscope).
  --
  --     bimap fst fst . shapeGuesses = id
  shapeGuesses
    :: t scope term
    -> t (scope, [t () ()]) (term, [t () ()])

  shapes :: [t IsHead IsHead]
  shapes = []

extractHeads :: HigherOrderUnifiable t => FreeScoped b t a -> [FreeScoped b t a]
extractHeads = \case
  t@PureScoped{} -> [t]
  t@(FreeScoped f) ->
    case bifoldMap goScoped go (shapeGuesses f) of
      []    -> [t]
      heads -> heads
  where
    go (_, [])  = []
    go (t, _:_) = extractHeads t

    goScoped (_, _) = []   -- FIXME: correct?

extractHead :: HigherOrderUnifiable t => FreeScoped b t a -> FreeScoped b t a
extractHead = head . extractHeads -- FIXME: use NonEmpty to avoid head

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

type UConstraint b t a v = Constraint b (t :+: MetaAppF v) a

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

data MetaAbs b t a = MetaAbs
  { metaAbsArity :: !Int
  , metaAbsBody  :: Bound.Scope Int (FreeScoped b t) a
  } deriving (Functor)

type USubst b t a v = (v, MetaAbs b (t :+: MetaAppF v) a)
type USubstClosed b t v = USubst b t Void v

newtype Substs b t v a = Substs { getSubsts :: [USubst b t a v] }
  deriving (Semigroup, Monoid, Functor)

type USubsts b t a v = Substs b t v a
type USubstsClosed b t v = Substs b t v Void

applyUSubsts
  :: (Eq v, Bifunctor t)
  => USubsts b t a v
  -> UFreeScoped b t a v
  -> UFreeScoped b t a v
applyUSubsts substs = \case
  t@PureScoped{} -> t
  t@(FreeScoped (InR (MetaAppF v args))) ->
    case lookup v (getSubsts substs) of
      Just (MetaAbs _arity s) ->
        Bound.instantiate (args !!) s
      Nothing -> t
  FreeScoped t -> FreeScoped (bimap (Bound.toScope . applyUSubsts (Bound.F <$> substs) . Bound.fromScope) (applyUSubsts substs) t)

applyUSubstsClosed
  :: (Eq v, Bifunctor t)
  => USubstsClosed b t v
  -> UFreeScoped b t a v
  -> UFreeScoped b t a v
applyUSubstsClosed = applyUSubsts . vacuous

substituteGuesses
  :: forall a b v t m.
    (HigherOrderUnifiable t, MonadFresh v m, MonadPlus m)
  => b -- default scope var
  -> UFreeScoped b t a v   -- ^ Term (with meta variables).
  -> m (USubstsClosed b t v)
substituteGuesses defaultBoundVar = fmap Substs . \case
  PureScoped _ -> pure []
  FreeScoped (InL t) -> bifold <$> bitraverse goScoped go (shapeGuesses t)
  FreeScoped (InR _) -> pure []
  where
    go :: forall x.
      (FreeScoped b (t :+: MetaAppF v) x, [t () ()])
      -> m [USubstClosed b t v]
    go = \case
      (FreeScoped (InL t), _guesses) ->
        bifold <$> bitraverse goScoped go (shapeGuesses t)
      (_t, []) -> pure []
      (FreeScoped (InR (MetaAppF v args)), guesses) -> do
        msum $ flip map guesses $ \guess -> do
          guess' <- bitraverse
            (const $ Bound.toScope . FreeScoped . InR . flip MetaAppF (pure (Bound.B defaultBoundVar) : (zipWith const (pure . Bound.F <$> [0..]) args)) <$> freshMeta)
            (const $ FreeScoped . InR . flip MetaAppF (zipWith const (pure <$> [0..]) args) <$> freshMeta)
            guess
          let arity = length args
          return [(v, MetaAbs arity $ Bound.abstractEither Left (FreeScoped (InL guess')))]
      (_, _guesses) -> pure []

    -- TODO: allow inner meta variables to access all bound variables by default?
    goScoped
      :: forall x.
        (Bound.Scope b (FreeScoped b (t :+: MetaAppF v)) x, [t () ()])
      -> m [USubstClosed b t v]
    goScoped (s, guesses) = go (Bound.fromScope s, guesses)

guessAndSubstitute
  :: (Eq v, HigherOrderUnifiable t, MonadFresh v m, MonadPlus m)
  => b -- ^ Default bound variable.
  -> UFreeScoped b t a v
  -> m (Maybe (UFreeScoped b t a v, USubstsClosed b t v))
guessAndSubstitute defaultBoundVar term = do
  substs <- substituteGuesses defaultBoundVar term
  return $ if null (getSubsts substs)
    then Nothing
    else Just (applyUSubstsClosed substs term, substs)

reduceAndGuess
  :: (Eq v, HigherOrderUnifiable t, Reducible t, MonadFresh v m, MonadPlus m)
  => b -- ^ Default bound variable.
  -> UFreeScoped b t a v
  -> m (UFreeScoped b t a v, USubstsClosed b t v)
reduceAndGuess defaultBoundVar t = guessAndSubstitute defaultBoundVar t' >>= \case
    Nothing  -> return (t', Substs [])
    Just (t'', substs) -> do
      (t''', moreSubsts) <- reduceAndGuess defaultBoundVar t''
      return (t''', substs <> moreSubsts)
  where
    t' = reduce t

data SimplifyResult a
  = CannotSimplify a
  | Simplified [a]
  deriving (Show, Functor, Foldable, Traversable)

isStuckU
  :: HigherOrderUnifiable t
  => UFreeScoped b t a v -> Bool
isStuckU = \case
  PureScoped{} -> False
  FreeScoped (InR (MetaAppF _ _)) -> True
  FreeScoped t -> getAny $ bifoldMap
    (Any . isStuckSubScope)
    (Any . isStuckSubTerm)
    (shapeGuesses t)
  where
    isStuckSubTerm (t, guesses)
      = not (null guesses) && isStuckU t

    isStuckSubScope (s, guesses)
      = not (null guesses) && isStuckU (Bound.fromScope s)

simplifyU
  :: ( HigherOrderUnifiable t, Reducible t
     , MonadPlus m, MonadFresh v m
     , Eq a, Eq b, Eq v )
  => b
  -> UConstraint b t a v
  -> m (SimplifyResult (UConstraint b t a v), USubstsClosed b t v)
simplifyU defaultBoundVar (ForAll c) =
  first (fmap (ForAll . Bound.Simple.toScope)) <$>
    simplifyU defaultBoundVar (Bound.Simple.fromScope c)

simplifyU defaultBoundVar (t1 :~: t2) = do
  (r1, substs1) <- reduceAndGuess defaultBoundVar t1
  (r2, substs2) <- reduceAndGuess defaultBoundVar (applyUSubstsClosed substs1 t2)
  flip (,) (applyUSubstsClosedS substs2 substs1 <> substs2) <$>
    case (r1, r2) of

      (t1', t2')
        | isStuckU t1' || isStuckU t2' -> return (CannotSimplify (t1' :~: t2'))

      (PureScoped x, PureScoped y)
        | x == y -> return (Simplified [])

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

      (t1', t2') -> return (CannotSimplify (t1' :~: t2'))

repeatedlySimplifyU
  :: ( HigherOrderUnifiable t, Reducible t
     , MonadPlus m, MonadFresh v m
     , Eq a, Eq b, Eq v )
  => b
  -> [UConstraint b t a v]
  -> m ([UConstraint b t a v], USubstsClosed b t v)
repeatedlySimplifyU defaultBoundVar = go
  where
    go [] = return ([], mempty)
    go (c:cs) = do
      simplifyU defaultBoundVar c >>= \case
        (CannotSimplify c', substs) -> do
          (cs', moreSubsts) <- go (applyUSubstsC (vacuous substs) <$> cs)
          return (c':cs', moreSubsts <> applyUSubstsClosedS moreSubsts substs)
        (Simplified c', substs) -> do
          (cs', moreSubsts) <- go (c' <> (applyUSubstsC (vacuous substs) <$> cs))
          return (cs', moreSubsts <> applyUSubstsClosedS moreSubsts substs)

tryFlexRigid
  :: ( HigherOrderUnifiable t, Reducible t
     , MonadPlus m, MonadFresh v m
     , Eq a, Eq b, Eq v )
  => UConstraint b t (Bound.Var b' a) v
  -> [m [USubsts b t a v]]
tryFlexRigid (ForAll c)  = tryFlexRigid (joinBound <$> Bound.Simple.fromScope c)
  where
    joinBound (Bound.B x)           = Bound.B (Left x)
    joinBound (Bound.F (Bound.B y)) = Bound.B (Right y)
    joinBound (Bound.F (Bound.F z)) = Bound.F z
tryFlexRigid (t1 :~: t2) =
  case t1 of
    FreeScoped (InR (MetaAppF v args)) ->
      generate v (length args) (extractHead t2)
    _ -> []
  where

    genFullMeta n = do
      v <- freshMeta
      let args = [ PureScoped (Bound.B i) | i <- [0 .. n - 1] ]
      return $ FreeScoped (InR (MetaAppF v args))

    generate v n h =
      [ mkSubst v n <$> (t >>= \t' -> pure t' `mplus` grow t' n)
      | t <- generateWithHead h n : [ generateWithBoundHead i | i <- [0 .. n - 1] ]
      ]

    grow t n = msum
      [ FreeScoped . InL <$> bitraverse goScope go shape
      | shape <- shapes ]
      where
        go HasHead = pure t `mplus` grow t n
        go NoHead  = genFullMeta n

        -- FIXME: we are not using extra bound variables here!
        goScope x = Bound.toScope . fmap Bound.F <$> go x

    mkSubst v n t = [Substs [(v, MetaAbs n (Bound.toScope t))]]

    generateWithBoundHead i = pure (PureScoped (Bound.B i))

    generateWithHead (PureScoped (Bound.B _)) _ = mzero
    generateWithHead h n = join <$> traverse go h
      where
        go (Bound.B _) = genFullMeta n
        go (Bound.F x) = pure (PureScoped (Bound.F x))

unify
  :: ( HigherOrderUnifiable t, Reducible t
     , MonadPlus m, MonadLogic m, MonadFresh v m
     , Eq a, Eq b, Eq v )
  => b
  -> USubsts b t a v
  -> [UConstraint b t a v]
  -> m ([UConstraint b t a v], USubsts b t a v)
unify defaultBoundVar substs constraints = do
  (constraints', moreSubsts) <- repeatedlySimplifyU defaultBoundVar constraints
  let moreSubsts' = vacuous moreSubsts
      substs' = applyUSubstsS moreSubsts' substs
      oldSubsts = substs' <> moreSubsts'
      (flexflex, flexrigid) = partition isFlexFlex constraints'
  case flexrigid of
    [] -> return (flexflex, oldSubsts)
    fr : _ -> do
      let psubsts = tryFlexRigid (Bound.F <$> fr)
      trySubsts oldSubsts psubsts (flexrigid <> flexflex)
  where
    trySubsts oldSubsts psubsts cs = do
      ss <- foldr interleave mzero psubsts
      msum
        [ unify defaultBoundVar newSubsts newConstraints
        | newS <- ss
        , let newConstraints = applyUSubstsC newS <$> cs
        , let newSubsts = newS <> applyUSubstsS newS oldSubsts
        ]

isFlexFlex
  :: (HigherOrderUnifiable t)
  => UConstraint b t a v
  -> Bool
isFlexFlex (t1 :~: t2) = isStuckU t1 && isStuckU t2
isFlexFlex (ForAll c)  = isFlexFlex (Bound.Simple.fromScope c)

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
  shapeGuesses t = noGuesses t
    where
      noGuesses = bimap (,[]) (,[])

  shapes = [ AppF HasHead NoHead ]

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
  = Bound.Scope (Name b ()) (FreeScoped (Name b ()) (TermF :+: MetaAppF v)) a

type Term'  = Term  Rzk.Var Rzk.Var
type UTerm' = UTerm Rzk.Var Rzk.Var Rzk.Var

type Constraint' = UConstraint (Name Rzk.Var ()) TermF Rzk.Var Rzk.Var

applyUSubstsClosedS
  :: (Eq b, Eq v, Bifunctor t)
  => USubstsClosed b t v
  -> USubstsClosed b t v
  -> USubstsClosed b t v
applyUSubstsClosedS substs (Substs ss) = Substs (second f <$> ss)
  where
    f (MetaAbs n body) = MetaAbs n . Bound.toScope $
      applyUSubsts (Bound.F <$> substs) (Bound.fromScope body)

applyUSubstsS
  :: (Eq a, Eq b, Eq v, Bifunctor t)
  => USubsts b t a v
  -> USubsts b t a v
  -> USubsts b t a v
applyUSubstsS substs (Substs ss) = Substs (fmap f <$> ss)
  where
    f (MetaAbs n body) = MetaAbs n . Bound.toScope $
      applyUSubsts (Bound.F <$> substs) (Bound.fromScope body)

applyUSubstsC
  :: (Eq a, Eq b, Eq v, Bifunctor t)
  => USubsts b t a v
  -> UConstraint b t a v
  -> UConstraint b t a v
applyUSubstsC substs = \case
  t1 :~: t2 ->
    applyUSubsts substs t1 :~: applyUSubsts substs t2
  ForAll c -> ForAll $ Bound.Simple.toScope $
    applyUSubstsC (Bound.F <$> substs) ((Bound.Simple.fromScope c))

-- ** Testing

type UnifyM' = LogicT (Fresh Rzk.Var)

runUnifyM' :: UnifyM' a -> [a]
runUnifyM' m = runFresh (observeAllT m) defaultFreshMetaVars
  where
    defaultFreshMetaVars = [ fromString ("M" <> toIndex i) | i <- [1..] ]

    toIndex n = index
      where
        digitToSub c = chr ((ord c - ord '0') + ord '₀')
        index = map digitToSub (show n)

-- | Unify to 'UTerm''s:
--
-- >>> t1 = MetaApp "f" [VarE "x"] :: UTerm'
-- >>> t2 = AppE (VarE "x") (VarE "y") :: UTerm'
-- >>> t1
-- ?f[x]
-- >>> t2
-- x y
-- >>> unifyUTerms'_ t1 t2
-- Just [(f,λx₁. x y)]
unifyUTerms'_ :: UTerm' -> UTerm' -> Maybe (USubsts (Name Rzk.Var ()) TermF Rzk.Var Rzk.Var)
unifyUTerms'_ t1 t2 = listToMaybe . runUnifyM' $ do
  (_flexflex, Substs substs) <- unify (Name Nothing ()) (Substs []) [t1 :~: t2]
  return $ Substs
    [ (v, t)
    | (v, t) <- substs
    , v `elem` metas ]    -- removing intermediate meta variables
  where
    metas = getMetas t1 <> getMetas t2

    getMetas :: (Bifunctor t, Bifoldable t) => UFreeScoped b t a v -> [v]
    getMetas = \case
      PureScoped{} -> []
      FreeScoped (InR (MetaAppF v args)) -> v : foldMap getMetas args
      FreeScoped (InL t) -> bifoldMap (getMetas . Bound.fromScope) getMetas t

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

pattern MetaApp :: v -> [UFreeScoped b t a v] -> UFreeScoped b t a v
pattern MetaApp v args = FreeScoped (InR (MetaAppF v args))

pattern MetaVar :: v -> UFreeScoped b t a v
pattern MetaVar v = MetaApp v []


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
    f y | x == y = Just (Name (Just x) ())
    f _ = Nothing


-- * Pretty-printing

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

instance Show Constraint' where show = show . pretty
instance Pretty Constraint' where
  pretty = ppConstraint defaultFreshVars
    where
      defaultFreshVars = [ fromString ("x" <> toIndex i) | i <- [1..] ]

      toIndex n = index
        where
          digitToSub c = chr ((ord c - ord '0') + ord '₀')
          index = map digitToSub (show n)

ppConstraint :: [Rzk.Var] -> Constraint' -> Doc ann
ppConstraint [] = error "not enough fresh variables"
ppConstraint vars@(x:xs) = \case
  t1 :~: t2 -> ppUTermArg vars t1 <+> "~" <+> ppUTermArg vars t2
  ForAll c -> "forall" <+> pretty x <> dot
    <+> ppConstraint xs (instantiateC (pure x) c)

-- ** Pretty-printing terms and constraints with meta variables

instance (IsString a, Pretty a, Pretty b, Pretty v, Show v) => Show (Substs (Name b ()) TermF v a) where
  show (Substs substs) = show substs

instance (IsString a, Pretty a, Pretty b, Pretty v) => Show (MetaAbs (Name b ()) (TermF :+: MetaAppF v) a) where
  show = show . pretty

instance (IsString a, Pretty a, Pretty b, Pretty v) => Pretty (MetaAbs (Name b ()) (TermF :+: MetaAppF v) a) where
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
  ExtE (MetaAppF v args) -> "?" <> pretty v <>
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
  ppUTerm xs (Bound.instantiate1 (VarE x) t)

-- FIXME: this is for closed MetaAbs only
ppMetaAbs :: (Pretty a, Pretty b, Pretty v) => [a] -> MetaAbs (Name b ()) (TermF :+: MetaAppF v) a -> Doc ann
ppMetaAbs vars (MetaAbs arity body) =
  (if null args then ("" <>) else (("λ" <> hsep (map pretty args) <> ".") <+>))
  (ppUTerm vars' (Bound.instantiate ((VarE <$> args) !!) body))
  where
    (args, vars') = splitAt arity vars

-- TH derived instances
deriveBifunctor ''TermF
deriveBifoldable ''TermF
deriveBitraversable ''TermF
