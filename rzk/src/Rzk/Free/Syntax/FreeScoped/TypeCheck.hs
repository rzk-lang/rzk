{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
module Rzk.Free.Syntax.FreeScoped.TypeCheck where

import           Bound.Scope                             (Scope, fromScope,
                                                          toScope)
import qualified Bound.Var                               as Bound
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Logic
import           Control.Monad.State
import           Data.Bifoldable                         (Bifoldable)
import           Data.Bifunctor
import           Data.Bifunctor.TH
import           Data.Bitraversable

import           Rzk.Free.Bound.Name
import           Rzk.Free.Syntax.FreeScoped
import           Rzk.Free.Syntax.FreeScoped.Unification  (MonadBind, UVar (..),
                                                          freshMeta)
import           Rzk.Free.Syntax.FreeScoped.Unification2 (Unifiable (..),
                                                          manySubst, unify)
import qualified Rzk.Syntax.Var                          as Rzk

class (Bifunctor t, Bifoldable t, Unifiable t) => TypeCheckable t where
  inferTypeFor
    :: (Eq a, Eq v)
    => t
          (TypeCheck (UTypedTermInScope t b a v) (Bound.Var (Name b ()) a) v
              (UScopedTypedTerm t b a v))
          (TypeCheck (UTypedTerm t b a v) a v (UTypedTerm t b a v))
    -> TypeCheck (UTypedTerm t b a v) a v
          (TypedF t (UScopedTypedTerm t b a v) (UTypedTerm t b a v))

  whnfT :: UTypedTerm t b a v -> UTypedTerm t b a v

  universeT :: UTypedTerm t b a v

type Term t b = FreeScoped (Name b ()) t
type TermInScope t b a = FreeScoped (Name b ()) t (Bound.Var (Name b ()) a)
type ScopedTerm t b = Scope (Name b ()) (Term t b)

type Term' t = Term t Rzk.Var Rzk.Var
type TermInScope' t = TermInScope t Rzk.Var Rzk.Var
type ScopedTerm' t = ScopedTerm t Rzk.Var Rzk.Var

data TypedF term scope typedTerm = TypedF
  { termF :: term scope typedTerm
  , typeF :: Maybe typedTerm
  } deriving (Show, Functor, Foldable, Traversable)

type TypedTerm t b = FreeScoped (Name b ()) (TypedF t)
type TypedTermInScope t b a = FreeScoped (Name b ()) (TypedF t) (Bound.Var (Name b ()) a)
type ScopedTypedTerm t b = Scope (Name b ()) (TypedTerm t b)

type TypedTerm' t = TypedTerm t Rzk.Var Rzk.Var
type TypedTermInScope' t = TypedTermInScope t Rzk.Var Rzk.Var
type ScopedTypedTerm' t = ScopedTypedTerm t Rzk.Var Rzk.Var

type UTypedTerm t b a v = FreeScoped (Name b ()) (TypedF t) (UVar (Name b ()) a v)
type UTypedTermInScope t b a v = FreeScoped (Name b ()) (TypedF t) (Bound.Var (Name b ()) (UVar (Name b ()) a v))
type UScopedTypedTerm t b a v = Scope (Name b ()) (TypedTerm t b) (UVar (Name b ()) a v)

type UTypedTerm' t = UTypedTerm t Rzk.Var Rzk.Var Rzk.Var
type UTypedTermInScope' t = UTypedTermInScope t Rzk.Var Rzk.Var Rzk.Var
type UScopedTypedTerm' t = UScopedTypedTerm t Rzk.Var Rzk.Var Rzk.Var

type InScope' = Bound.Var (Name Rzk.Var ())

-- | A variable.
pattern Var :: a -> Term t b a
pattern Var x = PureScoped x

-- | A variable.
pattern VarT :: a -> TypedTerm t b a
pattern VarT x = PureScoped x

pattern TypedT
  :: Maybe (TypedTerm t b a)
  -> t (ScopedTypedTerm t b a) (TypedTerm t b a)
  -> TypedTerm t b a
pattern TypedT ty t = FreeScoped (TypedF t ty)

data TypeInfo metavar ty var = TypeInfo
  { knownFreeVars :: [(var, ty)]      -- ^ types of free variables
  , knownMetaVars :: [(metavar, ty)]  -- ^ types of meta variables
  , knownSubsts   :: [(metavar, ty)]  -- ^ substitutions of meta variables
  , constraints   :: [(ty, ty)]       -- ^ leftover constraints (flex-flex)
  , freshMetaVars :: [metavar]        -- ^ a stream of fresh meta variables
  } deriving (Functor)

instance (Show metavar, Show ty, Show var) => Show (TypeInfo metavar ty var) where
  show TypeInfo{..} = unlines
    [ "TypeInfo"
    , "  { knownFreeVars = " <> show knownFreeVars
    , "  , knownMetaVars = " <> show knownMetaVars
    , "  , knownSubsts   = " <> show knownSubsts
    , "  , constraints   = " <> show constraints
    , "  , freshMetaVars = " <> init (show (take 5 freshMetaVars)) <> ",...]"
    , "  }"
    ]

type TypeInfo' t = TypeInfo Rzk.Var (UTypedTerm' t) Rzk.Var
type TypeInfoInScope' t
  = TypeInfo Rzk.Var (UTypedTermInScope' t) (Bound.Var (Name Rzk.Var ()) Rzk.Var)

-- toTypeInfoInScope :: TypeInfo' -> TypeInfoInScope'
toTypeInfoInScope
  :: Bifunctor t
  => TypeInfo v (UTypedTerm t b a v) a
  -> TypeInfo v (UTypedTermInScope t b a v) (Bound.Var (Name b ()) a)
toTypeInfoInScope TypeInfo{..} = TypeInfo
  { knownFreeVars = bimap updateVar updateType <$> knownFreeVars
  , constraints = bimap updateType updateType <$> constraints
  , knownMetaVars = second updateType <$> knownMetaVars
  , knownSubsts = second updateType <$> knownSubsts
  , ..
  }
  where
    updateVar = Bound.F
    updateType = fmap updateVar

assignType :: var -> ty -> TypeCheck ty var metavar ()
assignType x t = do
  info <- getTypeInfo
  put (pure info { knownFreeVars = (x, t) : knownFreeVars info })

assignTypeMeta :: metavar -> ty -> TypeCheck ty var metavar ()
assignTypeMeta x t = do
  info <- getTypeInfo
  put (pure info { knownMetaVars = (x, t) : knownMetaVars info })

addKnownFreeVar
  :: (Eq a, Eq v, TypeCheckable t)
  => a -> TypeCheck (UTypedTerm t b a v) a v ()
addKnownFreeVar x = do
  info <- getTypeInfo
  case lookup x (knownFreeVars info) of
    Nothing -> do
      v <- freshTypeMetaVar
      assignType x (VarT (UMetaVar v))
    Just _ -> return ()

fromTypeInfoInScope
  :: Bifunctor t
  => TypeInfo v (UTypedTermInScope t b a v) (Bound.Var (Name b ()) a)
  -> TypeInfo v (UTypedTerm t b a v) a
fromTypeInfoInScope TypeInfo{..} = TypeInfo
  { knownFreeVars = bimap id updateType <$> removeBoundVars knownFreeVars
  , constraints = bimap updateType updateType <$> constraints
  , knownMetaVars = second updateType <$> knownMetaVars
  , knownSubsts = second updateType <$> knownSubsts
  , ..
  }
  where
    removeBoundVars ((Bound.B _, _) : vars) = removeBoundVars vars
    removeBoundVars ((Bound.F x, t) : vars) = (x, t) : removeBoundVars vars
    removeBoundVars []                      = []

    updateType = fmap updateVar

    updateVar (Bound.B _) = error "bound variable leaked!"
    updateVar (Bound.F x) = x

data TypeError
  = TypeErrorOther String
  deriving (Show)

instance Semigroup TypeError where x <> _ = x

instance Monoid TypeError where mempty = TypeErrorOther "mempty"

newtype TypeCheck ty var metavar a = TypeCheck
  { runTypeCheck :: StateT (Logic (TypeInfo metavar ty var)) (ExceptT TypeError Logic) a
  }
  deriving (Functor, Applicative, Monad)
  deriving (MonadState (Logic (TypeInfo metavar ty var)))
  deriving (MonadError TypeError)

execTypeCheck :: [metavar] -> TypeCheck ty var metavar a -> Either TypeError a
execTypeCheck vars m = fst <$> runTypeCheckOnce vars m

runTypeCheckOnce :: [metavar] -> TypeCheck ty var metavar a -> Either TypeError (a, TypeInfo metavar ty var)
runTypeCheckOnce vars m = second observe <$> observe' (runExceptT (runStateT (runTypeCheck m) initState))
  where
    observe' x =
      case observeMany 1 x of
        [y] -> y
        _   -> Left (TypeErrorOther "no answer (unification failed somewhere)")

    initState = pure TypeInfo
      { freshMetaVars = vars
      , constraints = []
      , knownFreeVars = []
      , knownMetaVars = []
      , knownSubsts = []
      }

getTypeInfo :: TypeCheck ty var metavar (TypeInfo metavar ty var)
getTypeInfo = do
  states <- get
  TypeCheck (lift (lift states))

type TypeCheck' t = TypeCheck (UTypedTerm' t) Rzk.Var Rzk.Var
type TypeCheckInScope' t
  = TypeCheck (UTypedTermInScope' t) (Bound.Var (Name Rzk.Var ()) Rzk.Var) Rzk.Var

instance Alternative (TypeCheck ty a v) where
  empty = TypeCheck (StateT (const (ExceptT empty)))
  TypeCheck x <|> TypeCheck y = TypeCheck $ do
    states <- get
    (result, s') <- lift $ ExceptT $
      runExceptT (runStateT x states) `interleave` runExceptT (runStateT y states)
    put s'
    return result

instance MonadPlus (TypeCheck ty a v) where
  mzero = empty
  mplus = (<|>)

instance MonadFail (TypeCheck ty a v) where
  fail = throwError . TypeErrorOther

instance Eq v => MonadBind ty v (TypeCheck ty a v) where
  freshMeta = do
    info@TypeInfo{ freshMetaVars = var:vars } <- getTypeInfo
    put (pure (info { freshMetaVars = vars }))
    return var

-- typecheckInScope :: TypeCheckInScope' a -> TypeCheck' a
typecheckInScope
  :: Bifunctor t
  => TypeCheck (UTypedTermInScope t b a v) (Bound.Var (Name b ()) a) v r
  -> TypeCheck (UTypedTerm t b a v) a v r
typecheckInScope m = do
  info <- fmap toTypeInfoInScope <$> get
  (x, info') <- TypeCheck $
    lift (runStateT (runTypeCheck m) info)
  put (fmap fromTypeInfoInScope info')
  return x

typecheckDist
  :: Bifunctor t
  => TypeCheck (UTypedTerm t b (Bound.Var (Name b ()) a) v) (Bound.Var (Name b ()) a) v r
  -> TypeCheck (UTypedTermInScope t b a v) (Bound.Var (Name b ()) a) v r
typecheckDist m = do
  info <- fmap to <$> get
  (x, info') <- TypeCheck $
    lift (runStateT (runTypeCheck m) info)
  put (fmap from info')
  return x
  where
    to = bimap (fmap dist) id
    from = bimap (fmap dist') id

typecheckDist'
  :: Bifunctor t
  => TypeCheck (UTypedTermInScope t b a v) (Bound.Var (Name b ()) a) v r
  -> TypeCheck (UTypedTerm t b (Bound.Var (Name b ()) a) v) (Bound.Var (Name b ()) a) v r
typecheckDist' m = do
  info <- fmap from <$> get
  (x, info') <- TypeCheck $
    lift (runStateT (runTypeCheck m) info)
  put (fmap to info')
  return x
  where
    to = bimap (fmap dist) id
    from = bimap (fmap dist') id

typecheck
  :: (Eq a, Eq v, TypeCheckable t)
  => Term t b a
  -> UTypedTerm t b a v
  -> TypeCheck (UTypedTerm t b a v) a v (UTypedTerm t b a v)
typecheck term expectedType = do
  typedTerm <- infer term
  typedTerm `shouldHaveType` expectedType

unifyWithExpected
  :: (Eq a, Eq v, TypeCheckable t)
  => UTypedTerm t b a v
  -> UTypedTerm t b a v
  -> TypeCheck (UTypedTerm t b a v) a v (UTypedTerm t b a v)
unifyWithExpected t1 t2 = do
  info@TypeInfo{constraints = cs} <- getTypeInfo
  (substs, flexflex) <- unify whnfT [] ((t1, t2) : cs) <|> fail "unable to unify ..."
  put $ pure info
    { knownFreeVars = map (second (manySubst substs)) (knownFreeVars info)
    , knownMetaVars = map (second (manySubst substs)) (knownMetaVars info)
    , knownSubsts   = substs <> knownSubsts info
    , constraints   = flexflex
    }
  return (manySubst substs t1)

typeOf
  :: (Eq a, Eq v, TypeCheckable t)
  => UTypedTerm t b a v
  -> TypeCheck (UTypedTerm t b a v) a v (UTypedTerm t b a v)
typeOf = \case
  FreeScoped (TypedF _term Nothing) -> pure universeT
  FreeScoped (TypedF _term (Just ty)) -> pure ty
  PureScoped (UFreeVar x) -> do
    vars <- knownFreeVars <$> getTypeInfo
    case lookup x vars of
      Nothing -> error "typeOf: unknown free var"
      Just t  -> pure t
  PureScoped (UBoundVar _ _) -> error "typeOf: bound var"
  PureScoped (UMetaVar v) -> do
    vars <- knownMetaVars <$> getTypeInfo
    case lookup v vars of
      Nothing -> error "typeOf: unknown meta var"
      Just t  -> pure t

freshTypeMetaVar
  :: (Eq v, TypeCheckable t)
  => TypeCheck (UTypedTerm t b a v) a v v
freshTypeMetaVar = do
  v <- freshMeta
  assignTypeMeta v universeT
  return v

typeOfScopedWith
  :: (Eq a, Eq v, TypeCheckable t)
  => UTypedTerm t b a v
  -> UScopedTypedTerm t b a v
  -> TypeCheck (UTypedTerm t b a v) a v (UScopedTypedTerm t b a v)
typeOfScopedWith boundVarType scope = toScope <$> do
  case fromScope scope of
    FreeScoped (TypedF _ Nothing)   -> return (dist' <$> universeT)
    FreeScoped (TypedF _ (Just ty)) -> return ty
    PureScoped (Bound.F x)          -> fmap Bound.F <$> typeOf (PureScoped x)
    PureScoped (Bound.B _)          -> fmap Bound.F <$> pure boundVarType

clarifyTypedTerm
  :: (Eq a, Eq v, Bifunctor t)
  => UTypedTerm t b a v
  -> TypeCheck (UTypedTerm t b a v) a v (UTypedTerm t b a v)
clarifyTypedTerm t = do
  TypeInfo{knownSubsts = substs} <- getTypeInfo
  return (manySubst substs t)

clarifyScopedTypedTermWith
  :: (Eq a, Eq v, Bifunctor t, Bifoldable t)
  => UTypedTerm t b a v
  -> UScopedTypedTerm t b a v
  -> TypeCheck (UTypedTerm t b a v) a v (UScopedTypedTerm t b a v)
clarifyScopedTypedTermWith boundVarType scope = fmap (toScope . fmap dist') $ do
  typecheckInScope . typecheckDist $ do
    let registerBound (Bound.B x) =
          assignType (Bound.B x) (fmap (dist . Bound.F) boundVarType)
        registerBound _ = return ()
        scope' = fromScope scope
    mapM_ registerBound scope'
    clarifyTypedTerm (dist <$> scope')

shouldHaveType
  :: (Eq a, Eq v, TypeCheckable t)
  => UTypedTerm t b a v
  -> UTypedTerm t b a v
  -> TypeCheck (UTypedTerm t b a v) a v (UTypedTerm t b a v)
shouldHaveType term expectedType = do
  actualType <- typeOf term
  _ <- (actualType `unifyWithExpected` expectedType)
          <|> fail "expected type ... but actual type is ... for term ..."
  clarifyTypedTerm term

infer :: (Eq a, Eq v, TypeCheckable t)
      => Term t b a -> TypeCheck (UTypedTerm t b a v) a v (UTypedTerm t b a v)
infer = \case
  PureScoped x -> do
    addKnownFreeVar x
    return (PureScoped (UFreeVar x))
  FreeScoped t -> do
    ty <- FreeScoped <$> inferTypeFor (bimap inferScoped' infer t)
    clarifyTypedTerm ty
  where
    inferScoped' = fmap (toScope . fmap dist') . typecheckDist . infer . fromScope

inferScoped
  :: (Eq a, Eq v, TypeCheckable t)
  => ScopedTerm t b a
  -> TypeCheck (UTypedTerm t b a v) a v (UScopedTypedTerm t b a v)
inferScoped
  = fmap (toScope . fmap dist') . typecheckInScope . typecheckDist . infer . fromScope

inferScopedWith
  :: (Eq a, Eq v, TypeCheckable t)
  => UTypedTerm t b a v -- ^ Type of the bound variable.
  -> ScopedTerm t b a
  -> TypeCheck (UTypedTerm t b a v) a v (UScopedTypedTerm t b a v)
inferScopedWith boundVarType scope = fmap (toScope . fmap dist') $ do
  typecheckInScope . typecheckDist $ do
    let registerBound (Bound.B x) =
          assignType (Bound.B x) (fmap (dist . Bound.F) boundVarType)
        registerBound _ = return ()
        scope' = fromScope scope
    mapM_ registerBound scope'
    infer scope'

nonDep
  :: (Eq a, Eq v, Bitraversable t)
  => UScopedTypedTerm t b a v
  -> TypeCheck (UTypedTerm t b a v) a v (UTypedTerm t b a v)
nonDep scopedTerm = traverse fromBound (fromScope scopedTerm)
  where
    fromBound (Bound.B _) = fail "nonDep: dependent type"
    fromBound (Bound.F x) = pure x

dist :: Bound.Var b (UVar b' a v) -> UVar b' (Bound.Var b a) v
dist (Bound.B b)               = UFreeVar (Bound.B b)
dist (Bound.F (UFreeVar x))    = UFreeVar (Bound.F x)
dist (Bound.F (UBoundVar v b)) = UBoundVar v b
dist (Bound.F (UMetaVar m))    = UMetaVar m

dist' :: UVar b' (Bound.Var b a) v -> Bound.Var b (UVar b' a v)
dist' (UFreeVar (Bound.B b)) = (Bound.B b)
dist' (UFreeVar (Bound.F x)) = (Bound.F (UFreeVar x))
dist' (UBoundVar v b)        = (Bound.F (UBoundVar v b))
dist' (UMetaVar m)           = (Bound.F (UMetaVar m))

instance Unifiable term => Unifiable (TypedF term) where
  zipMatch (TypedF term1 type1) (TypedF term2 type2) = do
    term <- zipMatch term1 term2
    case (type1, type2) of
      (Just t1, Just t2) -> return (TypedF term (Just (Right (t1, t2))))
      _                  -> return (TypedF term Nothing)

  appSome fun args = (TypedF term (error "can't infer type"), args')
    where
      (term, args') = appSome fun args

  unAppSome (TypedF term _type) = do
    (fun, args) <- unAppSome term
    return (fun, args)

  abstract body = TypedF (abstract body) (error "can't infer type")

untyped :: (Bifunctor t) => TypedTerm t b a -> Term t b a
untyped = transFreeScopedT termF

untypedScoped :: (Bifunctor t) => ScopedTypedTerm t b a -> ScopedTerm t b a
untypedScoped = toScope . untyped . fromScope


deriveBifunctor ''TypeInfo

deriveBifunctor ''TypedF
deriveBifoldable ''TypedF
deriveBitraversable ''TypedF
