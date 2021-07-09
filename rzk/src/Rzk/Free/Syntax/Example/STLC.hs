{-# OPTIONS_GHC -fno-warn-orphans #-}
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
module Rzk.Free.Syntax.Example.STLC where

-- import           Debug.Trace
import           Unsafe.Coerce

import           Bound.Scope                             (Scope, fromScope,
                                                          toScope)
import qualified Bound.Scope                             as Scope
import qualified Bound.Var                               as Bound
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Logic
import           Control.Monad.State
import           Data.Bifunctor
import           Data.Bifunctor.TH
import           Data.Bitraversable
import           Data.Char                               (chr, ord)
import           Data.Maybe                              (fromMaybe)
import           Data.String                             (IsString (..))
import           Data.Text.Prettyprint.Doc               as Doc

import           Rzk.Free.Bound.Name
import           Rzk.Free.Syntax.FreeScoped
import           Rzk.Free.Syntax.FreeScoped.Unification  (MonadBind, UVar (..),
                                                          freshMeta)
import           Rzk.Free.Syntax.FreeScoped.Unification2 (Unifiable (..),
                                                          manySubst, unify)
import qualified Rzk.Syntax.Var                          as Rzk

trace :: String -> a -> a
trace = const id

traceShow :: Show b => b -> a -> a
traceShow = trace . show

-- | Generating functor for terms in Martin-Loef Type Theory.
data TermF scope term
  = UniverseF

  | FunF term term
  | LamF (Maybe term) scope
  | AppF term term

  | UnitTypeF
  | UnitF

  | LetF term scope

--  | TypeAsc term term
--  | ...

--  | BoolF
--  | TrueF
--  | FalseF
--  | IfF
  deriving (Show, Functor, Foldable, Traversable)

type Term b = FreeScoped (Name b ()) TermF
type TermInScope b a = FreeScoped (Name b ()) TermF (Bound.Var (Name b ()) a)
type ScopedTerm b = Scope (Name b ()) (Term b)

type Term' = Term Rzk.Var Rzk.Var
type TermInScope' = TermInScope Rzk.Var Rzk.Var
type ScopedTerm' = ScopedTerm Rzk.Var Rzk.Var

data TypedF term scope typedTerm = TypedF
  { termF :: term scope typedTerm
  , typeF :: Maybe typedTerm
  } deriving (Show, Functor, Foldable, Traversable)

type TypedTermF = TypedF TermF

type TypedTerm b = FreeScoped (Name b ()) TypedTermF
type TypedTermInScope b a = FreeScoped (Name b ()) TypedTermF (Bound.Var (Name b ()) a)
type ScopedTypedTerm b = Scope (Name b ()) (TypedTerm b)

type TypedTerm' = TypedTerm Rzk.Var Rzk.Var
type TypedTermInScope' = TypedTermInScope Rzk.Var Rzk.Var
type ScopedTypedTerm' = ScopedTypedTerm Rzk.Var Rzk.Var

type UTypedTerm b a v = FreeScoped (Name b ()) TypedTermF (UVar (Name b ()) a v)
type UTypedTermInScope b a v = FreeScoped (Name b ()) TypedTermF (Bound.Var (Name b ()) (UVar (Name b ()) a v))
type UScopedTypedTerm b a v = Scope (Name b ()) (TypedTerm b) (UVar (Name b ()) a v)

type UTypedTerm' = UTypedTerm Rzk.Var Rzk.Var Rzk.Var
type UTypedTermInScope' = UTypedTermInScope Rzk.Var Rzk.Var Rzk.Var
type UScopedTypedTerm' = UScopedTypedTerm Rzk.Var Rzk.Var Rzk.Var

type InScope' = Bound.Var (Name Rzk.Var ())

type UTypedTerm'1 = UTypedTerm Rzk.Var (InScope' Rzk.Var) Rzk.Var
type UTypedTerm'2 = UTypedTerm Rzk.Var (InScope' (InScope' Rzk.Var)) Rzk.Var

type TypeInfo'2 = TypeInfo Rzk.Var UTypedTerm'2 (InScope' (InScope' Rzk.Var))

-- | A variable.
pattern Var :: a -> Term b a
pattern Var x = PureScoped x

-- | Universe type \(\mathcal{U}_i\)
pattern Universe :: Term b a
pattern Universe = FreeScoped UniverseF

pattern Unit :: Term b a
pattern Unit = FreeScoped UnitF

pattern UnitType :: Term b a
pattern UnitType = FreeScoped UnitTypeF

pattern Let :: Term b a -> ScopedTerm b a -> Term b a
pattern Let u t = FreeScoped (LetF u t)

-- | A dependent product type (\(\Pi\)-type): \(\prod_{x : A} B(x)).
pattern Fun :: Term b a -> Term b a -> Term b a
pattern Fun a b = FreeScoped (FunF a b)

-- | A \(\lambda\)-abstraction.
pattern Lam :: Maybe (Term b a) -> ScopedTerm b a -> Term b a
pattern Lam ty body = FreeScoped (LamF ty body)

-- | An application of one term to another.
pattern App :: Term b a -> Term b a -> Term b a
pattern App t1 t2 = FreeScoped (AppF t1 t2)

{-# COMPLETE Var, Universe, UnitType, Unit, Let, Fun, Lam, App #-}

-- | A variable.
pattern VarT :: a -> TypedTerm b a
pattern VarT x = PureScoped x

-- | Universe type \(\mathcal{U}_i\)
pattern UniverseT :: TypedTerm b a -> TypedTerm b a
pattern UniverseT ty = FreeScoped (TypedF UniverseF (Just ty))

pattern UnitTypeT :: TypedTerm b a -> TypedTerm b a
pattern UnitTypeT ty = FreeScoped (TypedF UnitTypeF (Just ty))

pattern UnitT :: TypedTerm b a -> TypedTerm b a
pattern UnitT ty = FreeScoped (TypedF UnitF (Just ty))

-- | A dependent product type (\(\Pi\)-type): \(\prod_{x : A} B(x)).
pattern FunT :: TypedTerm b a -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a
pattern FunT ty a b = FreeScoped (TypedF (FunF a b) (Just ty))

-- | A \(\lambda\)-abstraction.
pattern LamT :: TypedTerm b a -> Maybe (TypedTerm b a) -> ScopedTypedTerm b a -> TypedTerm b a
pattern LamT ty argType body = FreeScoped (TypedF (LamF argType body) (Just ty))

-- | An application of one term to another.
pattern AppT :: TypedTerm b a -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a
pattern AppT ty t1 t2 = FreeScoped (TypedF (AppF t1 t2) (Just ty))

universeT :: TypedTerm b a
universeT = FreeScoped (TypedF UniverseF Nothing)

-- | Abstract over one variable in a term.
--
-- >>> lam "x" (App (Var "f") (Var "x")) :: Term String String
-- λx₁ → f x₁
-- >>> lam "f" (App (Var "f") (Var "x")) :: Term String String
-- λx₁ → x₁ x
lam :: Eq a => Maybe (Term a a) -> a -> Term a a -> Term a a
lam ty x body = Lam ty (abstract1Name x body)

lam_ :: Eq a => a -> Term a a -> Term a a
lam_ x body = Lam Nothing (abstract1Name x body)

let_ :: Eq a => Term a a -> a -> Term a a -> Term a a
let_ u x body = Let u (abstract1Name x body)

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

type TypeInfo' = TypeInfo Rzk.Var UTypedTerm' Rzk.Var
type TypeInfoInScope'
  = TypeInfo Rzk.Var UTypedTermInScope' (Bound.Var (Name Rzk.Var ()) Rzk.Var)

-- toTypeInfoInScope :: TypeInfo' -> TypeInfoInScope'
toTypeInfoInScope
  :: TypeInfo v (UTypedTerm b a v) a
  -> TypeInfo v (UTypedTermInScope b a v) (Bound.Var (Name b ()) a)
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

addKnownFreeVar :: (Eq a, Eq v) => a -> TypeCheck (UTypedTerm b a v) a v ()
addKnownFreeVar x = do
  info <- getTypeInfo
  case lookup x (knownFreeVars info) of
    Nothing -> do
      v <- freshTypeMetaVar
      assignType x (VarT (UMetaVar v))
    Just _ -> return ()

fromTypeInfoInScope
  :: TypeInfo v (UTypedTermInScope b a v) (Bound.Var (Name b ()) a)
  -> TypeInfo v (UTypedTerm b a v) a
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

execTypeCheck' :: TypeCheck' a -> Either TypeError a
execTypeCheck' = execTypeCheck defaultFreshMetaVars

execTypeCheck :: [metavar] -> TypeCheck ty var metavar a -> Either TypeError a
execTypeCheck vars m = fst <$> runTypeCheckOnce vars m

runTypeCheckOnce' :: TypeCheck' a -> Either TypeError (a, TypeInfo')
runTypeCheckOnce' = runTypeCheckOnce defaultFreshMetaVars

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

type TypeCheck' = TypeCheck UTypedTerm' Rzk.Var Rzk.Var
type TypeCheckInScope'
  = TypeCheck UTypedTermInScope' (Bound.Var (Name Rzk.Var ()) Rzk.Var) Rzk.Var

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
  :: TypeCheck (UTypedTermInScope b a v) (Bound.Var (Name b ()) a) v r
  -> TypeCheck (UTypedTerm b a v) a v r
typecheckInScope m = do
  info <- fmap toTypeInfoInScope <$> get
  (x, info') <- TypeCheck $
    lift (runStateT (runTypeCheck m) info)
  put (fmap fromTypeInfoInScope info')
  return x

typecheckDist
  :: TypeCheck (UTypedTerm b (Bound.Var (Name b ()) a) v) (Bound.Var (Name b ()) a) v r
  -> TypeCheck (UTypedTermInScope b a v) (Bound.Var (Name b ()) a) v r
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
  :: TypeCheck (UTypedTermInScope b a v) (Bound.Var (Name b ()) a) v r
  -> TypeCheck (UTypedTerm b (Bound.Var (Name b ()) a) v) (Bound.Var (Name b ()) a) v r
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
  :: (Eq a, Eq v)
  => Term b a
  -> UTypedTerm b a v
  -> TypeCheck (UTypedTerm b a v) a v (UTypedTerm b a v)
typecheck term expectedType = do
  typedTerm <- infer term
  trace "shouldHaveType #1" $ typedTerm `shouldHaveType` expectedType

unifyWithExpected
  :: (Eq a, Eq v)
  => UTypedTerm b a v
  -> UTypedTerm b a v
  -> TypeCheck (UTypedTerm b a v) a v (UTypedTerm b a v)
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

whnfT :: UTypedTerm b a v -> UTypedTerm b a v
whnfT = id

typeOf :: (Eq a, Eq v) => UTypedTerm b a v -> TypeCheck (UTypedTerm b a v) a v (UTypedTerm b a v)
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

freshTypeMetaVar :: (Eq v) => TypeCheck (UTypedTerm b a v) a v v
freshTypeMetaVar = do
  v <- freshMeta
  assignTypeMeta v universeT
  return v

typeOfScopedWith
  :: (Eq a, Eq v)
  => UTypedTerm b a v
  -> UScopedTypedTerm b a v
  -> TypeCheck (UTypedTerm b a v) a v (UScopedTypedTerm b a v)
typeOfScopedWith boundVarType scope = toScope <$> do
  case fromScope scope of
    FreeScoped (TypedF _ Nothing)   -> return universeT
    FreeScoped (TypedF _ (Just ty)) -> return ty
    PureScoped (Bound.F x)          -> fmap Bound.F <$> typeOf (PureScoped x)
    PureScoped (Bound.B _)          -> fmap Bound.F <$> pure boundVarType

-- typeOfScoped
--   :: (Eq a, Eq v)
--   => UScopedTypedTerm b a v -> TypeCheck (UTypedTerm b a v) a v (UScopedTypedTerm b a v)
-- typeOfScoped scope = toScope <$> do
--   case fromScope scope of
--     FreeScoped (TypedF _ Nothing) -> return universeT
--     FreeScoped (TypedF _ (Just ty)) -> return ty
--     PureScoped (Bound.F x)   -> fmap Bound.F <$> typeOf (PureScoped x)
--     PureScoped (Bound.B _)   -> PureScoped . Bound.F . UMetaVar <$> freshTypeMetaVar
--
-- typeOfScopedNonDep
--   :: (Eq a, Eq v)
--   => UScopedTypedTerm b a v -> TypeCheck (UTypedTerm b a v) a v (UTypedTerm b a v)
-- typeOfScopedNonDep scope = do
--   scopedType <- typeOfScoped scope
--   traverse fromBound (fromScope scopedType)
--   where
--     fromBound (Bound.B _) = fail "typeOfScopedNonDep: dependent type!"
--     fromBound (Bound.F x) = pure x

inferTypeForF2
  :: (Eq a, Eq v)
  => TermF
        (TypeCheck (UTypedTermInScope b a v) (Bound.Var (Name b ()) a) v
            (UScopedTypedTerm b a v))
        (TypeCheck (UTypedTerm b a v) a v (UTypedTerm b a v))
  -> TypeCheck (UTypedTerm b a v) a v
        (TypedTermF (UScopedTypedTerm b a v) (UTypedTerm b a v))
inferTypeForF2 term = case term of
  UniverseF -> pure (TypedF UniverseF (Just universeT))
  -- a -> b
  FunF inferA inferB -> do
    a <- inferA
    _ <- trace "shouldHaveType #2" $ a `shouldHaveType` universeT
    b <- inferB
    _ <- trace "shouldHaveType #3" $ b `shouldHaveType` universeT
    pure (TypedF (FunF a b) (Just universeT))

  LamF minferTypeOfArg inferBody -> trace "[inferTypeForF LamF]" $ do
    typeOfArg <- case minferTypeOfArg of
      Just inferTypeOfArg -> inferTypeOfArg
      Nothing             -> VarT . UMetaVar <$> freshTypeMetaVar
    typeOfArg' <- trace "shouldHaveType #4" $ typeOfArg `shouldHaveType` universeT
    scopedTypedBody <- typecheckInScope $ do
      assignType (Bound.B (Name Nothing ())) (fmap Bound.F typeOfArg') -- FIXME: unnamed?
      inferBody
    typeOfBody <- typeOfScopedWith typeOfArg' scopedTypedBody >>= nonDep
    typeOfBody' <- trace "shouldHaveType #5" $ typeOfBody `shouldHaveType` universeT
    pure $ TypedF
      (LamF (typeOfArg <$ minferTypeOfArg) scopedTypedBody)
      (Just (FunT universeT typeOfArg' typeOfBody'))

  AppF infer_f infer_x -> trace "[inferTypeForF AppF]" $ do
    f <- infer_f
    x <- infer_x
    TypedF (AppF f x) . Just <$> do
      typeOf f >>= \case
        FunT _ argType bodyType -> do
          info <- getTypeInfo
          typeOf_x <- typeOf x
          _ <-
            trace (show (unsafeCoerce x :: UTypedTerm'2) <> " `shouldHaveType` " <> show (unsafeCoerce argType :: UTypedTerm'2)) $
              trace (show (unsafeCoerce info :: TypeInfo'2)) $
                trace (show (unsafeCoerce typeOf_x :: UTypedTerm'2)) $
                  trace (show (unsafeCoerce typeOf_x :: UTypedTerm'2) <> " `unifyWithExpected` " <> show (unsafeCoerce argType :: UTypedTerm'2))
                  x `shouldHaveType` argType
          trace "shouldHaveType #7" $ bodyType `shouldHaveType` universeT
        t@(VarT _) -> do
          bodyType <- VarT . UMetaVar <$> freshTypeMetaVar
          typeOf_x <- typeOf x
          _ <- trace "unifyWithExpected #1" $ t `unifyWithExpected` FunT universeT typeOf_x bodyType
          clarifyTypedTerm bodyType
        _ -> fail "inferTypeForF: application of a non-function"

  UnitTypeF -> pure (TypedF UnitTypeF (Just universeT))
  UnitF -> pure (TypedF UnitF (Just (UnitTypeT universeT)))
  LetF inferArg inferBody -> do
    arg <- inferArg
    typeOfArg <- typeOf arg
    typeOfArg' <- typeOfArg `shouldHaveType` universeT
    scopedTypedBody <- typecheckInScope $ do
      assignType (Bound.B (Name Nothing ())) (fmap Bound.F typeOfArg')
      inferBody
    typeOfBody <- typeOfScopedWith typeOfArg' scopedTypedBody >>= nonDep
    typeOfBody' <- typeOfBody `shouldHaveType` universeT
    pure $ TypedF
      (LetF arg scopedTypedBody)
      (Just typeOfBody')

inferTypeForF
  :: (Eq a, Eq v)
  => TermF (UScopedTypedTerm b a v) (UTypedTerm b a v)
  -> TypeCheck (UTypedTerm b a v) a v (UTypedTerm b a v)
inferTypeForF term = case term of
  UniverseF -> pure universeT
  -- a -> b
  FunF a b -> do
    _ <- a `shouldHaveType` universeT
    _ <- b `shouldHaveType` universeT
    pure universeT

  LamF mtypeOfArg body -> trace "[inferTypeForF LamF]" $ do
    typeOfArg <- case mtypeOfArg of
      Just t  -> return t
      Nothing -> VarT . UMetaVar <$> freshTypeMetaVar
    typeOfArg' <- typeOfArg `shouldHaveType` universeT
    typeOfBody <- do
      scopedTypedBody <- clarifyScopedTypedTermWith typeOfArg' body
      typeOfScopedWith typeOfArg' scopedTypedBody >>= nonDep
    typeOfBody' <- typeOfBody `shouldHaveType` universeT
    pure (FunT universeT typeOfArg' typeOfBody')

  AppF f x -> trace "[inferTypeForF AppF]" $ do
    typeOf f >>= \case
      FunT _ argType bodyType -> do
        _ <- x `shouldHaveType` argType
        bodyType `shouldHaveType` universeT
      t@(VarT _) -> do
        bodyType <- VarT . UMetaVar <$> freshTypeMetaVar
        typeOf_x <- typeOf x
        _ <- trace "unifyWithExpected #2" $ t `unifyWithExpected` FunT universeT typeOf_x bodyType
        clarifyTypedTerm bodyType
      _ -> fail "inferTypeForF: application of a non-function"

  UnitTypeF -> pure universeT
  UnitF -> pure (UnitTypeT universeT)
  LetF _unit _body -> error "not implemented"
--    typecheckInScope . typecheckDist $ do
--      let registerBound x@(Bound.B _) = assignType x (FreeScoped (TypedF UnitTypeF universeT))
--      traverse registerBound (fromScope body)
--    typeOfScopedNonDep body

clarifyTypedTerm
  :: (Eq a, Eq v)
  => UTypedTerm b a v
  -> TypeCheck (UTypedTerm b a v) a v (UTypedTerm b a v)
clarifyTypedTerm t = do
  TypeInfo{knownSubsts = substs} <- getTypeInfo
  return (manySubst substs t)

clarifyScopedTypedTermWith
  :: (Eq a, Eq v)
  => UTypedTerm b a v
  -> UScopedTypedTerm b a v
  -> TypeCheck (UTypedTerm b a v) a v (UScopedTypedTerm b a v)
clarifyScopedTypedTermWith boundVarType scope = fmap (toScope . fmap dist') $ do
  typecheckInScope . typecheckDist $ do
    let registerBound (Bound.B x) =
          assignType (Bound.B x) (fmap (dist . Bound.F) boundVarType)
        registerBound _ = return ()
        scope' = fromScope scope
    mapM_ registerBound scope'
    clarifyTypedTerm (dist <$> scope')

shouldHaveType
  :: (Eq a, Eq v)
  => UTypedTerm b a v
  -> UTypedTerm b a v
  -> TypeCheck (UTypedTerm b a v) a v (UTypedTerm b a v)
shouldHaveType term expectedType = do
  actualType <- typeOf term
  _ <- trace "unifyWithExpected #3" $ (actualType `unifyWithExpected` expectedType)
          <|> fail "expected type ... but actual type is ... for term ..."
  clarifyTypedTerm term

inferF
  :: (Eq a, Eq v)
  => TermF (UScopedTypedTerm b a v) (UTypedTerm b a v)
  -> TypeCheck (UTypedTerm b a v) a v (TypedTermF (UScopedTypedTerm b a v) (UTypedTerm b a v))
inferF term = TypedF term . Just <$> do
  ty <- inferTypeForF term
  clarifyTypedTerm ty -- FIXME: too expensive?

infer :: (Eq a, Eq v)
      => Term b a -> TypeCheck (UTypedTerm b a v) a v (UTypedTerm b a v)
infer = \case
  PureScoped x -> do
    addKnownFreeVar x
    return (PureScoped (UFreeVar x))
  FreeScoped t -> do
    ty <- FreeScoped <$> inferTypeForF2 (bimap inferScoped infer t)
    clarifyTypedTerm ty
  where
    inferScoped = fmap (toScope . fmap dist') . typecheckDist . infer . fromScope

inferScoped
  :: (Eq a, Eq v)
  => ScopedTerm b a
  -> TypeCheck (UTypedTerm b a v) a v (UScopedTypedTerm b a v)
inferScoped
  = fmap (toScope . fmap dist') . typecheckInScope . typecheckDist . infer . fromScope

inferScopedWith
  :: (Eq a, Eq v)
  => UTypedTerm b a v -- ^ Type of the bound variable.
  -> ScopedTerm b a
  -> TypeCheck (UTypedTerm b a v) a v (UScopedTypedTerm b a v)
inferScopedWith boundVarType scope = trace "[inferScopedWith]" $ fmap (toScope . fmap dist') $ do
  typecheckInScope . typecheckDist $ do
    let registerBound (Bound.B x) =
          assignType (Bound.B x) (fmap (dist . Bound.F) boundVarType)
        registerBound _ = return ()
        scope' = fromScope scope
    mapM_ registerBound scope'
    trace "[inferScopedWith.infer]" $ infer scope'

nonDep
  :: (Eq a, Eq v)
  => UScopedTypedTerm b a v
  -> TypeCheck (UTypedTerm b a v) a v (UTypedTerm b a v)
nonDep scopedTerm = traverse fromBound (fromScope scopedTerm)
  where
    fromBound (Bound.B _) = fail "nonDep: dependent type"
    fromBound (Bound.F x) = pure x

inferF'
  :: TermF UScopedTypedTerm' UTypedTerm'
  -> TypeCheck' (TypedTermF UScopedTypedTerm' UTypedTerm')
inferF' = inferF

-- term : type
--
-- typecheckedTyped <- typecheck type Universe
-- typecheckedTerm <- typecheck term typecheckedTyped
--
--
-- typecheck' :: Term' -> TypedTerm' -> TypeCheck' UTypedTerm'

infer' :: Term' -> TypeCheck' UTypedTerm'
infer' = infer

inferScoped' :: ScopedTerm' -> TypeCheck' UScopedTypedTerm'
inferScoped' = inferScoped

inferInScope' :: TermInScope' -> TypeCheck' UTypedTermInScope'
inferInScope' = fmap (fmap dist') . typecheckInScope . typecheckDist . infer

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

instance Unifiable TermF where
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

  zipMatch (FunF arg1 body1) (FunF arg2 body2)
    = Just (FunF (Right (arg1, arg2)) (Right (body1, body2)))

  zipMatch UniverseF UniverseF = Just UniverseF

  zipMatch UnitTypeF UnitTypeF = Just UnitTypeF
  zipMatch UnitF UnitF = Just UnitF

  zipMatch (LetF u1 t1) (LetF u2 t2)
    = Just (LetF (Right (u1, u2)) (Right (t1, t2)))

  zipMatch FunF{} _ = Nothing
  zipMatch LamF{} _ = Nothing
  zipMatch UniverseF{} _ = Nothing
  zipMatch AppF{} _ = Nothing
  zipMatch UnitTypeF{} _ = Nothing
  zipMatch UnitF{} _ = Nothing
  zipMatch LetF{} _ = Nothing

  appSome _ []     = error "cannot apply to zero arguments"
  appSome f (x:xs) = (AppF f x, xs)

  unAppSome (AppF f x) = Just (f, [x])
  unAppSome _          = Nothing

  abstract = LamF (error "argument type")

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

instance Pretty Rzk.Var where
  pretty (Rzk.Var x) = pretty x

instance (Pretty n, Pretty b) => Pretty (Name n b) where
  pretty (Name Nothing b)     = pretty b
  pretty (Name (Just name) b) = "<" <> pretty name <> " " <> pretty b <> ">"

instance (Pretty b, Pretty a) => Pretty (Bound.Var b a) where
  pretty (Bound.B b) = "<bound " <> pretty b <> ">"
  pretty (Bound.F x) = "<free " <> pretty x <> ">"

instance IsString a => IsString (Bound.Var b a) where
  fromString = Bound.F . fromString

-- | Uses 'Pretty' instance.
--
-- >>> mkLams 5 (abstract (const Nothing) (Var "y")) :: Term String String
-- λx₁ → λx₂ → λx₃ → λx₄ → λx₅ → y
instance (Pretty a, Pretty b, IsString a) => Show (Term b a) where
  show = show . pretty

-- | Uses default names (@x@ with a positive integer subscript) for bound variables:
--
-- >>> pretty (mkLams 5 (abstract (const Nothing) (Var "y")) :: Term String String)
-- λx₁ → λx₂ → λx₃ → λx₄ → λx₅ → y
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
    FreeScoped (TypedF term ty) -> show (FreeScoped (bimap untypedScoped untyped term)) <> " : " <> show (untyped (fromMaybe universeT ty))
    t -> show (untyped t)

untyped :: TypedTerm b a -> Term b a
untyped = transFreeScopedT termF

untypedScoped :: ScopedTypedTerm b a -> ScopedTerm b a
untypedScoped = toScope . untyped . fromScope

ppTypedTerm :: (Pretty a, Pretty b) => [a] -> TypedTerm b a -> Doc ann
ppTypedTerm vars = ppTerm vars . untyped

-- | Pretty-print an untyped term.
ppTerm :: (Pretty a, Pretty b) => [a] -> Term b a -> Doc ann
ppTerm vars = \case
  Var x -> pretty x

  Universe -> "U"

  Fun a b -> ppTermFun vars a <+> "→" <+> ppTerm vars b
  Lam Nothing body -> ppScopedTerm vars body $ \x body' ->
    "λ" <> pretty x <+> "→" <+> body'
  Lam (Just ty) body -> ppScopedTerm vars body $ \x body' ->
    "λ" <> parens (pretty x <+> ":" <+> ppTerm vars ty) <+> "→" <+> body'
  App f x -> ppTermFun vars f <+> ppTermArg vars x

  UnitType -> "UNIT"
  Unit -> "unit"
  Let u t -> ppScopedTerm vars t $ \x t' ->
    align (hsep ["let" <+> pretty x <+> "=" <+> ppTerm vars u <+> "in", t'])

ppElimWithArgs :: (Pretty a, Pretty b) => [a] -> Doc ann -> [Term b a] -> Doc ann
ppElimWithArgs vars name args = name <> tupled (map (ppTermFun vars) args)

-- | Pretty-print an untyped in a head position.
ppTermFun :: (Pretty a, Pretty b) => [a] -> Term b a -> Doc ann
ppTermFun vars = \case
  t@Var{} -> ppTerm vars t
  t@App{} -> ppTerm vars t
  t@Universe{} -> ppTerm vars t
  t@Unit{} -> ppTerm vars t
  t@UnitType{} -> ppTerm vars t

  t@Lam{} -> Doc.parens (ppTerm vars t)
  t@Fun{} -> Doc.parens (ppTerm vars t)
  t@Let{} -> Doc.parens (ppTerm vars t)

-- | Pretty-print an untyped in an argument position.
ppTermArg :: (Pretty a, Pretty b) => [a] -> Term b a -> Doc ann
ppTermArg vars = \case
  t@Var{} -> ppTerm vars t
  t@Universe{} -> ppTerm vars t
  t@Unit{} -> ppTerm vars t
  t@UnitType{} -> ppTerm vars t

  t@App{} -> Doc.parens (ppTerm vars t)
  t@Lam{} -> Doc.parens (ppTerm vars t)
  t@Fun{} -> Doc.parens (ppTerm vars t)
  t@Let{} -> Doc.parens (ppTerm vars t)

ppScopedTerm
  :: (Pretty a, Pretty b)
  => [a] -> ScopedTerm b a -> (a -> Doc ann -> Doc ann) -> Doc ann
ppScopedTerm [] _ _            = error "not enough fresh names"
ppScopedTerm (x:xs) t withScope = withScope x (ppTerm xs (Scope.instantiate1 (Var x) t))

examples :: IO ()
examples = mapM_ runExample . zip [1..] $
  [ let_ (lam_ "f" $ lam_ "z" $ Var "z") "zero" $
    let_ (lam_ "n" $ lam_ "f" $ lam_ "z" $ App (Var "f") (App (App (Var "n") (Var "f")) (Var "z"))) "succ" $
      App (Var "succ") (App (Var "succ") (Var "zero"))

  , let_ (lam_ "f" $ lam_ "z" $ Var "z") "zero" $
      Var "zero"

  , App (lam_ "x" (Var "x")) $
      lam_ "f" $ lam_ "z" $ Var "z"

  , let_ Unit "x" Unit

  , let_ Unit "x" (Var "x")


  , lam Nothing "f" $
      lam Nothing "x" $
        App (Var "f") (Var "x") -- ok (fixed)

  , lam (Just (Fun UnitType UnitType)) "f" $
      lam (Just UnitType) "x" $
        App (Var "f") (Var "x") -- ok

  , lam (Just (Fun (Var "A") (Var "B"))) "f" $
      lam (Just (Var "A")) "x" $
        App (Var "f") (Var "x") -- ok (fixed)

  , lam Nothing "x" $
      lam Nothing "x" $
        Var "x" -- ok

  , lam Nothing "x" $
      lam Nothing "y" $
        Var "x" -- ok (fixed)

  , lam (Just (Fun (Var "A") (Var "B"))) "f" $
      lam Nothing "x" $
        App (Var "f") (Var "x") -- ok

  , lam Nothing "x" $
      Var "x" -- ok

  , lam (Just (Var "A")) "x" $
      Var "x" -- ok

  , lam (Just (Fun (Var "A") (Var "B"))) "f" $
      Var "f" -- ok

  , lam Nothing "f" $
      App (Var "f") (Var "f")  -- ok: type error

  , lam Nothing "f" $
      App (Var "f") Unit -- ok

  , Fun (Var "A") UnitType -- ok (looped because of unsafeCoerce)
  , Fun (Var "A") (Var "B") -- ok (looped because of unsafeCoerce)

  , lam Nothing "f" $
      lam (Just UnitType) "x" $
        App (Var "f") (App (Var "f") (Var "x"))
        -- ok

  , Unit                  -- ok
  , App Unit Unit         -- type error
  , UnitType              -- ok
  , Var "x"               -- ok-ish
  , App (Var "f") Unit    -- ambiguous

  , App (Var "f") (App (Var "f") Unit) -- ok (fixed)

  , Fun Unit Unit         -- type error
  , Fun UnitType UnitType -- ok

  , Var "x"
  , App (Var "f") (Var "x")
  , lam (Just Unit) "x" (Var "x")
  , lam (Just Unit) "x" (Var "y")
  , lam (Just (Var "A")) "x" (Var "x")
  , lam (Just (Fun (Var "A") (Var "B"))) "x" (Var "x")
  ]
  where
    runExample :: (Int, Term') -> IO ()
    runExample (n, term) = do
      putStrLn ("Example #" <> show n <> ":")
      -- putStr   "[input term]:          "
      print term
      -- _ <- getLine
      -- putStr   "[with inferred types]: "
      case runTypeCheckOnce' (infer term) of
        Left err -> putStrLn ("Type Error: " <> show err)
        Right (typedTerm, typeInfo) -> do
          print typedTerm
          print typeInfo
      putStrLn ""
      _ <- getLine
      return ()

deriveBifunctor ''TypeInfo

deriveBifunctor ''TermF
deriveBifoldable ''TermF
deriveBitraversable ''TermF

deriveBifunctor ''TypedF
deriveBifoldable ''TypedF
deriveBitraversable ''TypedF
