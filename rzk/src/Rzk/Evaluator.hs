{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
module Rzk.Evaluator where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.List            ((\\))
import           Data.Maybe           (fromMaybe, isNothing)

import           Rzk.Syntax.Term
import           Rzk.Syntax.Var

-- | Evaluation errors.
data EvalError var
  = EvalErrorUndefinedVariable var
  deriving (Show)

-- | Evaluation context.
data Context var = Context
  { contextDefinedVariables :: [(var, Term var)]
    -- ^ Defined variables (variables that have terms assigned to them).
  , contextFreeVariables    :: [var]
    -- ^ Known free variables, used to avoid name clashes during substitution.
  }

-- | Empty evaluation context.
emptyContext :: Context var
emptyContext = Context
  { contextDefinedVariables = []
  , contextFreeVariables = []
  }

-- | Reassign term value to a variable.
updateVar :: var -> Term var -> Context var -> Context var
updateVar x t context@Context{..} = context
  { contextDefinedVariables = (x, t) : contextDefinedVariables }

addVar :: var -> Context var -> Context var
addVar x context@Context{..} = context
  { contextFreeVariables = x : contextFreeVariables }

-- | List all known variables (used to avoid name clashes after substitution).
contextKnownVars :: Context var -> [var]
contextKnownVars Context{..} = contextFreeVariables <> map fst contextDefinedVariables

-- | Evaluation with variables of type @var@.
newtype Eval var a = Eval { runEval :: ReaderT (Context var) (Except (EvalError var)) a }
  deriving (Functor, Applicative, Monad, MonadReader (Context var), MonadError (EvalError var))

-- | Lookup definition of a variable (if exists).
lookupVar :: Eq var => var -> Eval var (Maybe (Term var))
lookupVar x = do
  value <- asks (lookup x . contextDefinedVariables)
  when (isNothing value) $ do
    knownFreeVariable <- asks ((x `elem`) . contextFreeVariables)
    when (not knownFreeVariable) $ do
      throwError (EvalErrorUndefinedVariable x)
  return value

-- | Add definition of a variable locally during evaluation.
localVar :: MonadReader (Context var) m => (var, Term var) -> m a -> m a
localVar (x, t) = local (updateVar x t)

-- | Add a free variable locally during evaluation.
localFreeVar :: MonadReader (Context var) m => var -> m a -> m a
localFreeVar x = local (addVar x)

-- | Evaluate a closed term (all variables are bound).
evalClosed :: (Eq var, Enum var) => Term var -> Either (EvalError var) (Term var)
evalClosed = runExcept . flip runReaderT emptyContext . runEval . eval

-- | Find all freely occuring variables in a term.
freeVars :: Eq var => Term var -> [var]
freeVars = \case
  Variable x -> [x]
  Hole _ -> []
  Universe -> []
  Pi t -> freeVars t
  Lambda x a m -> freeVars a <> (freeVars m \\ [x])
  App t1 t2 -> freeVars t1 <> freeVars t2
  Sigma t -> freeVars t
  Pair t1 t2 -> freeVars t1 <> freeVars t2
  First t -> freeVars t
  Second t -> freeVars t
  IdType a x y -> freeVars a <> freeVars x <> freeVars y
  Refl a x -> freeVars a <> freeVars x
  IdJ tA a tC d x p -> concatMap freeVars [tA, a, tC, d, x, p]

-- | Evaluate an open term (some variables might occur freely).
evalOpen :: (Eq var, Enum var) => Term var -> Either (EvalError var) (Term var)
evalOpen t = go t
  where
    go = runExcept . flip runReaderT context . runEval . eval
    context = emptyContext { contextFreeVariables = freeVars t }

-- | Evaluate a term.
eval :: (Eq var, Enum var) => Term var -> Eval var (Term var)
eval = \case
  Variable x -> fromMaybe (Variable x) <$> lookupVar x
  Hole x -> pure (Hole x)
  Universe -> pure Universe
  Pi t -> Pi <$> eval t
  Lambda x a m -> do
    vars <- asks contextKnownVars
    let x' = refreshVar vars x
    if x `elem` vars
      then Lambda x' <$> eval a <*> localVar (x', Variable x') (eval (renameVar x x' m))
      else Lambda x  <$> eval a <*> localVar (x , Variable x ) (eval m)
  App t1 t2 -> join (app <$> eval t1 <*> eval t2)
  Sigma t -> Sigma <$> eval t
  Pair t1 t2 -> Pair <$> eval t1 <*> eval t2
  First t -> eval t >>= pure . \case
    Pair f _ -> f
    t'       -> First t'
  Second t -> eval t >>= pure . \case
    Pair _ s -> s
    t'       -> Second t'
  IdType a x y -> IdType <$> eval a <*> eval x <*> eval y
  Refl a x -> Refl <$> eval a <*> eval x
  IdJ tA a tC d x p -> eval p >>= \case
    Refl _ _ -> eval d
    p' -> IdJ <$> eval tA <*> eval a <*> eval tC <*> eval d <*> eval x <*> pure p'

-- | Evaluate application of one (evaluated) term to another.
app :: (Eq var, Enum var) => Term var -> Term var -> Eval var (Term var)
app (Lambda x _ m) n = localVar (x, n) (eval m)
app m n              = pure (App m n)

-- | Rename a (free) variable in a term.
--
-- >>> renameVar "x" "y" "x (λ(x : A) → x)" :: Term Var
-- y (λ(x : A) → x)
renameVar :: (Eq var) => var -> var -> Term var -> Term var
renameVar x x' = go
  where
    go t = case t of
      Variable z
        | z == x    -> Variable x'
        | otherwise -> t
      Hole z -> Hole z
      Universe -> Universe
      Pi t' -> Pi (go t')
      Lambda z a m
        | z == x  -> t
        | otherwise -> Lambda z (go a) (go m)
      App t1 t2 -> App (go t1) (go t2)
      Sigma t' -> Sigma (go t')
      Pair f s -> Pair (go f) (go s)
      First t' -> First (go t')
      Second t' -> Second (go t')
      IdType a z y -> IdType (go a) (go z) (go y)
      Refl a z -> Refl (go a) (go z)
      IdJ tA a tC d z p -> IdJ (go tA) (go a) (go tC) (go d) (go z) (go p)
