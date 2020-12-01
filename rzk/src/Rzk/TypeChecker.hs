{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
module Rzk.TypeChecker where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Text            (Text)
import qualified Data.Text            as Text

import           Rzk.Evaluator
import           Rzk.Pretty.Text
import           Rzk.Syntax.Decl
import           Rzk.Syntax.Module
import           Rzk.Syntax.Term
import           Rzk.Syntax.Var

data TypeError var
  = TypeErrorInfinite var (Term var)
  | TypeErrorUnexpected (Term var) (Term var) (Term var)
  | TypeErrorEval (Term var) (EvalError var)
  | TypeErrorOther Text
  | TypeErrorCannotInferLambda
  | TypeErrorNotAFunction (Term var) (Term var) (Term var)
  | TypeErrorExpectedFunctionType (Term var) (Term var)
  | TypeErrorInvalidTypeFamily

instance Show (TypeError Var) where
  show = Text.unpack . ppTypeError

ppTypeError :: TypeError Var -> Text
ppTypeError = \case
  TypeErrorInfinite x t -> Text.intercalate "\n"
    [ "Can't construct infinite type " <> ppVar x <> " ~ " <> ppTerm t ]
  TypeErrorUnexpected term inferred expected -> Text.intercalate "\n"
    [ "Expected type"
    , "  " <> ppTerm expected
    , "but inferred"
    , "  " <> ppTerm inferred
    , "for the term"
    , "  " <> ppTerm term
    ]
  TypeErrorEval t err -> Text.intercalate "\n"
    [ "Error occured while evaluating type"
    , "    " <> ppTerm t
    , Text.pack (show err) -- FIXME: pretty print
    ]
  TypeErrorOther msg -> "Error occurred in the typechecker: " <> msg
  TypeErrorCannotInferLambda ->
    "Error while attempting to infer the type for a lambda abstraction"
  TypeErrorNotAFunction f t e -> Text.intercalate "\n"
    [ "Expected a function type but got"
    , "  " <> ppTerm t
    , "for the term"
    , "  " <> ppTerm f
    , "in expression"
    , "  " <> ppTerm (App f e)
    ]
  TypeErrorExpectedFunctionType term expected -> Text.intercalate "\n"
    [ "Expected type is not a function type"
    , "  " <> ppTerm expected
    , "but the term is a lambda abstraction"
    , "   " <> ppTerm term
    ]
  TypeErrorInvalidTypeFamily -> "Expected a type family, but got something else" -- FIXME

data TypingContext var = TypingContext
  { contextKnownTypes  :: [(var, Term var)]
  -- ^ Types for free variables.
  , contextKnownHoles  :: [(var, Term var)]
  -- ^ Type variables and holes (partially instantiated).
  , contextHoles       :: [var]
  -- ^ Type variables and holes ever defined.
  , freshTypeVariables :: [var]
  -- ^ An infinite stream of fresh type variable names.
  }

ppTypingContext :: TypingContext Var -> Text
ppTypingContext TypingContext{..} = Text.intercalate "\n"
  [ "Free variables and their known types:"
  , ppKnownTypes contextKnownTypes
  , "Type holes and their instantiations:"
  , ppKnownHoles contextKnownHoles
  ]

ppKnownTypes :: [(Var, Term Var)] -> Text
ppKnownTypes = Text.intercalate "\n" . map ppVarType
  where
    ppVarType (var, ty) = "  " <> ppVar var <> " : " <> ppTerm ty

ppKnownHoles :: [(Var, Term Var)] -> Text
ppKnownHoles = Text.unlines . map ppHoleType
  where
    ppHoleType (hole, ty) = "  " <> ppHole hole <> " := " <> ppTerm ty

instance Show (TypingContext Var) where show = Text.unpack . ppTypingContext

emptyTypingContext :: [var] -> TypingContext var
emptyTypingContext vars = TypingContext
  { contextKnownTypes   = []
  , contextKnownHoles   = []
  , contextHoles        = []
  , freshTypeVariables  = vars
  }

lookupTypeOf :: Eq var => var -> TypeCheck var (Maybe (Term var))
lookupTypeOf x = gets (lookup x . contextKnownTypes)

localTyping :: Eq var => (var, Term var) -> TypeCheck var a -> TypeCheck var a
localTyping (x, t) m = do
  modify (\context -> context { contextKnownTypes = (x, t) : contextKnownTypes context })
  oldContext <- get
  result <- localFreeVar x (local (\context -> context { contextDefinedVariables = contextKnownHoles oldContext <> contextDefinedVariables context }) m)
  modify (\context -> context { contextKnownTypes = filter ((/= x) . fst) (contextKnownTypes context) })
  return result

newtype TypeCheck var a =  TypeCheck
  { runTypeCheck :: ReaderT (Context var) (ExceptT (TypeError var) (State (TypingContext var))) a
  } deriving (Functor, Applicative, Monad, MonadState (TypingContext var), MonadError (TypeError var), MonadReader (Context var))

instance MonadFail (TypeCheck var) where
  fail = throwError . TypeErrorOther . Text.pack

lookupHole :: Eq var => var -> TypeCheck var (Maybe (Term var))
lookupHole x = gets (lookup x . contextKnownHoles)

instantiateHole :: (Eq var, Enum var) => (var, Term var) -> TypeCheck var ()
instantiateHole (a, t) = do
  context <- get
  let holes = contextKnownHoles context
  newHoles <- forM holes $ \(hole, ty) -> do
    ty' <- localVar (a, t) $ evalType ty
    return $ (hole, ty')
  put context { contextKnownHoles = (a, t) : newHoles }

evalType :: (Eq var, Enum var) => Term var -> TypeCheck var (Term var)
evalType t = evalInTypeCheck t (eval t)

evalInTypeCheck :: Term var -> Eval var a -> TypeCheck var a
evalInTypeCheck t e = do
  context <- ask
  case runExcept (runReaderT (runEval e) context) of
    Left err -> throwError (TypeErrorEval t err)
    Right a  -> return a

genFreshHole :: TypeCheck var var
genFreshHole = do
  ctx@TypingContext{ freshTypeVariables = t:ts, .. } <- get
  put ctx { freshTypeVariables = ts, contextHoles = t : contextHoles }
  return t

addTypeHoleFor :: Eq var => var -> TypeCheck var (Term var)
addTypeHoleFor x = do
  mty <- lookupTypeOf x
  case mty of
    Just ty -> return ty
    Nothing -> Hole <$> genFreshHole

infer :: (Eq var, Enum var) => Term var -> TypeCheck var (Term var)
infer = \case
  Variable x    -> do
    mty <- lookupTypeOf x
    case mty of
      Nothing -> addTypeHoleFor x
      Just ty -> return ty
  Hole x        -> infer (Variable x)
  Universe      -> pure Universe
  Pi t          -> inferTypeFamily t
  Lambda _ _ _  -> throwError TypeErrorCannotInferLambda
  App t1 t2 -> do
    ty <- infer t1
    case ty of
      Pi f@(Lambda _ a _) -> do
        typecheck t2 a
        evalType (App f t2)
      _ -> throwError (TypeErrorNotAFunction t1 ty t2)

inferTypeFamily :: (Eq var, Enum var) => Term var -> TypeCheck var (Term var)
inferTypeFamily = \case
  Lambda x a m -> do
    typecheck a Universe
    localTyping (x, a) $ typecheck m Universe
    pure Universe
  _ -> throwError TypeErrorInvalidTypeFamily

data TypeCheckResult var = TypeCheckResult
  { typecheckResultErrors  :: Maybe (TypeError var)
  , typecheckResultContext :: TypingContext var
  }

instance Show (TypeCheckResult Var) where
  show = Text.unpack . ppTypeCheckResult

ppTypeCheckResult :: TypeCheckResult Var -> Text
ppTypeCheckResult TypeCheckResult{..} = Text.intercalate "\n"
  [ case typecheckResultErrors of
      Nothing -> "Everything is ok!"
      Just err -> Text.intercalate "\n"
        [ "Type error:"
        , ppTypeError err]
  , ""
  , ppTypingContext typecheckResultContext
  ]

getTypeCheckResult :: Context var -> TypingContext var -> TypeCheck var () -> TypeCheckResult var
getTypeCheckResult initialEvalContext initialTypingContext
  = mkTypeCheckResult
  . flip runState initialTypingContext
  . runExceptT
  . flip runReaderT initialEvalContext
  . runTypeCheck
  where
    mkTypeCheckResult (Left err, context) = TypeCheckResult (Just err) context
    mkTypeCheckResult (Right _, context)  = TypeCheckResult Nothing context

typecheckModule :: (Eq var, Enum var) => [var] -> Module var -> TypeCheckResult var
typecheckModule freshVars Module{..} = do
  getTypeCheckResult initialEvalContext initialTypingContext $
    forM_ moduleDecls $ \Decl{..} -> do
      ty <- evalType declType
      typecheck declBody ty
      modify (\context -> context { contextKnownTypes = (declName, ty) : contextKnownTypes context})
  where
    initialEvalContext = Context
      { contextDefinedVariables = map (\Decl{..} -> (declName, declBody)) moduleDecls
      , contextFreeVariables = map declName moduleDecls
      }
    initialTypingContext = TypingContext
      { contextKnownTypes = []
      , contextKnownHoles = []
      , contextHoles = []
      , freshTypeVariables = concat (drop 1 (iterate (map succ) freshVars))
      }

typecheckClosed :: (Eq var, Enum var) => [var] -> Term var -> Term var -> TypeCheckResult var
typecheckClosed vars term
  = getTypeCheckResult emptyContext (emptyTypingContext vars)
  . typecheck term

typecheck :: (Eq var, Enum var) => Term var -> Term var -> TypeCheck var ()
typecheck term expectedType =
  case (term, expectedType) of
    (Lambda y c m, Pi f@(Lambda _ a _)) -> do
      typecheck c Universe
      unify (Variable y) c a
      localTyping (y, a) $ do
        bodyType <- evalType (App f (Variable y))
        typecheck m bodyType
    (Lambda _ _ _, _) -> do
      throwError (TypeErrorExpectedFunctionType term expectedType)
    _ -> do
      inferredType <- infer term
      unify term inferredType expectedType

checkInfiniteType :: forall var. (Eq var, Enum var) => Term var -> var -> Term var -> TypeCheck var (Term var)
checkInfiniteType tt x = go
  where
    go :: Term var -> TypeCheck var (Term var)
    go Universe = pure Universe
    go t@(Variable _) = pure t
    go t@(Hole y)
      | x == y && tt == t = return t
      | x == y    = throwError (TypeErrorInfinite x tt)
      | otherwise = do
          yt <- lookupHole y
          case yt of
            Nothing -> return t
            Just t' -> do
              -- instantiateHole (y, t') in tt?
              go t'

    go (Pi t) = Pi <$> checkInfiniteType tt x t

    go (Lambda y a b)
      | x == y = Lambda y <$> go a <*> pure b
      | otherwise = do
          vars <- asks contextKnownVars
          let y' = refreshVar vars y
          Lambda y' <$> go a <*> localVar (y', Variable y') (go (renameVar y y' b))

    go (App t1 t2) = App <$> go t1 <*> go t2

unify :: (Eq var, Enum var) => Term var -> Term var -> Term var -> TypeCheck var ()
unify term t1 t2 = do
  TypingContext{..} <- get
  t1' <- evalType t1
  t2' <- evalType t2
  unify' t1' t2'
  where
    unify' (Hole x) (Hole y)
      | x == y = return ()
    unify' (Hole x) t = do
      mty <- lookupHole x
      case mty of
        Nothing -> do
          t' <- checkInfiniteType t x t
          instantiateHole (x, t')
        Just xty -> unify' xty t
    unify' t (Hole x) = unify' (Variable x) t

    unify' (Variable x) (Variable y) | x == y = pure ()
    unify' Universe Universe = pure ()
    unify' (Pi t) (Pi t') = unify' t t'
    unify' (Lambda x a b) (Lambda y c d) = do
      unify' a c
      unify' b (renameVar y x d)
    unify' (App u1 u2) (App v1 v2) = do
      unify' u1 v1
      unify' u2 v2
    unify' tt1 tt2 = throwError (TypeErrorUnexpected term tt1 tt2)

