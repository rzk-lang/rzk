{-# LANGUAGE CPP           #-}
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

import           Control.Applicative  (liftA2, (<|>))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as Fail
#endif

import           Data.Foldable        (sequenceA_, traverse_)
import           Data.List            (nub)
import           Data.Text            (Text)
import qualified Data.Text            as Text

import           Rzk.Evaluator
import           Rzk.Pretty.Text
import           Rzk.Syntax.Decl
import           Rzk.Syntax.Module
import           Rzk.Syntax.Term
import           Rzk.Syntax.Var

data TypeCheckerAction var
  = ActionTypeCheck (Term var) (Term var)
  | ActionInferType (Term var)
  | ActionUnifyTypesFor (Term var) (Term var) (Term var)
  | ActionUnify (Term var) (Term var)
  | ActionEval (Term var)

data TypeError var
  = TypeErrorInfinite var (Term var)
  | TypeErrorUnexpected (Term var) (Term var) (Term var) (Term var) (Term var)
  | TypeErrorEval (Term var) (EvalError var)
  | TypeErrorOther Text
  | TypeErrorCannotInferLambda (Term var)
  | TypeErrorCannotInferPair (Term var)
  | TypeErrorNotAFunction (Term var) (Term var) (Term var)
  | TypeErrorNotAPair (Term var) (Term var) (Term var)
  | TypeErrorExpectedFunctionType (Term var) (Term var)
  | TypeErrorInvalidTypeFamily
  | TypeErrorTopeContextNotSatisfied (Term var) (Term var) [Term var]

instance Show (TypeError Var) where
  show = Text.unpack . ppTypeError

data TypeErrorWithContext var = TypeErrorWithContext
  { typeError              :: TypeError var
  , typeErrorTypingContext :: TypingContext var
  , typeErrorContext       :: Context var
  }

ppTypeError :: TypeError Var -> Text
ppTypeError = \case
  TypeErrorInfinite x t -> Text.intercalate "\n"
    [ "Can't construct infinite type " <> ppVar x <> " ~ " <> ppTerm t ]
  TypeErrorUnexpected term inferredFull expectedFull inferred expected -> Text.intercalate "\n"
    [ "Expected type"
    , "  " <> ppTerm expected
    , "but inferred"
    , "  " <> ppTerm inferred
    , "when trying to unify expected type"
    , "  " <> ppTerm expectedFull
    , "with inferred type"
    , "  " <> ppTerm inferredFull
    , "for the term"
    , "  " <> ppTerm term
    ]
  TypeErrorEval t err -> Text.intercalate "\n"
    [ "Error occured while evaluating type"
    , "    " <> ppTerm t
    , Text.pack (show err) -- FIXME: pretty print
    ]
  TypeErrorOther msg -> "Error occurred in the typechecker: " <> msg
  TypeErrorCannotInferLambda t -> Text.intercalate "\n"
    [ "Error while attempting to infer the type for a lambda abstraction"
    , "  " <> ppTerm t
    ]
  TypeErrorCannotInferPair t -> Text.intercalate "\n"
    [ "Error while attempting to infer the type for a dependent tuple"
    , "  " <> ppTerm t
    ]
  TypeErrorNotAFunction f t e -> Text.intercalate "\n"
    [ "Expected a function type but got"
    , "  " <> ppTerm t
    , "for the term"
    , "  " <> ppTerm f
    , "in expression"
    , "  " <> ppTerm (App f e)
    ]
  TypeErrorNotAPair f t e -> Text.intercalate "\n"
    [ "Expected a dependent pair (sum) type or a cube product but got"
    , "  " <> ppTerm t
    , "for the term"
    , "  " <> ppTerm f
    , "in expression"
    , "  " <> ppTerm e
    ]
  TypeErrorExpectedFunctionType term expected -> Text.intercalate "\n"
    [ "Expected type is not a function type"
    , "  " <> ppTerm expected
    , "but the term is a lambda abstraction"
    , "   " <> ppTerm term
    ]
  TypeErrorInvalidTypeFamily -> "Expected a type family, but got something else" -- FIXME
  TypeErrorTopeContextNotSatisfied term phi topes -> Text.intercalate "\n"
    [ "Cannot satisfy the tope constraint:"
    , "  " <> ppTerm phi
    , "in local tope context"
    , Text.intercalate "\n" (map (("  " <>) . ppTerm) topes)
    , "when typechecking term"
    , "  " <> ppTerm term
    ]

data TypingContext var = TypingContext
  { contextKnownTypes  :: [(var, Term var)]
  -- ^ Types for free variables.
  , contextKnownHoles  :: [(var, Term var)]
  -- ^ Type variables and holes (partially instantiated).
  , contextHoles       :: [var]
  -- ^ Type variables and holes ever defined.
  , freshTypeVariables :: [var]
  -- ^ Action stack (for error messages and debugging).
  , actionStack        :: [TypeCheckerAction var]
  }

ppTypingContext :: TypingContext Var -> Text
ppTypingContext TypingContext{..} = Text.intercalate "\n"
  [ "Free variables and their known types:"
  , ppKnownTypes contextKnownTypes
  , "Type holes and their instantiations:"
  , ppKnownHoles contextKnownHoles
  , ppActionStack actionStack
  ]

ppActionStack :: [TypeCheckerAction Var] -> Text
ppActionStack = Text.intercalate "\n" . map ppAction . reverse

ppAction :: TypeCheckerAction Var -> Text
ppAction = \case
  ActionTypeCheck term expectedType -> Text.intercalate "\n"
    [ "when trying to typecheck"
    , "  " <> ppTerm term
    , "against type"
    , "  " <> ppTerm expectedType
    ]
  ActionInferType term -> Text.intercalate "\n"
    [ "when trying to infer the type of"
    , "  " <> ppTerm term
    ]
  ActionUnifyTypesFor term type1 type2 -> Text.intercalate "\n"
    [ "when trying to unify expected type"
    , "  " <> ppTerm type1
    , "with inferred type"
    , "  " <> ppTerm type2
    , "for the term"
    , "  " <> ppTerm term
    ]
  ActionUnify term1 term2 -> Text.intercalate "\n"
    [ "when trying to unify term"
    , "  " <> ppTerm term1
    , "with term"
    , "  " <> ppTerm term2
    ]
  ActionEval term -> Text.intercalate "\n"
    [ "when trying to evaluate type/term"
    , "  " <> ppTerm term
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

emptyTypingContext :: Enum var => [var] -> TypingContext var
emptyTypingContext vars = TypingContext
  { contextKnownTypes   = []
  , contextKnownHoles   = []
  , contextHoles        = []
  , freshTypeVariables  = concat (drop 1 (iterate (map succ) vars))
  , actionStack         = []
  }

lookupTypeOf :: Eq var => var -> TypeCheck var (Maybe (Term var))
lookupTypeOf x = gets (lookup x . contextKnownTypes)

setTypeOf :: Eq var => var -> Term var -> TypeCheck var ()
setTypeOf x ty = modify $ \context -> context
  { contextKnownTypes = (x, ty) : contextKnownTypes context }

unsetTypeOf :: Eq var => var -> TypeCheck var ()
unsetTypeOf x = modify $ \context -> context
  { contextKnownTypes = filter ((/= x) . fst) (contextKnownTypes context) }

localAction :: TypeCheckerAction var -> TypeCheck var a -> TypeCheck var a
localAction action m = do
  modify (\context -> context { actionStack = action : actionStack context })
  result <- m
  modify (\context -> context { actionStack = drop 1 (actionStack context) })
  return result

localTyping :: Eq var => (var, Maybe (Term var)) -> TypeCheck var a -> TypeCheck var a
localTyping (x, t) m = do
  traverse_ (setTypeOf x) t
  oldContext <- get
  result <- localFreeVar x (local (\context -> context { contextDefinedVariables = contextKnownHoles oldContext <> contextDefinedVariables context }) m)
  unsetTypeOf x
  return result

localPattern' :: (Term var, Term var) -> TypeCheck var a -> TypeCheck var a
localPattern' pt m = runExceptT (localPattern pt (lift m)) >>= \case
  Left err -> issueTypeError (TypeErrorEval (fst pt) err)
  Right x  -> return x

localPatternTyping
  :: Eq var => (Term var, Maybe (Term var)) -> TypeCheck var a -> TypeCheck var a
localPatternTyping = \case
  (Variable x, t) -> localTyping (x, t)
  (Pair x y, Nothing) ->
    localPatternTyping (y, Nothing) . localPatternTyping (x, Nothing)
  (Pair x y, Just (Sigma g@(Lambda _ (Just a) Nothing _))) ->
    localPatternTyping (y, Just (App g x)) . localPatternTyping (x, Just a)
  (Pair x y, Just (CubeProd i j)) ->
    localPatternTyping (y, Just j) . localPatternTyping (x, Just i)
  -- TODO: more fine-grained errors
  (pattern, _) -> const $ issueTypeError (TypeErrorEval pattern (EvalErrorInvalidPattern pattern))

newtype TypeCheck var a =  TypeCheck
  { runTypeCheck :: ReaderT (Context var) (ExceptT (TypeErrorWithContext var) (State (TypingContext var))) a
  } deriving (Functor, Applicative, Monad, MonadState (TypingContext var), MonadError (TypeErrorWithContext var), MonadReader (Context var))

instance MonadFail (TypeCheck var) where
  fail = issueTypeError . TypeErrorOther . Text.pack

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

evalExtensionApps :: (Eq var, Enum var) => Term var -> TypeCheck var (Term var)
evalExtensionApps = go
  where
    go = \case
      App t1 t2 -> do
        case t1 of
          Lambda _ _ _ _ -> App <$> go t1 <*> go t2
          _ -> do
            -- FIXME: instead of inferring info one more time,
            -- we need to use already inferred information
            (fmap stripExplicitTypeAnnotations <$> inferLenient t1) >>= \ty -> case ty of
              Just (ExtensionType s _ _ _ phi a) -> do
                Context{..} <- ask
                localPattern' (s, t2) $ do
                  phi' <- evalType phi
                  contextTopes' <- unfoldTopes' contextTopes
                  if contextTopes' `entailTope` phi'
                    then evalType a >>= go
                    else App <$> go t1 <*> go t2   -- FIXME: bring outside localVar?
              _ -> App <$> go t1 <*> go t2

      TypedTerm t a -> TypedTerm <$> go t <*> pure a
      Pi t -> Pi <$> go t
      Sigma t -> Sigma <$> go t

      Lambda x a Nothing m -> do
        a' <- traverse go a
        localPatternTyping (x, a) $ do
          m' <- go m
          return (Lambda x a' Nothing m')
      Lambda x a (Just phi) m -> do
        a' <- traverse go a
        localPatternTyping (x, a) $ do
          phi' <- go phi
          localConstraint phi' $ do
            m' <- go m
            return (Lambda x a' (Just phi') m')

      Pair f s -> Pair <$> go f <*> go s
      First t -> First <$> go t
      Second t -> Second <$> go t

      IdType a x y -> IdType <$> go a <*> go x <*> go y
      Refl a x -> Refl <$> traverse go a <*> go x
      IdJ tA a tC d x p -> IdJ <$> go tA <*> go a <*> go tC <*> go d <*> go x <*> go p

      CubeProd x y -> CubeProd <$> go x <*> go y
      TopeOr x y -> TopeOr <$> go x <*> go y
      TopeAnd x y -> TopeAnd <$> go x <*> go y
      TopeEQ x y -> TopeEQ <$> go x <*> go y

      RecOr psi phi a b -> RecOr <$> go psi <*> go phi <*> go a <*> go b
      ExtensionType t i psi tA phi a -> do
        i' <- go i
        localPatternTyping (t, Just i) $ do
          psi' <- go psi
          localConstraint psi' $ do
            tA' <- go tA
            phi' <- go phi
            localConstraint phi' $ do
              a' <- go a
              return (ExtensionType t i' psi' tA' phi' a')

      t@(Variable _) -> pure t
      t@(Hole _) -> pure t
      t@Cube -> pure t
      t@CubeUnit -> pure t
      t@CubeUnitStar -> pure t
      t@Tope -> pure t
      t@TopeTop -> pure t
      t@TopeBottom -> pure t
      t@Universe -> pure t
      t@RecBottom -> pure t
      t@Cube2 -> pure t
      t@Cube2_0 -> pure t
      t@Cube2_1 -> pure t
      t@(TopeLEQ _ _) -> pure t

evalType :: (Eq var, Enum var) => Term var -> TypeCheck var (Term var)
evalType t = localAction (ActionEval t) $ do
  t' <- evalInTypeCheck t (eval t)
  t'' <- evalExtensionApps t'
  evalInTypeCheck t (eval t'')

evalInTypeCheck :: Term var -> Eval var a -> TypeCheck var a
evalInTypeCheck t e = do
  context <- ask
  case runExcept (runReaderT (runEval e) context) of
    Left err -> issueTypeError (TypeErrorEval t err)
    Right a  -> return a

typecheckInEval :: TypingContext var -> TypeCheck var a -> Eval var (Maybe a)
typecheckInEval tyContext m = do
  context <- ask
  case evalState (runExceptT (runReaderT (runTypeCheck m) context)) tyContext of
    Left _err -> return Nothing
    Right x   -> return (Just x)

issueTypeError :: TypeError var -> TypeCheck var a
issueTypeError err = do
  tyContext <- get
  context <- ask
  throwError TypeErrorWithContext
    { typeError = err
    , typeErrorTypingContext = tyContext
    , typeErrorContext = context
    }

issueTypeError_ :: TypeError var -> TypeCheck var ()
issueTypeError_ = issueTypeError

genFreshVar :: TypeCheck var var
genFreshVar = do
  ctx@TypingContext{ freshTypeVariables = t:ts, .. } <- get
  put ctx { freshTypeVariables = ts }
  return t

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

unfoldTopeWithInclusions :: (Eq var, Enum var) => Term var -> TypeCheck var [Term var]
unfoldTopeWithInclusions = go
  where
    go = fmap nub . \case
      TopeOr phi psi -> do
        xs <- go phi
        ys <- go psi
        return (TopeOr <$> xs <*> ys)
      TopeAnd phi psi -> do
        xs <- go phi
        ys <- go psi
        return (TopeAnd <$> xs <*> ys)
      psi@(App f x) -> do
        f' <- map (`App` x) <$> go f
        typeOf_f <- infer f
        case typeOf_f of
          Pi (Lambda t _i (Just phi) _a) -> do
            phi' <- localPattern' (t, x) $ evalType phi
            phi'' <- go phi'
            return (psi : phi'' <> f')
          _ -> return (psi : f')
      phi -> return [phi]

unfoldTopes' :: (Eq var, Enum var) => [Term var] -> TypeCheck var [Term var]
unfoldTopes' topes = concat <$>
  traverse unfoldTopeWithInclusions topes

inferLenient :: (Eq var, Enum var) => Term var -> TypeCheck var (Maybe (Term var))
inferLenient term = do
  context <- get
  result <- (Just <$> infer term) `catchError` \_ -> return Nothing
  put context
  return result

infer :: (Eq var, Enum var) => Term var -> TypeCheck var (Term var)
infer term = localAction (ActionInferType term) $ ($ term) $ \case
  Variable x    -> do
    mty <- lookupTypeOf x
    case mty of
      Nothing -> addTypeHoleFor x
      Just ty -> return ty
  TypedTerm term' ty -> do
    typecheck term' ty
    evalType ty
  Hole _        -> issueTypeError (TypeErrorOther "attemting to infer type of a hole!")
  Universe      -> pure Universe
  Pi t          -> inferTypeFamily t
  t@(Lambda _ _ _ _) -> issueTypeError (TypeErrorCannotInferLambda t)
  term'@(App t1 t2) -> do

    ty <- infer t1
    case ty of
      TypedTerm ty' _ -> do
        infer (App (TypedTerm t1 ty') t2)
      Pi f@(Lambda _ (Just a) Nothing _) -> do
        typecheck t2 a
        evalType (App f t2)
      Pi (Lambda t (Just i) (Just phi) a) -> do
        typecheck t2 i
        localPattern' (t, t2) $ do
          phi' <- evalType phi
          ensureTopeContext term' phi'
          evalType a
      ExtensionType t cI psi tA _phi _a -> do  -- FIXME: do we lose information?
        typecheck t2 cI
        localPattern' (t, t2) $ do
          psi' <- evalType psi
          ensureTopeContext term' psi'
          evalType tA
      _ -> issueTypeError (TypeErrorNotAFunction t1 ty t2)
  Sigma t -> inferTypeFamily t
  t@(Pair f s) -> do
    i <- infer f
    typeOf_i <- infer i
    case typeOf_i of
      Cube -> do
        j <- infer s
        typecheck j Cube
        return (CubeProd i j)
      _ -> issueTypeError (TypeErrorCannotInferPair t)
  First t -> do
    ty <- infer t
    case ty of
      Sigma (Lambda _ (Just a) Nothing _) -> return a
      CubeProd i _j -> return i
      _ -> issueTypeError (TypeErrorNotAPair t ty (First t))
  Second t -> do
    ty <- infer t
    case ty of
      Sigma f@(Lambda _ a Nothing _) -> do
        x <- genFreshVar
        evalType (App (TypedTerm f (Pi (Lambda (Variable x) a Nothing Universe))) (First t))
      CubeProd _i j -> return j
      _ -> issueTypeError (TypeErrorNotAPair t ty (Second t))

  IdType a x y -> do
    typecheck a Universe
    typecheck x a
    typecheck y a
    return Universe
  Refl a x -> do
    typeof_x <- case a of
      Just a' -> do
        typecheck a' Universe
        typecheck x a'
        return a'
      Nothing -> infer x
    return (IdType typeof_x x x)
  IdJ tA a tC d x p -> do
    typecheck tA Universe
    typecheck a tA
    x' <- genFreshVar
    p' <- genFreshVar
    typecheck tC
      (Pi (Lambda (Variable x') (Just tA) Nothing
        (Pi (Lambda (Variable p') (Just (IdType tA a (Variable x'))) Nothing Universe))))
    typecheck d (App (App tC a) (Refl (Just tA) a))
    typecheck x tA
    typecheck p (IdType tA a x)
    evalType (App (App tC x) p)

  Cube -> pure Universe -- FIXME: issueTypeError (TypeErrorOther "attempting to infer a type for CUBE")
  CubeUnit -> pure Cube
  CubeUnitStar -> pure CubeUnit
  CubeProd i j -> do
    typecheck i Cube
    typecheck j Cube
    return Cube

  Tope -> pure Universe -- FIXME: issueTypeError (TypeErrorOther "attempting to infer a type for TOPE")

  TopeTop -> pure Tope
  TopeBottom -> pure Tope
  TopeOr psi phi -> do
    typecheck psi Tope
    typecheck phi Tope
    return Tope
  TopeAnd psi phi -> do
    typecheck psi Tope
    typecheck phi Tope
    return Tope
  TopeEQ t s -> do
    typeOf_t <- infer t
    typecheck typeOf_t Cube
    typecheck s typeOf_t
    return Tope

  t@RecBottom -> do
    ensureTopeContext t TopeBottom
    Hole <$> genFreshHole
  t@(RecOr psi phi a b) -> do
    typecheck psi Tope
    typecheck phi Tope
    ensureTopeContext t (TopeOr psi phi)
    typeOf_a <- localConstraint psi $ infer a
    typeOf_b <- localConstraint phi $ infer b
    localConstraint (TopeAnd psi phi) $ do
      unify t typeOf_a typeOf_b
      unify t a b
    return (RecOr psi phi typeOf_a typeOf_b)

  ExtensionType t cI psi tA phi a -> do
    typecheck cI Cube
    localPatternTyping (t, Just cI) $ do
      psi' <- evalType psi
      typecheck psi' Tope
      localConstraint psi' $ do
        tA' <- evalType tA
        typecheck tA' Universe
        phi' <- evalType phi
        typecheck phi' Tope
        ensureSubTope a psi' phi'
        localConstraint phi' $ do
          a' <- evalType a
          Context{..} <- ask
          typecheck a' tA'
          return Universe

  Cube2 -> pure Cube
  Cube2_0 -> pure Cube2
  Cube2_1 -> pure Cube2
  TopeLEQ t s -> do
    typecheck t Cube2
    typecheck s Cube2
    return Tope

ensureTopeContext :: (Eq var, Enum var) => Term var -> Term var -> TypeCheck var ()
ensureTopeContext term phi = do
  Context{..} <- ask
  contextTopes' <- unfoldTopes' contextTopes
  unless (contextTopes' `entailTope` phi) $ do
    issueTypeError (TypeErrorTopeContextNotSatisfied term phi contextTopes)

ensureSubTope :: (Eq var, Enum var) => Term var -> Term var -> Term var -> TypeCheck var ()
ensureSubTope term psi phi = do
  Context{..} <- ask
  phi' <- unfoldTopes' [phi]
  unless (phi' `entailTope` psi) $ do
    issueTypeError (TypeErrorTopeContextNotSatisfied term psi phi')

ensureEqTope :: (Eq var, Enum var) => Term var -> Term var -> TypeCheck var ()
ensureEqTope psi phi = do
  Context{..} <- ask
  phi' <- unfoldTopes' (phi : contextTopes)
  psi' <- unfoldTopes' (psi : contextTopes)
  unless (phi' `entailTope` psi) $ do
    issueTypeError (TypeErrorTopeContextNotSatisfied psi psi phi')
  unless (psi' `entailTope` phi) $ do
    issueTypeError (TypeErrorTopeContextNotSatisfied phi phi psi')

inferTypeFamily :: (Eq var, Enum var) => Term var -> TypeCheck var (Term var)
inferTypeFamily = \case
  Lambda x (Just a) Nothing m -> do
    typeOf_a <- infer a
    typecheck typeOf_a Universe
    localPatternTyping (x, Just a) $
      typecheck m Universe
    pure Universe
  Lambda t (Just i) (Just phi) m -> do
    typecheck i Cube
    localPatternTyping (t, Just i) $ do
      typecheck phi Tope
      phi' <- evalType phi
      localConstraint phi' $
        typecheck m Universe
    pure Universe
  _ -> issueTypeError TypeErrorInvalidTypeFamily

data TypeCheckResult var = TypeCheckResult
  { typecheckResultErrors  :: Maybe (TypeErrorWithContext var)
  , typecheckResultContext :: TypingContext var
  }

instance Show (TypeCheckResult Var) where
  show = Text.unpack . ppTypeCheckResult

ppTypeCheckResult :: TypeCheckResult Var -> Text
ppTypeCheckResult TypeCheckResult{..} =
  case typecheckResultErrors of
      Nothing -> Text.intercalate "\n"
        [ ppTypingContext typecheckResultContext
        , ""
        , "Everything is ok!"
        ]
      Just err -> Text.intercalate "\n"
        [ ppTypeErrorWithContext err
        , ""
        , "Failed to typecheck due to a type error!"
        ]

ppTypeErrorWithContext :: TypeErrorWithContext Var -> Text
ppTypeErrorWithContext TypeErrorWithContext{..} = Text.intercalate "\n" $ reverse
  [ ppTypeError typeError
  , ""
  , ppContextTopes (contextTopes typeErrorContext)
  , ""
  , ppTypingContext typeErrorTypingContext
  , ""
  , ppContext typeErrorContext
  ]

ppContextTopes :: [Term Var] -> Text
ppContextTopes topes = Text.intercalate "\n"
  [ "Local tope context:"
  , Text.intercalate "\n" (map (("  " <>) . ppTerm) topes)
  ]


ppContext :: Context Var -> Text
ppContext Context{..} = Text.intercalate "\n"
  [ "Defined variables:"
  , Text.intercalate "\n" (map ppDef contextDefinedVariables)
  ]
    where
      ppDef (x, t) = ppVar x <> " := " <> ppTerm t

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
      , contextTopes = []
      , contextTopeInclusions = []
      }
    initialTypingContext = TypingContext
      { contextKnownTypes = []
      , contextKnownHoles = []
      , contextHoles = []
      , freshTypeVariables = concat (drop 1 (iterate (map succ) freshVars))
      , actionStack = []
      }

typecheckClosed :: (Eq var, Enum var) => [var] -> Term var -> Term var -> TypeCheckResult var
typecheckClosed vars term
  = getTypeCheckResult emptyContext (emptyTypingContext vars)
  . typecheck term

runTypeCheckClosed :: (Eq var, Enum var) => [var] -> TypeCheck var () -> TypeCheckResult var
runTypeCheckClosed vars = getTypeCheckResult emptyContext (emptyTypingContext vars)

typecheck :: (Eq var, Enum var) => Term var -> Term var -> TypeCheck var ()
typecheck term expectedType = localAction (ActionTypeCheck term expectedType) $
  case (term, expectedType) of
    (Lambda y c psi' m, ExtensionType t cI psi tA phi a) -> do
      case c of
        Just c' -> do
          typecheck c' Cube
          unify y c' cI
        Nothing -> return ()
      localPatternTyping (y, Just cI) $ do
        let psi'' = case (c, psi') of
                      (Nothing, Nothing) -> psi       -- if no type/tope was specified in a lambda binding then assume tope from the type of lambda expression
                      (_, Nothing)       -> TopeTop   -- if only type was specified, then tope is defaulted to TOP
                      (_, Just psi'_)    -> psi'_     -- otherwise we use specified tope
        psi'_e <- evalType psi''
        psi_e <- localPattern' (t, y) $ evalType psi
        ensureEqTope psi'_e psi_e
        localConstraint psi_e $ do
          tA' <- localPattern' (t, y) $ evalType tA
          typecheck m tA'
          phi_e <- localPattern' (t, y) $ evalType phi
          localConstraint phi_e $ do
            m' <- evalType m
            a' <- localPattern' (t, y) $ evalType a
            unify term m' a'

    (Lambda y c Nothing m, Pi f@(Lambda _ (Just a) Nothing _)) -> do
      case c of
        Just c' -> unify y c' a
        Nothing -> return ()
      localPatternTyping (y, Just a) $ do
        bodyType <- evalType (App f y)
        typecheck m bodyType
    (Lambda y c (Just phi) m, Pi (Lambda t (Just a) (Just psi) m')) -> do
      case c of
        Just c' -> unify y c' a
        Nothing -> return ()
      localPatternTyping (y, Just a) $ do
        phi' <- evalType phi
        psi' <- localPattern' (t, y) $ evalType psi
        ensureEqTope phi' psi'
        localConstraint phi' $ do
          bodyType <- localPattern' (t, y) $ evalType m'
          typecheck m bodyType
    (Lambda _ _ _ _, _) -> do
      issueTypeError (TypeErrorExpectedFunctionType term expectedType)
    (Pair f s, Sigma g@(Lambda _ (Just a) Nothing _)) -> do
      typecheck f a
      secondType <- evalType (App g f)
      typecheck s secondType
    (Variable x, ty) -> do
      _ <- evalInTypeCheck (Variable x) $ lookupVar x  -- FIXME: improve error message
      mty <- lookupTypeOf x
      case mty of
        Nothing  -> setTypeOf x ty
        Just xty -> unify (Variable x) xty ty
    (Hole x, ty) -> do
      mty <- lookupTypeOf x
      case mty of
        Nothing  -> setTypeOf x ty
        Just xty -> unify (Variable x) xty ty
    _ -> do
      inferredType <- infer term
      unify term inferredType expectedType

checkInfiniteType :: forall var. (Eq var, Enum var) => Term var -> var -> Term var -> TypeCheck var (Term var)
checkInfiniteType tt x = go
  where
    go :: Term var -> TypeCheck var (Term var)
    go Universe = pure Universe
    go t@(Variable _) = pure t
    go (TypedTerm term ty) = TypedTerm <$> go term <*> go ty
    go t@(Hole y)
      | x == y && tt == t = return t
      | x == y    = issueTypeError (TypeErrorInfinite x tt)
      | otherwise = do
          yt <- lookupHole y
          case yt of
            Nothing -> return t
            Just t' -> do
              -- instantiateHole (y, t') in tt?
              go t'

    go (Pi t) = Pi <$> go t

    go (Lambda y a phi b)
      | x `elem` freeVars y = Lambda y <$> traverse go a <*> pure phi <*> pure b
      | otherwise = Lambda y <$> traverse go a <*> traverse go phi <*> go b

    go (App t1 t2) = App <$> go t1 <*> go t2

    go (Sigma t) = Pi <$> go t
    go (Pair f s) = Pair <$> go f <*> go s
    go (First t) = First <$> go t
    go (Second t) = Second <$> go t

    go (IdType a x' y') = IdType <$> go a <*> go x' <*> go y'
    go (Refl a x') = Refl <$> traverse go a <*> go x'
    go (IdJ tA a tC d x' p) = IdJ <$> go tA <*> go a <*> go tC <*> go d <*> go x' <*> go p

    go Cube = pure Cube
    go CubeUnit = pure CubeUnit
    go CubeUnitStar = pure CubeUnitStar
    go (CubeProd i j) = CubeProd <$> go i <*> go j

    go Tope = pure Tope
    go TopeTop = pure TopeTop
    go TopeBottom = pure TopeBottom
    go (TopeOr psi phi) = TopeOr <$> go psi <*> go phi
    go (TopeAnd psi phi) = TopeAnd <$> go psi <*> go phi
    go (TopeEQ t s) = TopeEQ <$> go t <*> go s

    go RecBottom = pure RecBottom
    go (RecOr psi phi a b) = RecOr <$> go psi <*> go phi <*> go a <*> go b

    go (ExtensionType t cI psi tA phi a)
      | x `elem` freeVars t = ExtensionType t <$> go cI <*> pure psi <*> pure tA <*> pure phi <*> pure a
      | otherwise = ExtensionType t <$> go cI <*> go psi <*> go tA <*> go phi <*> go a

    go Cube2 = pure Cube2
    go Cube2_0 = pure Cube2_0
    go Cube2_1 = pure Cube2_1
    go (TopeLEQ t s) = TopeLEQ <$> go t <*> go s

appExt :: (Eq var, Enum var) => Term var -> Term var -> TypeCheck var (Maybe (Term var))
appExt f x = do
  typeOf_f <- infer f
  case typeOf_f of
    ExtensionType t _I _psi _tA phi a ->
      localPattern' (t, x) $ do
        Context{..} <- ask
        phi' <- evalType phi
        contextTopes' <- unfoldTopes' contextTopes
        if contextTopes' `entailTope` phi'
           then Just <$> evalType a
           else pure Nothing
    _ -> pure Nothing

unify :: (Eq var, Enum var) => Term var -> Term var -> Term var -> TypeCheck var ()
unify term t1 t2 = localAction (ActionUnifyTypesFor term t1 t2) $ do
  TypingContext{..} <- get
  t1' <- evalType t1
  t2' <- evalType t2
  unify' t1' t2'
  where
    unify' tt1 tt2 = localAction (ActionUnify tt1 tt2) $ do
      Context{..} <- ask
      contextTopes' <- unfoldTopes' contextTopes
      if contextTopes' `entailTope` TopeBottom
         then return () -- anything unifies in an empty context
         else unify'' tt1 tt2

    unify'' (Hole x) (Hole y)
      | x == y = return ()
    unify'' (Hole x) t = do
      mty <- lookupHole x
      case mty of
        Nothing -> do
          t' <- checkInfiniteType t x t
          instantiateHole (x, t')
        Just xty -> unify' xty t
    unify'' t (Hole x) = unify' (Variable x) t

    unify'' (Variable x) (Variable y) | x == y = pure ()
    unify'' (TypedTerm t ty) (TypedTerm t' ty') = do
      unify' ty ty'
      unify' t t'
    unify'' (TypedTerm t _ty) t'  = unify' t t'
    unify'' t (TypedTerm t' _ty') = unify' t t'
    unify'' Universe Universe = pure ()
    unify'' (Pi t) (Pi t') = unify' t t'
    unify'' (Lambda x a Nothing b) (Lambda y c Nothing d) = do
      sequenceA_ (liftA2 unify' a c)
      d' <- localVars (freeVars x) $ localPattern' (y, x) $ evalType d
      localPatternTyping (x, a) $ do
        unify' b d'
    unify'' (Lambda x a (Just phi) b) (Lambda y c (Just psi) d) = do
      sequenceA_ (liftA2 unify' a c)
      localPatternTyping (x, a <|> c) $ do
        phi' <- evalType phi
        psi' <- localPattern' (y, x) $ evalType psi
        ensureEqTope phi' psi'
        localConstraint phi' $ do
          b' <- evalType b
          d' <- localPattern' (y, x) $ evalType d
          unify' b' d'
    unify'' tt1@(App u1 u2) tt2@(App v1 v2) = do
      appExt u1 u2 >>= \case
        Nothing -> appExt v1 v2 >>= \case
          Nothing -> do
            unify' u1 v1
            unify' u2 v2
          Just tt2' -> unify' tt1 tt2'
        Just tt1' -> unify' tt1' tt2

    unify'' (Sigma t) (Sigma t') = unify' t t'
    unify'' (Pair f s) (Pair f' s') = do
      unify' f f'
      unify' s s' -- FIXME: double check (do we need to adjust types?)
    unify'' (First t) (First t') = unify' t t'
    unify'' (Second t) (Second t') = unify' t t'

    unify'' (IdType a x y) (IdType a' x' y') = do
      unify' a a'
      unify' x x'
      unify' y y'
    unify'' (Refl a x) (Refl a' x') = do
      sequenceA_ (liftA2 unify' a a')
      unify' x x'
    unify'' (IdJ tA a tC d x p) (IdJ tA' a' tC' d' x' p') = do
      unify' tA tA'
      unify' a a'
      unify' tC tC'
      unify' d d'
      unify' x x'
      unify' p p'

    unify'' Cube Cube = return ()
    unify'' CubeUnit CubeUnit = return ()
    unify'' CubeUnitStar CubeUnitStar = return ()
    unify'' (CubeProd i j) (CubeProd i' j') = do
      unify' i i'
      unify' j j'

    unify'' Tope Tope = return ()
    unify'' TopeTop TopeTop = return ()
    unify'' TopeBottom TopeBottom = return ()
    unify'' (TopeOr phi psi) (TopeOr phi' psi') = do
      ensureEqTope phi phi'
      ensureEqTope psi psi'
    unify'' (TopeAnd phi psi) (TopeAnd phi' psi') = do
      ensureEqTope phi phi'
      ensureEqTope psi psi'
    unify'' (TopeEQ t s) (TopeEQ t' s') = do
      unify' t t'
      unify' s s'

    unify'' Cube2 Cube2 = return ()
    unify'' Cube2_0 Cube2_0 = return ()
    unify'' Cube2_1 Cube2_1 = return ()
    unify'' (TopeLEQ t s) (TopeLEQ t' s') = do
      unify' t t'
      unify' s s'

    unify'' RecBottom RecBottom = return ()
    unify'' RecBottom t = 
      ensureTopeContext t TopeBottom
    unify'' t RecBottom = do
      ensureTopeContext t TopeBottom
    unify'' (RecOr psi phi a b) r = do
      localConstraint psi $ do
        a' <- evalType a
        r' <- evalType r
        unify' a' r'
      localConstraint phi $ do
        b' <- evalType b
        r' <- evalType r
        unify' b' r'
    unify'' l (RecOr psi phi a b) = do
      localConstraint psi $ do
        a' <- evalType a
        l' <- evalType l
        unify' l' a'
      localConstraint phi $ do
        b' <- evalType b
        l' <- evalType l
        unify' l' b'

    unify'' (ExtensionType t cI psi tA phi a) (ExtensionType t' cI' psi' tA' phi' a') = do
      unify' cI cI'
      localPatternTyping (t', Just cI') $ do
        psi_ <- localPattern' (t, t') $ evalType psi
        psi'_ <- localPattern' (t, t') $ evalType psi'
        unify' psi_ psi'_
        localConstraint psi' $ do
          tA_ <- localPattern' (t, t') $ evalType tA
          tA'_ <- evalType tA'
          unify' tA_ tA'_
          phi_ <- localPattern' (t, t') $ evalType phi
          phi'_ <- evalType phi'
          unify' phi_ phi'_
          localConstraint phi' $ do
            a_ <- localPattern' (t, t') $ evalType a
            a'_ <- evalType a'
            unify' a_ a'_

    -- unification by eta-expansion!
    unify'' (Lambda x a Nothing m) tt2 = do
      vars <- asks contextFreeVariables
      let xs = freeVars x
          doRename = any (`elem` vars) xs
          xxs' = refreshVars (vars <> allVars m) xs
          rename = if doRename then renameVars xxs' else id
          xs' = if doRename then map snd xxs' else xs
          x' = rename x
      localPatternTyping (x', a) $ do
        localVars xs' $ do
          m' <- localPattern' (x, x') $ evalType m
          unify' m' (App tt2 x')
    unify'' tt1 (Lambda x a Nothing m) = do
      vars <- asks contextFreeVariables
      let xs = freeVars x
          doRename = any (`elem` vars) xs
          xxs' = refreshVars (vars <> allVars m) xs
          rename = if doRename then renameVars xxs' else id
          xs' = if doRename then map snd xxs' else xs
          x' = rename x
      localPatternTyping (x', a) $ do
        localVars xs' $ do
          m' <- localPattern' (x, x') $ evalType m
          unify' (App tt1 x') m'
    unify'' (Lambda x a (Just phi) m) tt2 = do
      vars <- asks contextFreeVariables
      let xs = freeVars x
          doRename = any (`elem` vars) xs
          xxs' = refreshVars (vars <> allVars m <> allVars phi) xs
          rename = if doRename then renameVars xxs' else id
          xs' = if doRename then map snd xxs' else xs
          x' = rename x
      localPatternTyping (x', a) $ do
        localVars xs' $ do
          phi' <- localPattern' (x, x') $ evalType phi
          localConstraint phi' $ do
            tt1' <- localPattern' (x, x') $ evalType m
            let tt2' = App tt2 x'
            unify' tt1' tt2'
    unify'' tt1 (Lambda x a (Just phi) m) = do
      vars <- asks contextFreeVariables
      let xs = freeVars x
          doRename = any (`elem` vars) xs
          xxs' = refreshVars (vars <> allVars m <> allVars phi) xs
          rename = if doRename then renameVars xxs' else id
          xs' = if doRename then map snd xxs' else xs
          x' = rename x
      localPatternTyping (x', a) $ do
        localVars xs' $ do
          phi' <- localPattern' (x, x') $ evalType phi
          localConstraint phi' $ do
            tt1' <- localPattern' (x, x') $ evalType m
            let tt2' = (App tt1 x')
            unify' tt1' tt2'

    -- unification by eta-expansion for pairs
    unify'' (Pair f s) tt2 = do
      unify' f (First  tt2)
      unify' s (Second tt2)
    unify'' tt1 (Pair f s) = do
      unify' (First  tt1) f
      unify' (Second tt1) s

    unify'' tt1 tt2 = do
      typeOf_tt1 <- stripExplicitTypeAnnotations <$> infer tt1
      case typeOf_tt1 of
        ExtensionType (Variable s) i psi _tA _phi _a -> do -- FIXME: make it work for patterns
          vars <- asks contextFreeVariables
          let s' = refreshVar (vars <> allVars i) s
          localTyping (s', Just i) $ do
            psi' <- localVar (s, Variable s') $ evalType psi
            localConstraint psi' $ do
              -- issueTypeError_ (TypeErrorUnexpected term t1 t2 tt1 tt2) -- FIXME: dead code
              unify' (App tt1 (Variable s')) (App tt2 (Variable s'))
        _ -> issueTypeError (TypeErrorUnexpected term t1 t2 tt1 tt2)

stripExplicitTypeAnnotations :: Term var -> Term var
stripExplicitTypeAnnotations = \case
  TypedTerm t _ -> stripExplicitTypeAnnotations t
  t -> t
