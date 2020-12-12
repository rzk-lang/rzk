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

import           Rzk.Debug.Trace

data TypeError var
  = TypeErrorInfinite var (Term var)
  | TypeErrorUnexpected (Term var) (Term var) (Term var) (Term var) (Term var)
  | TypeErrorUnexpectedLayer (Term var) (Term var) TypingLayer
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
  TypeErrorUnexpectedLayer t ty layer -> Text.intercalate "\n"
    [ "Wrong typing layer! Expected"
    , "  " <> Text.intercalate " or " (map ppTerm (layerExpected layer))
    , "but inferred"
    , "  " <> ppTerm ty
    , "for the term"
    , "  " <> ppTerm t
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
    [ "Expected a dependent pair (sum) type but got"
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
    [ "Term is expecting the following in the tope context"
    , "  " <> ppTerm phi
    , "but local tope context is"
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
  -- ^ An infinite stream of fresh type variable names.
  , contextLayer       :: TypingLayer
  }

data TypingLayer = CubeLayer | TopeLayer | TypeLayer
  deriving (Eq, Ord, Show)

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

emptyTypingContext :: Enum var => [var] -> TypingContext var
emptyTypingContext vars = TypingContext
  { contextKnownTypes   = []
  , contextKnownHoles   = []
  , contextHoles        = []
  , freshTypeVariables  = concat (drop 1 (iterate (map succ) vars))
  , contextLayer        = CubeLayer
  }

lookupTypeOf :: Eq var => var -> TypeCheck var (Maybe (Term var))
lookupTypeOf x = gets (lookup x . contextKnownTypes)

setTypeOf :: Eq var => var -> Term var -> TypeCheck var ()
setTypeOf x ty = modify $ \context -> context
  { contextKnownTypes = (x, ty) : contextKnownTypes context }

unsetTypeOf :: Eq var => var -> TypeCheck var ()
unsetTypeOf x = modify $ \context -> context
  { contextKnownTypes = filter ((/= x) . fst) (contextKnownTypes context) }

localTyping :: Eq var => (var, Term var) -> TypeCheck var a -> TypeCheck var a
localTyping (x, t) m = do
  setTypeOf x t
  oldContext <- get
  result <- localFreeVar x (local (\context -> context { contextDefinedVariables = contextKnownHoles oldContext <> contextDefinedVariables context }) m)
  unsetTypeOf x
  return result

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

evalType :: (Eq var, Enum var) => Term var -> TypeCheck var (Term var)
evalType t = evalInTypeCheck t (eval t)

evalInTypeCheck :: Term var -> Eval var a -> TypeCheck var a
evalInTypeCheck t e = do
  context <- ask
  case runExcept (runReaderT (runEval e) context) of
    Left err -> issueTypeError (TypeErrorEval t err)
    Right a  -> return a

issueTypeError :: TypeError var -> TypeCheck var a
issueTypeError err = do
  tyContext <- get
  context <- evalInTypeCheck undefined ask
  throwError TypeErrorWithContext
    { typeError = err
    , typeErrorTypingContext = tyContext
    , typeErrorContext = context
    }

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
    go = \case
      TopeOr phi psi -> do
        xs <- go phi
        ys <- go psi
        return (TopeOr <$> xs <*> ys)
      TopeAnd phi psi -> do
        xs <- go phi
        ys <- go psi
        return (TopeOr <$> xs <*> ys)
      psi@(App f x) -> do
        f' <- unfoldTopeWithInclusions f
        typeOf_f <- infer f
        case typeOf_f of
          Pi (Lambda t _i (Just phi) _a) -> do
          -- PiShape t _i phi _a -> do
            phi' <- localVar (t, x) $ evalType phi
            phi'' <- unfoldTopeWithInclusions phi'
            return (psi : phi'' <> f')
          _ -> return (psi : f')
      phi -> return [phi]

infer :: (Eq var, Enum var) => Term var -> TypeCheck var (Term var)
infer = \case
  Variable x    -> do
    mty <- lookupTypeOf x
    case mty of
      Nothing -> addTypeHoleFor x
      Just ty -> return ty
  TypedTerm term ty -> do
    typecheck term ty
    return ty
  Hole _        -> issueTypeError (TypeErrorOther "attemting to infer type of a hole!")
  Universe      -> pure Universe
  Pi t          -> inferTypeFamily t
--  PiShape t i phi a -> do
--    typecheck i Cube
--    localTyping (t, i) $ do
--      typecheck phi Tope
--      typecheck a Universe
--      return Universe
  t@(Lambda _ _ _ _) -> issueTypeError (TypeErrorCannotInferLambda t)
  term@(App t1 t2) -> do

    ty <- infer t1
    case ty of
      Pi f@(Lambda _ a Nothing _) -> do
        typecheck t2 a
        evalType (App f t2)
      Pi (Lambda t i (Just phi) a) -> do
        typecheck t2 i
        localVar (t, t2) $ do
          ensureTopeContext term phi
          evalType a
      ExtensionType t cI psi tA _phi _a -> do  -- FIXME: do we lose information?
        typecheck t2 cI
        localVar (t, t2) $ do
          psi' <- evalType psi
          unsafeTraceTerm "infer App ExtensionType" term $ ensureTopeContext term psi'
          evalType tA
--      PiShape t i psi a -> do
--        typecheck t2 i
--        localVar (t, t2) $ do
--          ensureTopeContext term psi
--          evalType a
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
      Sigma (Lambda _ a Nothing _) -> return a
      CubeProd i _j -> return i
      _ -> issueTypeError (TypeErrorNotAPair t ty (First t))
  Second t -> do
    ty <- infer t
    case ty of
      Sigma f@(Lambda _ a Nothing _) -> do
        x <- genFreshVar
        evalType (App (TypedTerm f (Pi (Lambda x a Nothing Universe))) (First t))
      CubeProd _i j -> return j
      _ -> issueTypeError (TypeErrorNotAPair t ty (Second t))

  IdType a x y -> do
    typecheck a Universe
    typecheck x a
    typecheck y a
    return Universe
  Refl a x -> do
    typecheck a Universe
    typecheck x a
    return (IdType a x x)
  IdJ tA a tC d x p -> do
    typecheck tA Universe
    typecheck a tA
    x' <- genFreshVar
    p' <- genFreshVar
    typecheck tC
      (Pi (Lambda x' tA Nothing
        (Pi (Lambda p' (IdType tA a (Variable x')) Nothing Universe))))
    typecheck d (App (App tC a) (Refl tA a))
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
    typeOf_a <- infer a
    typecheck b typeOf_a
    return typeOf_a

  ExtensionType t cI psi tA phi a -> do
    typecheck cI Cube
    localTyping (t, cI) $ do
      typecheck psi Tope
      localConstraint psi $ do
        typecheck tA Universe
        typecheck phi Tope
        unsafeTraceTerm "infer ExtensionType" phi $ ensureSubTope a psi phi
        localConstraint phi $ do
          typecheck a tA
          return Universe

ensureTopeContext :: (Eq var, Enum var) => Term var -> Term var -> TypeCheck var ()
ensureTopeContext term phi = do
  Context{..} <- ask
  contextTopes' <- concat <$> traverse unfoldTopeWithInclusions contextTopes
  unless (contextTopes' `entailTope` phi) $ do
    issueTypeError (TypeErrorTopeContextNotSatisfied term phi contextTopes)

ensureSubTope :: (Eq var, Enum var) => Term var -> Term var -> Term var -> TypeCheck var ()
ensureSubTope term psi phi = do
  Context{..} <- ask
  phi' <- concat <$> traverse unfoldTopeWithInclusions [phi]
  unless (phi' `entailTope` psi) $ do
    issueTypeError (TypeErrorTopeContextNotSatisfied term psi phi')

ensureEqTope :: (Eq var, Enum var) => Term var -> Term var -> TypeCheck var ()
ensureEqTope psi phi = do
  Context{..} <- ask
  phi' <- concat <$> traverse unfoldTopeWithInclusions [phi]
  psi' <- concat <$> traverse unfoldTopeWithInclusions [psi]
  unless (phi' `entailTope` psi) $ do
    issueTypeError (TypeErrorTopeContextNotSatisfied psi psi phi')
  unless (psi' `entailTope` phi) $ do
    issueTypeError (TypeErrorTopeContextNotSatisfied phi phi psi')

inferTypeFamily :: (Eq var, Enum var) => Term var -> TypeCheck var (Term var)
inferTypeFamily = \case
  Lambda x a Nothing m -> do
    typeOf_a <- infer a
    typecheck typeOf_a Universe
    localTyping (x, a) $
      typecheck m Universe
    pure Universe
  Lambda t i (Just phi) m -> do
    typecheck i Cube
    localTyping (t, i) $ do
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
        [ "Type error:"
        , ppTypeErrorWithContext err
        ]

ppTypeErrorWithContext :: TypeErrorWithContext Var -> Text
ppTypeErrorWithContext TypeErrorWithContext{..} = Text.intercalate "\n"
  [ ppTypeError typeError
  , ""
  , ppTypingContext typeErrorTypingContext
  , ""
  , ppContext typeErrorContext
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
      , contextLayer = CubeLayer
      }

typecheckClosed :: (Eq var, Enum var) => [var] -> Term var -> Term var -> TypeCheckResult var
typecheckClosed vars term
  = getTypeCheckResult emptyContext (emptyTypingContext vars)
  . typecheck term

runTypeCheckClosed :: (Eq var, Enum var) => [var] -> TypeCheck var () -> TypeCheckResult var
runTypeCheckClosed vars = getTypeCheckResult emptyContext (emptyTypingContext vars)

typecheck :: (Eq var, Enum var) => Term var -> Term var -> TypeCheck var ()
typecheck term expectedType =
  unsafeTraceTyping "typecheck" term expectedType $
  case (term, expectedType) of
    (Lambda y c (Just psi') m, ExtensionType t cI psi tA phi a) -> do
      typecheck c Cube
      unify (Variable y) c cI
      localTyping (y, cI) $ do
        psi'_e <- evalType psi'
        psi_e <- evalType (renameVar t y psi)
        ensureEqTope psi'_e psi_e
        localConstraint psi_e $ do
          typecheck m (renameVar t y tA)
          localConstraint (renameVar t y phi) $ do
            m' <- evalType m
            a' <- evalType (renameVar t y a)
            unify term m' a'

--    (Lambda y c  m, PiShape t i phi a) -> do
--      typecheck c Cube
--      unify (Variable y) c i
--      localTyping (y, i) $
--        localConstraint (renameVar t y phi) $
--          typecheck m (renameVar t y a)
--
    (Lambda y c Nothing m, Pi f@(Lambda _ a Nothing _)) -> do
      unify (Variable y) c a
      localTyping (y, a) $ do
        bodyType <- evalType (App f (Variable y))
        typecheck m bodyType
    (Lambda y c (Just phi) m, Pi f@(Lambda t a (Just psi) m')) -> do
      unify (Variable y) c a
      localTyping (y, a) $ do
        phi' <- evalType phi
        psi' <- evalType (renameVar t y psi)
        ensureEqTope phi' psi'
        localConstraint phi' $ do
          bodyType <- evalType (renameVar t y m')
          typecheck m bodyType
    (Lambda _ _ _ _, _) -> do
      issueTypeError (TypeErrorExpectedFunctionType term expectedType)
    (Pair f s, Sigma g@(Lambda _ a Nothing _)) -> do
      setLayer TypeLayer
      typecheck f a
      secondType <- unsafeTraceTerm "second" (App g f) $ evalType (App g f)
      typecheck s secondType
    (Variable x, ty) -> do
      evalInTypeCheck (Variable x) $ lookupVar x  -- FIXME: improve error message
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
--    go (PiShape t i phi a)
--      | x == t    = PiShape t <$> go i <*> pure phi <*> pure a
--      | otherwise = PiShape t <$> go i <*> go phi <*> go a

    go (Lambda y a phi b)
      | x == y = Lambda y <$> go a <*> pure phi <*> pure b
      | otherwise = Lambda y <$> go a <*> traverse go phi <*> go b

    go (App t1 t2) = App <$> go t1 <*> go t2

    go (Sigma t) = Pi <$> go t
    go (Pair f s) = Pair <$> go f <*> go s
    go (First t) = First <$> go t
    go (Second t) = Second <$> go t

    go (IdType a x' y') = IdType <$> go a <*> go x' <*> go y'
    go (Refl a x') = Refl <$> go a <*> go x'
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
      | x == t = ExtensionType t <$> go cI <*> pure psi <*> pure tA <*> pure phi <*> pure a
      | otherwise = ExtensionType t <$> go cI <*> go psi <*> go tA <*> go phi <*> go a

appExt :: (Eq var, Enum var) => Term var -> Term var -> TypeCheck var (Maybe (Term var))
appExt f x = do
  typeOf_f <- unsafeTraceTerm "appExt" (App f x) (infer f)
  case typeOf_f of
    ExtensionType t _I _psi _tA phi a ->
      localVar (t, x) $ do
        Context{..} <- ask
        phi' <- evalType phi
        if contextTopes `entailTope` phi'
           then unsafeTraceTyping "appExt replacing" (App f x) a $
             Just <$> evalType a
           else unsafeTraceTyping "appExt NOT replacing" (App f x) a $
            unsafeTraceTerm "appExt contextTopes" (foldl1 TopeAnd contextTopes) $
              unsafeTraceTerm "appExt phi" phi' $ pure Nothing
--    PiShape t i psi Tope ->
--      unsafeTraceTerm "ololo" (App f x) $
--      localVar (t, x) $ do
--        Context{..} <- ask
--        if contextTopes `entailTope` psi
--           then pure (Just TopeTop) -- FIXME: is it ok to always return Top?
--           else pure Nothing
    _ -> pure Nothing

unify :: (Eq var, Enum var) => Term var -> Term var -> Term var -> TypeCheck var ()
unify term t1 t2 = unsafeTraceTyping "unify" t1 t2 $ do
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
    unify' (TypedTerm t ty) (TypedTerm t' ty') = do
      unify' ty ty'
      unify' t t'
    unify' (TypedTerm t _ty) t'  = unify' t t'
    unify' t (TypedTerm t' _ty') = unify' t t'
    unify' Universe Universe = pure ()
    unify' (Pi t) (Pi t') = unify' t t'
    unify' (Lambda x a Nothing b) (Lambda y c Nothing d) = do
      unify' a c
      unify' b (renameVar y x d)
    unify' (Lambda x a (Just phi) b) (Lambda y c (Just psi) d) = do
      unify' a c
      localTyping (x, a) $ do
        phi' <- evalType phi
        psi' <- evalType (renameVar y x psi)
        ensureEqTope phi' psi'
        localConstraint phi' $ do
          unify' b (renameVar y x d)
    unify' tt1@(App u1 u2) tt2@(App v1 v2) = unsafeTraceTyping "unify' App App" tt1 tt2 $ do
      appExt u1 u2 >>= \case
        Nothing -> appExt v1 v2 >>= \case
          Nothing -> do
            unify' u1 v1
            unify' u2 v2
          Just tt2' -> unify' tt1 tt2'
        Just tt1' -> unify' tt1' tt2

    unify' (Sigma t) (Sigma t') = unify' t t'
    unify' (Pair f s) (Pair f' s') = do
      unify' f f'
      unify' s s'
    unify' (First t) (First t') = unify' t t'
    unify' (Second t) (Second t') = unify' t t'

    unify' (IdType a x y) (IdType a' x' y') = do
      unify' a a'
      unify' x x'
      unify' y y'
    unify' (Refl a x) (Refl a' x') = do
      unify' a a'
      unify' x x'
    unify' (IdJ tA a tC d x p) (IdJ tA' a' tC' d' x' p') = do
      unify' tA tA'
      unify' a a'
      unify' tC tC'
      unify' d d'
      unify' x x'
      unify' p p'

    unify' Cube Cube = return ()
    unify' CubeUnit CubeUnit = return ()
    unify' CubeUnitStar CubeUnitStar = return ()
    unify' (CubeProd i j) (CubeProd i' j') = do
      unify' i i'
      unify' j j'

    unify' Tope Tope = return ()
    unify' TopeTop TopeTop = return ()
    unify' TopeBottom TopeBottom = return ()
    unify' (TopeOr phi psi) (TopeOr phi' psi') = do
      ensureEqTope phi phi'
      ensureEqTope psi psi'
    unify' (TopeAnd phi psi) (TopeAnd phi' psi') = do
      ensureEqTope phi phi'
      ensureEqTope psi psi'
    unify' (TopeEQ t s) (TopeEQ t' s') = do
      unify' t t'
      unify' s s'

    unify' RecBottom RecBottom = return ()
    unify' RecBottom t = do
      ensureTopeContext t TopeBottom
    unify' t RecBottom = do
      ensureTopeContext t TopeBottom
    unify' (RecOr psi phi a b) (RecOr psi' phi' a' b') = do
      ensureEqTope psi psi'
      ensureEqTope phi phi'
      unify' a a'
      unify' b b'

    unify' tt1@(ExtensionType t cI psi tA phi a) tt2@(ExtensionType t' cI' psi' tA' phi' a') =
      unsafeTraceTyping "unify' ExtensionType ExtensionType" tt1 tt2 $ do
        unify' cI cI'
        localTyping (t', cI') $ do
          unify' (renameVar t t' psi) psi'
          localConstraint psi' $ do
            unify' (renameVar t t' tA)  tA'
            unify' (renameVar t t' phi) phi'
            localConstraint phi' $ do
              unify' (renameVar t t' a)   a'

    unify' (PiShape t i psi a) (PiShape t' i' psi' a') = do
      unify' i i'
      localTyping (t', i') $ do
        unify' (renameVar t t' psi) psi'
        localConstraint psi' $
          unify' (renameVar t t' a) a'

    -- unification by eta-expansion!
    unify' (Lambda x _a Nothing m) tt2 = do
      vars <- asks contextFreeVariables
      let x' = refreshVar (vars <> freeVars m <> freeVars tt2) x
      localVar (x', Variable x') $ do
        unify' (renameVar x x' m) (App tt2 (Variable x'))
    unify' tt1 (Lambda x a Nothing m) = do
      vars <- asks contextFreeVariables
      let x' = refreshVar (vars <> freeVars m <> freeVars tt1) x
      localTyping (x', a) $ do
        localVar (x', Variable x') $ do
          unify' (App tt1 (Variable x')) (renameVar x x' m)
    unify' (Lambda x _a (Just phi) m) tt2 = do
      vars <- asks contextFreeVariables
      let x' = refreshVar (vars <> freeVars m <> freeVars tt2) x
      localVar (x', Variable x') $ do
        phi' <- evalType (renameVar x x' phi)
        localConstraint phi' $ do
          unify' (renameVar x x' m) (App tt2 (Variable x'))
    unify' tt1 (Lambda x a (Just phi) m) = do
      vars <- asks contextFreeVariables
      let x' = refreshVar (vars <> freeVars m <> freeVars tt1) x
      localVar (x', Variable x') $ do
        phi' <- evalType (renameVar x x' phi)
        localConstraint phi' $ do
          unify' (renameVar x x' m) (App tt1 (Variable x'))

    unify' tt1 tt2 = issueTypeError (TypeErrorUnexpected term t1 t2 tt1 tt2)

layerOf :: (Eq var, Enum var) => Term var -> TypeCheck var TypingLayer
layerOf = \case
  Cube -> pure CubeLayer
  Tope -> pure TopeLayer
  Universe -> pure TypeLayer
  t -> unsafeTraceTerm "layerOf" t $ infer t >>= layerOf

layerExpected :: TypingLayer -> [Term var]
layerExpected = \case
  CubeLayer -> [Cube, Tope, Universe]
  TopeLayer -> [Tope, Universe]
  TypeLayer -> [Universe]

-- | FIXME: controlling layers should probably happen in evaluator?
inferWithLayer :: (Eq var, Enum var) => Term var -> TypeCheck var (Term var)
inferWithLayer t = do
  typeOf_t <- infer t
  layer <- gets contextLayer
  layerOf_t <- layerOf typeOf_t
  unless (layerOf_t >= layer) $ do
    issueTypeError (TypeErrorUnexpectedLayer t typeOf_t layer)
  when (layerOf_t > layer) $ do
    setLayer layerOf_t
  return typeOf_t

setLayer :: TypingLayer -> TypeCheck var ()
setLayer newLayer = modify (\context -> context { contextLayer = newLayer })
