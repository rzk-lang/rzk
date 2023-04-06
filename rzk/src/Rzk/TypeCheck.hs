{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Rzk.TypeCheck where

import Control.Monad.Reader
import Control.Monad.Except
import Data.List (tails, (\\), intercalate)

import Free.Scoped
import Language.Rzk.Free.Syntax
import qualified Language.Rzk.Syntax as Rzk

import Debug.Trace
import Unsafe.Coerce

data TypeError var
  = TypeErrorOther String
  | TypeErrorUnify (Term var) (TermT var) (TermT var)
  | TypeErrorUndefined var
  deriving (Functor, Foldable)

data TypeErrorInContext var = TypeErrorInContext
  { typeErrorError   :: TypeError var
  , typeErrorContext :: Context var
  } deriving (Functor, Foldable)

data TypeErrorInScopedContext var
  = PlainTypeError (TypeErrorInContext var)
  | ScopedTypeError (TypeErrorInScopedContext (Inc var))
  deriving (Functor, Foldable)

type TypeError' = TypeError Rzk.VarIdent

ppTypeError' :: TypeError' -> String
ppTypeError' = \case
  TypeErrorOther msg -> msg
  TypeErrorUnify term expected actual -> unlines
    [ "cannot unify expected type"
    , "  " <> show (untyped expected)
    , "with actual type"
    , "  " <> show (untyped actual)
    , "for term"
    , "  " <> show term ]
  TypeErrorUndefined var -> unlines
    [ "undefined variable: " <> show (Pure var :: Term') ]

ppTypeErrorInContext :: TypeErrorInContext Rzk.VarIdent -> String
ppTypeErrorInContext TypeErrorInContext{..} = intercalate "\n"
  [ ppTypeError' typeErrorError
  ]

ppTypeErrorInScopedContextWith'
  :: [Rzk.VarIdent]
  -> [Rzk.VarIdent]
  -> TypeErrorInScopedContext Rzk.VarIdent
  -> String
ppTypeErrorInScopedContextWith' used vars = \case
  PlainTypeError err -> ppTypeErrorInContext err
  ScopedTypeError err -> withFresh Nothing $ \(x, xs) ->
    ppTypeErrorInScopedContextWith' (x:used) xs $ fmap (g x) err
  where
    g x Z = x
    g _ (S y) = y

    withFresh Nothing f =
      case vars of
        x:xs -> f (x, xs)
        _ -> error "impossible: not enough fresh variables!"
    withFresh (Just z) f = f (z', filter (/= z') vars)    -- FIXME: very inefficient filter
      where
        z' = refreshVar used z

ppTypeErrorInScopedContext' :: TypeErrorInScopedContext Rzk.VarIdent -> String
ppTypeErrorInScopedContext' err = ppTypeErrorInScopedContextWith' vars (defaultVarIdents \\ vars) err
  where
    vars = foldMap pure err

issueTypeError :: TypeError var -> TypeCheck var a
issueTypeError err = do
  context <- ask
  throwError $ PlainTypeError $ TypeErrorInContext
    { typeErrorError = err
    , typeErrorContext = context
    }

data Action var
  = ActionTypeCheck (Term var) (TermT var)
  | ActionUnify (Term var) (TermT var) (TermT var)
  | ActionUnifyTerms (TermT var) (TermT var)
  | ActionInfer (Term var)
  deriving (Functor, Foldable)

type Action' = Action Rzk.VarIdent

ppAction :: Int -> Action' -> String
ppAction n = unlines . map (replicate (2 * n) ' ' <>) . \case
  ActionTypeCheck term ty ->
    [ "typechecking"
    , "  " <> show term
    , "against type"
    , "  " <> show (untyped ty) ]

  ActionUnify term expected actual ->
    [ "unifying expected type"
    , "  " <> show expected
    , "with actual type"
    , "  " <> show actual
    , "for term"
    , "  " <> show term ]

  ActionUnifyTerms expected actual ->
    [ "unifying term"
    , "  " <> show expected
    , "with term"
    , "  " <> show actual ]

  ActionInfer term ->
    [ "inferring type for term"
    , "  " <> show term ]

traceAction' :: Int -> Action' -> a -> a
traceAction' n action = trace ("[debug]\n" <> ppAction n action)

unsafeTraceAction' :: Int -> Action var -> a -> a
unsafeTraceAction' n = traceAction' n . unsafeCoerce

data Context var = Context
  { varTypes    :: [(var, TermT var)]
  , varValues   :: [(var, TermT var)]
  , localTopes  :: [TermT var]
  , actionStack :: [Action var]
  } deriving (Functor, Foldable)

emptyContext :: Context var
emptyContext = Context
  { varTypes = []
  , varValues = []
  , localTopes = []
  , actionStack = []
  }

type TypeCheck var = ReaderT (Context var) (Except (TypeErrorInScopedContext var))

entail :: Eq var => [TermT var] -> TermT var -> Bool
entail topes tope = tope `elem` topes

contextEntails :: Eq var => TermT var -> TypeCheck var ()
contextEntails tope = do
  topes <- asks localTopes
  unless (topes `entail` tope) $ do
    error "tope constraints are not satisfied"

enterScopeContext :: TermT var -> Context var -> Context (Inc var)
enterScopeContext ty Context{..} = Context
  { varTypes = (Z, S <$> ty) : [ (S x, fmap S t) | (x, t) <- varTypes ]
  , varValues = (Z, S <$> ty) : [ (S x, fmap S t) | (x, t) <- varValues ]
  , localTopes = map (fmap S) localTopes
  , actionStack = map (fmap S) actionStack
  }

enterScope :: TermT var -> TypeCheck (Inc var) b -> TypeCheck var b
enterScope ty action = do
  newContext <- asks (enterScopeContext ty)
  lift $ withExceptT ScopedTypeError $
    runReaderT action newContext

performing :: Action var -> TypeCheck var a -> TypeCheck var a
performing action tc = do
  Context{..} <- ask
  -- unsafeTraceAction' (length actionStack) action $
  local (const Context { actionStack = action : actionStack, .. }) $ tc

-- | Compute a typed term to its WHNF.
--
-- >>> whnfT "(\\p -> first (second p)) (x, (y, z))" :: Term'
-- y
whnfT :: TermT a -> TermT a
whnfT = \case
  AppT ty f x ->
    case whnfT f of
      LambdaT _ty _orig _arg body ->
        whnfT (substitute x body)
      f' -> AppT ty f' x
  FirstT ty t ->
    case whnfT t of
      PairT _ l _r -> whnfT l
      t' -> FirstT ty t'
  SecondT ty t ->
    case whnfT t of
      PairT _ _l r -> whnfT r
      t' -> SecondT ty t'
  IdJT ty tA a tC d x p ->
    case whnfT p of
      ReflT{} -> whnfT d
      ReflTermT{} -> whnfT d
      ReflTermTypeT{} -> whnfT d
      p' -> IdJT ty tA a tC d x p'
  TypeAscT _ty t _ty' -> whnfT t
  t -> t

typeOfVar :: Eq var => var -> TypeCheck var (TermT var)
typeOfVar x = asks (lookup x . varTypes) >>= \case
  Nothing -> issueTypeError $ TypeErrorUndefined x
  Just ty -> return ty

typeOf :: Eq var => TermT var -> TypeCheck var (TermT var)
typeOf = \case
  Pure x -> typeOfVar x
  Free (TypedF ty _) -> pure ty

unify :: Eq var => Term var -> TermT var -> TermT var -> TypeCheck var ()
unify term expected actual = performing (ActionUnify term expected actual) $ do
  unless (untyped (whnfT expected) == untyped (whnfT actual)) $
    issueTypeError (TypeErrorUnify term expected actual)

unifyTerms :: Eq var => TermT var -> TermT var -> TypeCheck var ()
unifyTerms expected actual = performing (ActionUnifyTerms expected actual) $ do
  unless (whnfT expected == whnfT actual) $
    error "unification failed"

localTope :: TermT var -> TypeCheck var a -> TypeCheck var a
localTope tope = local f
  where
    f Context{..} = Context{ localTopes = tope : localTopes, .. }

universeT :: TermT var
universeT = iterate UniverseT (error "weird: going too deep into universe type") !! 3

cubeT :: TermT var
cubeT = UniverseCubeT universeT

topeT :: TermT var
topeT = UniverseTopeT universeT

cubeUnitT :: TermT var
cubeUnitT = CubeUnitT cubeT

cubeUnitStarT :: TermT var
cubeUnitStarT = CubeUnitStarT cubeUnitT

cube2T :: TermT var
cube2T = Cube2T cubeT

cube2_0T :: TermT var
cube2_0T = Cube2_0T cube2T

cube2_1T :: TermT var
cube2_1T = Cube2_1T cube2T

topeBottomT :: TermT var
topeBottomT = TopeBottomT topeT

recBottomT :: TermT var
recBottomT = RecBottomT recBottomT

typecheck :: Eq var => Term var -> TermT var -> TypeCheck var (TermT var)
typecheck term ty = performing (ActionTypeCheck term ty) $ do
  case term of
    Pure x -> do
      unify term ty =<< typeOfVar x
      return (Pure x)

    Universe -> do
      unify term ty universeT
      return universeT

    _ -> do
      term' <- infer term
      inferredType <- typeOf term'
      unify term ty inferredType
      return term'

inferAs :: Eq var => TermT var -> Term var -> TypeCheck var (TermT var)
inferAs expectedKind term = do
  term' <- infer term
  ty <- typeOf term'
  kind <- typeOf ty
  unify (untyped ty) expectedKind kind
  return term'

infer :: Eq var => Term var -> TypeCheck var (TermT var)
infer tt = performing (ActionInfer tt) $ case tt of
  Pure x -> pure (Pure x)

  Universe     -> pure universeT
  UniverseCube -> pure cubeT
  UniverseTope -> pure topeT

  CubeUnit      -> pure cubeUnitT
  CubeUnitStar  -> pure cubeUnitStarT

  Cube2 -> pure cube2T
  Cube2_0 -> pure cube2_0T
  Cube2_1 -> pure cube2_1T

  CubeProduct l r -> do
    l' <- typecheck l cubeT
    r' <- typecheck r cubeT
    return (CubeProductT cubeT l' r')

  Pair l r -> do
    l' <- infer l
    r' <- infer r
    lt <- typeOf l'
    rt <- typeOf r'
    -- NOTE: infer as a non-dependent pair!
    return (PairT (TypeSigmaT universeT Nothing lt (S <$> rt)) l' r')

  First t -> do
    t' <- infer t
    typeOf t' >>= \case
      TypeSigmaT _ty _orig lt _rt ->
        return (FirstT lt t')
      CubeProductT _ty l _r ->
        return (FirstT l t')
      _ -> error "not a pair type"

  Second t -> do
    t' <- infer t
    typeOf t' >>= \case
      TypeSigmaT _ty _orig lt rt ->
        return (SecondT (substitute (FirstT lt t') rt) t')
      CubeProductT _ty _l r ->
        return (SecondT r t')
      _ -> error "not a pair type"

  TopeTop -> pure (TopeTopT topeT)
  TopeBottom -> pure (TopeBottomT topeT)

  TopeEQ l r -> do
    l' <- inferAs cubeT l
    lt <- typeOf l'
    r' <- typecheck r lt
    return (TopeEQT topeT l' r')

  TopeLEQ l r -> do
    l' <- typecheck l cube2T
    r' <- typecheck r cube2T
    return (TopeLEQT topeT l' r')

  TopeAnd l r -> do
    l' <- typecheck l topeT
    r' <- typecheck r topeT
    return (TopeAndT topeT l' r')

  TopeOr l r -> do
    l' <- typecheck l topeT
    r' <- typecheck r topeT
    return (TopeOrT topeT l' r')

  RecBottom -> do
    contextEntails topeBottomT
    return recBottomT

  RecOr [] -> do
    contextEntails topeBottomT
    return (RecOrT recBottomT [])

  RecOr rs -> do
    ttts <- forM rs $ \(tope, term) -> do
      tope' <- typecheck tope topeT
      term' <- inferAs universeT term
      ty <- typeOf term'
      return (tope', (term', ty))
    let rs' = map (fmap fst) ttts
        ts  = map (fmap snd) ttts
    sequence_ [ checkCoherence l r | l:rs'' <- tails rs', r <- rs'' ]
    return (RecOrT (RecOrT universeT ts) rs')

  TypeFun orig a Nothing b -> do
    a' <- inferAs universeT a  -- FIXME: separate universe of universes from universe of types
    b' <- enterScope a' $ inferAs universeT b
    return (TypeFunT universeT orig a' Nothing b')

  TypeFun orig cube (Just tope) ret -> do
    cube' <- typecheck cube cubeT
    enterScope cube' $ do
      tope' <- typecheck tope topeT
      ret' <- inferAs universeT ret
      return (TypeFunT universeT orig cube' (Just tope') ret')

  TypeSigma orig a b -> do
    a' <- inferAs universeT a  -- FIXME: separate universe of universes from universe of types
    b' <- enterScope a' $ inferAs universeT b
    return (TypeSigmaT universeT orig a' b')

  TypeId x tA y -> do
    tA' <- typecheck tA universeT
    x' <- typecheck x tA'
    y' <- typecheck y tA'
    return (TypeIdT universeT x' tA' y')

  TypeIdSimple x y -> do
    x' <- inferAs universeT x
    tA <- typeOf x'
    y' <- typecheck y tA
    return (TypeIdT universeT x' tA y')

  App f x -> do
    f' <- inferAs universeT f
    typeOf f' >>= \case
      TypeFunT _ty _orig a mtope b -> do
        x' <- typecheck x a
        mapM_ (contextEntails . substitute x') mtope
        return (AppT (substitute x' b) f' x')
      _ -> error "cannot apply a non-function"

  Lambda _orig Nothing _body -> do
    error "cannot infer type for the argument of a lambda abstraction"
  Lambda orig (Just (ty, Nothing)) body -> do
    ty' <- typecheck ty universeT
    enterScope ty' $ do
      body' <- infer body
      ret <- typeOf body' 
      return (LambdaT (TypeFunT universeT orig ty' Nothing ret) orig (Just (ty', Nothing)) body')
  Lambda orig (Just (cube, Just tope)) body -> do
    cube' <- typecheck cube universeT
    enterScope cube' $ do
      tope' <- infer tope
      body' <- localTope tope' $ infer body
      ret <- typeOf body'
      return (LambdaT (TypeFunT universeT orig cube' (Just tope') ret) orig (Just (cube', Just tope')) body')

  Refl -> error "cannot infer type of bare refl"
  ReflTerm x -> do
    x' <- inferAs universeT x
    ty <- typeOf x'
    return (ReflTermT (TypeIdT universeT x' ty x') x')
  ReflTermType x ty -> do
    ty' <- typecheck ty universeT
    x' <- typecheck x ty'
    return (ReflTermTypeT (TypeIdT universeT x' ty' x') x' ty')

  IdJ{} -> undefined

  TypeAsc term ty -> do
    ty' <- inferAs universeT ty
    term' <- typecheck term ty'
    return (TypeAscT ty' term' ty')

checkCoherence
  :: Eq var
  => (TermT var, TermT var)
  -> (TermT var, TermT var)
  -> TypeCheck var ()
checkCoherence (ltope, lterm) (rtope, rterm) = do
  localTope (TopeAndT topeT ltope rtope) $ do
    unifyTerms lterm rterm

inferStandalone :: Eq var => Term var -> Either (TypeErrorInScopedContext var) (TermT var)
inferStandalone term = runExcept (runReaderT (infer term) emptyContext)

unsafeInferStandalone' :: Term' -> TermT'
unsafeInferStandalone' t =
  case inferStandalone t of
    Left err -> error $ intercalate "\n"
      [ "Type Error:"
      , ppTypeErrorInScopedContext' err
      ]
    Right tt -> tt
