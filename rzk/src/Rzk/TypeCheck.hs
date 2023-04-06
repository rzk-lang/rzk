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
  | TypeErrorUnifyTerms (TermT var) (TermT var)
  | TypeErrorNotPair (TermT var) (TermT var)
  | TypeErrorNotFunction (TermT var) (TermT var)
  | TypeErrorCannotInferBareLambda (Term var)
  | TypeErrorCannotInferBareRefl (Term var)
  | TypeErrorUndefined var
  | TypeErrorTopeNotSatisfied (TermT var)
  | TypeErrorTopesNotEquivalent (TermT var) (TermT var)
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
  TypeErrorUnifyTerms expected actual -> unlines
    [ "cannot unify term"
    , "  " <> show (untyped expected)
    , "with term"
    , "  " <> show (untyped actual) ]
  TypeErrorNotPair term ty -> unlines
    [ "expected a cube product or dependent pair"
    , "but got type"
    , "  " <> show (untyped ty)
    , "for term"
    , "  " <> show (untyped term)
    ]
  TypeErrorNotFunction term ty -> unlines
    [ "expected a function or extension type"
    , "but got type"
    , "  " <> show (untyped ty)
    , "for term"
    , "  " <> show (untyped term)
    ]
  TypeErrorCannotInferBareLambda term -> unlines
    [ "cannot infer the type of the argument"
    , "in lambda abstraction"
    , "  " <> show term
    ]
  TypeErrorCannotInferBareRefl term -> unlines
    [ "cannot infer the type of term"
    , "  " <> show term
    ]
  TypeErrorUndefined var -> unlines
    [ "undefined variable: " <> show (Pure var :: Term') ]
  TypeErrorTopeNotSatisfied tope -> unlines
    [ "cannot satisfy tope"
    , "  " <> show (untyped tope) ]
  TypeErrorTopesNotEquivalent expected actual -> unlines
    [ "expected tope"
    , "  " <> show (untyped expected)
    , "but got"
    , "  " <> show (untyped actual) ]

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
    issueTypeError $ TypeErrorTopeNotSatisfied tope

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

unifyTopes :: Eq var => TermT var -> TermT var -> TypeCheck var ()
unifyTopes l r =
  unless (untyped (whnfT l) == untyped (whnfT r)) $
    issueTypeError (TypeErrorTopesNotEquivalent l r)

unify :: Eq var => Maybe (Term var) -> TermT var -> TermT var -> TypeCheck var ()
unify mterm expected actual = performing action $ do
  case whnfT expected of
    Pure{} -> def

    UniverseT{} -> def
    UniverseCubeT{} -> def
    UniverseTopeT{} -> def

    CubeUnitT{} -> def
    CubeUnitStarT{} -> def
    Cube2T{} -> def
    Cube2_0T{} -> def
    Cube2_1T{} -> def
    CubeProductT _ l r ->
      case whnfT actual of
        RecBottomT{} -> return () -- unifies with anything
        CubeProductT _ l' r' -> do
          unify Nothing l l'
          unify Nothing r r'
        _ -> err

    PairT _ty l r ->
      case whnfT actual of
        RecBottomT{} -> return () -- unifies with anything
        PairT _ty' l' r' -> do
          unify Nothing l l'
          unify Nothing r r'
        _ -> err

    FirstT _ty t ->
      case whnfT actual of
        RecBottomT{} -> return () -- unifies with anything
        FirstT _ty' t' -> unify Nothing t t'
        _ -> err

    SecondT _ty t ->
      case whnfT actual of
        RecBottomT{} -> return () -- unifies with anything
        SecondT _ty' t' -> unify Nothing t t'
        _ -> err

    TopeTopT{}    -> unifyTopes expected actual
    TopeBottomT{} -> unifyTopes expected actual
    TopeEQT{}     -> unifyTopes expected actual
    TopeLEQT{}    -> unifyTopes expected actual
    TopeAndT{}    -> unifyTopes expected actual
    TopeOrT{}     -> unifyTopes expected actual

    RecBottomT{} -> return () -- unifies with anything
    RecOrT _ty rs ->
      forM_ rs $ \(tope, term) ->
        localTope tope $
          unify Nothing term actual

    TypeFunT _ty _orig cube mtope ret ->
      case whnfT actual of
        RecBottomT{} -> return () -- unifies with anything
        TypeFunT _ty' _orig' cube' mtope' ret' -> do
          unify Nothing cube cube'
          enterScope cube $ do
            case (mtope, mtope') of
              (Just tope, Just tope') -> unify Nothing tope tope'
              (Nothing, Nothing) -> return ()
              _ -> errS
            unify Nothing ret ret'
        _ -> err

    TypeSigmaT _ty _orig a b ->
      case whnfT actual of
        RecBottomT{} -> return () -- unifies with anything
        TypeSigmaT _ty' _orig' a' b' -> do
          unify Nothing a a'
          enterScope a $ unify Nothing b b'
        _ -> err

    TypeIdT _ty x _tA y ->
      case whnfT actual of
        RecBottomT{} -> return () -- unifies with anything
        TypeIdT _ty' x' _tA' y' -> do
          -- unify Nothing tA tA' -- TODO: do we need this check?
          unify Nothing x x'
          unify Nothing y y'
        _ -> err

    AppT _ty f x ->
      case whnfT actual of
        RecBottomT{} -> return () -- unifies with anything
        AppT _ty' f' x' -> do
          unify Nothing f f'
          unify Nothing x x'
        _ -> err

    LambdaT (TypeFunT _ty _origF param mtope _ret) _orig _mparam body ->
      case whnfT actual of
        RecBottomT{} -> return () -- unifies with anything
        LambdaT (TypeFunT _ty' _origF' param' mtope' _ret') _orig' _mparam' body' -> do
          unify Nothing param param'
          enterScope param $ do
            case (mtope, mtope') of
              (Just tope, Just tope') -> unify Nothing tope tope'
              (Nothing, Nothing) -> return ()
              _ -> errS
            unify Nothing body body'
        _ -> err
    LambdaT{} -> error "impossible: lambda with non-function type!"

    ReflT (TypeIdT _ty x _tA y) _x ->
      case whnfT actual of
        RecBottomT{} -> return () -- unifies with anything
        ReflT (TypeIdT _ty' x' _tA' y') _x' -> do
          -- unify Nothing tA tA' -- TODO: do we need this check?
          unify Nothing x x'
          unify Nothing y y'
        _ -> err
    ReflT{} -> error "impossible: refl with non-identity type!"

    IdJT _ty a b c d e f ->
      case whnfT actual of
        RecBottomT{} -> return () -- unifies with anything
        IdJT _ty' a' b' c' d' e' f' -> do
          unify Nothing a a'
          unify Nothing b b'
          unify Nothing c c'
          unify Nothing d d'
          unify Nothing e e'
          unify Nothing f f'
        _ -> err

    TypeAscT{} -> error "impossible: type ascription in WHNF!"

  where
    action = case mterm of
               Nothing -> ActionUnifyTerms expected actual
               Just term -> ActionUnify term expected actual
    def = unless (untyped (whnfT expected) == untyped (whnfT actual)) err
    err = case mterm of
            Nothing   -> issueTypeError (TypeErrorUnifyTerms expected actual)
            Just term -> issueTypeError (TypeErrorUnify term expected actual)
    errS =
      case mterm of
        Nothing   -> issueTypeError (TypeErrorUnifyTerms (S <$> expected) (S <$> actual))
        Just term -> issueTypeError (TypeErrorUnify (S <$> term) (S <$> expected) (S <$> actual))

unifyTypes :: Eq var => Term var -> TermT var -> TermT var -> TypeCheck var ()
unifyTypes = unify . Just

unifyTerms :: Eq var => TermT var -> TermT var -> TypeCheck var ()
unifyTerms = unify Nothing

localTope :: TermT var -> TypeCheck var a -> TypeCheck var a
localTope tope = local f
  where
    f Context{..} = Context{ localTopes = tope : localTopes, .. }

universeT :: TermT var
universeT = iterate UniverseT (error msg) !! 3
  where
    msg = "something bad happened: going too deep into a universe type"

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
      unifyTypes term ty =<< typeOfVar x
      return (Pure x)

    Universe -> do
      unifyTypes term ty universeT
      return universeT

    _ -> do
      term' <- infer term
      inferredType <- typeOf term'
      unifyTypes term ty inferredType
      return term'

inferAs :: Eq var => TermT var -> Term var -> TypeCheck var (TermT var)
inferAs expectedKind term = do
  term' <- infer term
  ty <- typeOf term'
  kind <- typeOf ty
  unifyTypes (untyped ty) expectedKind kind
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
      ty -> issueTypeError $ TypeErrorNotPair t' ty

  Second t -> do
    t' <- infer t
    typeOf t' >>= \case
      TypeSigmaT _ty _orig lt rt ->
        return (SecondT (substitute (FirstT lt t') rt) t')
      CubeProductT _ty _l r ->
        return (SecondT r t')
      ty -> issueTypeError $ TypeErrorNotPair t' ty

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

  RecOr rs -> do
    ttts <- forM rs $ \(tope, term) -> do
      tope' <- typecheck tope topeT
      term' <- inferAs universeT term
      ty <- typeOf term'
      return (tope', (term', ty))
    let rs' = map (fmap fst) ttts
        ts  = map (fmap snd) ttts
    sequence_ [ checkCoherence l r | l:rs'' <- tails rs', r <- rs'' ]
    contextEntails (foldr (TopeOrT topeT) topeBottomT (map fst ttts) )
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

  TypeId x (Just tA) y -> do
    tA' <- typecheck tA universeT
    x' <- typecheck x tA'
    y' <- typecheck y tA'
    return (TypeIdT universeT x' (Just tA') y')

  TypeId x Nothing y -> do
    x' <- inferAs universeT x
    tA <- typeOf x'
    y' <- typecheck y tA
    return (TypeIdT universeT x' (Just tA) y')

  App f x -> do
    f' <- inferAs universeT f
    typeOf f' >>= \case
      TypeFunT _ty _orig a mtope b -> do
        x' <- typecheck x a
        mapM_ (contextEntails . substitute x') mtope
        return (AppT (substitute x' b) f' x')
      ty -> issueTypeError $ TypeErrorNotFunction f' ty

  Lambda _orig Nothing _body -> do
    issueTypeError $ TypeErrorCannotInferBareLambda tt
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

  Refl Nothing -> issueTypeError $ TypeErrorCannotInferBareRefl tt
  Refl (Just (x, Nothing)) -> do
    x' <- inferAs universeT x
    ty <- typeOf x'
    return (ReflT (TypeIdT universeT x' (Just ty) x') (Just (x', Just ty)))
  Refl (Just (x, Just ty)) -> do
    ty' <- typecheck ty universeT
    x' <- typecheck x ty'
    return (ReflT (TypeIdT universeT x' (Just ty') x') (Just (x', Just ty')))

  IdJ tA a tC d x p -> do
    tA' <- typecheck tA universeT
    a' <- typecheck a tA'
    let typeOf_C =
          TypeFunT universeT Nothing tA' Nothing $
            TypeFunT universeT Nothing (TypeIdT universeT (S <$> a') (Just (S <$> tA')) (Pure Z)) Nothing $
              universeT
    tC' <- typecheck tC typeOf_C
    let typeOf_d =
          AppT universeT
            (AppT (TypeFunT universeT Nothing (TypeIdT universeT a' (Just tA') a') Nothing universeT)
              tC' a')
            (ReflT (TypeIdT universeT a' (Just tA') a') Nothing)
    d' <- typecheck d typeOf_d
    x' <- typecheck x tA'
    p' <- typecheck p (TypeIdT universeT a' (Just tA') x')
    let ret =
          AppT universeT
            (AppT (TypeFunT universeT Nothing (TypeIdT universeT a' (Just tA') x') Nothing universeT)
              tC' x')
            p'
    return (IdJT ret tA' a' tC' d' x' p')

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
