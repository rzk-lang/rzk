{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
module Rzk.TypeCheck where

import Control.Monad.Reader
import Control.Monad.Except
import Data.List (tails, (\\), intercalate)
import Data.Maybe (fromMaybe)

import Free.Scoped
import Language.Rzk.Free.Syntax
import qualified Language.Rzk.Syntax as Rzk

import Debug.Trace
import Unsafe.Coerce

defaultTypeCheck
  :: TypeCheck Rzk.VarIdent a
  -> Either (TypeErrorInScopedContext Rzk.VarIdent) a
defaultTypeCheck tc = runExcept (runReaderT tc emptyContext)

typecheckModule :: Rzk.Module -> TypeCheck Rzk.VarIdent ()
typecheckModule (Rzk.Module _lang commands) = go 1 commands
  where
    totalCommands = length commands

    go :: Integer -> [Rzk.Command] -> TypeCheck Rzk.VarIdent ()
    go _i [] = return ()
    go  i (Rzk.CommandDefine name ty term : moreCommands) =
      trace ("[ " <> show i <> " out of " <> show totalCommands <> " ]"
          <> " Checking #def " <> show (Pure name :: Term') ) $ do
        ty' <- typecheck (toTerm' ty) universeT
        term' <- typecheck (toTerm' term) ty'
        localDecl name ty' term' $
          go (i + 1) moreCommands

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
  | ScopedTypeError (Maybe Rzk.VarIdent) (TypeErrorInScopedContext (Inc var))
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
  [ ppContext' typeErrorContext
  , ppTypeError' typeErrorError
  ]

ppTypeErrorInScopedContextWith'
  :: [Rzk.VarIdent]
  -> [Rzk.VarIdent]
  -> TypeErrorInScopedContext Rzk.VarIdent
  -> String
ppTypeErrorInScopedContextWith' used vars = \case
  PlainTypeError err -> ppTypeErrorInContext err
  ScopedTypeError orig err -> withFresh orig $ \(x, xs) ->
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
    , "  " <> show (untyped expected)
    , "with actual type"
    , "  " <> show (untyped actual)
    , "for term"
    , "  " <> show term ]

  ActionUnifyTerms expected actual ->
    [ "unifying term"
    , "  " <> show (untyped expected)
    , "with term"
    , "  " <> show (untyped actual) ]

  ActionInfer term ->
    [ "inferring type for term"
    , "  " <> show term ]

traceAction' :: Int -> Action' -> a -> a
traceAction' n action = trace ("[debug]\n" <> ppAction n action)

unsafeTraceAction' :: Int -> Action var -> a -> a
unsafeTraceAction' n = traceAction' n . unsafeCoerce

data Context var = Context
  { varTypes    :: [(var, TermT var)]
  , varValues   :: [(var, Maybe (TermT var))]
  , varOrigs    :: [Maybe Rzk.VarIdent]
  , localTopes  :: [TermT var]
  , actionStack :: [Action var]
  } deriving (Functor, Foldable)

emptyContext :: Context var
emptyContext = Context
  { varTypes = []
  , varValues = []
  , varOrigs = []
  , localTopes = []
  , actionStack = []
  }

ppContext' :: Context Rzk.VarIdent -> String
ppContext' Context{..} = unlines
  [ "Definitions in context:"
  , unlines
      [ show (Pure x :: Term') <> " : " <> show (untyped ty)
      | (x, ty) <- reverse varTypes ]
  , unlines
      [ show (Pure x :: Term') <> " = " <> show (untyped term)
      | (x, Just term) <- reverse varValues ]
  , intercalate "\n" (map (("when " <>) . ppAction 0) (reverse actionStack))
  , "Local tope context:"
  , unlines (map show localTopes)
  ]

localDecl :: var -> TermT var -> TermT var -> TypeCheck var a -> TypeCheck var a
localDecl x ty term = local $ \Context{..} -> Context
  { varTypes = (x, ty) : varTypes
  , varValues = (x, Just term) : varValues
  , .. }

type TypeCheck var = ReaderT (Context var) (Except (TypeErrorInScopedContext var))

entail :: Eq var => [TermT var] -> TermT var -> Bool
entail topes tope = tope `elem` topes

contextEntails :: Eq var => TermT var -> TypeCheck var ()
contextEntails tope = do
  topes <- asks localTopes
  unless (topes `entail` tope) $ do
    issueTypeError $ TypeErrorTopeNotSatisfied tope

enterScopeContext :: Maybe Rzk.VarIdent -> TermT var -> Context var -> Context (Inc var)
enterScopeContext orig ty Context{..} = Context
  { varTypes = (Z, S <$> ty) : [ (S x, fmap S t) | (x, t) <- varTypes ]
  , varValues = (Z, Nothing) : [ (S x, fmap S <$> t) | (x, t) <- varValues ]
  , varOrigs = orig : varOrigs
  , localTopes = map (fmap S) localTopes
  , actionStack = map (fmap S) actionStack
  }

enterScope :: Maybe Rzk.VarIdent -> TermT var -> TypeCheck (Inc var) b -> TypeCheck var b
enterScope orig ty action = do
  newContext <- asks (enterScopeContext orig ty)
  lift $ withExceptT (ScopedTypeError orig) $
    runReaderT action newContext

performing :: Action var -> TypeCheck var a -> TypeCheck var a
performing action tc = do
  Context{..} <- ask
  -- unsafeTraceAction' (length actionStack) action $
  local (const Context { actionStack = action : actionStack, .. }) $ tc

etaMatch :: Eq var => TermT var -> TermT var -> TypeCheck var (TermT var, TermT var)
etaMatch expected@LambdaT{} actual@LambdaT{} = pure (expected, actual)
etaMatch expected@PairT{}   actual@PairT{}   = pure (expected, actual)
etaMatch expected@LambdaT{} actual = do
  actual' <- etaExpand actual
  pure (expected, actual')
etaMatch expected actual@LambdaT{} = do
  expected' <- etaExpand expected
  pure (expected', actual)
etaMatch expected@PairT{} actual = do
  actual' <- etaExpand actual
  pure (expected, actual')
etaMatch expected actual@PairT{} = do
  expected' <- etaExpand expected
  pure (expected', actual)
etaMatch expected actual = pure (expected, actual)

etaExpand :: Eq var => TermT var -> TypeCheck var (TermT var)
etaExpand term@LambdaT{} = pure term
etaExpand term@PairT{} = pure term
etaExpand term = do
  typeOf term >>= \case
    ty@(TypeFunT _ty orig param mtope ret) -> pure $
      LambdaT ty orig (Just (param, mtope))
        (AppT ret (S <$> term) (Pure Z))

    ty@(TypeSigmaT _ty _orig a b) -> pure $
      PairT ty
        (FirstT a term)
        (SecondT (substitute (FirstT a term) b) term)

    ty@(CubeProductT _ty a b) -> pure $
      PairT ty
        (FirstT a term)
        (SecondT b term)

    _ -> pure term

-- | Compute a typed term to its WHNF.
--
-- >>> whnfT "(\\p -> first (second p)) (x, (y, z))" :: Term'
-- y
whnfT :: Eq var => TermT var -> TypeCheck var (TermT var)
whnfT = \case
  t@(Pure var) ->
    valueOfVar var >>= \case
      Nothing -> pure t
      Just term -> whnfT term
  AppT ty f x ->
    whnfT f >>= \case
      LambdaT _ty _orig _arg body ->
        whnfT (substitute x body)
      f' -> pure (AppT ty f' x)
  FirstT ty t ->
    whnfT t >>= \case
      PairT _ l _r -> whnfT l
      t' -> pure (FirstT ty t')
  SecondT ty t ->
    whnfT t >>= \case
      PairT _ _l r -> whnfT r
      t' -> pure (SecondT ty t')
  IdJT ty tA a tC d x p ->
    whnfT p >>= \case
      ReflT{} -> whnfT d
      p' -> pure (IdJT ty tA a tC d x p')
  TypeAscT _ty t _ty' -> whnfT t
  t -> pure t


valueOfVar :: Eq var => var -> TypeCheck var (Maybe (TermT var))
valueOfVar x = asks (lookup x . varValues) >>= \case
  Nothing -> issueTypeError $ TypeErrorUndefined x
  Just ty -> return ty

typeOfVar :: Eq var => var -> TypeCheck var (TermT var)
typeOfVar x = asks (lookup x . varTypes) >>= \case
  Nothing -> issueTypeError $ TypeErrorUndefined x
  Just ty -> return ty

typeOf :: Eq var => TermT var -> TypeCheck var (TermT var)
typeOf = \case
  Pure x -> typeOfVar x >>= whnfT   -- FIXME: store WHNF, do not compute every time!
  Free (TypedF ty _) -> whnfT ty    -- FIXME: store WHNF, do not compute every time!

unifyTopes :: Eq var => TermT var -> TermT var -> TypeCheck var ()
unifyTopes l r = do
  l' <- whnfT l
  r' <- whnfT r
  unless (untyped l' == untyped r') $
    issueTypeError (TypeErrorTopesNotEquivalent l r)

unify :: Eq var => Maybe (Term var) -> TermT var -> TermT var -> TypeCheck var ()
unify mterm expected actual = performing action $ do
  expectedVal <- whnfT expected
  actualVal <- whnfT actual
  (expected', actual') <- etaMatch expectedVal actualVal
  let def = unless (untyped expected' == untyped actual') err
      err =
        case mterm of
          Nothing   -> issueTypeError (TypeErrorUnifyTerms expected' actual')
          Just term -> issueTypeError (TypeErrorUnify term expected' actual')
      errS = do
        expected'' <- whnfT (S <$> expected')
        actual'' <- whnfT (S <$> actual')
        case mterm of
          Nothing   -> issueTypeError (TypeErrorUnifyTerms expected'' actual'')
          Just term -> issueTypeError (TypeErrorUnify (S <$> term) expected'' actual'')
  case expected' of
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
      case actual' of
        RecBottomT{} -> return () -- unifies with anything
        CubeProductT _ l' r' -> do
          unify Nothing l l'
          unify Nothing r r'
        _ -> err

    PairT _ty l r ->
      case actual' of
        RecBottomT{} -> return () -- unifies with anything
        PairT _ty' l' r' -> do
          unify Nothing l l'
          unify Nothing r r'

        -- one part of eta-expansion for pairs
        -- FIXME: add symmetric version!
        _ -> err

    FirstT _ty t ->
      case actual' of
        RecBottomT{} -> return () -- unifies with anything
        FirstT _ty' t' -> unify Nothing t t'
        _ -> err

    SecondT _ty t ->
      case actual' of
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
      case actual' of
        RecBottomT{} -> return () -- unifies with anything
        TypeFunT _ty' orig' cube' mtope' ret' -> do
          unify Nothing cube cube'
          enterScope orig' cube $ do
            case (mtope, mtope') of
              (Just tope, Just tope') -> unify Nothing tope tope'
              (Nothing, Nothing) -> return ()
              _ -> errS
            unify Nothing ret ret'
        _ -> err

    TypeSigmaT _ty _orig a b ->
      case actual' of
        RecBottomT{} -> return () -- unifies with anything
        TypeSigmaT _ty' orig' a' b' -> do
          unify Nothing a a'
          enterScope orig' a $ unify Nothing b b'
        _ -> err

    TypeIdT _ty x _tA y ->
      case actual' of
        RecBottomT{} -> return () -- unifies with anything
        TypeIdT _ty' x' _tA' y' -> do
          -- unify Nothing tA tA' -- TODO: do we need this check?
          unify Nothing x x'
          unify Nothing y y'
        _ -> err

    AppT _ty f x ->
      case actual' of
        RecBottomT{} -> return () -- unifies with anything
        AppT _ty' f' x' -> do
          unify Nothing f f'
          unify Nothing x x'
        _ -> err

    LambdaT (TypeFunT _ty _origF param mtope _ret) _orig _mparam body ->
      case actual' of
        RecBottomT{} -> return () -- unifies with anything
        LambdaT (TypeFunT _ty' _origF' param' mtope' _ret') orig' _mparam' body' -> do
          unify Nothing param param'
          enterScope orig' param $ do
            case (mtope, mtope') of
              (Just tope, Just tope') -> unify Nothing tope tope'
              (Nothing, Nothing) -> return ()
              _ -> errS
            unify Nothing body body'
        _ -> err

    LambdaT{} -> error "impossible: lambda with non-function type!"

    ReflT (TypeIdT _ty x _tA y) _x ->
      case actual' of
        RecBottomT{} -> return () -- unifies with anything
        ReflT (TypeIdT _ty' x' _tA' y') _x' -> do
          -- unify Nothing tA tA' -- TODO: do we need this check?
          unify Nothing x x'
          unify Nothing y y'
        _ -> err
    ReflT{} -> error "impossible: refl with non-identity type!"

    IdJT _ty a b c d e f ->
      case actual' of
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
    Lambda orig mparam body ->
      whnfT ty >>= \case
        ty'@(TypeFunT _ty _orig' param' mtope' ret) -> do
          case mparam of
            Nothing -> return ()
            Just (param, mtope) -> do
              param'' <- inferAs universeT param
              unifyTerms param' param''
              enterScope orig param' $ do
                mtope'' <- typecheck (fromMaybe TopeTop mtope) topeT
                unifyTerms (fromMaybe (TopeTopT topeT) mtope') mtope''

          enterScope orig param' $ do
            let maybeLocalTope =
                  case mtope' of
                    Nothing -> id
                    Just tope -> localTope tope
            maybeLocalTope $ do
              body' <- typecheck body ret
              return (LambdaT ty' orig (Just (param', mtope')) body')

        _ -> issueTypeError $ TypeErrorOther "unexpected lambda abstraction"

    Pair l r ->
      whnfT ty >>= \case
        ty'@(CubeProductT _ty a b) -> do
          l' <- typecheck l a
          r' <- typecheck r b
          return (PairT ty' l' r')
        ty'@(TypeSigmaT _ty _orig a b) -> do
          l' <- typecheck l a
          r' <- typecheck r (substitute l' b)
          return (PairT ty' l' r')
        _ -> issueTypeError $ TypeErrorOther "expected cube product or dependent sum"

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
    b' <- enterScope orig a' $ inferAs universeT b
    return (TypeFunT universeT orig a' Nothing b')

  TypeFun orig cube (Just tope) ret -> do
    cube' <- typecheck cube cubeT
    enterScope orig cube' $ do
      tope' <- typecheck tope topeT
      ret' <- inferAs universeT ret
      return (TypeFunT universeT orig cube' (Just tope') ret')

  TypeSigma orig a b -> do
    a' <- inferAs universeT a  -- FIXME: separate universe of universes from universe of types
    b' <- enterScope orig a' $ inferAs universeT b
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
    enterScope orig ty' $ do
      body' <- infer body
      ret <- typeOf body' 
      return (LambdaT (TypeFunT universeT orig ty' Nothing ret) orig (Just (ty', Nothing)) body')
  Lambda orig (Just (cube, Just tope)) body -> do
    cube' <- typecheck cube universeT
    enterScope orig cube' $ do
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
