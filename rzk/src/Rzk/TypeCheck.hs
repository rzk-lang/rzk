{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
module Rzk.TypeCheck where

import Control.Monad.Reader
import Control.Monad.Except
import Data.List (tails, (\\), intercalate, nub)
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
  | TypeErrorTopeNotSatisfied [TermT var] (TermT var)
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
  TypeErrorTopeNotSatisfied topes tope -> unlines
    [ "local context is not included in (does not entail) the tope"
    , "  " <> show (untyped tope)
    , "in local context (normalised)"
    , intercalate "\n" (map ("  " <>) (map show topes))
    , intercalate "\n" (map ("  " <>) (map show (generateTopesForPoints (allTopePoints tope))))] -- FIXME: remove
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
        z' = refreshVar used z -- FIXME: inefficient

ppTypeErrorInScopedContext' :: TypeErrorInScopedContext Rzk.VarIdent -> String
ppTypeErrorInScopedContext' err = ppTypeErrorInScopedContextWith' vars (defaultVarIdents \\ vars) err
  where
    vars = nub (foldMap pure err)

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
  | ActionContextEntailedBy (TermT var)
  | ActionContextEntails (TermT var)
  | ActionContextEquiv [TermT var]
  | ActionWHNF (TermT var)
  | ActionNF (TermT var)
  | ActionCheckCoherence (TermT var, TermT var) (TermT var, TermT var)
  deriving (Functor, Foldable)

type Action' = Action Rzk.VarIdent

ppSomeAction :: Eq var => Int -> Action var -> String
ppSomeAction n action = ppAction n (toRzkVarIdent <$> action)
  where
    vars = nub (foldMap pure action)
    mapping = zip vars defaultVarIdents
    toRzkVarIdent = fromMaybe (Rzk.VarIdent "_") . flip lookup mapping

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
    , "  " <> show expected
    , "with term"
    , "  " <> show actual ]

  ActionInfer term ->
    [ "inferring type for term"
    , "  " <> show term ]

  ActionContextEntailedBy term ->
    [ "checking if local context includes (is entailed by) restriction tope"
    , "  " <> show (untyped term) ]

  ActionContextEntails term ->
    [ "checking if local context is included in (entails) the tope"
    , "  " <> show (untyped term) ]

  ActionContextEquiv terms ->
    [ "checking if local context is equivalent to the union of the topes"
    , intercalate "\n" (map (("  " <>) . show . untyped) terms) ]

  ActionWHNF term ->
    [ "computing WHNF for term"
    , "  " <> show (untyped term) ]

  ActionNF term ->
    [ "computing normal form for term"
    , "  " <> show (untyped term) ]

  ActionCheckCoherence (ltope, lterm) (rtope, rterm) ->
    [ "checking coherence for"
    , "  " <> show (untyped ltope)
    , "  |-> " <> show (untyped lterm)
    , "and"
    , "  " <> show (untyped rtope)
    , "  |-> " <> show (untyped rterm) ]


traceAction' :: Int -> Action' -> a -> a
traceAction' n action = trace ("[debug]\n" <> ppAction n action)

unsafeTraceAction' :: Int -> Action var -> a -> a
unsafeTraceAction' n = traceAction' n . unsafeCoerce

data Context var = Context
  { varTypes          :: [(var, TermT var)]
  , varValues         :: [(var, Maybe (TermT var))]
  , varOrigs          :: [Maybe Rzk.VarIdent]
  , localTopes        :: [TermT var]
  , localTopesNF      :: [TermT var]
  , localTopesNFUnion :: [[TermT var]]
  , localTopesEntailBottom  :: Bool
  , actionStack       :: [Action var]
  } deriving (Functor, Foldable)

emptyContext :: Context var
emptyContext = Context
  { varTypes = []
  , varValues = []
  , varOrigs = []
  , localTopes = []
  , localTopesNF = []
  , localTopesNFUnion = [[]]
  , localTopesEntailBottom = False
  , actionStack = []
  }

ppContext' :: Context Rzk.VarIdent -> String
ppContext' Context{..} = unlines
  [ "Definitions in context:"
  , unlines
      [ show (Pure x :: Term') <> " : " <> show (untyped ty)
      | (x, ty) <- reverse varTypes ]
--  , unlines
--      [ show (Pure x :: Term') <> " = " <> show (untyped term)
--      | (x, Just term) <- reverse varValues ]
  , intercalate "\n" (map (("when " <>) . ppAction 0) (reverse actionStack))
  , "Local tope context:"
  , intercalate "\n" (map (("  " <>) . show . untyped) localTopes)
--  , "Local tope context (expanded):"
--  , intercalate "\n" (map (("  " <>) . show . untyped) (intercalate [TopeAndT topeT topeBottomT topeBottomT] (saturateTopes [] <$> simplifyLHS localTopes)))
  ]

localDecl :: Eq var => var -> TermT var -> TermT var -> TypeCheck var a -> TypeCheck var a
localDecl x ty term tc = do
  ty' <- whnfT ty
  term' <- whnfT term
  flip local tc $ \Context{..} -> Context
    { varTypes = (x, ty') : varTypes
    , varValues = (x, Just term') : varValues
    , .. }

type TypeCheck var = ReaderT (Context var) (Except (TypeErrorInScopedContext var))

showSomeTermTs :: Eq var => [TermT var] -> String
showSomeTermTs terms = show (map (\term -> untyped $ fmap (\x -> maybe (error "impossible") id (lookup x mapping)) term) terms)
  where
    vars = nub (foldMap (foldMap pure) terms)
    mapping = zip vars defaultVarIdents

entail :: Eq var => [TermT var] -> TermT var -> Bool
entail topes tope = all (`solveRHS` tope) $
  saturateTopes (allTopePoints tope) <$>
    simplifyLHS topes'
  where
    topes' = nubTermT (topes <> generateTopesForPoints (allTopePoints tope))

nubTermT :: Eq var => [TermT var] -> [TermT var]
nubTermT [] = []
nubTermT (t:ts) = t : nubTermT (filter (\t' -> untyped t' /= untyped t) ts)

saturateTopes :: Eq var => [TermT var] -> [TermT var] -> [TermT var]
saturateTopes _points topes = {- trace ("saturateTopes " <> show (length topes)) $ -} saturateWith
  (\tope ts -> untyped tope `elem` map untyped ts)
  generateTopes
  topes

-- FIXME: cleanup
saturateWith :: (a -> [a] -> Bool) -> ([a] -> [a] -> [a]) -> [a] -> [a]
saturateWith elem' step zs = go (nub' zs) []
  where
    go lastNew xs
      | null new = lastNew
      | otherwise = lastNew <> go new xs'
      where
        xs' = lastNew <> xs
        new = filter (not . (`elem'` xs')) (nub' $ step lastNew xs)
    nub' [] = []
    nub' (x:xs) = x : nub' (filter (not . (`elem'` [x])) xs)

generateTopes :: Eq var => [TermT var] -> [TermT var] -> [TermT var]
generateTopes newTopes oldTopes
  | TopeBottom `elem` map untyped newTopes = []
  | TopeEQ Cube2_0 Cube2_1 `elem` map untyped newTopes = [topeBottomT]
  | otherwise = concat
      [  -- symmetry EQ
        [ TopeEQT topeT y x | TopeEQT _ty x y <- newTopes ]
        -- transitivity EQ (1)
      , [ TopeEQT topeT x z
        | TopeEQT _ty x y : newTopes' <- tails newTopes
        , TopeEQT _ty y' z <- newTopes' <> oldTopes
        , untyped y == untyped y' ]
        -- transitivity EQ (2)
      , [ TopeEQT topeT x z
        | TopeEQT _ty y z : newTopes' <- tails newTopes
        , TopeEQT _ty x y' <- newTopes' <> oldTopes
        , untyped y == untyped y' ]

        -- transitivity LEQ (1)
      , [ TopeLEQT topeT x z
        | TopeLEQT _ty x y : newTopes' <- tails newTopes
        , TopeLEQT _ty y' z <- newTopes' <> oldTopes
        , untyped y == untyped y' ]
        -- transitivity LEQ (2)
      , [ TopeLEQT topeT x z
        | TopeLEQT _ty y z : newTopes' <- tails newTopes
        , TopeLEQT _ty x y' <- newTopes' <> oldTopes
        , untyped y == untyped y' ]

        -- antisymmetry LEQ
      , [ TopeEQT topeT x y
        | TopeLEQT _ty x y : newTopes' <- tails newTopes
        , TopeLEQT _ty y' x' <- newTopes' <> oldTopes
        , untyped y == untyped y'
        , untyped x == untyped x' ]

--        -- FIXME: special case of substitution of EQ
--        -- transitivity EQ-LEQ (1)
--      , [ TopeLEQT topeT x z
--        | TopeEQT  _ty y z : newTopes' <- tails newTopes
--        , TopeLEQT _ty x y' <- newTopes' <> oldTopes
--        , untyped y == untyped y' ]
--
--        -- FIXME: special case of substitution of EQ
--        -- transitivity EQ-LEQ (2)
--      , [ TopeLEQT topeT x z
--        | TopeEQT  _ty x y : newTopes' <- tails newTopes
--        , TopeLEQT _ty y' z <- newTopes' <> oldTopes
--        , untyped y == untyped y' ]
--
--        -- FIXME: special case of substitution of EQ
--        -- transitivity EQ-LEQ (3)
--      , [ TopeLEQT topeT x z
--        | TopeLEQT  _ty y z : newTopes' <- tails newTopes
--        , TopeEQT _ty x y' <- newTopes' <> oldTopes
--        , untyped y == untyped y' ]
--
--        -- FIXME: special case of substitution of EQ
--        -- transitivity EQ-LEQ (4)
--      , [ TopeLEQT topeT x z
--        | TopeLEQT  _ty x y : newTopes' <- tails newTopes
--        , TopeEQT _ty y' z <- newTopes' <> oldTopes
--        , untyped y == untyped y' ]
      ]

generateTopesForPoints :: Eq var => [TermT var] -> [TermT var]
generateTopesForPoints points = nubTermT $ concat
  [ [ TopeOrT topeT (TopeLEQT topeT x y) (TopeLEQT topeT y x)
    | x : points' <- tails points, y <- points'
    , untyped x /= untyped y
    , untyped x `notElem` [Cube2_0, Cube2_1]
    , untyped y `notElem` [Cube2_0, Cube2_1]]
  ]

allTopePoints :: Eq var => TermT var -> [TermT var]
allTopePoints = nubTermT . foldMap subPoints . nubTermT . topePoints

topePoints :: TermT var -> [TermT var]
topePoints = \case
  TopeTopT{} -> []
  TopeBottomT{} -> []
  TopeAndT _ l r -> topePoints l <> topePoints r
  TopeOrT  _ l r -> topePoints l <> topePoints r
  TopeEQT  _ x y -> [x, y]
  TopeLEQT _ x y -> [x, y]
  _ -> []

subPoints :: TermT var -> [TermT var]
subPoints = \case
  p@(PairT _ x y) -> p : foldMap subPoints [x, y]
  p@Pure{} -> [p]
  p@(Free (TypedF Cube2T{} _)) -> [p]
  _ -> []

simplifyLHS :: Eq var => [TermT var] -> [[TermT var]]
simplifyLHS topes = {- trace ("simplifyLHS " <> show (fmap (untyped . (Rzk.VarIdent "_" <$)) topes)) $ -} map nubTermT $
  case topes of
    [] -> [[]]
    TopeTopT{} : topes' -> simplifyLHS topes'
    TopeBottomT{} : _  -> [[topeBottomT]]
    TopeAndT _ l r : topes' -> simplifyLHS (l : r : topes')
    TopeOrT  _ l r : topes' -> simplifyLHS (l : topes') <> simplifyLHS (r : topes')
    TopeEQT  _ (PairT _ x y) (PairT _ x' y') : topes' ->
      simplifyLHS (TopeEQT topeT x x' : TopeEQT topeT y y' : topes')
    t : topes' -> map (t:) (simplifyLHS topes')

solveRHS :: Eq var => [TermT var] -> TermT var -> Bool
solveRHS topes tope =
  case tope of
    _ | TopeBottom `elem` map untyped topes -> True
    TopeTopT{}     -> True
    TopeEQT  _ty (PairT _ty1 x y) (PairT _ty2 x' y')
      | solveRHS topes (TopeEQT topeT x x') && solveRHS topes (TopeEQT topeT y y') -> True
    TopeEQT  _ty l r -> or
      [ untyped l == untyped r
      , untyped tope `elem` map untyped topes
      , TopeEQ (untyped r) (untyped l) `elem` map untyped topes
      ]
    TopeLEQT _ty l r
      | untyped l == untyped r -> True
      | solveRHS topes (TopeEQT topeT l r) -> True
      | solveRHS topes (TopeEQT topeT l (Cube2_0T cube2T)) -> True
      | solveRHS topes (TopeEQT topeT r (Cube2_1T cube2T)) -> True
    -- TopeBottomT{}  -> solveLHS topes tope
    TopeAndT _ l r -> solveRHS topes l && solveRHS topes r
    TopeOrT  _ l r -> solveRHS topes l || solveRHS topes r
    _ -> untyped tope `elem` map untyped topes

checkTope :: Eq var => TermT var -> TypeCheck var Bool
checkTope tope = performing (ActionContextEntails tope) $ do
  topes' <- asks localTopesNF
  tope' <- nfT tope
  return (topes' `entail` tope')

contextEntailedBy :: Eq var => TermT var -> TypeCheck var ()
contextEntailedBy tope = performing (ActionContextEntailedBy tope) $ do
  contextTopes <- asks localTopesNF
  restrictionTope <- nfT tope
  let contextTopesRHS = foldr (TopeOrT topeT) topeBottomT contextTopes
  unless ([restrictionTope] `entail` contextTopesRHS) $
    issueTypeError $ TypeErrorTopeNotSatisfied [restrictionTope] contextTopesRHS

contextEntails :: Eq var => TermT var -> TypeCheck var ()
contextEntails tope = performing (ActionContextEntails tope) $ do
  topeIsEntailed <- checkTope tope
  topes' <- asks localTopesNF
  tope' <- nfT tope
  unless topeIsEntailed $
    issueTypeError $ TypeErrorTopeNotSatisfied topes' tope'

topesEquiv :: Eq var => TermT var -> TermT var -> TypeCheck var Bool
topesEquiv expected actual = do
  expected' <- nfT expected
  actual' <- nfT actual
  return ([expected'] `entail` actual' && [actual'] `entail` expected')

contextEquiv :: Eq var => [TermT var] -> TypeCheck var ()
contextEquiv topes = performing (ActionContextEquiv topes) $ do
  contextTopes <- asks localTopesNF
  recTopes <- mapM nfT topes
  let contextTopesRHS = foldr (TopeOrT topeT) topeBottomT contextTopes
      recTopesRHS     = foldr (TopeOrT topeT) topeBottomT recTopes
  unless (contextTopes `entail` recTopesRHS) $
    issueTypeError $ TypeErrorTopeNotSatisfied contextTopes recTopesRHS
  unless (recTopes `entail` contextTopesRHS) $
    issueTypeError $ TypeErrorTopeNotSatisfied recTopes contextTopesRHS

enterScopeContext :: Maybe Rzk.VarIdent -> TermT var -> Context var -> Context (Inc var)
enterScopeContext orig ty Context{..} = Context
  { varTypes = (Z, S <$> ty) : [ (S x, fmap S t) | (x, t) <- varTypes ]
  , varValues = (Z, Nothing) : [ (S x, fmap S <$> t) | (x, t) <- varValues ]
  , varOrigs = orig : varOrigs
  , localTopes = map (fmap S) localTopes
  , localTopesNF = map (fmap S) localTopesNF
  , localTopesNFUnion = map (map (fmap S)) localTopesNFUnion
  , actionStack = map (fmap S) actionStack
  , localTopesEntailBottom = localTopesEntailBottom
  }

enterScope :: Maybe Rzk.VarIdent -> TermT var -> TypeCheck (Inc var) b -> TypeCheck var b
enterScope orig ty action = do
  newContext <- asks (enterScopeContext orig ty)
  lift $ withExceptT (ScopedTypeError orig) $
    runReaderT action newContext

performing :: Eq var => Action var -> TypeCheck var a -> TypeCheck var a
performing action tc = do
  Context{..} <- ask
  unless (length actionStack < 100) $
    issueTypeError $ TypeErrorOther "maximum depth reached"
  -- unsafeTraceAction' (length actionStack) action $
--  let actionName =
--        case action of
--          ActionWHNF{}        -> "whnfT"
--          ActionNF{}          -> "nfT"
--          ActionTypeCheck{}   -> "typecheck"
--          ActionUnify{}       -> "unify"
--          ActionUnifyTerms{}  -> "unifyTerms"
--          ActionInfer{}       -> "infer"
--          ActionContextEntailedBy{} -> "context entailed by"
--          ActionContextEntails{}    -> "context entails"
--          ActionContextEquiv{}      -> "context equiv"
--          ActionCheckCoherence{}    -> "check coherence"
  id $ -- trace (ppSomeAction (length actionStack) action) $
    local (const Context { actionStack = action : actionStack, .. }) $ tc

stripTypeRestrictions :: TermT var -> TermT var
stripTypeRestrictions (TypeRestrictedT _ty ty _restriction) = stripTypeRestrictions ty
stripTypeRestrictions t = t

-- | Perform at most one \(\eta\)-expansion at the top-level to assist unification.
etaMatch :: Eq var => TermT var -> TermT var -> TypeCheck var (TermT var, TermT var)
-- FIXME: double check the next 3 rules
etaMatch expected@TypeRestrictedT{} actual@TypeRestrictedT{} = pure (expected, actual)
etaMatch expected (TypeRestrictedT _ty ty (_tope, _term)) = etaMatch expected ty
etaMatch expected@TypeRestrictedT{} actual =
  etaMatch expected (TypeRestrictedT universeT actual (topeBottomT, recBottomT))
-- ------------------------------------
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
  ty <- typeOf term
  case stripTypeRestrictions ty of
    TypeFunT _ty orig param mtope ret -> pure $
      LambdaT ty orig (Just (param, mtope))
        (AppT ret (S <$> term) (Pure Z))

    TypeSigmaT _ty _orig a b -> pure $
      PairT ty
        (FirstT a term)
        (SecondT (substitute (FirstT a term) b) term)

    CubeProductT _ty a b -> pure $
      PairT ty
        (FirstT a term)
        (SecondT b term)

    _ -> pure term

inCubeLayer :: Eq var => TermT var -> TypeCheck var Bool
inCubeLayer = \case
  RecBottomT{} -> pure False
  UniverseT{} -> pure False

  UniverseCubeT{} -> pure True
  CubeProductT{} -> pure True
  CubeUnitT{} -> pure True
  CubeUnitStarT{} -> pure True
  Cube2T{} -> pure True
  Cube2_0T{} -> pure True
  Cube2_1T{} -> pure True

  t -> typeOf t >>= inCubeLayer

inTopeLayer :: Eq var => TermT var -> TypeCheck var Bool
inTopeLayer = \case
  RecBottomT{} -> pure False
  UniverseT{} -> pure False

  UniverseCubeT{} -> pure True
  UniverseTopeT{} -> pure True

  CubeProductT{} -> pure True
  CubeUnitT{} -> pure True
  CubeUnitStarT{} -> pure True
  Cube2T{} -> pure True
  Cube2_0T{} -> pure True
  Cube2_1T{} -> pure True

  TopeTopT{} -> pure True
  TopeBottomT{} -> pure True
  TopeAndT{} -> pure True
  TopeOrT{} -> pure True
  TopeEQT{} -> pure True
  TopeLEQT{} -> pure True

  TypeFunT _ty orig param _mtope ret -> do
    enterScope orig param $ inTopeLayer ret

  t -> typeOfUncomputed t >>= inTopeLayer

-- | Compute a typed term to its WHNF.
--
-- >>> whnfT "(\\p -> first (second p)) (x, (y, z))" :: Term'
-- y
whnfT :: Eq var => TermT var -> TypeCheck var (TermT var)
whnfT tt = case tt of
  -- universe constants
  UniverseT{} -> pure tt
  UniverseCubeT{} -> pure tt
  UniverseTopeT{} -> pure tt

  -- cube layer (except vars, pairs, and applications)
  CubeProductT{} -> nfT tt
  CubeUnitT{} -> pure tt
  CubeUnitStarT{} -> pure tt
  Cube2T{} -> pure tt
  Cube2_0T{} -> pure tt
  Cube2_1T{} -> pure tt

  -- tope layer (except vars, pairs of points, and applications)
  TopeTopT{} -> pure tt
  TopeBottomT{} -> pure tt
  TopeAndT{} -> nfT tt
  TopeOrT{} -> nfT tt
  TopeEQT{} -> nfT tt
  TopeLEQT{} -> nfT tt

  -- type layer terms that should not be evaluated further
  LambdaT{} -> pure tt
  PairT{} -> pure tt
  ReflT{} -> pure tt
  TypeFunT{} -> pure tt
  TypeSigmaT{} -> pure tt
  TypeIdT{} -> pure tt
  RecBottomT{} -> pure tt
  RecOrT{} -> pure tt -- FIXME: should we consider recOR to be WHNF?

  -- type ascriptions are ignored, since we already have a typechecked term
  TypeAscT _ty term _ty' -> whnfT term

  -- check if we have cube or a tope term (if so, compute NF)
  _ -> typeOfUncomputed tt >>= \case
    UniverseCubeT{} -> nfT tt
    UniverseTopeT{} -> nfT tt

    -- check if we have cube point term (if so, compute NF)
    typeOf_tt -> typeOfUncomputed typeOf_tt >>= \case
      UniverseCubeT{} -> nfT tt

      -- now we are in the type layer
      _ -> do
        -- check if we are in the empty context
        inBottom <- asks localTopesEntailBottom
        if inBottom
           then pure recBottomT -- if so, reduce to recBOT
           else tryRestriction tt typeOf_tt >>= \case
            Just tt' -> whnfT tt'
            Nothing -> go tt

  where
    tryRestriction term type_ =
      case type_ of
        TypeRestrictedT _ _ (tope, term') -> do
          checkTope tope >>= \case
            True -> pure (Just term')
            False -> -- pure Nothing
--               -- see if it can be reduced in one of the alternatives
--               alts <- asks localTopesNFUnion
--               splitTopes <- forM alts $ \topes' -> do
--                 local (\Context{..} -> Context
--                     { localTopes = topes'
--                     , localTopesNF = topes'
--                     , localTopesNFUnion = [topes']
--                     , .. }) $
--                   checkTope tope >>= \case
--                     False -> pure (Left tope)
--                     True -> pure (Right tope)
--               case (lefts splitTopes, rights splitTopes) of
--                 (_, []) -> pure Nothing
--                 (ls, rs) -> pure Nothing
              pure Nothing
        _ -> pure Nothing

    go tt' = performing (ActionWHNF tt') . ($ tt') $ \case
      t@(Pure var) ->
        valueOfVar var >>= \case
          Nothing -> pure t
          Just term -> whnfT term

      AppT ty f x ->
        whnfT f >>= \case
          LambdaT _ty _orig _arg body ->
            whnfT (substitute x body)
          f' -> typeOf f' >>= \case
            TypeFunT _ty _orig _param (Just tope) UniverseTopeT{} -> do
              TopeAndT topeT
                <$> (AppT ty <$> nfT f' <*> nfT x)
                <*> nfT (substitute x tope)
            _ -> pure (AppT ty f' x)

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

      TopeAndT ty l r -> TopeAndT ty <$> whnfT l <*> whnfT r
      TopeOrT ty l r  -> TopeOrT ty <$> whnfT l <*> whnfT r

--    --------------------------------------------
--    FIXME: figure out why this is not correct
--    --------------------------------------------
--      RecOrT ty rs -> do
--        terms <- forM rs $ \(tope, term) -> do
--          tope' <- nfT tope
--          entailed <- checkTope tope'
--          return ((tope', term), not entailed)
--        case filter snd terms of
--          [] -> pure (RecOrT ty (map fst terms))
--          ((_tope, term), _) : _ -> whnfT term
--    --------------------------------------------

      TypeRestrictedT ty type_ (tope, term) -> do
        nfT tope >>= \case
          TopeBottomT{} -> whnfT type_  -- get rid of restrictions at BOT
          tope' -> TypeRestrictedT ty <$> whnfT type_ <*> pure (tope', term)

      t -> pure t

-- | Compute a typed term to its NF.
--
-- >>> nfT "(\\p -> first (second p)) (x, (y, z))" :: Term'
-- y
nfT :: Eq var => TermT var -> TypeCheck var (TermT var)
nfT tt = case tt of
  -- universe constants
  UniverseT{} -> pure tt
  UniverseCubeT{} -> pure tt
  UniverseTopeT{} -> pure tt

  -- cube layer constants
  CubeUnitT{} -> pure tt
  CubeUnitStarT{} -> pure tt
  Cube2T{} -> pure tt
  Cube2_0T{} -> pure tt
  Cube2_1T{} -> pure tt

  -- tope layer constants
  TopeTopT{} -> pure tt
  TopeBottomT{} -> pure tt

  _ -> do
    inBottom <- asks localTopesEntailBottom
    if inBottom
       then pure recBottomT
       else typeOfUncomputed tt >>= \case
        TypeRestrictedT _ _ (tope, tt') -> do
          inRestriction <- checkTope tope
          if inRestriction
             then nfT tt'
             else go tt
        _ -> go tt
  where
    go tt' = performing (ActionNF tt') . ($ tt') $ \case
      t@(Pure var) ->
        valueOfVar var >>= \case
          Nothing -> pure t
          Just term -> nfT term

      AppT ty f x ->
        whnfT f >>= \case
          LambdaT _ty _orig _arg body ->
            nfT (substitute x body)
          f' -> typeOf f' >>= \case
            TypeFunT _ty _orig _param (Just tope) UniverseTopeT{} -> do
              TopeAndT topeT
                <$> (AppT ty <$> nfT f' <*> nfT x)
                <*> nfT (substitute x tope)
            _ -> AppT ty <$> nfT f' <*> nfT x

      FirstT ty t ->
        whnfT t >>= \case
          PairT _ l _r -> nfT l
          t' -> FirstT ty <$> nfT t'
      SecondT ty t ->
        whnfT t >>= \case
          PairT _ _l r -> nfT r
          t' -> SecondT ty <$> nfT t'
      IdJT ty tA a tC d x p ->
        whnfT p >>= \case
          ReflT{} -> nfT d
          p' -> IdJT ty <$> nfT tA <*> nfT a <*> nfT tC <*> nfT d <*> nfT x <*> nfT p'
      TypeAscT _ty t _ty' -> nfT t

--      RecOrT ty rs -> do
--        terms <- forM rs $ \(tope, term) -> do
--          tope' <- nfT tope
--          entailed <- checkTope tope'
--          return ((tope', term), entailed)
--        case filter snd terms of
--          [] -> RecOrT ty <$> traverse (\(tope, term) -> (,) tope <$> nfT term) (map fst terms)
--          ((_tope, term), _):_ -> nfT term

      TopeAndT ty l r ->
        nfT l >>= \case
          TopeBottomT{} -> pure topeBottomT
          l' -> nfT r >>= \case
            TopeBottomT{} -> pure topeBottomT
            r' -> pure (TopeAndT ty l' r')

      TopeOrT  ty l r -> do
        l' <- nfT l
        r' <- nfT r
        case (l', r') of
          (TopeBottomT{}, _) -> pure r'
          (_, TopeBottomT{}) -> pure l'
          _ -> pure (TopeOrT ty l' r')

      TopeEQT  ty l r -> TopeEQT  ty <$> nfT l <*> nfT r
      TopeLEQT ty l r -> TopeLEQT ty <$> nfT l <*> nfT r

      PairT ty l r -> PairT ty <$> nfT l <*> nfT r

      t@TopeTopT{} -> pure t
      t@TopeBottomT{} -> pure t

      t@UniverseT{} -> pure t
      t@UniverseCubeT{} -> pure t
      t@UniverseTopeT{} -> pure t

      t@CubeUnitT{} -> pure t
      t@CubeUnitStarT{} -> pure t

      t@Cube2T{}    -> pure t
      t@Cube2_0T{}  -> pure t
      t@Cube2_1T{}  -> pure t

      CubeProductT ty l r -> CubeProductT ty <$> nfT l <*> nfT r

      TypeFunT ty orig param mtope ret -> do
        param' <- nfT param
        enterScope orig param' $ do
          mtope' <- traverse nfT mtope
          maybe id (localTope "1") mtope' $
            TypeFunT ty orig param' mtope' <$> nfT ret

      TypeSigmaT ty orig a b -> do
        a' <- nfT a
        enterScope orig a' $ do
          TypeSigmaT ty orig a' <$> nfT b

      TypeIdT ty x tA y -> TypeIdT ty <$> nfT x <*> traverse nfT tA <*> nfT y

      ReflT ty m -> fmap (ReflT ty) $ forM m $ \(x, mty) -> do
        x' <- nfT x
        mty' <- traverse nfT mty
        return (x', mty')

      LambdaT ty orig _mparam body -> do
        case stripTypeRestrictions ty of
          TypeFunT _ty _orig param mtope _ret -> do
            param' <- nfT param
            enterScope orig param' $ do
              mtope' <- traverse nfT mtope
              maybe id (localTope "2") mtope' $
                LambdaT ty orig (Just (param', mtope')) <$> nfT body
          _ -> error "impossible: lambda with non-function type!"

      TypeRestrictedT ty type_ (tope, term) -> do
        nfT tope >>= \case
          TopeBottomT{} -> nfT type_  -- get rid of restrictions at BOT
          tope' -> TypeRestrictedT ty <$> nfT type_ <*> do
            term' <- (localTope "3") tope' $
              nfT term
            return (tope', term')

      RecBottomT{} -> pure recBottomT

valueOfVar :: Eq var => var -> TypeCheck var (Maybe (TermT var))
valueOfVar x = asks (lookup x . varValues) >>= \case
  Nothing -> issueTypeError $ TypeErrorUndefined x
  Just ty -> return ty

typeOfVar :: Eq var => var -> TypeCheck var (TermT var)
typeOfVar x = asks (lookup x . varTypes) >>= \case
  Nothing -> issueTypeError $ TypeErrorUndefined x
  Just ty -> return ty

typeOfUncomputed :: Eq var => TermT var -> TypeCheck var (TermT var)
typeOfUncomputed = \case
  Pure x -> typeOfVar x
  Free (TypedF ty _) -> pure ty

typeOf :: Eq var => TermT var -> TypeCheck var (TermT var)
typeOf t = typeOfUncomputed t >>= whnfT 

unifyTopes :: Eq var => TermT var -> TermT var -> TypeCheck var ()
unifyTopes l r = do
  equiv <- topesEquiv l r
  l' <- nfT l
  r' <- nfT r
  unless equiv $
    issueTypeError (TypeErrorTopesNotEquivalent l' r')

inAllSubContexts :: TypeCheck var () -> TypeCheck var () -> TypeCheck var ()
inAllSubContexts handleSingle tc = do
  topeSubContexts <- asks localTopesNFUnion
  case topeSubContexts of
    [] -> error "something went wrong?"
    [_] -> handleSingle
    _:_:_ -> do
      forM_ topeSubContexts $ \topes' ->
        local (\Context{..} -> Context
            { localTopes = topes'
            , localTopesNF = topes'
            , localTopesNFUnion = [topes']
            , .. }) $
          tc

unify :: Eq var => Maybe (Term var) -> TermT var -> TermT var -> TypeCheck var ()
unify mterm expected actual = performUnification `catchError` \typeError -> do
  inAllSubContexts (throwError typeError) performUnification
-- `catchError` \typeError -> trace "in all contexts" $ do
--   typeOf expected >>= \case
--     TypeRestrictedT{} -> inAllSubContexts (throwError typeError) performUnification
--     _ -> typeOf actual >>= \case
--       TypeRestrictedT{} -> inAllSubContexts (throwError typeError) performUnification
--       _ -> throwError typeError
  where
    performUnification = unifyInCurrentContext mterm expected actual 

unifyInCurrentContext :: Eq var => Maybe (Term var) -> TermT var -> TermT var -> TypeCheck var ()
unifyInCurrentContext mterm expected actual = performing action $
  unless (untyped expected == untyped actual) $ do
    expectedVal <- whnfT expected
    actualVal <- whnfT actual
    (expected', actual') <- etaMatch expectedVal actualVal
  --  expected'' <- nfT expected'
  --  actual'' <- nfT actual'
    case actual' of
      RecBottomT{} -> return ()
      RecOrT _ty rs' ->
        case expected' of
          RecOrT _ty rs -> sequence_ $
            checkCoherence <$> rs <*> rs'
          _ -> do
            forM_ rs' $ \(tope, term) ->
              localTope "14" tope $
                unify Nothing expected term
      _ -> typeOf expected' >>= typeOf >>= \case
        UniverseCubeT{} -> contextEntails (TopeEQT topeT expected' actual')
        _ -> do
          let def = unless (untyped expected' == untyped actual') err
              err =
                case mterm of
                  Nothing   -> issueTypeError (TypeErrorUnifyTerms expected' actual')
                  Just term -> issueTypeError (TypeErrorUnify term expected' actual')
              errS = do
                let expectedS = S <$> expected'
                    actualS = S <$> actual'
                case mterm of
                  Nothing   -> issueTypeError (TypeErrorUnifyTerms expectedS actualS)
                  Just term -> issueTypeError (TypeErrorUnify (S <$> term) expectedS actualS)
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
                CubeProductT _ l' r' -> do
                  unify Nothing l l'
                  unify Nothing r r'
                _ -> err

            PairT _ty l r ->
              case actual' of
                PairT _ty' l' r' -> do
                  unify Nothing l l'
                  unify Nothing r r'

                -- one part of eta-expansion for pairs
                -- FIXME: add symmetric version!
                _ -> err

            FirstT _ty t ->
              case actual' of
                FirstT _ty' t' -> unify Nothing t t'
                _ -> err

            SecondT _ty t ->
              case actual' of
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
              case actual' of
                RecOrT _ty rs' -> sequence_ $
                  checkCoherence <$> rs <*> rs'
                _ -> do
                  forM_ rs $ \(tope, term) ->
                    localTope "4" tope $
                      unify Nothing term actual

            TypeFunT _ty _orig cube mtope ret ->
              case actual' of
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
                TypeSigmaT _ty' orig' a' b' -> do
                  unify Nothing a a'
                  enterScope orig' a $ unify Nothing b b'
                _ -> err

            TypeIdT _ty x _tA y ->
              case actual' of
                TypeIdT _ty' x' _tA' y' -> do
                  -- unify Nothing tA tA' -- TODO: do we need this check?
                  unify Nothing x x'
                  unify Nothing y y'
                _ -> err

            AppT _ty f x ->
              case actual' of
                AppT _ty' f' x' -> do
                  unify Nothing f f'
                  unify Nothing x x'
                _ -> err

            LambdaT ty _orig _mparam body ->
              case stripTypeRestrictions ty of
                TypeFunT _ty _origF param mtope _ret ->
                  case actual' of
                    LambdaT ty' orig' _mparam' body' -> do
                      case stripTypeRestrictions ty' of
                        TypeFunT _ty' _origF' param' mtope' _ret' -> do
                          unify Nothing param param'
                          enterScope orig' param $ do
                            case (mtope, mtope') of
                              (Just tope, Just tope') -> do
                                unify Nothing tope tope'
                                localTope "5" tope $ unify Nothing body body'
                              (Nothing, Nothing) -> do
                                unify Nothing body body'
                              _ -> errS
                        _ -> err
                    _ -> err
                _ -> err

            ReflT (TypeIdT _ty x _tA y) _x ->
              case actual' of
                ReflT (TypeIdT _ty' x' _tA' y') _x' -> do
                  -- unify Nothing tA tA' -- TODO: do we need this check?
                  unify Nothing x x'
                  unify Nothing y y'
                _ -> err
            ReflT{} -> error "impossible: refl with non-identity type!"

            IdJT _ty a b c d e f ->
              case actual' of
                IdJT _ty' a' b' c' d' e' f' -> do
                  unify Nothing a a'
                  unify Nothing b b'
                  unify Nothing c c'
                  unify Nothing d d'
                  unify Nothing e e'
                  unify Nothing f f'
                _ -> err

            TypeAscT{} -> error "impossible: type ascription in WHNF!"

            TypeRestrictedT _ty ty (tope, term) ->
              case actual' of
                TypeRestrictedT _ty' ty' (tope', term') -> do
                  unify mterm ty ty'
                  unify Nothing tope tope'
                  localTope "6" tope $
                    unify Nothing term term'
                _ -> err    -- FIXME: need better unification for restrictions

  where
    action = case mterm of
               Nothing -> ActionUnifyTerms expected actual
               Just term -> ActionUnify term expected actual

unifyTypes :: Eq var => Term var -> TermT var -> TermT var -> TypeCheck var ()
unifyTypes = unify . Just

unifyTerms :: Eq var => TermT var -> TermT var -> TypeCheck var ()
unifyTerms = unify Nothing

localTope :: Eq var => String -> TermT var -> TypeCheck var a -> TypeCheck var a
localTope tag tope tc = {- trace ("[" <> tag <> "] localTope") $ -} do
  tope' <- nfT tope
  localTopes' <- asks localTopesNF
  local (f tope' localTopes') tc
  where
    f tope' localTopes' Context{..} = Context
      { localTopes = tope : localTopes
      , localTopesNF = tope' : localTopesNF
      , localTopesNFUnion =
          [ new <> old
          | new <- simplifyLHS [tope']
          , old <- localTopesNFUnion ]
      , localTopesEntailBottom = entailsBottom
      , .. }
      where
        entailsBottom = (tope' : localTopes') `entail` topeBottomT

universeT :: TermT var
universeT = iterate UniverseT (error msg) !! 30
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
  whnfT ty >>= \case

    RecBottomT{} -> do
      return recBottomT

    TypeRestrictedT _ty ty' (tope, rterm) -> do
      term' <- typecheck term ty'
      contextEntailedBy tope
      localTope "7" tope $
        unifyTerms rterm term'
      return term'    -- FIXME: correct?

    ty' -> case term of
      Lambda orig mparam body ->
        case ty' of
          TypeFunT _ty _orig' param' mtope' ret -> do
            case mparam of
              Nothing -> return ()
              Just (param, mtope) -> do
                param'' <- typecheck param =<< typeOf param'
                unifyTerms param' param''
                enterScope orig param' $ do
                  mtope'' <- typecheck (fromMaybe TopeTop mtope) topeT
                  unifyTerms (fromMaybe (TopeTopT topeT) mtope') mtope''

            enterScope orig param' $ do
              maybe id (localTope "8") mtope' $ do
                body' <- typecheck body ret
                return (LambdaT ty' orig (Just (param', mtope')) body')

          _ -> issueTypeError $ TypeErrorOther "unexpected lambda abstraction"

      Pair l r ->
        case ty' of
          CubeProductT _ty a b -> do
            l' <- typecheck l a
            r' <- typecheck r b
            return (PairT ty' l' r')
          TypeSigmaT _ty _orig a b -> do
            l' <- typecheck l a
            r' <- typecheck r (substitute l' b)
            return (PairT ty' l' r')
          _ -> issueTypeError $ TypeErrorOther "expected cube product or dependent sum"

      _ -> do
        term' <- infer term
        inferredType <- typeOf term'
        unifyTypes term ty' inferredType
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
    typeOf lt >>= \case
      UniverseCubeT{} -> return (PairT (CubeProductT cubeT lt rt) l' r')
      _ -> do
        -- NOTE: infer as a non-dependent pair!
        return (PairT (TypeSigmaT universeT Nothing lt (S <$> rt)) l' r')

  First t -> do
    t' <- infer t
    fmap stripTypeRestrictions (typeOf t') >>= \case
      RecBottomT{} -> pure recBottomT -- FIXME: is this ok?
      TypeSigmaT _ty _orig lt _rt ->
        return (FirstT lt t')
      CubeProductT _ty l _r ->
        return (FirstT l t')
      ty -> issueTypeError $ TypeErrorNotPair t' ty

  Second t -> do
    t' <- infer t
    fmap stripTypeRestrictions (typeOf t') >>= \case
      RecBottomT{} -> pure recBottomT -- FIXME: is this ok?
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
      localTope "9" tope' $ do
        term' <- inferAs universeT term
        ty <- typeOf term'
        return (tope', (term', ty))
    let rs' = map (fmap fst) ttts
        ts  = map (fmap snd) ttts
    sequence_ [ checkCoherence l r | l:rs'' <- tails rs', r <- rs'' ]
    contextEquiv (map fst ttts)
    return (RecOrT (RecOrT universeT ts) rs')

  TypeFun orig a Nothing b -> do
    a' <- inferAs universeT a  -- FIXME: separate universe of universes from universe of types
    b' <- enterScope orig a' $ inferAs universeT b
    return (TypeFunT universeT orig a' Nothing b')

  TypeFun orig cube (Just tope) ret -> do
    cube' <- typecheck cube cubeT
    enterScope orig cube' $ do
      tope' <- typecheck tope topeT
      localTope "10" tope' $ do
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
    fmap stripTypeRestrictions (typeOf f') >>= \case
      RecBottomT{} -> pure recBottomT -- FIXME: is this ok?
      TypeFunT _ty _orig a mtope b -> do
        x' <- typecheck x a
        case b of
          UniverseTopeT{} -> return ()
          _ -> mapM_ (contextEntails . substitute x') mtope   -- FIXME: need to check?
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
      body' <- localTope "11" tope' $ infer body
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

  TypeRestricted ty (tope, term) -> do
    ty' <- typecheck ty universeT
    tope' <- typecheck tope topeT
    term' <- localTope "12" tope' $ typecheck term ty'
    return (TypeRestrictedT universeT ty' (tope', term'))

checkCoherence
  :: Eq var
  => (TermT var, TermT var)
  -> (TermT var, TermT var)
  -> TypeCheck var ()
checkCoherence (ltope, lterm) (rtope, rterm) =
  performing (ActionCheckCoherence (ltope, lterm) (rtope, rterm)) $ do
    localTope "13" (TopeAndT topeT ltope rtope) $ do
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
