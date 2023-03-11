{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Rzk.Evaluator where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Functor.Identity
import           Data.List             (nub, (\\))
import           Data.Maybe            (fromMaybe, isNothing)
import           Data.Monoid           (Endo (..))
import           Data.Text             (Text)
import qualified Data.Text             as Text

import           Rzk.Pretty.Text       (ppTerm, ppVar)
import           Rzk.Syntax.Term
import           Rzk.Debug.Trace
import           Rzk.Syntax.Var

-- $setup
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import Rzk.Parser.Text

-- | Evaluation errors.
data EvalError var
  = EvalErrorUndefinedVariable var
  | EvalErrorInvalidPattern (Term var)

instance Show (EvalError Var) where
  show = Text.unpack . ppEvalError

ppEvalError :: EvalError Var -> Text
ppEvalError = \case
  EvalErrorUndefinedVariable x -> Text.intercalate "\n"
    [ "Undefined variable " <> ppVar x ]
  EvalErrorInvalidPattern pattern -> Text.intercalate "\n"
    [ "Invalid pattern " <> ppTerm pattern ]

-- | Evaluation context.
data Context var = Context
  { contextDefinedVariables :: [(var, Term var)]
    -- ^ Defined variables (variables that have terms assigned to them).
  , contextFreeVariables    :: [var]
    -- ^ Known free variables, used to avoid name clashes during substitution.
  , contextTopes            :: [Term var]
    -- ^ Topes in the context.
  , contextTopeInclusions   :: [(Term var, Term var)]
    -- ^ Known tope inclusions \( \phi \vdash \psi \).
  }

-- | Empty evaluation context.
emptyContext :: Context var
emptyContext = Context
  { contextDefinedVariables = []
  , contextFreeVariables = []
  , contextTopes = []
  , contextTopeInclusions = []
  }

-- | Reassign term value to a variable.
updateVar :: var -> Term var -> Context var -> Context var
updateVar x t context@Context{..} = context
  { contextDefinedVariables = (x, t) : contextDefinedVariables }

-- | Reassign term value to a variable.
updateVars :: [(var, Term var)] -> Context var -> Context var
updateVars xs context@Context{..} = context
  { contextDefinedVariables = xs <> contextDefinedVariables }

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

localVars :: MonadReader (Context var) m => [var] -> m a -> m a
localVars xs = local (updateVars (zip xs (Variable <$> xs)))

localPattern
  :: (MonadError (EvalError var) m, MonadReader (Context var) m)
  => (Term var, Term var) -> m a -> m a
localPattern = \case
  (Variable x, t) -> localVar (x, t)
  (Pair x y,   t) -> localPattern (x, First t) . localPattern (y, Second t)
  (pattern, _) -> const $ throwError (EvalErrorInvalidPattern pattern)

-- | Add tope constraint locally during evaluation.
localConstraint :: MonadReader (Context var) m => Term var -> m a -> m a
localConstraint phi = local (\context -> context { contextTopes = phi : contextTopes context })

-- | Add tope inclusion information locally during evaluation.
localTopeInclusion :: MonadReader (Context var) m => Term var -> Term var -> m a -> m a
localTopeInclusion psi phi = local (\context -> context { contextTopeInclusions = (psi, phi) : contextTopeInclusions context })

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
  TypedTerm term ty -> freeVars term <> freeVars ty
  Hole _ -> []
  Universe -> []
  Pi t -> freeVars t
  Lambda x a phi m -> foldMap freeVars a <> ((foldMap freeVars phi <> freeVars m) \\ freeVars x)
  App t1 t2 -> freeVars t1 <> freeVars t2
  Sigma t -> freeVars t
  Pair t1 t2 -> freeVars t1 <> freeVars t2
  First t -> freeVars t
  Second t -> freeVars t
  IdType a x y -> freeVars a <> freeVars x <> freeVars y
  Refl a x -> foldMap freeVars a <> freeVars x
  IdJ tA a tC d x p -> concatMap freeVars [tA, a, tC, d, x, p]

  Cube -> []
  CubeUnit -> []
  CubeUnitStar -> []
  CubeProd i j -> freeVars i <> freeVars j

  Tope -> []
  TopeTop -> []
  TopeBottom -> []
  TopeOr psi phi -> freeVars psi <> freeVars phi
  TopeAnd psi phi -> freeVars psi <> freeVars phi
  TopeEQ t s -> freeVars t <> freeVars s
  RecBottom -> []
  RecOr psi phi a b -> concatMap freeVars [psi, phi, a, b]

  ExtensionType t cI psi tA phi a ->
    freeVars cI <> (concatMap freeVars [psi, tA, phi, a] \\ freeVars t)

  Cube2 -> []
  Cube2_0 -> []
  Cube2_1 -> []
  TopeLEQ t s -> freeVars t <> freeVars s

-- | Evaluate an open term (some variables might occur freely).
--
-- >>> evalOpen @Var "(λ(x : A) → x =_{A} (λ(y : B) → y)) y"
-- Right y =_{A} (λ(y₁ : B) → y₁)
evalOpen :: (Eq var, Enum var) => Term var -> Either (EvalError var) (Term var)
evalOpen t = go t
  where
    go = runExcept . flip runReaderT context . runEval . eval
    context = emptyContext { contextFreeVariables = freeVars t }

-- | Evaluate a term.
--
-- * \((\lambda x. M) N \mapsto M[N/x]\)
-- * \(\pi_1 (x, y) \mapsto x\)
-- * \(\pi_2 (x, y) \mapsto y\)
-- * \(\mathcal{J}(A,a,C,d,a,\mathsf{refl}_a) \mapsto d\)
eval :: (Eq var, Enum var) => Term var -> Eval var (Term var)
eval = \case
  Variable x -> fromMaybe (Variable x) <$> lookupVar x
  TypedTerm term ty -> TypedTerm <$> eval term <*> eval ty -- FIXME: maybe type first?
  Hole x -> pure (Hole x)
  Universe -> pure Universe
  Pi t -> Pi <$> eval t
  Lambda x a phi m -> do
    vars <- asks contextKnownVars
    let xs = freeVars x
        doRename = any (`elem` vars) xs
        xxs' = refreshVars (vars <> freeVars m <> foldMap freeVars phi) xs
        rename = if doRename then renameVars xxs' else id
        xs' = if doRename then map snd xxs' else xs
        x' = rename x
        ev = localVars xs' . eval . rename
    Lambda x' <$> traverse eval a <*> traverse ev phi <*> ev m
  App t1 t2 -> join (app <$> eval t1 <*> eval t2)
  Sigma t -> Sigma <$> eval t
  Pair t1 t2 -> pair <$> eval t1 <*> eval t2
  First t -> eval t >>= pure . \case
    Pair f _ -> f
    t'       -> First t'
  Second t -> eval t >>= pure . \case
    Pair _ s -> s
    t'       -> Second t'
  IdType a x y -> IdType <$> eval a <*> eval x <*> eval y
  Refl a x -> Refl <$> traverse eval a <*> eval x
  IdJ tA a tC d x p -> eval p >>= \case
    Refl _ _ -> eval d
    p' -> IdJ <$> eval tA <*> eval a <*> eval tC <*> eval d <*> eval x <*> pure p'

  Cube -> pure Cube
  CubeUnit -> pure CubeUnit
  CubeUnitStar -> pure CubeUnitStar
  CubeProd i j -> CubeProd <$> eval i <*> eval j

  Tope -> pure Tope
  TopeTop -> pure TopeTop
  TopeBottom -> pure TopeBottom

  TopeOr  psi phi -> TopeOr <$> eval psi <*> eval phi
  TopeAnd psi phi -> TopeAnd <$> eval psi <*> eval phi
  TopeEQ  x   y   -> TopeEQ <$> eval x <*> eval y

  RecBottom -> pure RecBottom
  RecOr psi phi a b -> do
    Context{..} <- ask
    psi' <- eval psi
    if (contextTopes `entailTope` psi') then eval a else do
      phi' <- eval phi
      if (contextTopes `entailTope` phi') then eval b else do
        a' <- eval a
        b' <- eval b
        pure $ if a' == b'
                  then a'
                  else RecOr psi' phi' a' b'

  ExtensionType t cI psi tA phi a -> do
    vars <- asks contextKnownVars

    let ts = freeVars t
        doRename = any (`elem` vars) ts
        tts' = refreshVars (vars <> concatMap freeVars [psi, tA, phi, a]) ts
        rename = if doRename then renameVars tts' else id
        ts' = if doRename then map snd tts' else ts
        t' = rename t
        ev = localVars ts' . eval . rename
    ExtensionType t' <$> eval cI <*> ev psi <*> ev tA <*> ev phi <*> ev a

  Cube2 -> pure Cube2
  Cube2_0 -> pure Cube2_0
  Cube2_1 -> pure Cube2_1
  TopeLEQ t s -> TopeLEQ <$> eval t <*> eval s

unfoldTopes :: Eq var => [Term var] -> [[Term var]]
unfoldTopes [] = [[]]
unfoldTopes (tope:topes) = nub $
  case tope of
    TopeBottom      -> [[TopeBottom]]
    TopeTop         -> topeses
    TopeOr phi psi  -> concat
      [ unfoldTopes (tope':topes)
      | tope' <- [phi, psi]
      ]
    TopeAnd phi psi -> unfoldTopes (phi : psi : topes)
    t@(TopeEQ x y)  -> (\ts -> [t, TopeEQ y x] ++ ts {- apply substition? -}) <$> topeses
    t               -> (t:) <$> topeses
  where
    topeses = unfoldTopes topes

unfoldRepeatedly :: Eq a => ([a] -> [a]) -> [a] -> [a]
unfoldRepeatedly unfold xs
  | null xs' = xs
  | otherwise = unfoldRepeatedly unfold (nub (xs' <> xs))
  where
    xs' = unfold xs \\ xs

unfoldRepeatedlyN :: Eq a => ([a] -> [a]) -> Int -> [a] -> [a]
unfoldRepeatedlyN unfold n xs
  | n == 0 = xs
  | null xs' = xs
  | otherwise = unfoldRepeatedlyN unfold (n - 1) (nub (xs' <> xs))
  where
    xs' = unfold xs \\ xs

unfoldTopesInCube2 :: Eq var => [Term var] -> [Term var]
unfoldTopesInCube2
  = unfoldRepeatedlyN unfoldTopesInCube2Once 10

unfoldTopesInCube2Once :: Eq var => [Term var] -> [Term var]
unfoldTopesInCube2Once
  = antisymmetryTopesInCube2
      <> distinctTopes
      <> transitivityTopesInCube2
      <> transitivityTopesEQ
      <> unfoldConjunction

unfoldConjunction :: [Term var] -> [Term var]
unfoldConjunction topes =
  [ t
  | TopeAnd psi phi <- topes
  , t <- [psi, phi]
  ]

transitivityTopesInCube2 :: Eq var => [Term var] -> [Term var]
transitivityTopesInCube2 topes =
  [ TopeLEQ x z
  | TopeLEQ x y <- topes
  , TopeLEQ y' z <- topes
  , y == y'
  , x /= z
  ]

transitivityTopesEQ :: Eq var => [Term var] -> [Term var]
transitivityTopesEQ topes =
  [ TopeEQ x z
  | TopeEQ x y <- topes
  , TopeEQ y' z <- topes
  , y == y'
  , x /= z
  ]

antisymmetryTopesInCube2 :: Eq var => [Term var] -> [Term var]
antisymmetryTopesInCube2 topes =
  [ TopeEQ x y
  | TopeLEQ x y <- topes
  , TopeLEQ y' x' <- topes
  , x == x'
  , y == y'
  ]

distinctTopes :: [Term var] -> [Term var]
distinctTopes topes = [ TopeBottom | TopeEQ Cube2_1 Cube2_0 <- topes ]

entailTope :: Eq var => [Term var] -> Term var -> Bool
entailTope topes t =
  runIdentity (entailTopeM (\x y -> pure (x == y)) topes t)

entailTopeM
  :: (Monad m, Eq var)
  => (Term var -> Term var -> m Bool) -> [Term var] -> Term var -> m Bool
entailTopeM isIncludedIn topes tope = and <$>
  traverse (\topes' -> entailTopeM' isIncludedIn (unfoldTopesInCube2 topes') tope) (unfoldTopes (topes ++ addLEQs tope))

addLEQs :: Term var -> [Term var]
addLEQs (TopeLEQ x y)     = [TopeOr (TopeLEQ x y) (TopeLEQ y x)]
addLEQs (TopeOr phi psi)  = addLEQs phi <> addLEQs psi
addLEQs (TopeAnd phi psi) = addLEQs phi <> addLEQs psi
addLEQs (TopeEQ _ _)      = []
addLEQs TopeTop           = []
addLEQs TopeBottom        = []
addLEQs _tope             = []

entailTopeM'
  :: (Monad m, Eq var)
  => (Term var -> Term var -> m Bool) -> [Term var] -> Term var -> m Bool
entailTopeM' isIncludedIn topes = go
  where
    go = \case
      _ | TopeBottom `elem` topes -> pure True
      TopeTop -> pure True
      TopeAnd phi psi -> and <$> sequenceA [ go phi , go psi ]
      TopeOr (TopeLEQ x y) (TopeLEQ y' x')
        | x == x' && y == y' -> return True
      TopeOr phi psi -> go phi >>= \case
        True -> pure True
        False -> go psi
      TopeEQ (First  (Pair x _y)) s -> go (TopeEQ x s)
      TopeEQ (Second (Pair _x y)) s -> go (TopeEQ y s)
      TopeEQ t s | or
        [ t == CubeUnitStar
        , s == CubeUnitStar
        , t == s
        , TopeEQ s t `elem` topes
        ] -> pure True
      TopeLEQ x y -> go (TopeEQ x y) >>= \case
        True -> pure True
        False -> go (TopeEQ x Cube2_0) >>= \case
          True -> pure True
          False -> go (TopeEQ y Cube2_1)
      tope -> anyM (`isIncludedIn` tope) topes

    anyM _ [] = pure False
    anyM p (x:xs) = p x >>= \case
      True -> pure True
      False -> anyM p xs

pair :: (Eq var, Enum var) => Term var -> Term var -> Term var
pair f s =
  case (f, s) of
    (First x, Second y)
      | x == y -> x
    _ -> Pair f s

-- | Evaluate application of one (evaluated) term to another.
app :: (Eq var, Enum var) => Term var -> Term var -> Eval var (Term var)
app t1 n =
  case t1 of
    Lambda x (Just (ExtensionType _ _ _ _ phi a)) Nothing m -> do  -- FIXME: double check
      Context{..} <- ask
      if contextTopes `entailTope` phi
         then eval a
         else localPattern (x, n) (eval m)
    Lambda x _ Nothing m -> localPattern (x, n) (eval m)
    Lambda x _ (Just phi) m -> do
      localPattern (x, n) $ do
        phi' <- eval phi
        localConstraint phi' $ do
          eval m
    TypedTerm _ (ExtensionType _ _ _ _ phi a) -> do
      Context{..} <- ask
      if contextTopes `entailTope` phi
         then do eval a
         else pure (App t1 n)
    TypedTerm t (Pi f) -> do
      TypedTerm <$> app t n <*> app f n
    TypedTerm _ _ -> pure (App t1 n)
    _ -> pure (App t1 n)

renameVars :: (Eq var) => [(var, var)] -> Term var -> Term var
renameVars = appEndo . foldMap (Endo . uncurry renameVar)

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
      TypedTerm term ty -> TypedTerm (go term) (go ty)
      Hole z -> Hole z
      Universe -> Universe
      Pi t' -> Pi (go t')
      Lambda z a phi m
        | x `elem` freeVars z -> Lambda z (go <$> a) phi m
        | otherwise -> Lambda z (go <$> a) (go <$> phi) (go m)
      App t1 t2 -> App (go t1) (go t2)
      Sigma t' -> Sigma (go t')
      Pair f s -> Pair (go f) (go s)
      First t' -> First (go t')
      Second t' -> Second (go t')
      IdType a z y -> IdType (go a) (go z) (go y)
      Refl a z -> Refl (fmap go a) (go z)
      IdJ tA a tC d z p -> IdJ (go tA) (go a) (go tC) (go d) (go z) (go p)

      Cube -> Cube
      CubeUnit -> CubeUnit
      CubeUnitStar -> CubeUnitStar
      CubeProd i j -> CubeProd (go i) (go j)

      Tope -> Tope
      TopeTop -> TopeTop
      TopeBottom -> TopeBottom
      TopeOr psi phi -> TopeOr (go psi) (go phi)
      TopeAnd psi phi -> TopeAnd (go psi) (go phi)
      TopeEQ t' s -> TopeEQ (go t') (go s)
      RecBottom -> RecBottom
      RecOr psi phi a b -> RecOr (go psi) (go phi) (go a) (go b)

      ExtensionType s cI psi tA phi a
        | x `elem` freeVars s -> ExtensionType s (go cI) psi tA phi a
        | otherwise           -> ExtensionType s (go cI) (go psi) (go tA) (go phi) (go a)

      Cube2 -> Cube2
      Cube2_0 -> Cube2_0
      Cube2_1 -> Cube2_1
      TopeLEQ t' s -> TopeLEQ (go t') (go s)

-- | Substitute a (free) variable in a term.
substitute :: (Eq var, Enum var) => var -> Term var -> Term var -> Term var
substitute x tt = go
  where
    go t = case t of
      Variable z
        | z == x    -> tt
        | otherwise -> t
      TypedTerm term ty -> TypedTerm (go term) (go ty)
      Hole z -> Hole z
      Universe -> Universe
      Pi t' -> Pi (go t')
      Lambda z a phi m
        | x `elem` freeVars z -> Lambda z (go <$> a) phi m
        | otherwise -> Lambda z (go <$> a) (go <$> phi) (go m)
      App t1 t2 -> App (go t1) (go t2)
      Sigma t' -> Sigma (go t')
      Pair f s -> Pair (go f) (go s)
      First t' -> First (go t')
      Second t' -> Second (go t')
      IdType a z y -> IdType (go a) (go z) (go y)
      Refl a z -> Refl (fmap go a) (go z)
      IdJ tA a tC d z p -> IdJ (go tA) (go a) (go tC) (go d) (go z) (go p)

      Cube -> Cube
      CubeUnit -> CubeUnit
      CubeUnitStar -> CubeUnitStar
      CubeProd i j -> CubeProd (go i) (go j)

      Tope -> Tope
      TopeTop -> TopeTop
      TopeBottom -> TopeBottom
      TopeOr psi phi -> TopeOr (go psi) (go phi)
      TopeAnd psi phi -> TopeAnd (go psi) (go phi)
      TopeEQ t' s -> TopeEQ (go t') (go s)
      RecBottom -> RecBottom
      RecOr psi phi a b -> RecOr (go psi) (go phi) (go a) (go b)

      ExtensionType s cI psi tA phi a
        | x `elem` freeVars s -> ExtensionType s (go cI) psi tA phi a
        | otherwise           -> ExtensionType s (go cI) (go psi) (go tA) (go phi) (go a)

      Cube2 -> Cube2
      Cube2_0 -> Cube2_0
      Cube2_1 -> Cube2_1
      TopeLEQ t' s -> TopeLEQ (go t') (go s)

