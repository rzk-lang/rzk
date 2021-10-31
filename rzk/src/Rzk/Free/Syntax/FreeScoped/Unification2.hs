{-# LANGUAGE CPP             #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
module Rzk.Free.Syntax.FreeScoped.Unification2 where

import           Bound.Name
import           Bound.Scope                            (instantiate,
                                                         instantiate1, toScope)
import qualified Bound.Scope                            as Bound
import           Bound.Term                             (substitute)
import           Bound.Var
import           Data.Maybe                             (mapMaybe)

import           Control.Monad.State

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif

import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bifunctor.TH
import           Data.Bitraversable
import qualified Data.Foldable                          as F
import           Data.List                              (elemIndex, partition)

import qualified Rzk.Free.Bound.Name                    as Rzk
import           Rzk.Free.Syntax.FreeScoped
import           Rzk.Free.Syntax.FreeScoped.Unification (AssocBindT (..),
                                                         BindState (..),
                                                         Constraint,
                                                         MonadBind (..), Subst,
                                                         UFreeScoped, UVar (..),
                                                         initBindState)
import qualified Rzk.Syntax.Var                         as Rzk

class IndexVar a where
  fromIndex :: Int -> a
  fromIndices :: Int -> ([a], Int)
  toIndex :: a -> Int

instance IndexVar Int where
  fromIndex = fromIntegral
  fromIndices n = ([0..n-1], 0)
  toIndex = fromIntegral

instance IndexVar () where
  fromIndex _ = ()
  fromIndices n = ([()], n-1)
  toIndex _ = 0

instance (Monoid n, IndexVar b) => IndexVar (Name n b) where
  fromIndex i = Name mempty (fromIndex i)
  fromIndices n =
    case fromIndices n of
      (bs, m) -> (Name mempty <$> bs, m)
  toIndex (Name _ i) = toIndex i

instance IndexVar b => IndexVar (Rzk.Name n b) where
  fromIndex i = Rzk.Name Nothing (fromIndex i)
  fromIndices n =
    case fromIndices n of
      (bs, m) -> (Rzk.Name Nothing <$> bs, m)
  toIndex (Rzk.Name _ i) = toIndex i

class Bitraversable t => Unifiable t where
  zipMatch
    :: t scope term
    -> t scope term
    -> Maybe (t (Either scope (scope, scope)) (Either term (term, term)))

class Unifiable t => HigherOrderUnifiable t where
  appSome :: term -> [term] -> (t scope term, [term])

  unAppSome :: t scope term -> Maybe (term, [term])

  abstract :: scope -> t scope term

instance (Unifiable f, Unifiable g) => Unifiable (f :+: g) where
  zipMatch (InL f1) (InL f2) = fmap InL (zipMatch f1 f2)
  zipMatch (InR g1) (InR g2) = fmap InR (zipMatch g1 g2)
  zipMatch _ _               = Nothing

instance (Unifiable f, HigherOrderUnifiable g) => HigherOrderUnifiable (f :+: g) where
  appSome f args = bimap InR id (appSome f args)

  unAppSome (InL _) = Nothing
  unAppSome (InR t) = unAppSome t

  abstract body = InR (abstract body)

-- data ExprF var expr
--   = LambdaF var expr
--   | AppF expr expr
--   | VarF var
--
-- appSome f []     = (f, [])
-- appSome f (x:xs) = (AppF f x, [xs])
--
-- unAppSome (App f arg) = Just (f, [arg])
-- unAppSome _           = Nothing

-- f(3) = 3 + 3
-- f(x) = (?1 x) + (?2 x)
-- f(x) = ?0
-- f(x) = ?0 (?1 x)
-- f(x) = ?0 (?1 x) (?2 x)
-- f(x) = ?0 (?1 x) (?2 x) (?3 x) ...

peelApps
  :: HigherOrderUnifiable term
  => FreeScoped b term a
  -> (FreeScoped b term a, [FreeScoped b term a])
peelApps = \case
  x@PureScoped{} -> (x, [])
  f@(FreeScoped t) ->
    case unAppSome t of
      Nothing        -> (f, [])
      Just (g, args) -> (<> args) <$> peelApps g

mkApps
  :: HigherOrderUnifiable term
  => FreeScoped b term a
  -> [FreeScoped b term a]
  -> FreeScoped b term a
mkApps g [] = g
mkApps g args = mkApps (FreeScoped g') args'
  where
    (g', args') = appSome g args

isStuck :: HigherOrderUnifiable term => UFreeScoped b term a v -> Bool
isStuck t =
  case peelApps t of
    (PureScoped (UMetaVar _), _) -> True
    _                            -> False

data SimplifyResult a
  = CannotSimplify
  | Simplified [a]

simplify
  :: ( MonadBind v m
     , MonadPlus m
     , Eq a, Eq b, Eq v
     , HigherOrderUnifiable term )
  => (UFreeScoped b term a v -> UFreeScoped b term a v)
  -> Constraint b term a v
  -> m (SimplifyResult (Constraint b term a v))
simplify reduce (t1, t2)
  = -- unsafeTraceConstraint' "[simplify]" (t1, t2) $
  case (reduce t1, reduce t2) of
    (PureScoped b1@UBoundVar{}, PureScoped b2@UBoundVar{})
      | b1 == b2  -> return (Simplified [])
      | otherwise -> mzero
    (t1', t2')
      | isStuck t1' || isStuck t2' -> return CannotSimplify
    (t1', t2')
      | (PureScoped x1, args1) <- peelApps t1'
      , (PureScoped x2, args2) <- peelApps t2' -> do
          guard (x1 == x2)
          guard (length args1 == length args2)
          return (Simplified (zip args1 args2))
    (FreeScoped t1', FreeScoped t2')
      | Just t <- zipMatch t1' t2' -> do
          let go (Left _)           = return []
              go (Right (tt1, tt2)) = return [(tt1, tt2)]

              goScope (Left _) = return []
              goScope (Right (s1, s2)) = do
                i <- freshMeta
                let ss1 = instantiate (pure . UBoundVar i) s1
                    ss2 = instantiate (pure . UBoundVar i) s2
                return [(ss1, ss2)]
          Simplified . bifold <$> bitraverse goScope go t
    _ -> return CannotSimplify

repeatedlySimplify
  :: ( MonadBind v m
     , MonadPlus m
     , HigherOrderUnifiable term
     , Eq a, Eq b, Eq v )
  => (UFreeScoped b term a v -> UFreeScoped b term a v)
  -> [Constraint b term a v]
  -> m [Constraint b term a v]
repeatedlySimplify reduce = go
  where
    go [] = return []
    go (c:cs) = do
      simplify reduce c >>= \case
        CannotSimplify -> do
          cs' <- go cs
          return (c:cs')
        Simplified c' -> do
          go (c' <> cs)

metavars :: Bifoldable term => UFreeScoped b term a v -> [v]
metavars = foldMap F.toList . F.toList

ignoreVarInTerm
  :: (HigherOrderUnifiable term, MonadBind v m, IndexVar b, Eq b, Eq a)
  => a -> UFreeScoped b term a v -> m (Subst b term a v)
ignoreVarInTerm x t =
  case peelApps t of
    (PureScoped (UMetaVar m), args)
      | any (any (== UFreeVar x)) args -> do
          m' <- freshMeta
          let new = abstractInt n (mkApps (PureScoped (F (UMetaVar m'))) args')
              n = length args
              args' = mapMaybe keepOrSkip (zip [0..] args)
              keepOrSkip (i, arg)
                | any (== UFreeVar x) arg = Nothing
                | otherwise = Just (PureScoped (B (n - i - 1)))
          return [(m, new)]
      | otherwise -> return []
    _ -> return [] -- error "expected only flex-terms"

ignoreVarInConstraints
  :: (MonadBind v m, HigherOrderUnifiable term, IndexVar b, Eq b, Eq a)
  => a -> [Constraint b term a v] -> m (Subst b term a v, [Constraint b term a v])
ignoreVarInConstraints x cs =
  getFirst (ignoreVarInTerm x) (concatMap both cs) >>= \case
    [] -> return ([], cs)
    subst -> do
      (moreSubst, cs') <- ignoreVarInConstraints x (bimap (manySubst subst) (manySubst subst) <$> cs)
      return (moreSubst <+> subst, cs')
  where
    both (t1, t2) = [t1, t2]

    getFirst _ [] = return []
    getFirst g (z:zs) = g z >>= \case
      []    -> getFirst g zs
      subst -> return subst

ignoreVarIn
  :: (MonadBind v m, HigherOrderUnifiable term, IndexVar b, Eq b, Eq a, Functor t, Foldable t)
  => a -> t (UFreeScoped b term a v) -> m (Subst b term a v, t (UFreeScoped b term a v))
ignoreVarIn x ts =
  getFirst (ignoreVarInTerm x) (F.toList ts) >>= \case
    [] -> return ([], ts)
    subst -> do
      (moreSubst, ts') <- ignoreVarIn x (manySubst subst <$> ts)
      return (moreSubst <+> subst, ts')
  where
    getFirst _ [] = return []
    getFirst g (z:zs) = g z >>= \case
      []    -> getFirst g zs
      subst -> return subst

ignoreVarInLeft
  :: (MonadBind v m, HigherOrderUnifiable term, IndexVar b, Eq b, Eq a, Bifunctor t, Bifoldable t)
  => a -> t (UFreeScoped b term a v) x -> m (Subst b term a v, t (UFreeScoped b term a v) x)
ignoreVarInLeft x ts =
  getFirst (ignoreVarInTerm x) (bifoldMap pure (const []) ts) >>= \case
    [] -> return ([], ts)
    subst -> do
      (moreSubst, ts') <- ignoreVarInLeft x (bimap (manySubst subst) id ts)
      return (moreSubst <+> subst, ts')
  where
    getFirst _ [] = return []
    getFirst g (z:zs) = g z >>= \case
      []    -> getFirst g zs
      subst -> return subst

tryFlexRigid
  :: forall b term a v m.
    ( MonadBind v m
    , MonadFail m
    , MonadPlus m
    , HigherOrderUnifiable term
    , Eq a, Eq b, Eq v
    , IndexVar b)
  => Constraint b term a v -> [m [Subst b term a v]]
tryFlexRigid (t1, t2)
  | (PureScoped (UMetaVar i), cxt1) <- peelApps t1,
    (stuckTerm, _cxt2) <- peelApps t2,
    not (i `elem` metavars t2) =
      if null cxt1
         then [pure [[(i, t2)]]]
         else proj (length cxt1) i stuckTerm 0
  | (PureScoped (UMetaVar i), cxt1) <- peelApps t2,
    (stuckTerm, _cxt2) <- peelApps t1,
    not (i `elem` metavars t1) =
      if null cxt1
         then [pure [[(i, t2)]]]
         else proj (length cxt1) i stuckTerm 0
  | otherwise = []
  where
    -- proj :: Int -> v -> UFreeScoped b term a v -> Int -> [m [Subst b term a v]]
    proj bvars mv f nargs = map (generateSubst bvars mv f) [nargs .. maxArgs]
      ++ fail ("too many invalid guesses in a higher-order unification procedure")

    maxArgs = 100 -- FIXME: make configurable (and optional?)

    -- generateSubst :: Int -> v -> UFreeScoped b term a v -> Int -> m [Subst b term a v]
    generateSubst bvars mv f nargs = do
      let saturateMV tm = mkApps tm (map (PureScoped . B . fromIndex) [0..bvars - 1])
      let mkSubst t = [(mv, t)]
      args <- map saturateMV . map (PureScoped . F . UMetaVar)
                <$> replicateM nargs freshMeta
      return
        [ mkSubst (abstractInt bvars (mkApps t args))
        | t <- map (PureScoped . B) [0..bvars - 1]
                ++ [F <$> abstractUBoundVars f]]

abstractUBoundVars
  :: (HigherOrderUnifiable term, IndexVar b)
  => UFreeScoped b term a v
  -> UFreeScoped b term a v
abstractUBoundVars t = abstractInt n $
  flip evalState 0 $ do
    forM t $ \case
      UBoundVar{} -> do
        i <- get
        modify (+1)
        return (B i)
      v -> return (F v)
  where
    n = length (filter isUBoundVar (F.toList t))

    isUBoundVar UBoundVar{} = True
    isUBoundVar _           = False

abstractInt
  :: (HigherOrderUnifiable term, IndexVar b)
  => Int -> FreeScoped b term (Var Int a) -> FreeScoped b term a
abstractInt n
  | n <= 0 = fmap unsafeExtractFreeVar
  | otherwise =
      case fromIndices n of
        (bs, m) -> abstractInt m .
          FreeScoped . abstract . toScope . fmap (wrapSomeBoundVars bs)
  where
    unsafeExtractFreeVar (F x) = x
    unsafeExtractFreeVar (B _) = error "unexpected bound variable!"

    wrapSomeBoundVars bs (B i)
      | i < length bs = B (bs !! i)
      | otherwise = F (B (i - length bs))
    wrapSomeBoundVars _bs (F x) = F (F x)

substMV
  :: (Bifunctor term, Eq a, Eq v, Eq b)
  => UFreeScoped b term a v
  -> v
  -> UFreeScoped b term a v
  -> UFreeScoped b term a v
substMV new v t = substitute (UMetaVar v) new t

manySubst
  :: (Bifunctor term, Eq a, Eq v, Eq b)
  => Subst b term a v -> UFreeScoped b term a v -> UFreeScoped b term a v
manySubst s t = foldr (\(mv, t') sol -> substMV t' mv sol) t s

(<+>)
  :: (Bifunctor term, Eq a, Eq v, Eq b)
  => Subst b term a v -> Subst b term a v -> Subst b term a v
-- s1 <+> s2 | not (null (intersect s1 s2)) = error "Impossible"
s1 <+> s2 = (fmap (manySubst s1) <$> s2) ++ s1

data LambdaF scope term
  = AbsF scope
  | AppF term term
  deriving (Functor, Foldable, Traversable)

deriveBifunctor ''LambdaF
deriveBifoldable ''LambdaF
deriveBitraversable ''LambdaF

type Lambda a = FreeScoped Int LambdaF a

type ULambda a = UFreeScoped Int LambdaF a Int

instance Show a => Show (Lambda a) where
  show = ppLambda . fmap show

whnf :: Lambda a -> Lambda a
whnf = \case
  t@PureScoped{} -> t
  t@(FreeScoped ft) ->
    case ft of
      AbsF{} -> t
      AppF (FreeScoped (AbsF body)) arg ->
        whnf (instantiate1 arg body)
      AppF f arg -> FreeScoped (AppF (whnf f) arg)

ex1 :: Lambda String
ex1 = FreeScoped (AbsF (Bound.abstract (`elemIndex` ["x"]) (PureScoped "x")))

ex2 :: Lambda String
ex2 = FreeScoped (AppF ex1 ex1)

ex3 :: Lambda String
ex3 = whnf ex2

u :: Lambda a -> ULambda a
u = fmap UFreeVar

ex4 :: ULambda String
ex4 = FreeScoped (AppF (PureScoped (UMetaVar 0)) (u ex1))

ppLambda :: Lambda String -> String
ppLambda = go (map pure ['a'..'z'])
  where
    go vars = \case
      PureScoped z -> z
      FreeScoped t ->
        case t of
          AbsF scope -> "\\" <> x <> "." <> go xs (instantiate1 (PureScoped x) scope)
          AppF t1 t2 -> goLeft vars t1 <> goParens vars t2
      where
        x:xs = vars

    goLeft vars = \case
      t@PureScoped{} -> go vars t
      t@(FreeScoped ft) ->
        case ft of
          AbsF{} -> parens (go vars t)
          AppF{} -> go vars t

    goParens vars = \case
      t@PureScoped{} -> go vars t
      t@(FreeScoped ft) ->
        case ft of
          AbsF{} -> parens (go vars t)
          AppF{} -> parens (go vars t)

    parens s = "(" <> s <> ")"

instance Unifiable LambdaF where
  zipMatch t1 t2 =
    case (t1, t2) of
      (AbsF body1, AbsF body2) -> Just (AbsF (Right (body1, body2)))
      (AppF f1 x1, AppF f2 x2) -> Just (AppF (Right (f1, f2)) (Right (x1, x2)))
      _ -> Nothing

instance HigherOrderUnifiable LambdaF where
  appSome _f []    = error "cannot apply to zero arguments"
  appSome f (x:xs) = (AppF f x, xs)

  unAppSome = \case
    AppF t1 t2 -> Just (t1, [t2])
    _ -> Nothing

  abstract body = AbsF body

run :: (Monad m) => AssocBindT term var m a -> m a
run = flip evalStateT initBindState . runAssocBindT

unify
  :: ( MonadBind v m
     , MonadFail m
     , MonadPlus m
     , HigherOrderUnifiable term
     , Eq a, Eq b, Eq v
     , IndexVar b )
  => (UFreeScoped b term a v -> UFreeScoped b term a v)
  -> Subst b term a v
  -> [Constraint b term a v]
  -> m (Subst b term a v, [Constraint b term a v])
unify reduce s cs = do
  -- unsafeTraceConstraints' "[unify]" cs $ do
    -- unsafeTraceConstraints' "[unify2]" cs' $ do
  cs'' <- repeatedlySimplify reduce cs'
  let (flexflexes, flexrigids) = partition flexflex cs''
  case flexrigids of
    [] -> return (s', flexflexes)
    fr:_ -> do
      let psubsts = tryFlexRigid fr
      trySubsts psubsts (flexrigids <> flexflexes)
  where
    (csSubsts, _csWithoutSubsts) = extractSubsts cs
    s'  = csSubsts <+> s
    cs' = applySubst csSubsts (applySubst s' (filter (not . trivial) cs))

    trivial (PureScoped (UMetaVar v1), PureScoped (UMetaVar v2))
      = v1 == v2
    trivial _ = False

    extractSubsts = \case
      [] -> ([], [])
      (PureScoped (UMetaVar v1), PureScoped (UMetaVar v2)):cs''
        | v1 == v2 -> extractSubsts cs''
      (PureScoped (UMetaVar v), t):cs''
        | v `notElem` metavars t ->
          case extractSubsts cs'' of
            (ss, cs''') -> ([(v, t)] <+> ss, cs''')
      (t, PureScoped (UMetaVar v)):cs''
        | v `notElem` metavars t ->
          case extractSubsts cs'' of
            (ss, cs''') -> ([(v, t)] <+> ss, cs''')
      c:cs'' ->
        case extractSubsts cs'' of
          (ss, cs''') -> (ss, c:cs''')

    applySubst st = map (\(t1, t2) -> (manySubst st t1, manySubst st t2))
    flexflex (t1, t2) = isStuck t1 && isStuck t2
    trySubsts [] _cs = mzero
    trySubsts (mss : psubsts) cs'' = do
      ss <- mss
      let these = foldr mplus mzero [unify reduce (newS <+> s') cs'' | newS <- ss]
      let those = trySubsts psubsts cs''
      these `mplus` those

driver
  :: (MonadPlus m, HigherOrderUnifiable term, Eq v, Eq a, Eq b, IndexVar b)
  => [v]
  -> (UFreeScoped b term a v -> UFreeScoped b term a v)
  -> Constraint b term a v
  -> m (Subst b term a v, [Constraint b term a v])
driver mvars reduce
  = flip evalStateT initBindState { freshMetas = mvars }
  . runAssocBindT
  . unify reduce []
  . (\x -> [x])

driverDefault
  :: (HigherOrderUnifiable term, Eq a, Eq b, IndexVar b)
  => (UFreeScoped b term a Rzk.Var -> UFreeScoped b term a Rzk.Var)
  -> Constraint b term a Rzk.Var
  -> [(Subst b term a Rzk.Var, [Constraint b term a Rzk.Var])]
driverDefault = driver (iterate succ "?")
