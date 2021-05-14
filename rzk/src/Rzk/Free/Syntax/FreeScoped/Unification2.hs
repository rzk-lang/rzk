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
module Rzk.Free.Syntax.FreeScoped.Unification2 where

import           Bound.Name
import           Bound.Scope                            (instantiate,
                                                         instantiate1, toScope)
import qualified Bound.Scope                            as Bound
import           Bound.Term                             (substitute)
import           Bound.Var

import           Control.Monad.State
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
                                                         initBindState,
                                                         noUBoundVarsIn)
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

  appSome :: term -> [term] -> (t scope term, [term])

  unAppSome :: t scope term -> Maybe (term, [term])

  abstract :: scope -> t scope term
  -- abstract :: scope? -> t (Scope Int scope) term


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
  :: Unifiable term
  => FreeScoped b term a
  -> (FreeScoped b term a, [FreeScoped b term a])
peelApps = \case
  x@PureScoped{} -> (x, [])
  f@(FreeScoped t) ->
    case unAppSome t of
      Nothing        -> (f, [])
      Just (g, args) -> (<> args) <$> peelApps g

mkApps
  :: Unifiable term
  => FreeScoped b term a
  -> [FreeScoped b term a]
  -> FreeScoped b term a
mkApps g [] = g
mkApps g args = mkApps (FreeScoped g') args'
  where
    (g', args') = appSome g args

isStuck :: Unifiable term => UFreeScoped b term a v -> Bool
isStuck t =
  case peelApps t of
    (PureScoped (UMetaVar _), _) -> True
    _                            -> False

simplify
  :: ( MonadBind (UFreeScoped b term a v) v m
     , MonadPlus m
     , Eq a, Eq b, Eq v
     , Unifiable term )
  => (UFreeScoped b term a v -> UFreeScoped b term a v)
  -> Constraint b term a v
  -> m (Maybe [Constraint b term a v])
simplify reduce (t1, t2)
  = -- unsafeTraceConstraint' "[simplify]" (t1, t2) $
  case (reduce t1, reduce t2) of
    (PureScoped b1@UBoundVar{}, PureScoped b2@UBoundVar{})
      | b1 == b2  -> return (Just [])
      | otherwise -> mzero
    (t1', t2')
      | isStuck t1' || isStuck t2' -> return Nothing
    (t1', t2')
      | (PureScoped x1, args1@(_:_)) <- peelApps t1'
      , (PureScoped x2, args2@(_:_)) <- peelApps t2' -> do
          guard (x1 == x2)
          guard (length args1 == length args2)
          if length args1 == 0
             then return Nothing
             else return (Just (zip args1 args2))
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
          Just . bifold <$> bitraverse goScope go t
    _ -> return Nothing

repeatedlySimplify
  :: ( MonadBind (UFreeScoped b term a v) v m
     , MonadPlus m
     , Unifiable term
     , Eq a, Eq b, Eq v )
  => (UFreeScoped b term a v -> UFreeScoped b term a v)
  -> [Constraint b term a v]
  -> m [Constraint b term a v]
repeatedlySimplify reduce = go
  where
    go [] = return []
    go (c:cs) = do
      simplify reduce c >>= \case
        Nothing -> do
          cs' <- go cs
          return (c:cs')
        Just c' -> do
          go (c' <> cs)

metavars :: Bifoldable term => UFreeScoped b term a v -> [v]
metavars = foldMap F.toList . F.toList

tryFlexRigid
  :: forall b term a v m.
    ( MonadBind (UFreeScoped b term a v) v m
    , MonadPlus m
    , Unifiable term
    , Eq a, Eq b, Eq v
    , IndexVar b)
  => Constraint b term a v -> [m [Subst b term a v]]
tryFlexRigid (t1, t2)
  | (PureScoped (UMetaVar i), cxt1) <- peelApps t1,
    (stuckTerm, cxt2) <- peelApps t2,
    not (i `elem` metavars t2) = proj (length cxt1) i stuckTerm 0
  | (PureScoped (UMetaVar i), cxt1) <- peelApps t2,
    (stuckTerm, cxt2) <- peelApps t1,
    not (i `elem` metavars t1) = proj (length cxt1) i stuckTerm 0
  | otherwise = []
  where
    -- proj :: Int -> v -> UFreeScoped b term a v -> Int -> [m [Subst b term a v]]
    proj bvars mv f nargs = map (generateSubst bvars mv f) [nargs ..]

    -- generateSubst :: Int -> v -> UFreeScoped b term a v -> Int -> m [Subst b term a v]
    generateSubst bvars mv f nargs = do
      let saturateMV tm = mkApps tm (map (PureScoped . B . fromIndex) [0..bvars - 1])
      let mkSubst t = [(mv, t)]
      args <- map saturateMV . map (PureScoped . F . UMetaVar)
                <$> replicateM nargs freshMeta
      return
        [ mkSubst (abstractInt bvars (mkApps t args))
        | t <- map (PureScoped . B) [0..bvars - 1] ++
                  if noUBoundVarsIn f then [fmap F f] else []]

abstractInt
  :: (Unifiable term, IndexVar b)
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
    wrapSomeBoundVars bs (F x) = F (F x)

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
manySubst s t = foldr (\(mv, t) sol -> substMV t mv sol) t s

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
      PureScoped x -> x
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
      (AppF t1 t2, AppF e1 e2) -> Just (AppF (Right (t1, e1)) (Right (t2, e2)))
      _ -> Nothing

  appSome f []     = error "cannot apply to zero arguments"
  appSome f (x:xs) = (AppF f x, xs)

  unAppSome = \case
    AppF t1 t2 -> Just (t1, [t2])
    _ -> Nothing

  abstract body = AbsF body

run :: (Monad m) => AssocBindT term var m a -> m a
run = flip evalStateT initBindState . runAssocBindT

unify
  :: ( MonadBind (UFreeScoped b term a v) v m
     , MonadPlus m
     , Unifiable term
     , Eq a, Eq b, Eq v
     , IndexVar b )
  => (UFreeScoped b term a v -> UFreeScoped b term a v)
  -> Subst b term a v
  -> [Constraint b term a v]
  -> m (Subst b term a v, [Constraint b term a v])
unify reduce s cs = do
  -- unsafeTraceConstraints' "[unify]" cs $ do
  let cs' = applySubst s cs
    -- unsafeTraceConstraints' "[unify2]" cs' $ do
  cs'' <- repeatedlySimplify reduce cs'
  let (flexflexes, flexrigids) = partition flexflex cs''
  case flexrigids of
    [] -> return (s, flexflexes)
    fr:_ -> do
      let psubsts = tryFlexRigid fr
      trySubsts psubsts (flexrigids <> flexflexes)
  where
    applySubst s = map (\(t1, t2) -> (manySubst s t1, manySubst s t2))
    flexflex (t1, t2) = isStuck t1 && isStuck t2
    trySubsts [] cs = mzero
    trySubsts (mss : psubsts) cs = do
      ss <- mss
      let these = foldr mplus mzero [unify reduce (newS <+> s) cs | newS <- ss]
      let those = trySubsts psubsts cs
      these `mplus` those

driver
  :: (MonadPlus m, Unifiable term, Eq v, Eq a, Eq b, IndexVar b)
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
  :: (Unifiable term, Eq a, Eq b, IndexVar b)
  => (UFreeScoped b term a Rzk.Var -> UFreeScoped b term a Rzk.Var)
  -> Constraint b term a Rzk.Var
  -> [(Subst b term a Rzk.Var, [Constraint b term a Rzk.Var])]
driverDefault = driver (iterate succ "?")
