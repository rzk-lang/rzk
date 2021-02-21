{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
module Rzk.Simple.Evaluator where

import qualified Bound
import           Bound.Scope
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader                      (ReaderT, asks,
                                                            runReaderT)
import           Control.Monad.State
import           Data.Hashable                             (Hashable)
import           Data.HashMap.Strict                       (HashMap)
import qualified Data.HashMap.Strict                       as HashMap
import           Rzk.Simple.Syntax.Term
import           Rzk.Simple.Syntax.Var

import           Text.Trifecta                             (Span, Spanned (..))
import qualified Text.Trifecta                             as Trifecta

import qualified Data.Text.Lazy                            as Text
import           Data.Text.Prettyprint.Doc                 (Doc,
                                                            defaultLayoutOptions,
                                                            layoutPretty)
import           Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle,
                                                            renderLazy)

class Monad m => MonadLookup k v m where
  lookupM :: k -> m (Maybe v)

instance MonadLookup k v Identity where
  lookupM = const (return Nothing)

newtype ForgetLookupT m a = ForgetLookupT { runForgetLookupT :: m a }
  deriving (Functor, Applicative, Monad)

instance Monad m => MonadLookup k v (ForgetLookupT m) where
  lookupM = const (return Nothing)

newtype HashMapLookupT k v m a
  = HashMapLookupT { runHashMapLookupT :: ReaderT (HashMap k v) m a }
  deriving (Functor, Applicative, Monad)

instance (Eq k, Hashable k, Monad m) => MonadLookup k v (HashMapLookupT k v m) where
  lookupM = HashMapLookupT . asks . HashMap.lookup

data Thunk a
  = Unevaluated a
  | Evaluating
  | Evaluated a

newtype WhnfHashMapLookupT ann var a m b
  = WhnfHashMapLookupT { runWhnfHashMapLookupT :: StateT (HashMap a (Thunk (AnnotatedTerm ann var a))) m b }
  deriving (Functor, Applicative, Monad)

instance (Eq a, Hashable a, Monad m) => MonadLookup a (AnnotatedTerm ann var a) (WhnfHashMapLookupT ann var a m) where
  lookupM x = WhnfHashMapLookupT $ do
    gets (HashMap.lookup x) >>= \case
      Nothing -> return Nothing
      Just (Unevaluated t) -> do
        modify (HashMap.insert x Evaluating)
        t' <- runWhnfHashMapLookupT (whnfM t)
        modify (HashMap.insert x (Evaluated t'))
        return (Just t')
      Just (Evaluated t) -> return (Just t)
      Just Evaluating -> error "unable to evaluate self-referencing value"

data UndefinedError k = UndefinedError k
  deriving (Show)

ppUndefinedError :: UndefinedError SpannedVar -> Doc AnsiStyle
ppUndefinedError (UndefinedError (SpannedVar (x :~ span)))
  = Trifecta.prettyRendering (Trifecta.render (Trifecta.Fixit span "undefined variable"))

newtype EnsureDefinedT k m a = EnsureDefinedT { runEnsureDefinedT :: ExceptT (UndefinedError k) m a }
  deriving (Functor, Applicative, Monad)

instance (MonadLookup k v m) => MonadLookup k v (EnsureDefinedT k m) where
  lookupM x = EnsureDefinedT $ do
    lift (lookupM x) >>= \case
      Nothing -> throwError (UndefinedError x)
      Just t -> return (Just t)

newtype BoundedVarLookupT m a = BoundedVarLookupT { runBoundedVarLookupT :: m a }
  deriving (Functor, Applicative, Monad)

instance MonadLookup k (AnnotatedTerm ann var k) m
  => MonadLookup (Bound.Var b k) (AnnotatedTerm ann var (Bound.Var b k)) (BoundedVarLookupT m) where
    lookupM x = BoundedVarLookupT $ do
      case x of
        Bound.B b -> return Nothing
        Bound.F y -> fmap (fmap (fmap Bound.F)) (lookupM y)

nfScopedM
  :: MonadLookup a (AnnotatedTerm ann var a) m
  => Scope b (AnnotatedTerm ann var) a -> m (Scope b (AnnotatedTerm ann var) a)
nfScopedM = fmap toScope . runBoundedVarLookupT . nfM . fromScope

nfM
  :: MonadLookup a (AnnotatedTerm ann var a) m
  => AnnotatedTerm ann var a -> m (AnnotatedTerm ann var a)
nfM (AnnotatedTerm ann tt) = fmap (appendAnnotation ann) $ case tt of
  t@(Variable x) -> lookupM x >>= \case
    Nothing -> return (unannotated t)
    Just t' -> nfM t'
  App t1 t2   -> whnfM t1 >>= \case
    AnnotatedTerm _ (Lambda _a _phi body) -> nfM (instantiate1 t2 body)
    t1'                 -> unannotated <$> (App <$> nfM t1' <*> nfM t2)

  First t -> whnfM t >>= \case
    AnnotatedTerm _ (Pair t1 _t2) -> nfM t1
    t'          -> nfM t'
  Second t -> whnfM t >>= \case
    AnnotatedTerm _ (Pair _t1 t2) -> nfM t2
    t'          -> nfM t'

  IdJ tA a tC d x p -> whnfM p >>= \case
    AnnotatedTerm _ Refl{} -> nfM d
    p'     -> unannotated <$>
      (IdJ <$> nfM tA <*> nfM a <*> nfM tC <*> nfM d <*> nfM x <*> nfM p')

  t@Universe -> return (unannotated t)

  ExtensionType tC psi tA phi a -> fmap unannotated $ ExtensionType
    <$> nfM tC
    <*> nfScopedM psi
    <*> nfScopedM tA
    <*> nfScopedM phi
    <*> nfScopedM a

  Pi t'       -> fmap unannotated $ Pi <$> nfM t'
  Lambda a phi body -> fmap unannotated $ Lambda
    <$> traverse nfM a
    <*> traverse nfScopedM phi
    <*> nfScopedM body

  Sigma t'    -> fmap unannotated $ Sigma <$> nfM t'
  Pair t1 t2  -> fmap unannotated $ Pair <$> nfM t1 <*> nfM t2

  IdType a x y -> fmap unannotated $ IdType <$> nfM a <*> nfM x <*> nfM y
  Refl a x -> fmap unannotated $ Refl <$> traverse nfM a <*> nfM x

  Cube -> return (unannotated Cube)
  CubeUnit -> return (unannotated CubeUnit)
  CubeUnitStar -> return (unannotated CubeUnitStar)

  CubeProd t1 t2 -> fmap unannotated $ CubeProd <$> nfM t1 <*> nfM t2

  Tope -> return (unannotated Tope)
  TopeTop -> return (unannotated TopeTop)
  TopeBottom -> return (unannotated TopeBottom)
  TopeOr  t1 t2 -> fmap unannotated $ TopeOr  <$> nfM t1 <*> nfM t2
  TopeAnd t1 t2 -> fmap unannotated $ TopeAnd <$> nfM t1 <*> nfM t2
  TopeEQ  t1 t2 -> fmap unannotated $ TopeEQ  <$> nfM t1 <*> nfM t2

  RecBottom -> return (unannotated RecBottom)
  RecOr psi phi t1 t2 -> fmap unannotated $ RecOr <$> nfM psi <*> nfM phi <*> nfM t1 <*> nfM t2

  Cube2 -> return (unannotated Cube2)
  Cube2_0 -> return (unannotated Cube2_0)
  Cube2_1 -> return (unannotated Cube2_1)

  TopeLEQ t1 t2 -> fmap unannotated $ TopeLEQ <$> nfM t1 <*> nfM t2

whnfM
  :: MonadLookup a (AnnotatedTerm ann var a) m
  => AnnotatedTerm ann var a -> m (AnnotatedTerm ann var a)
whnfM (AnnotatedTerm ann tt) = fmap (appendAnnotation ann) $ case tt of
  t@(Variable x) -> lookupM x >>= \case
    Nothing -> return (unannotated t)
    Just t' -> whnfM t'
  App t1 t2   -> whnfM t1 >>= \case
    AnnotatedTerm _ (Lambda _a _phi body) -> whnfM (instantiate1 t2 body)
    t1'                 -> return (unannotated (App t1' t2))

  First t -> whnfM t >>= \case
    AnnotatedTerm _ (Pair t1 _t2) -> whnfM t1
    t'          -> return t'
  Second t -> whnfM t >>= \case
    AnnotatedTerm _ (Pair _t1 t2) -> whnfM t2
    t'          -> return t'

  IdJ tA a tC d x p -> whnfM p >>= \case
    AnnotatedTerm _ Refl{} -> whnfM d
    p'     -> return (unannotated (IdJ tA a tC d x p'))

  t@Universe -> return (unannotated t)
  t@Lambda{} -> return (unannotated t)
  t@ExtensionType{} -> return (unannotated t)
  t@Pi{} -> return (unannotated t)
  t@Sigma{} -> return (unannotated t)
  t@Pair{} -> return (unannotated t)

  t@IdType{} -> return (unannotated t)
  t@Refl{} -> return (unannotated t)

  t@Cube -> return (unannotated t)
  t@CubeUnit -> return (unannotated t)
  t@CubeUnitStar -> return (unannotated t)
  t@CubeProd{} -> return (unannotated t)

  t@Tope -> return (unannotated t)
  t@TopeTop -> return (unannotated t)
  t@TopeBottom -> return (unannotated t)

  t@TopeOr{} -> return (unannotated t)
  t@TopeAnd{} -> return (unannotated t)
  t@TopeEQ{} -> return (unannotated t)

  t@RecBottom -> return (unannotated t)
  t@RecOr{} -> return (unannotated t)

  t@Cube2 -> return (unannotated t)
  t@Cube2_0 -> return (unannotated t)
  t@Cube2_1 -> return (unannotated t)
  t@TopeLEQ{} -> return (unannotated t)

nf :: AnnotatedTerm ann var a -> AnnotatedTerm ann var a
nf = runIdentity . nfM

whnf :: AnnotatedTerm ann var a -> AnnotatedTerm ann var a
whnf = runIdentity . whnfM

nfWith :: (Eq a, Hashable a) => [(a, AnnotatedTerm ann var a)] -> AnnotatedTerm ann var a -> AnnotatedTerm ann var a
nfWith definitions = runIdentity . flip runReaderT defs . runHashMapLookupT . nfM
  where
    defs = HashMap.fromList definitions

whnfWith :: (Eq a, Hashable a) => [(a, AnnotatedTerm ann var a)] -> AnnotatedTerm ann var a -> AnnotatedTerm ann var a
whnfWith definitions = runIdentity . flip runReaderT defs . runHashMapLookupT . whnfM
  where
    defs = HashMap.fromList definitions

nfWith' :: (Eq a, Hashable a) => [(a, AnnotatedTerm ann var a)] -> AnnotatedTerm ann var a -> AnnotatedTerm ann var a
nfWith' definitions = runIdentity . flip evalStateT defs . runWhnfHashMapLookupT . nfM
  where
    defs = Unevaluated <$> HashMap.fromList definitions

whnfWith' :: (Eq a, Hashable a) => [(a, AnnotatedTerm ann var a)] -> AnnotatedTerm ann var a -> Either (UndefinedError a) (AnnotatedTerm ann var a)
whnfWith' definitions = runIdentity . flip evalStateT defs . runWhnfHashMapLookupT . runExceptT . runEnsureDefinedT . whnfM
  where
    defs = Unevaluated <$> HashMap.fromList definitions

withSpannedVars :: AnnotatedTerm Span var Var -> AnnotatedTerm Span var SpannedVar
withSpannedVars = mapWithAnnotation $ \[span] var -> SpannedVar (var :~ span)

-- |
-- >>> zero = "\\s -> \\z -> z" :: AnnotatedTerm Span SpannedVar SpannedVar
-- >>> n3 = "\\s -> \\z -> s (s (s z))" :: AnnotatedTerm Span SpannedVar SpannedVar
-- >>> unsafeNfWith [("0", zero), ("3", n3)] "3 3"
-- λx₀ → λx₁ → x₀ (x₀ (x₀ (x₀ (x₀ (x₀ (x₀ (x₀ (x₀ (x₀ (x₀ (x₀ (x₀ (x₀ (x₀ (x₀ (x₀ (x₀ (x₀ (x₀ (x₀ (x₀ (x₀ (x₀ (x₀ (x₀ (x₀ x₁))))))))))))))))))))))))))
unsafeNfWith
  :: [(SpannedVar, AnnotatedTerm ann SpannedVar SpannedVar)]
  -> AnnotatedTerm ann SpannedVar SpannedVar
  -> AnnotatedTerm ann SpannedVar SpannedVar
unsafeNfWith definitions t =
  case eval t of
    Left err  -> error ("\n" <> Text.unpack (renderLazy (layoutPretty defaultLayoutOptions (ppUndefinedError err))))
    Right res -> res
  where
    eval = runIdentity . flip evalStateT defs . runWhnfHashMapLookupT . runExceptT . runEnsureDefinedT . nfM
    defs = Unevaluated <$> HashMap.fromList definitions
