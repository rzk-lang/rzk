{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | A throwaway spike for the free-foil migration (roadmap M2).
--
-- It answers, in code rather than in prose, the questions the handoff says must
-- be settled before the real port begins:
--
--   1. Does rzk's annotated signature port to free-foil? Every node of a rzk
--      term carries its type, and the type lives in the node's /own/ scope. In
--      @'Foil.AST' binder sig n@ the signature's term parameter /is/
--      @'Foil.AST' binder sig n@, so an @AnnSig'@ whose annotation is applied to
--      that parameter expresses exactly this. (free-foil-hou's @AnnSig@ cannot:
--      its annotation has kind 'Type' and cannot hold an open term.)
--
--   3. Is the memoised WHNF\/NF still expressible? The annotation is an ordinary
--      record, so yes; whether it is still worth having is a measurement.
--
--   6. How do the closed constants (@universeT@ and friends) generalise? They
--      are closed, so they are @forall n. Term n@.
--
-- What is NOT covered here: the full 44-constructor signature, the checker's
-- context, and the tope layer. This is a subset (Π, λ, application, Σ, pair,
-- projections, universe) that exercises binders, annotations, substitution,
-- weak head normal form and α-equivalence.
module Rzk.FoilSpike where

import           Control.Monad.Foil      (Distinct, DExt, NameBinder, Scope,
                                          addSubst, emptyScope, extendScope,
                                          identitySubst, nameOf, sink, withFresh)
import qualified Control.Monad.Foil      as Foil
import           Control.Monad.Free.Foil (AST (..), ScopedAST (..), ZipMatch (..),
                                          alphaEquiv, substitute)
import qualified Control.Monad.Free.Foil as FF
import           Data.Bifoldable         (Bifoldable (..))
import           Data.Bifunctor          (Bifunctor (..))
import           Data.Bifunctor.TH       (deriveBifoldable, deriveBifunctor,
                                          deriveBitraversable)
import           Data.Bitraversable      (Bitraversable (..))
-- NOTE: in free-foil 0.2.0 (the Hackage/LTS release) the polykinded ZipMatchK
-- class lives here; on the unreleased main it has moved to Data.ZipMatchK.
import           Control.Monad.Free.Foil.Generic (Mappings (..),
                                                  ZipMatchK (..),
                                                  genericZipMatch2)
import           Generics.Kind.TH        (deriveGenericK)
import qualified GHC.Generics            as GHC

-- * The term signature (a subset of rzk's @TermF@)

data TermSig scope term
  = UniverseF
  | TypeFunF term scope        -- ^ @(x : A) -> B x@
  | TypeSigmaF term scope      -- ^ @Sigma (x : A), B x@
  | LambdaF scope
  | AppF term term
  | PairF term term
  | FirstF term
  | SecondF term
  deriving (Eq, Functor, Foldable, Traversable, GHC.Generic)

deriveBifunctor ''TermSig
deriveBifoldable ''TermSig
deriveBitraversable ''TermSig

-- * The annotation, and the annotated signature
--
-- This is rzk's 'Free.Scoped.AnnF', transliterated: the annotation is applied to
-- the signature's /term/ parameter, so an annotation is a term in the same scope
-- as the node it annotates. That is what a dependent theory needs and what
-- free-foil does not provide.

data TypeInfo term = TypeInfo
  { infoType :: term
  , infoWHNF :: Maybe term
  } deriving (Eq, Functor, Foldable, Traversable, GHC.Generic)

data AnnSig ann sig scope term = AnnSig
  { annOf  :: ann term
  , sigOf  :: sig scope term
  } deriving (Functor, Foldable, Traversable, GHC.Generic)

-- | As in rzk today: equality ignores the annotation.
instance Eq (sig scope term) => Eq (AnnSig ann sig scope term) where
  AnnSig _ l == AnnSig _ r = l == r

instance (Functor ann, Bifunctor sig) => Bifunctor (AnnSig ann sig) where
  bimap f g (AnnSig ann sig) = AnnSig (fmap g ann) (bimap f g sig)

-- | As in rzk today: the fold does not visit the annotation, so free variables
-- of a term do not include those occurring only in its type.
instance Bifoldable sig => Bifoldable (AnnSig ann sig) where
  bifoldMap f g (AnnSig _ann sig) = bifoldMap f g sig

instance (Traversable ann, Bitraversable sig) => Bitraversable (AnnSig ann sig) where
  bitraverse f g (AnnSig ann sig) = AnnSig <$> traverse g ann <*> bitraverse f g sig

deriveGenericK ''TermSig
deriveGenericK ''TypeInfo

instance ZipMatchK TermSig

-- | Derived generically from the kind-generics representation.
instance ZipMatch TermSig where
  zipMatch = genericZipMatch2

-- | The instance the whole port hinges on, and it has to be written by hand:
-- every instance in the free-foil ecosystem compares annotations, and we need
-- α-equivalence to ignore them, exactly as rzk's 'Eq' does today.
--
-- The zip cannot simply drop the annotation — the class must return the
-- signature at the /result/ index, annotation included — so it zips the
-- annotation as well. That is harmless, and it does not compare types:
-- 'alphaEquiv' instantiates the mapping at pairing (which always succeeds) and
-- then consumes the zipped structure with 'bifoldMap', whose instance above does
-- not visit the annotation. So the paired annotation is built and discarded, and
-- the universe tower inside a type is never walked.
instance ZipMatchK sig => ZipMatchK (AnnSig TypeInfo sig) where
  zipMatchWithK f@(_fScope :^: (fTerm :^: M0)) (AnnSig ann1 sig1) (AnnSig ann2 sig2) =
    AnnSig <$> zipTypeInfo fTerm ann1 ann2 <*> zipMatchWithK f sig1 sig2

zipTypeInfo :: (a -> b -> Maybe c) -> TypeInfo a -> TypeInfo b -> Maybe (TypeInfo c)
zipTypeInfo f (TypeInfo ty1 whnf1) (TypeInfo ty2 whnf2) =
  TypeInfo <$> f ty1 ty2 <*> pure (zipMaybe whnf1 whnf2)
  where
    -- The memoised WHNF is a cache, not part of the term: two α-equivalent terms
    -- may have it filled in on one side only, and that must not decide equality.
    zipMaybe (Just a) (Just b) = f a b
    zipMaybe _ _               = Nothing

-- | 'alphaEquiv' in free-foil 0.2.0 is driven by this class, not by 'ZipMatchK'.
-- Its result index is fixed to /pairs/, which is exactly what rzk wants: the
-- annotation is paired up lazily and never compared, and since 'bifoldMap' above
-- does not visit the annotation, the pair is discarded unforced. This is the
-- annotation-blind equality rzk has today, recovered on the new representation.
instance ZipMatch sig => ZipMatch (AnnSig TypeInfo sig) where
  zipMatch (AnnSig ann1 sig1) (AnnSig ann2 sig2) =
    AnnSig (pairTypeInfo ann1 ann2) <$> zipMatch sig1 sig2
    where
      pairTypeInfo (TypeInfo ty1 w1) (TypeInfo ty2 w2) =
        TypeInfo (ty1, ty2) ((,) <$> w1 <*> w2)

-- * Terms

type Term = AST NameBinder (AnnSig TypeInfo TermSig)

pattern Universe :: TypeInfo (Term n) -> Term n
pattern Universe ty = Node (AnnSig ty UniverseF)

pattern App :: TypeInfo (Term n) -> Term n -> Term n -> Term n
pattern App ty f x = Node (AnnSig ty (AppF f x))

pattern Lam :: TypeInfo (Term n) -> NameBinder n l -> Term l -> Term n
pattern Lam ty binder body = Node (AnnSig ty (LambdaF (ScopedAST binder body)))

pattern Pair :: TypeInfo (Term n) -> Term n -> Term n -> Term n
pattern Pair ty l r = Node (AnnSig ty (PairF l r))

pattern First :: TypeInfo (Term n) -> Term n -> Term n
pattern First ty t = Node (AnnSig ty (FirstF t))

pattern Second :: TypeInfo (Term n) -> Term n -> Term n
pattern Second ty t = Node (AnnSig ty (SecondF t))

-- | Decision 6: the closed constants are closed, so they generalise over the
-- scope index for free. (rzk's @universeT@ is a 30-deep chain ending in a panic;
-- the knot is unchanged by the representation, so it ports as-is.)
universe :: Term n
universe = Universe TypeInfo { infoType = universe, infoWHNF = Just universe }

-- | Decision 3: the memoised WHNF is just a field, so it survives the port. The
-- self-referential knot works exactly as it does today.
termIsWHNF :: Term n -> Term n
termIsWHNF t@Var{} = t
termIsWHNF (Node (AnnSig info sig)) = t'
  where t' = Node (AnnSig info { infoWHNF = Just t' } sig)

-- * Reduction
--
-- The point of the exercise: substitution takes the ambient scope and an
-- 'Foil.Substitution', and weakening a term into a larger scope is 'sink', which
-- is a coercion rather than a traversal. Compare the current representation,
-- where a beta-step renumbers every node of the body.

whnf :: Distinct n => Scope n -> Term n -> Term n
whnf scope = \case
  App ty f x ->
    case whnf scope f of
      Lam _ty binder body ->
        let subst = addSubst identitySubst binder x
         in whnf scope (substitute scope subst body)
      f' -> App ty f' x
  First ty t ->
    case whnf scope t of
      Pair _ty l _r -> whnf scope l
      t'            -> First ty t'
  Second ty t ->
    case whnf scope t of
      Pair _ty _l r -> whnf scope r
      t'            -> Second ty t'
  t -> t

-- * A worked example
--
-- >>> alphaEquiv emptyScope (whnf emptyScope identityApplied) identityTerm
-- True
--
-- The annotations of the two sides differ (the left one has been reduced), and
-- α-equivalence still holds: that is the annotation-blind 'ZipMatchK' at work.

-- | @\\ x -> x@, annotated (with a nonsense type: the spike does not typecheck).
identityTerm :: Term Foil.VoidS
identityTerm = withFresh emptyScope $ \binder ->
  Lam ann binder (Var (nameOf binder))
  where ann = TypeInfo { infoType = universe, infoWHNF = Nothing }

-- | @(\\ x -> x) (\\ x -> x)@, which reduces to @\\ x -> x@.
identityApplied :: Term Foil.VoidS
identityApplied = App ann identityTerm identityTerm
  where ann = TypeInfo { infoType = universe, infoWHNF = Nothing }
