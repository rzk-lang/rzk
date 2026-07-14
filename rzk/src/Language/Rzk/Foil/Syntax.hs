{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | The core syntax on @free-foil@ (roadmap M2, stage 1).
--
-- This is the successor of "Language.Rzk.Free.Syntax"'s @TermF@ \/ @TermT@,
-- built on 'Foil.AST' instead of the vendored @Free.Scoped@. It is compiled but
-- not yet consumed: the checker still runs on the old representation, and the
-- two are swapped over in a later stage.
--
-- Three things carry over unchanged, and are imported rather than duplicated:
-- 'VarIdent' (a surface identifier), 'Binder' (the /names/ a binder introduces,
-- including a pair pattern, which still binds exactly one variable), 'TModality',
-- and 'TypeInfo' (a node's type plus its memoised weak head and normal forms).
--
-- What changes is the variable representation. A binder is a 'Foil.NameBinder',
-- a variable is a 'Foil.Name' (an @Int@), and weakening a term into a larger
-- scope is 'Foil.sink', a coercion rather than a traversal of every node.
module Language.Rzk.Foil.Syntax where

import           Control.Monad.Foil             (NameBinder)
import qualified Control.Monad.Foil             as Foil
import           Control.Monad.Free.Foil        (AST (..), ScopedAST (..),
                                                 ZipMatch (..))
import           Control.Monad.Free.Foil.Generic (Mappings (..),
                                                 ZipMatchK (..),
                                                 genericZipMatch2,
                                                 zipMatchViaChooseLeft,
                                                 zipMatchViaEq)
import           Data.Bifoldable                (Bifoldable (..))
import           Data.Bifunctor                 (Bifunctor (..))
import           Data.Bifunctor.TH              (deriveBifoldable,
                                                 deriveBifunctor,
                                                 deriveBitraversable)
import           Data.Bitraversable             (Bitraversable (..))
import           Generics.Kind.TH                (deriveGenericK)
import qualified GHC.Generics                   as GHC

import           Language.Rzk.Free.Syntax       (Binder (..), TModality (..),
                                                 TypeInfo (..), VarIdent)

-- * The signature
--
-- A transliteration of @TermF@: same constructors, same fields, same order. The
-- @scope@ positions are the ones that bind, and there are seven of them across
-- five constructors ('TypeFunF' and 'LambdaF' each carry a tope scope as well as
-- a body scope, under the same binder).

-- | The optional domain annotation of a λ: its modality, its parameter type,
-- and (for a shape) the tope the parameter is restricted by. It was an anonymous
-- triple in the old signature; the generic machinery needs a named type here, and
-- it reads better anyway.
data LambdaParam scope term = LambdaParam TModality term (Maybe scope)
  deriving (Eq, Functor, Foldable, Traversable, GHC.Generic)

data TermSig scope term
    = UniverseF
    | UniverseCubeF
    | UniverseTopeF
    | CubeUnitF
    | CubeUnitStarF
    | Cube2F
    | Cube2_0F
    | Cube2_1F
    | CubeIF
    | CubeI_0F
    | CubeI_1F
    | CubeProductF term term
    | CubeFlipF term
    | CubeUnflipF term
    | TopeTopF
    | TopeBottomF
    | TopeEQF term term
    | TopeLEQF term term
    | TopeAndF term term
    | TopeOrF term term
    | TopeInvF term
    | TopeUninvF term
    | RecBottomF
    | RecOrF [(term, term)]
    | TypeFunF Binder TModality term (Maybe scope) scope
    | TypeSigmaF Binder TModality term scope
    | TypeIdF term (Maybe term) term
    | AppF term term
    | LetF Binder (Maybe term) term scope
    | LambdaF Binder (Maybe (LambdaParam scope term)) scope
    | PairF term term
    | FirstF term
    | SecondF term
    | ReflF (Maybe (term, Maybe term))
    | IdJF term term term term term term
    | UnitF
    | TypeUnitF
    | TypeAscF term term
    | TypeRestrictedF term [(term, term)]
    | TypeModalF TModality term
    | ModAppF TModality term
    | ModExtractF TModality TModality term
    | LetModF Binder TModality TModality (Maybe term) term scope
    | HoleF (Maybe VarIdent)
    deriving (Eq, Functor, Foldable, Traversable, GHC.Generic)

deriveBifunctor ''LambdaParam
deriveBifoldable ''LambdaParam
deriveBitraversable ''LambdaParam
deriveGenericK ''LambdaParam

deriveBifunctor ''TermSig
deriveBifoldable ''TermSig
deriveBitraversable ''TermSig
deriveGenericK ''TermSig

-- | Matching the non-recursive fields of the signature.
--
-- A modality and a hole's name are part of the term: they must agree. A 'Binder'
-- is /not/: it records the names a binder introduces, purely so that goals and
-- error messages can show the user's own names, and two terms that differ only
-- in them are the same term. The old representation compared them (its 'Eq' was
-- derived), so @\ x -> x@ and @\ y -> y@ compared unequal; on the new one they
-- are α-equivalent, as they should be.
instance ZipMatchK TModality where
  zipMatchWithK = zipMatchViaEq

instance ZipMatchK VarIdent where
  zipMatchWithK = zipMatchViaEq

instance ZipMatchK Binder where
  zipMatchWithK = zipMatchViaChooseLeft

-- | A hole's name is a whole field ('HoleF'), so it is matched as a constant
-- rather than through the 'Maybe' functor.
instance ZipMatchK (Maybe VarIdent) where
  zipMatchWithK = zipMatchViaEq

-- | Pairs occur in the signature (a @recOR@'s branches, an extension type's
-- restrictions, the endpoints of @refl@), and the generic machinery needs to know
-- how to match them.
instance ZipMatchK (,) where
  zipMatchWithK (f :^: (g :^: M0)) (a1, b1) (a2, b2) = (,) <$> f a1 a2 <*> g b1 b2

instance ZipMatchK LambdaParam

-- | Structural matching, derived generically; 'Control.Monad.Free.Foil.alphaEquiv'
-- is driven by it.
instance ZipMatch TermSig where
  zipMatch = genericZipMatch2

-- * Annotations
--
-- 'AnnSig' is the old @AnnF@: the annotation is applied to the signature's /term/
-- parameter, so a node's type is a term in the node's own scope. That is what a
-- dependent theory needs, and it is why free-foil's own annotation mechanisms
-- (a plain type parameter, as BNFC's @--functor@ produces) do not fit.

data AnnSig ann sig scope term = AnnSig
  { annOf :: ann term
  , sigOf :: sig scope term
  } deriving (Functor, Foldable, Traversable, GHC.Generic)

-- | Important: does not compare the annotation.
instance Eq (sig scope term) => Eq (AnnSig ann sig scope term) where
  AnnSig _ l == AnnSig _ r = l == r

instance (Functor ann, Bifunctor sig) => Bifunctor (AnnSig ann sig) where
  bimap f g (AnnSig ann sig) = AnnSig (fmap g ann) (bimap f g sig)

-- | Important: does not fold over the annotation, so the free variables of a
-- term do not include those occurring only in its type (as before, the checker
-- adds them explicitly where it needs them).
instance Bifoldable sig => Bifoldable (AnnSig ann sig) where
  bifoldMap f g (AnnSig _ann sig) = bifoldMap f g sig

instance (Traversable ann, Bitraversable sig) => Bitraversable (AnnSig ann sig) where
  bitraverse f g (AnnSig ann sig) = AnnSig <$> traverse g ann <*> bitraverse f g sig

-- | α-equivalence must ignore the annotation, as the old structural 'Eq' did.
--
-- It does, and for free: 'zipMatch' returns the signature at /pairs/, so the
-- annotation is paired lazily and never compared, and 'Control.Monad.Free.Foil.alphaEquiv'
-- then consumes the zipped structure with 'bifoldMap', which (above) does not
-- visit the annotation. So the pair is discarded unforced — in particular the
-- universe tower inside a type is never walked.
instance ZipMatch sig => ZipMatch (AnnSig TypeInfo sig) where
  zipMatch (AnnSig ann1 sig1) (AnnSig ann2 sig2) =
    AnnSig (pairTypeInfo ann1 ann2) <$> zipMatch sig1 sig2
    where
      pairTypeInfo (TypeInfo ty1 whnf1 nf1) (TypeInfo ty2 whnf2 nf2) = TypeInfo
        { infoType = (ty1, ty2)
        , infoWHNF = (,) <$> whnf1 <*> whnf2
        , infoNF   = (,) <$> nf1 <*> nf2
        }

-- * Terms

-- | An untyped term: the surface syntax, elaborated but without annotations.
type Term = AST NameBinder TermSig

-- | A typed term: every node carries its type. The successor of @TermT@.
type TermT = AST NameBinder (AnnSig TypeInfo TermSig)

-- | A scope: a binder together with the term it binds over.
type ScopedTermT = ScopedAST NameBinder (AnnSig TypeInfo TermSig)

-- | Drop every annotation, for printing and for the surface-facing API.
untyped :: TermT n -> Term n
untyped (Var name)              = Var name
untyped (Node (AnnSig _ann sig)) = Node (bimap untypedScoped untyped sig)
  where
    untypedScoped (ScopedAST binder body) = ScopedAST binder (untyped body)

-- | Memoise a node's own weak head normal form (the self-referential knot of the
-- old representation, unchanged).
termIsWHNF :: TermT n -> TermT n
termIsWHNF t@Var{} = t
termIsWHNF (Node (AnnSig info sig)) = t'
  where t' = Node (AnnSig info { infoWHNF = Just t' } sig)

termIsNF :: TermT n -> TermT n
termIsNF t@Var{} = t
termIsNF (Node (AnnSig info sig)) = t'
  where t' = Node (AnnSig info { infoWHNF = Just t', infoNF = Just t' } sig)

-- * Pattern synonyms
--
-- One per constructor, as @makePatternsAll@ generated before. A @scope@ field is
-- a 'ScopedTermT', so going under a binder means matching on 'ScopedAST', which
-- is where the existential scope index appears.

pattern UniverseT info = Node (AnnSig info UniverseF)
pattern UniverseCubeT info = Node (AnnSig info UniverseCubeF)
pattern UniverseTopeT info = Node (AnnSig info UniverseTopeF)
pattern CubeUnitT info = Node (AnnSig info CubeUnitF)
pattern CubeUnitStarT info = Node (AnnSig info CubeUnitStarF)
pattern Cube2T info = Node (AnnSig info Cube2F)
pattern Cube2_0T info = Node (AnnSig info Cube2_0F)
pattern Cube2_1T info = Node (AnnSig info Cube2_1F)
pattern CubeIT info = Node (AnnSig info CubeIF)
pattern CubeI_0T info = Node (AnnSig info CubeI_0F)
pattern CubeI_1T info = Node (AnnSig info CubeI_1F)
pattern CubeProductT info l r = Node (AnnSig info (CubeProductF l r))
pattern CubeFlipT info t = Node (AnnSig info (CubeFlipF t))
pattern CubeUnflipT info t = Node (AnnSig info (CubeUnflipF t))
pattern TopeTopT info = Node (AnnSig info TopeTopF)
pattern TopeBottomT info = Node (AnnSig info TopeBottomF)
pattern TopeEQT info l r = Node (AnnSig info (TopeEQF l r))
pattern TopeLEQT info l r = Node (AnnSig info (TopeLEQF l r))
pattern TopeAndT info l r = Node (AnnSig info (TopeAndF l r))
pattern TopeOrT info l r = Node (AnnSig info (TopeOrF l r))
pattern TopeInvT info t = Node (AnnSig info (TopeInvF t))
pattern TopeUninvT info t = Node (AnnSig info (TopeUninvF t))
pattern RecBottomT info = Node (AnnSig info RecBottomF)
pattern RecOrT info rs = Node (AnnSig info (RecOrF rs))
pattern TypeFunT info orig md param mtope ret = Node (AnnSig info (TypeFunF orig md param mtope ret))
pattern TypeSigmaT info orig md a b = Node (AnnSig info (TypeSigmaF orig md a b))
pattern TypeIdT info a mtA b = Node (AnnSig info (TypeIdF a mtA b))
pattern AppT info f x = Node (AnnSig info (AppF f x))
pattern LetT info orig mparam val body = Node (AnnSig info (LetF orig mparam val body))
pattern LambdaT info orig mparam body = Node (AnnSig info (LambdaF orig mparam body))
pattern PairT info l r = Node (AnnSig info (PairF l r))
pattern FirstT info t = Node (AnnSig info (FirstF t))
pattern SecondT info t = Node (AnnSig info (SecondF t))
pattern ReflT info mx = Node (AnnSig info (ReflF mx))
pattern IdJT info a b c d e f = Node (AnnSig info (IdJF a b c d e f))
pattern UnitT info = Node (AnnSig info UnitF)
pattern TypeUnitT info = Node (AnnSig info TypeUnitF)
pattern TypeAscT info term ty = Node (AnnSig info (TypeAscF term ty))
pattern TypeRestrictedT info ty rs = Node (AnnSig info (TypeRestrictedF ty rs))
pattern TypeModalT info md ty = Node (AnnSig info (TypeModalF md ty))
pattern ModAppT info md t = Node (AnnSig info (ModAppF md t))
pattern ModExtractT info app inn t = Node (AnnSig info (ModExtractF app inn t))
pattern LetModT info orig app inn mparam val body = Node (AnnSig info (LetModF orig app inn mparam val body))
pattern HoleT info mname = Node (AnnSig info (HoleF mname))

{-# COMPLETE Var, UniverseT, UniverseCubeT, UniverseTopeT, CubeUnitT,
  CubeUnitStarT, Cube2T, Cube2_0T, Cube2_1T, CubeIT, CubeI_0T, CubeI_1T,
  CubeProductT, CubeFlipT, CubeUnflipT, TopeTopT, TopeBottomT, TopeEQT, TopeLEQT,
  TopeAndT, TopeOrT, TopeInvT, TopeUninvT, RecBottomT, RecOrT, TypeFunT,
  TypeSigmaT, TypeIdT, AppT, LetT, LambdaT, PairT, FirstT, SecondT, ReflT, IdJT,
  UnitT, TypeUnitT, TypeAscT, TypeRestrictedT, TypeModalT, ModAppT, ModExtractT,
  LetModT, HoleT #-}

-- * Closed constants
--
-- They are closed, so they generalise over the scope index: no shifting, no
-- per-scope construction. (The universe is still the 30-deep chain of the old
-- representation, ending in a bottom; making it a real level-polymorphic
-- universe is a separate FIXME.)

universeT :: TermT n
universeT = iterate f (error "going too high up the universe levels") !! 30
  where
    f t = UniverseT TypeInfo { infoType = t, infoWHNF = Just universeT, infoNF = Just universeT }

cubeT :: TermT n
cubeT = UniverseCubeT TypeInfo
  { infoType = universeT, infoWHNF = Just cubeT, infoNF = Just cubeT }

topeT :: TermT n
topeT = UniverseTopeT TypeInfo
  { infoType = universeT, infoWHNF = Just topeT, infoNF = Just topeT }
