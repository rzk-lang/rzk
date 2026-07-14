-- The 'ZipMatchK' instances below are orphans: the class is free-foil's and the
-- constant types they cover ('TModality', 'VarIdent', 'Binder') are still the old
-- module's. They come home when the old core goes away.
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures -fno-warn-orphans #-}
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

-- | The core syntax on @free-foil@.
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
import           Control.Monad.Foil.Internal    (unsafeAssertFresh)
import           Control.Monad.Free.Foil        (AST (..), ScopedAST (..),
                                                 ZipMatch (..), alphaEquiv,
                                                 substitute, unsafeEqAST)
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
import           Unsafe.Coerce                  (unsafeCoerce)

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

-- | A scope of an untyped term.
type ScopedTerm = ScopedAST NameBinder TermSig

-- | The annotation of a node: its type, and its memoised normal forms. A
-- variable carries none — its type lives in the context.
typeInfoOf :: TermT n -> Maybe (TypeInfo (TermT n))
typeInfoOf (Var _)                = Nothing
typeInfoOf (Node (AnnSig info _)) = Just info

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

-- * Equality

-- | Syntactic equality of two terms of the same scope.
--
-- Annotation-blind, as the old derived 'Eq' was, but it also requires the two to
-- bind the same names, so it is /conservative/: two α-equivalent terms whose
-- binders differ are not equal. That is what the tope-context scans want — the
-- terms there come from the same context — and it is cheaper than 'alphaEqT',
-- which walks the scope.
eqT :: Foil.Distinct n => TermT n -> TermT n -> Bool
eqT = unsafeEqAST

-- | α-equivalence: name-blind and annotation-blind. The old representation's 'Eq'
-- compared binder names, so @\\ x -> x@ and @\\ y -> y@ were unequal; they are the
-- same term, and this says so.
alphaEqT :: Foil.Distinct n => Foil.Scope n -> TermT n -> TermT n -> Bool
alphaEqT = alphaEquiv

elemT :: Foil.Distinct n => TermT n -> [TermT n] -> Bool
elemT t = any (eqT t)

notElemT :: Foil.Distinct n => TermT n -> [TermT n] -> Bool
notElemT t = not . elemT t

nubT :: Foil.Distinct n => [TermT n] -> [TermT n]
nubT []       = []
nubT (t : ts) = t : nubT (filter (not . eqT t) ts)

-- * Holes

isHoleT :: TermT n -> Bool
isHoleT HoleT{} = True
isHoleT _       = False

-- | Does the term contain a hole anywhere (including nested, e.g. @f ?@)?
containsHole :: TermT n -> Bool
containsHole HoleT{} = True
containsHole (Var _) = False
containsHole (Node (AnnSig _ sig)) =
  bifoldr (\scoped acc -> containsHoleScoped scoped || acc) (\t acc -> containsHole t || acc) False sig
  where
    containsHoleScoped (ScopedAST _ body) = containsHole body

-- * Going under a binder, and substituting

-- | Go under the binder of a scoped term.
--
-- The binder is used as it stands when its name is free in the ambient scope,
-- which is the common case and costs nothing. It has to be renamed when the name
-- is taken — sinking a term into a scope that has grown since the term was built
-- can do that — and only then is the body traversed.
withScopedT
  :: Foil.Distinct n
  => Foil.Scope n
  -> ScopedTermT n
  -> (forall l. Foil.DExt n l => NameBinder n l -> TermT l -> r)
  -> r
withScopedT scope (ScopedAST binder body) k
  | Foil.member (Foil.nameOf binder) scope =
      Foil.withFresh scope $ \binder' ->
        let scope' = Foil.extendScope binder' scope
            rename = Foil.addRename (Foil.sink Foil.identitySubst) binder (Foil.nameOf binder')
         in k binder' (substitute scope' rename body)
  | otherwise =
      -- The name is fresh here, so the body already /is/ a term of the extended
      -- scope; the coercion says exactly that, and is the same one
      -- 'Foil.withRefreshed' performs on its own fast path.
      unsafeAssertFresh binder $ \binder' -> k binder' (unsafeCoerce body)

-- | Go under two scoped terms that stand for /one/ variable.
--
-- A Π-type and a λ over a shape each carry a tope scope beside the body, under
-- what the user wrote as a single binder. On free-foil each 'ScopedAST' has its
-- own 'NameBinder', so the second is instantiated with the first's name: they
-- behave as two abstractions over one argument, as they did before.
withScopedT2
  :: Foil.Distinct n
  => Foil.Scope n
  -> ScopedTermT n
  -> ScopedTermT n
  -> (forall l. Foil.DExt n l => NameBinder n l -> TermT l -> TermT l -> r)
  -> r
withScopedT2 scope scoped1 scoped2 k =
  withScopedT scope scoped1 $ \binder body1 ->
    k binder body1 (openWith (Foil.extendScope binder scope) (Foil.nameOf binder) scoped2)

-- | Open a scoped term with a name that is already in scope.
openWith
  :: Foil.DExt n l
  => Foil.Scope l -> Foil.Name l -> ScopedTermT n -> TermT l
openWith scope name (ScopedAST binder body) =
  substitute scope (Foil.addRename (Foil.sink Foil.identitySubst) binder name) body

-- | Instantiate a scoped term with a term: the successor of @substituteT x scope@.
instantiateT :: Foil.Distinct n => Foil.Scope n -> ScopedTermT n -> TermT n -> TermT n
instantiateT scope (ScopedAST binder body) arg =
  substituteT scope (Foil.addSubst Foil.identitySubst binder arg) body

-- | Substitution that invalidates the memoised normal forms of every node it
-- rebuilds, and substitutes into each node's type.
--
-- A renaming ('substitute') keeps the memo, since a renamed term reduces exactly
-- as the original does. A real substitution does not: a variable is in weak head
-- normal form, and what replaces it need not be. This is one traversal, as
-- substituting and invalidating separately was two.
substituteT
  :: Foil.Distinct o
  => Foil.Scope o
  -> Foil.Substitution TermT i o
  -> TermT i
  -> TermT o
substituteT scope subst term = go scope subst term
  where
    go
      :: forall i' o'. Foil.Distinct o'
      => Foil.Scope o' -> Foil.Substitution TermT i' o' -> TermT i' -> TermT o'
    go _ subst' (Var name) = Foil.lookupSubst subst' name
    go scope' subst' (Node (AnnSig info sig)) = Node (AnnSig info' sig')
      where
        info' = TypeInfo
          { infoType = go scope' subst' (infoType info)
          , infoWHNF = Nothing
          , infoNF   = Nothing
          }
        sig' = bimap goScoped (go scope' subst') sig

        goScoped (ScopedAST binder body) =
          Foil.withRefreshed scope' (Foil.nameOf binder) $ \binder' ->
            let scope'' = Foil.extendScope binder' scope'
                subst'' = Foil.addRename (Foil.sink subst') binder (Foil.nameOf binder')
             in ScopedAST binder' (go scope'' subst'' body)

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

-- ** Untyped patterns
--
-- The same constructors on 'Term' (no annotation), for the surface conversions
-- and the printer.

pattern Universe = Node UniverseF
pattern UniverseCube = Node UniverseCubeF
pattern UniverseTope = Node UniverseTopeF
pattern CubeUnit = Node CubeUnitF
pattern CubeUnitStar = Node CubeUnitStarF
pattern Cube2 = Node Cube2F
pattern Cube2_0 = Node Cube2_0F
pattern Cube2_1 = Node Cube2_1F
pattern CubeI = Node CubeIF
pattern CubeI_0 = Node CubeI_0F
pattern CubeI_1 = Node CubeI_1F
pattern CubeProduct l r = Node (CubeProductF l r)
pattern CubeFlip t = Node (CubeFlipF t)
pattern CubeUnflip t = Node (CubeUnflipF t)
pattern TopeTop = Node TopeTopF
pattern TopeBottom = Node TopeBottomF
pattern TopeEQ l r = Node (TopeEQF l r)
pattern TopeLEQ l r = Node (TopeLEQF l r)
pattern TopeAnd l r = Node (TopeAndF l r)
pattern TopeOr l r = Node (TopeOrF l r)
pattern TopeInv t = Node (TopeInvF t)
pattern TopeUninv t = Node (TopeUninvF t)
pattern RecBottom = Node RecBottomF
pattern RecOr rs = Node (RecOrF rs)
pattern TypeFun orig md param mtope ret = Node (TypeFunF orig md param mtope ret)
pattern TypeSigma orig md a b = Node (TypeSigmaF orig md a b)
pattern TypeId a mtA b = Node (TypeIdF a mtA b)
pattern App f x = Node (AppF f x)
pattern Let orig mparam val body = Node (LetF orig mparam val body)
pattern Lambda orig mparam body = Node (LambdaF orig mparam body)
pattern Pair l r = Node (PairF l r)
pattern First t = Node (FirstF t)
pattern Second t = Node (SecondF t)
pattern Refl mx = Node (ReflF mx)
pattern IdJ a b c d e f = Node (IdJF a b c d e f)
pattern Unit = Node UnitF
pattern TypeUnit = Node TypeUnitF
pattern TypeAsc term ty = Node (TypeAscF term ty)
pattern TypeRestricted ty rs = Node (TypeRestrictedF ty rs)
pattern TypeModal md ty = Node (TypeModalF md ty)
pattern ModApp md t = Node (ModAppF md t)
pattern ModExtract app inn t = Node (ModExtractF app inn t)
pattern LetMod orig app inn mparam val body = Node (LetModF orig app inn mparam val body)
pattern Hole mname = Node (HoleF mname)

{-# COMPLETE Var, Universe, UniverseCube, UniverseTope, CubeUnit, CubeUnitStar,
  Cube2, Cube2_0, Cube2_1, CubeI, CubeI_0, CubeI_1, CubeProduct, CubeFlip,
  CubeUnflip, TopeTop, TopeBottom, TopeEQ, TopeLEQ, TopeAnd, TopeOr, TopeInv,
  TopeUninv, RecBottom, RecOr, TypeFun, TypeSigma, TypeId, App, Let, Lambda,
  Pair, First, Second, Refl, IdJ, Unit, TypeUnit, TypeAsc, TypeRestricted,
  TypeModal, ModApp, ModExtract, LetMod, Hole #-}

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

cubeUnitT :: TermT n
cubeUnitT = CubeUnitT TypeInfo
  { infoType = cubeT, infoWHNF = Just cubeUnitT, infoNF = Just cubeUnitT }

cubeUnitStarT :: TermT n
cubeUnitStarT = CubeUnitStarT TypeInfo
  { infoType = cubeUnitT, infoWHNF = Just cubeUnitStarT, infoNF = Just cubeUnitStarT }

cube2T :: TermT n
cube2T = Cube2T TypeInfo
  { infoType = cubeT, infoWHNF = Just cube2T, infoNF = Just cube2T }

cube2_0T :: TermT n
cube2_0T = Cube2_0T TypeInfo
  { infoType = cube2T, infoWHNF = Just cube2_0T, infoNF = Just cube2_0T }

cube2_1T :: TermT n
cube2_1T = Cube2_1T TypeInfo
  { infoType = cube2T, infoWHNF = Just cube2_1T, infoNF = Just cube2_1T }

cubeIT :: TermT n
cubeIT = CubeIT TypeInfo
  { infoType = cubeT, infoWHNF = Just cubeIT, infoNF = Just cubeIT }

cubeI_0T :: TermT n
cubeI_0T = CubeI_0T TypeInfo
  { infoType = cubeIT, infoWHNF = Just cubeI_0T, infoNF = Just cubeI_0T }

cubeI_1T :: TermT n
cubeI_1T = CubeI_1T TypeInfo
  { infoType = cubeIT, infoWHNF = Just cubeI_1T, infoNF = Just cubeI_1T }

topeTopT :: TermT n
topeTopT = TopeTopT TypeInfo
  { infoType = topeT, infoWHNF = Just topeTopT, infoNF = Just topeTopT }

topeBottomT :: TermT n
topeBottomT = TopeBottomT TypeInfo
  { infoType = topeT, infoWHNF = Just topeBottomT, infoNF = Just topeBottomT }

typeUnitT :: TermT n
typeUnitT = TypeUnitT TypeInfo
  { infoType = universeT, infoWHNF = Just typeUnitT, infoNF = Just typeUnitT }

unitT :: TermT n
unitT = UnitT TypeInfo
  { infoType = typeUnitT, infoWHNF = Just unitT, infoNF = Just unitT }

-- | @recBOT@ is its own type: it inhabits every type in a contradictory context.
recBottomT :: TermT n
recBottomT = RecBottomT TypeInfo
  { infoType = recBottomT, infoWHNF = Just recBottomT, infoNF = Just recBottomT }

-- * Smart constructors
--
-- Each builds the 'TypeInfo' of the node it makes, so the checker never writes a
-- raw @FooT@. A node whose head is already a value ('lambdaT', 'pairT', the type
-- formers) memoises itself as its own WHNF.

-- ** The tope layer

topeEQT :: TermT n -> TermT n -> TermT n
topeEQT l r = TopeEQT (topeInfo topeT) l r

topeLEQT :: TermT n -> TermT n -> TermT n
topeLEQT l r = TopeLEQT (topeInfo topeT) l r

topeOrT :: TermT n -> TermT n -> TermT n
topeOrT l r = TopeOrT (topeInfo topeT) l r

topeAndT :: TermT n -> TermT n -> TermT n
topeAndT l r = TopeAndT (topeInfo topeT) l r

topeInvT :: TermT n -> TermT n
topeInvT t = TopeInvT (topeInfo (typeModalT universeT Op topeT)) t

topeUninvT :: TermT n -> TermT n
topeUninvT t = TopeUninvT (topeInfo topeT) t

-- | An unreduced node of the given type.
topeInfo :: TermT n -> TypeInfo (TermT n)
topeInfo ty = TypeInfo { infoType = ty, infoWHNF = Nothing, infoNF = Nothing }

-- ** Cubes

cubeProductT :: TermT n -> TermT n -> TermT n
cubeProductT l r = CubeProductT (topeInfo cubeT) l r

cubeFlipT :: TermT n -> TermT n -> TermT n
cubeFlipT cubeTy t = CubeFlipT (topeInfo (typeModalT cubeT Op cubeTy)) t

cubeUnflipT :: TermT n -> TermT n -> TermT n
cubeUnflipT cubeTy t = CubeUnflipT (topeInfo cubeTy) t

-- ** Types

typeFunT
  :: Binder -> TModality -> TermT n -> Maybe (ScopedTermT n) -> ScopedTermT n
  -> TermT n
typeFunT orig md cube mtope ret = t
  where t = TypeFunT (valueInfo t universeT) orig md cube mtope ret

typeSigmaT :: Binder -> TModality -> TermT n -> ScopedTermT n -> TermT n
typeSigmaT orig md a b = t
  where t = TypeSigmaT (valueInfo t universeT) orig md a b

typeIdT :: TermT n -> Maybe (TermT n) -> TermT n -> TermT n
typeIdT x tA y = t
  where t = TypeIdT (valueInfo t universeT) x tA y

typeRestrictedT :: TermT n -> [(TermT n, TermT n)] -> TermT n
typeRestrictedT ty rs = TypeRestrictedT (topeInfo universeT) ty rs

typeModalT :: TermT n -> TModality -> TermT n -> TermT n
typeModalT ty md te = TypeModalT (topeInfo ty) md te

typeAscT :: TermT n -> TermT n -> TermT n
typeAscT x ty = TypeAscT (topeInfo ty) x ty

-- | A node that is already a value: it is its own weak head normal form.
valueInfo :: TermT n -> TermT n -> TypeInfo (TermT n)
valueInfo t ty = TypeInfo { infoType = ty, infoWHNF = Just t, infoNF = Nothing }

-- ** Terms

lambdaT
  :: TermT n -> Binder -> Maybe (LambdaParam (ScopedTermT n) (TermT n))
  -> ScopedTermT n -> TermT n
lambdaT ty orig mparam body = t
  where t = LambdaT (valueInfo t ty) orig mparam body

pairT :: TermT n -> TermT n -> TermT n -> TermT n
pairT ty l r = t
  where t = PairT (valueInfo t ty) l r

appT :: TermT n -> TermT n -> TermT n -> TermT n
appT ty f x = AppT (topeInfo ty) f x

firstT :: TermT n -> TermT n -> TermT n
firstT ty arg = FirstT (topeInfo ty) arg

secondT :: TermT n -> TermT n -> TermT n
secondT ty arg = SecondT (topeInfo ty) arg

letT :: TermT n -> Binder -> Maybe (TermT n) -> TermT n -> ScopedTermT n -> TermT n
letT ty orig mparam val body = LetT (topeInfo ty) orig mparam val body

letModT
  :: TermT n -> Binder -> TModality -> TModality -> Maybe (TermT n) -> TermT n
  -> ScopedTermT n -> TermT n
letModT ty orig app inn mparam val body =
  LetModT (topeInfo ty) orig app inn mparam val body

-- | @refl@ normalises to a bare @refl@: its endpoints are recoverable from the
-- type, so they are dropped from the normal form.
reflT :: TermT n -> Maybe (TermT n, Maybe (TermT n)) -> TermT n
reflT ty mx = ReflT info mx
  where
    info = TypeInfo
      { infoType = ty
      , infoWHNF = Just (ReflT info Nothing)
      , infoNF   = Just (ReflT info Nothing)
      }

idJT
  :: TermT n -> TermT n -> TermT n -> TermT n -> TermT n -> TermT n -> TermT n
  -> TermT n
idJT ty tA a tC d x p = IdJT (topeInfo ty) tA a tC d x p

recOrT :: TermT n -> [(TermT n, TermT n)] -> TermT n
recOrT ty rs = RecOrT (topeInfo ty) rs

modAppT :: TermT n -> TModality -> TermT n -> TermT n
modAppT ty md term = ModAppT (topeInfo ty) md term

modExtractT :: TermT n -> TModality -> TModality -> TermT n -> TermT n
modExtractT ty app inn term = ModExtractT (topeInfo ty) app inn term

holeT :: TermT n -> Maybe VarIdent -> TermT n
holeT ty mname = HoleT (topeInfo ty) mname
