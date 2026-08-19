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
import           Control.Monad.Foil.Internal    (Substitution (..),
                                                 unsafeAssertFresh)
import           Control.Monad.Free.Foil        (AST (..), ScopedAST (..),
                                                 alphaEquiv,
                                                 substitute, unsafeEqAST)
import           Data.ZipMatchK                 (Mappings (..),
                                                 ZipMatchK (..),
                                                 zipMatchViaChooseLeft,
                                                 zipMatchViaEq)
import           Control.Monad.Free.Foil.Annotated (AnnSig (..))
import           Data.ZipMatchK.TH              (deriveZipMatchK)
import           Data.Bifoldable                (Bifoldable (..))
import           Data.Bifunctor                 (Bifunctor (..))
import           Data.Bifunctor.TH              (deriveBifoldable,
                                                 deriveBifunctor,
                                                 deriveBitraversable)
import qualified Data.IntMap                    as IntMap
import           Generics.Kind.TH                (deriveGenericK)
import qualified GHC.Generics                   as GHC
import           Unsafe.Coerce                  (unsafeCoerce)

import           Language.Rzk.Foil.Names        (Binder (..), RzkPosition (..),
                                                 TModality (..), TypeInfo (..),
                                                 VarIdent)

-- * The signature
--
-- A transliteration of @TermF@: same constructors, same fields, same order. The
-- @scope@ positions are the ones that bind, and there are eight of them across
-- six constructors ('TypeFunF' and 'LambdaF' each carry a tope scope as well as
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
    | CubeSupF term term
    | CubeInfF term term
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
    -- | A match over a @#data@ scrutinee: scrutinee, optional motive, and one
    -- branch per constructor (constructor name, arm chain). The node is
    -- elaborated into the generated eliminator during typechecking, so a
    -- /typed/ match never exists.
    | MatchF term (Maybe term) [(VarIdent, term)]
    -- | One branch binder of a match; the body is the next arm, or the branch
    -- body once the binders run out. Valid only inside a 'MatchF' branch; the
    -- parser cannot produce it anywhere else.
    | MatchArmF Binder scope
    | UnitF
    | TypeUnitF
    | TypeAscF term term
    | TypeRestrictedF term [(term, term)]
    | TypeModalF TModality term
    | ModAppF TModality term
    | ModExtractF TModality TModality term
    | LetModF Binder TModality TModality (Maybe term) (Maybe term) term scope
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

instance ZipMatchK LambdaParam

-- | The node matcher, TH-derived: an explicit instance, so no @Generics.Kind@
-- view is rebuilt per comparison. It drives 'Control.Monad.Free.Foil.alphaEquiv'
-- and 'Control.Monad.Free.Foil.unsafeEqAST', run on every comparison of two
-- terms, which in a dependent checker is most of the work. This replaced a
-- hand-written 44-constructor matcher carried on free-foil 0.2.0 (which had no
-- deriver); the deriver is the point of moving to this free-foil.
deriveZipMatchK ''TermSig


-- * Annotations
--
-- 'AnnSig' is @Control.Monad.Free.Foil.Annotated@'s: the annotation is a functor
-- of the signature's /term/ parameter, so a node's type is a term in the node's
-- own scope. free-foil provides it with a /derived/ (explicit) 'ZipMatchK', its
-- 'Bifunctor' (recurses into the annotation, for substitution) and its
-- 'Bifoldable' (does not, so a term's free variables exclude those only in its
-- type). All rzk adds is how its own annotation, 'TypeInfo', matches.

-- | The annotation is ignored in matching, so two terms differing only in their
-- types are α-equivalent. The 'ZipMatchK' API makes an annotation holding terms
-- construct its result through the mapping, so it cannot be dropped from the
-- match — but it is made lazy: 'infoType' below is a thunk never forced, because
-- 'AnnSig's 'Bifoldable' does not visit the annotation. So the 30-deep universe
-- tower inside a type is never walked. The memoised forms are dropped (every
-- consumer of the zipped result discards them). This is the annotation-blind
-- pattern from the 'Control.Monad.Free.Foil.Annotated' haddock.
instance ZipMatchK TypeInfo where
  zipMatchWithK (f :^: M0) (TypeInfo t1 _ _) (TypeInfo t2 _ _) =
    Just (TypeInfo (maybe (error "ZipMatchK TypeInfo: annotation forced") id (f t1 t2)) Nothing Nothing)

-- | Where a term was written.
--
-- This is the annotation of an /untyped/ term, so that a diagnostic can point
-- at the sub-term it is about rather than at the declaration around it (issue
-- #81). It is a phantom in the node's term parameter: the annotation machinery
-- wants a functor of the term, and a position holds no terms.
newtype SrcPos term = SrcPos RzkPosition
  deriving (Functor, Foldable, Traversable)

-- | Two terms written in different places are the same term, so a position is
-- ignored in matching, as a node's type is.
instance ZipMatchK SrcPos where
  zipMatchWithK _ (SrcPos pos) _ = Just (SrcPos pos)

-- | No position: what a node the checker builds itself carries, and what a node
-- gets until the conversion from the surface syntax puts the real one on it.
noSrcPos :: SrcPos term
noSrcPos = SrcPos (RzkPosition Nothing Nothing)

-- * Terms

-- | An untyped term: the surface syntax, elaborated, every node carrying where
-- it was written.
type Term = AST NameBinder (AnnSig SrcPos TermSig)

-- | A typed term: every node carries its type. The successor of @TermT@.
type TermT = AST NameBinder (AnnSig TypeInfo TermSig)

-- | A scope: a binder together with the term it binds over.
type ScopedTermT = ScopedAST NameBinder (AnnSig TypeInfo TermSig)

-- | A scope of an untyped term.
type ScopedTerm = ScopedAST NameBinder (AnnSig SrcPos TermSig)

-- | Record where a term was written.
--
-- A variable carries no node of its own, so it is returned unchanged; where a
-- variable occurrence was written is on its 'VarIdent' instead.
atSrcPos :: RzkPosition -> Term n -> Term n
atSrcPos pos (Node (AnnSig _ sig)) = Node (AnnSig (SrcPos pos) sig)
atSrcPos _ t@(Var _)               = t

-- | Where the term was written, when it came from a file.
positionOfTerm :: Term n -> Maybe RzkPosition
positionOfTerm (Var _)                        = Nothing
positionOfTerm (Node (AnnSig (SrcPos pos) _)) =
  case rzkLineCol pos of
    Nothing -> Nothing
    Just _  -> Just pos

-- | The annotation of a node: its type, and its memoised normal forms. A
-- variable carries none — its type lives in the context.
typeInfoOf :: TermT n -> Maybe (TypeInfo (TermT n))
typeInfoOf (Var _)                = Nothing
typeInfoOf (Node (AnnSig info _)) = Just info

-- | Drop every annotation, for printing and for the surface-facing API.
untyped :: TermT n -> Term n
untyped (Var name)              = Var name
untyped (Node (AnnSig _ann sig)) = UntypedNode (bimap untypedScoped untyped sig)
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

-- * Free variables

-- | The free variables of a term.
--
-- free-foil has @freeVarsOf@ only on its unreleased @main@, so this is written
-- here. A name bound on the way down is dropped from the result, which is what
-- makes the coercion back to the outer scope right.
freeVarsOfTerm :: Term n -> [Foil.Name n]
freeVarsOfTerm (Var x)    = [x]
freeVarsOfTerm (UntypedNode sig) = bifoldMap freeVarsOfScoped freeVarsOfTerm sig
  where
    freeVarsOfScoped :: ScopedTerm n' -> [Foil.Name n']
    freeVarsOfScoped (ScopedAST binder body) =
      unsafeCoerce
        [ x
        | x <- freeVarsOfTerm body
        , Foil.nameId x /= Foil.nameId (Foil.nameOf binder)
        ]

-- | The free variables of a typed term, not counting those that occur only in the
-- types of its nodes ('Bifoldable' skips the annotation, as it did before).
freeVarsOfTermT :: TermT n -> [Foil.Name n]
freeVarsOfTermT = freeVarsOfTerm . untyped

-- | Does the term mention the universe @U@ anywhere? A constructor field
-- whose type does is what makes an inductive type /large/ (see the
-- largeness warning in "Rzk.TypeCheck.Decl").
containsUniverse :: Term n -> Bool
containsUniverse Universe   = True
containsUniverse (Var _)    = False
containsUniverse (UntypedNode sig) =
  bifoldr (\scoped acc -> containsUniverseScoped scoped || acc)
          (\t acc -> containsUniverse t || acc) False sig
  where
    containsUniverseScoped (ScopedAST _ body) = containsUniverse body

-- * Holes

isHoleT :: TermT n -> Bool
isHoleT HoleT{} = True
isHoleT _       = False

-- | The name of every hole in a term.
holeNamesOf :: Term n -> [Maybe VarIdent]
holeNamesOf (Hole mname) = [mname]
holeNamesOf (Var _)      = []
holeNamesOf (UntypedNode sig) = bifoldr (\scoped acc -> holeNamesOfScoped scoped <> acc)
                                   (\t acc -> holeNamesOf t <> acc) [] sig
  where
    holeNamesOfScoped (ScopedAST _ body) = holeNamesOf body

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
  :: (Bifunctor sig, Foil.Distinct n)
  => Foil.Scope n
  -> ScopedAST NameBinder sig n
  -> (forall l. Foil.DExt n l => NameBinder n l -> AST NameBinder sig l -> r)
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
--
-- Generic in the signature, so that a λ's (untyped) body and the codomain of the
-- Π it is checked against can be opened under one and the same binder.
openWith
  :: (Bifunctor sig, Foil.DExt n l)
  => Foil.Scope l -> Foil.Name l -> ScopedAST NameBinder sig n -> AST NameBinder sig l
openWith scope name (ScopedAST binder body) =
  substitute scope (Foil.addRename (Foil.sink Foil.identitySubst) binder name) body

-- | Replace a /free/ name by a term.
--
-- A section's assumption is a free name at the top level, and closing the section
-- abstracts it out of the definitions that use it; this is how those definitions
-- are rewritten. free-foil's substitutions are keyed by a binder, so the map is
-- built directly.
substituteName
  :: Foil.Distinct n
  => Foil.Scope n -> Foil.Name n -> TermT n -> TermT n -> TermT n
substituteName scope name value =
  substituteT scope (UnsafeSubstitution (IntMap.singleton (Foil.nameId name) value))

-- | Abstract a free name out of a term: the binder the continuation receives binds
-- what the name stood for.
--
-- The name stays in the scope index (a scope only ever grows), but the term no
-- longer mentions it, which is what makes the resulting Π or λ closed over it.
abstractName
  :: Foil.Distinct n
  => Foil.Scope n
  -> Foil.Name n
  -> TermT n
  -> (forall l. Foil.DExt n l => NameBinder n l -> TermT l -> r)
  -> r
abstractName scope name term k =
  Foil.withFresh scope $ \binder ->
    let scope' = Foil.extendScope binder scope
        term' = substituteName scope' (Foil.sink name) (Var (Foil.nameOf binder))
                  (Foil.sink term)
     in k binder term'

-- | Instantiate a scoped term with a term: the successor of @substituteT x scope@.
instantiateT :: Foil.Distinct n => Foil.Scope n -> ScopedTermT n -> TermT n -> TermT n
instantiateT scope (ScopedAST binder body) arg =
  substituteT scope (Foil.addSubst Foil.identitySubst binder arg) body

-- | Instantiate a scoped /untyped/ term. There are no memoised normal forms to
-- invalidate, so this is free-foil's own substitution.
instantiateUntyped :: Foil.Distinct n => Foil.Scope n -> ScopedTerm n -> Term n -> Term n
instantiateUntyped scope (ScopedAST binder body) arg =
  substitute scope (Foil.addSubst Foil.identitySubst binder arg) body

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
pattern CubeSupT info l r = Node (AnnSig info (CubeSupF l r))
pattern CubeInfT info l r = Node (AnnSig info (CubeInfF l r))
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
pattern MatchT info scrut mmotive branches = Node (AnnSig info (MatchF scrut mmotive branches))
pattern MatchArmT info orig arm = Node (AnnSig info (MatchArmF orig arm))
pattern UnitT info = Node (AnnSig info UnitF)
pattern TypeUnitT info = Node (AnnSig info TypeUnitF)
pattern TypeAscT info term ty = Node (AnnSig info (TypeAscF term ty))
pattern TypeRestrictedT info ty rs = Node (AnnSig info (TypeRestrictedF ty rs))
pattern TypeModalT info md ty = Node (AnnSig info (TypeModalF md ty))
pattern ModAppT info md t = Node (AnnSig info (ModAppF md t))
pattern ModExtractT info app inn t = Node (AnnSig info (ModExtractF app inn t))
pattern LetModT info orig app inn mparam mmotive val body = Node (AnnSig info (LetModF orig app inn mparam mmotive val body))
pattern HoleT info mname = Node (AnnSig info (HoleF mname))

{-# COMPLETE Var, UniverseT, UniverseCubeT, UniverseTopeT, CubeUnitT,
  CubeUnitStarT, Cube2T, Cube2_0T, Cube2_1T, CubeIT, CubeI_0T, CubeI_1T,
  CubeProductT, CubeFlipT, CubeUnflipT, CubeSupT, CubeInfT, TopeTopT, TopeBottomT,
  TopeEQT, TopeLEQT, TopeAndT, TopeOrT, TopeInvT, TopeUninvT, RecBottomT, RecOrT,
  TypeFunT, TypeSigmaT, TypeIdT, AppT, LetT, LambdaT, PairT, FirstT, SecondT,
  ReflT, IdJT, MatchT, MatchArmT, UnitT, TypeUnitT, TypeAscT, TypeRestrictedT,
  TypeModalT, ModAppT, ModExtractT, LetModT, HoleT #-}

-- ** Untyped patterns
--
-- The same constructors on 'Term', for the surface conversions and the printer.
--
-- Each goes through 'UntypedNode', which ignores a node's position on the way in
-- and leaves it unset on the way out. So the checker builds and matches untyped
-- terms exactly as it did before terms carried a position, and the conversion
-- from the surface syntax ('atSrcPos') is the only place that puts a real one on.

-- | An untyped node, taken and made without regard for where it was written.
pattern UntypedNode :: TermSig (ScopedTerm n) (Term n) -> Term n
pattern UntypedNode sig <- Node (AnnSig _ sig) where
  UntypedNode sig = Node (AnnSig noSrcPos sig)

{-# COMPLETE Var, UntypedNode #-}

pattern Universe = UntypedNode UniverseF
pattern UniverseCube = UntypedNode UniverseCubeF
pattern UniverseTope = UntypedNode UniverseTopeF
pattern CubeUnit = UntypedNode CubeUnitF
pattern CubeUnitStar = UntypedNode CubeUnitStarF
pattern Cube2 = UntypedNode Cube2F
pattern Cube2_0 = UntypedNode Cube2_0F
pattern Cube2_1 = UntypedNode Cube2_1F
pattern CubeI = UntypedNode CubeIF
pattern CubeI_0 = UntypedNode CubeI_0F
pattern CubeI_1 = UntypedNode CubeI_1F
pattern CubeProduct l r = UntypedNode (CubeProductF l r)
pattern CubeFlip t = UntypedNode (CubeFlipF t)
pattern CubeUnflip t = UntypedNode (CubeUnflipF t)
pattern CubeSup l r = UntypedNode (CubeSupF l r)
pattern CubeInf l r = UntypedNode (CubeInfF l r)
pattern TopeTop = UntypedNode TopeTopF
pattern TopeBottom = UntypedNode TopeBottomF
pattern TopeEQ l r = UntypedNode (TopeEQF l r)
pattern TopeLEQ l r = UntypedNode (TopeLEQF l r)
pattern TopeAnd l r = UntypedNode (TopeAndF l r)
pattern TopeOr l r = UntypedNode (TopeOrF l r)
pattern TopeInv t = UntypedNode (TopeInvF t)
pattern TopeUninv t = UntypedNode (TopeUninvF t)
pattern RecBottom = UntypedNode RecBottomF
pattern RecOr rs = UntypedNode (RecOrF rs)
pattern TypeFun orig md param mtope ret = UntypedNode (TypeFunF orig md param mtope ret)
pattern TypeSigma orig md a b = UntypedNode (TypeSigmaF orig md a b)
pattern TypeId a mtA b = UntypedNode (TypeIdF a mtA b)
pattern App f x = UntypedNode (AppF f x)
pattern Let orig mparam val body = UntypedNode (LetF orig mparam val body)
pattern Lambda orig mparam body = UntypedNode (LambdaF orig mparam body)
pattern Pair l r = UntypedNode (PairF l r)
pattern First t = UntypedNode (FirstF t)
pattern Second t = UntypedNode (SecondF t)
pattern Refl mx = UntypedNode (ReflF mx)
pattern IdJ a b c d e f = UntypedNode (IdJF a b c d e f)
pattern Match scrut mmotive branches = UntypedNode (MatchF scrut mmotive branches)
pattern MatchArm orig arm = UntypedNode (MatchArmF orig arm)
pattern Unit = UntypedNode UnitF
pattern TypeUnit = UntypedNode TypeUnitF
pattern TypeAsc term ty = UntypedNode (TypeAscF term ty)
pattern TypeRestricted ty rs = UntypedNode (TypeRestrictedF ty rs)
pattern TypeModal md ty = UntypedNode (TypeModalF md ty)
pattern ModApp md t = UntypedNode (ModAppF md t)
pattern ModExtract app inn t = UntypedNode (ModExtractF app inn t)
pattern LetMod orig app inn mparam mmotive val body = UntypedNode (LetModF orig app inn mparam mmotive val body)
pattern Hole mname = UntypedNode (HoleF mname)

{-# COMPLETE Var, Universe, UniverseCube, UniverseTope, CubeUnit, CubeUnitStar,
  Cube2, Cube2_0, Cube2_1, CubeI, CubeI_0, CubeI_1, CubeProduct, CubeFlip,
  CubeUnflip, CubeSup, CubeInf, TopeTop, TopeBottom, TopeEQ, TopeLEQ, TopeAnd,
  TopeOr, TopeInv, TopeUninv, RecBottom, RecOr, TypeFun, TypeSigma, TypeId, App,
  Let, Lambda, Pair, First, Second, Refl, IdJ, Match, MatchArm, Unit, TypeUnit,
  TypeAsc, TypeRestricted, TypeModal, ModApp, ModExtract, LetMod, Hole #-}

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

cubeSupT :: TermT n -> TermT n -> TermT n -> TermT n
cubeSupT cubeTy l r = CubeSupT (topeInfo cubeTy) l r

cubeInfT :: TermT n -> TermT n -> TermT n -> TermT n
cubeInfT cubeTy l r = CubeInfT (topeInfo cubeTy) l r

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
  :: TermT n -> Binder -> TModality -> TModality -> Maybe (TermT n)
  -> Maybe (TermT n) -> TermT n -> ScopedTermT n -> TermT n
letModT ty orig app inn mparam mmotive val body =
  LetModT (topeInfo ty) orig app inn mparam mmotive val body

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
