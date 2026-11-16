{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
-- The 'ZipMatchK' instances of the surface types are orphans: the class is
-- free-foil's and the types are the surface syntax's, and they belong with
-- the signature that holds them.
{-# OPTIONS_GHC -Wno-orphans #-}
-- | The syntax of the checker: one signature over a pattern binder, with a
-- raw term (every node carrying where it was written) and a core term (no
-- annotation), which closures hold and readback produces.
--
-- A binder is a 'Pattern': a variable, the unit pattern, or a pair of
-- patterns, each leaf binding one name. A pair pattern binds its names
-- directly, so no projection of a single variable stands for a component.
--
-- A shape is a term: the shape of a Π or of a λ over a cube is a tope
-- family @cube → TOPE@, applied to the bound point where it is assumed. So
-- @(t : I | φ)@ holds the family @\\ t → φ@, and @(t : Δ¹)@ holds @Δ¹@.
module Rzk.Check.Syntax
  ( -- * Binders
    Pattern (..)
  , patternNames
    -- * The signature
  , Param (..)
  , Sig (..)
    -- * Terms
  , Raw
  , ScopedRaw
  , PlainCore
  , ScopedPlainCore
  , Core
  , ScopedCore
  , SrcPos (..)
  , rawNode
  , panicImpossible
  , noPosition
  , positionOfRaw
  , stripPositions
    -- * Core patterns
  , pattern Universe, pattern UniverseCube, pattern UniverseTope
  , pattern CubeUnit, pattern CubeUnitStar, pattern Cube2, pattern Cube2_0, pattern Cube2_1
  , pattern CubeI, pattern CubeI_0, pattern CubeI_1
  , pattern CubeProduct, pattern CubeFlip, pattern CubeUnflip, pattern CubeSup, pattern CubeInf
  , pattern TopeTop, pattern TopeBottom, pattern TopeEQ, pattern TopeLEQ, pattern TopeAnd, pattern TopeOr
  , pattern TopeInv, pattern TopeUninv
  , pattern RecBottom, pattern RecOr
  , pattern TypeFun, pattern TypeShape, pattern TypeSigma, pattern TypeId, pattern App, pattern Let, pattern Lambda
  , pattern Pair, pattern First, pattern Second, pattern Refl, pattern IdJ
  , pattern Match, pattern MatchArm, pattern Unit, pattern TypeUnit, pattern TypeAsc
  , pattern TypeRestricted, pattern TypeModal, pattern ModApp, pattern ModExtract, pattern LetMod
  , pattern Hole
    -- * Raw patterns
  , pattern RawNode
  , pattern RUniverse
  , pattern RUniverseCube
  , pattern RUniverseTope
  , pattern RCubeUnit
  , pattern RCubeUnitStar
  , pattern RCube2
  , pattern RCube2_0
  , pattern RCube2_1
  , pattern RCubeI
  , pattern RCubeI_0
  , pattern RCubeI_1
  , pattern RCubeProduct
  , pattern RCubeFlip
  , pattern RCubeUnflip
  , pattern RCubeSup
  , pattern RCubeInf
  , pattern RTopeTop
  , pattern RTopeBottom
  , pattern RTopeEQ
  , pattern RTopeLEQ
  , pattern RTopeAnd
  , pattern RTopeOr
  , pattern RTopeInv
  , pattern RTopeUninv
  , pattern RRecBottom
  , pattern RRecOr
  , pattern RTypeFun
  , pattern RTypeShape
  , pattern RTypeSigma
  , pattern RTypeId
  , pattern RApp
  , pattern RLet
  , pattern RLambda
  , pattern RPair
  , pattern RFirst
  , pattern RSecond
  , pattern RRefl
  , pattern RIdJ
  , pattern RMatch
  , pattern RMatchArm
  , pattern RUnit
  , pattern RTypeUnit
  , pattern RTypeAsc
  , pattern RTypeRestricted
  , pattern RTypeModal
  , pattern RModApp
  , pattern RModExtract
  , pattern RLetMod
  , pattern RHole
    -- * Queries
  , freeNamesOf
  , eraseNeutralTypes
  , stripAnnotations
  , substituteName
  , abstractName
  , containsHole
  , holeNamesOf
  , isHoleHeaded
  , eqCore
  ) where

import           Control.Monad.Foil                (Distinct, NameBinder, S)
import qualified Control.Monad.Foil                as Foil
import           Control.Monad.Free.Foil           (AST (..), ScopedAST (..),
                                                    substitute,
                                                    unsafeEqAST)
import           Control.Monad.Free.Foil.Annotated (AnnSig (..))
import           Data.Bifoldable                   (bifoldMap, bifoldr)
import           Data.Bifunctor                    (bimap)
import           Data.Functor.Const                (Const (..))
import           Data.Functor.Identity             (Identity (..))
import           Data.Maybe                        (mapMaybe)
import           Data.Bifunctor.TH                 (deriveBifoldable,
                                                    deriveBifunctor,
                                                    deriveBitraversable)
import           Data.ZipMatchK                    (ZipMatchFoldK (..), ZipMatchK (..),
                                                    zipMatchViaEq)
import           Data.ZipMatchK.Mappings           (Folds (..), Mappings (..))
import           Generics.Kind.TH                  (deriveGenericK)
import           Unsafe.Coerce                     (unsafeCoerce)

import           Language.Rzk.Foil.Names           (RzkPosition (..),
                                                    TModality (..), VarIdent)
import           Rzk.Check.Foil.Extras             (singletonSubstitution)

-- * Binders

-- | A binder. A variable carries the name the user wrote, if any. A wildcard
-- binds a hidden name, which conversion may need when binder shapes differ.
-- The unit pattern binds no name. A pair pattern binds the names of both
-- components, the right one in the scope of the left.
data Pattern (n :: S) (l :: S) where
  PatternVar      :: Maybe VarIdent -> NameBinder n l -> Pattern n l
  PatternWildcard :: NameBinder n l -> Pattern n l
  PatternUnit     :: Pattern n n
  PatternPair     :: Pattern n i -> Pattern i l -> Pattern n l

deriveGenericK ''Pattern
instance Foil.SinkableK Pattern
instance Foil.CoSinkable Pattern
instance Foil.HasNameBinders Pattern
instance Foil.UnifiablePattern Pattern

-- | The names a pattern binds, in binding order, with their display names.
-- The names of the left component are names of the right one's scope too:
-- a name sinks by coercion, as free-foil's own 'Foil.sink' does.
patternNames :: (Distinct n, Distinct l) => Pattern n l -> [(Maybe VarIdent, Foil.Name l)]
patternNames = \case
  PatternVar x b    -> [(x, Foil.nameOf b)]
  PatternWildcard b -> [(Nothing, Foil.nameOf b)]
  PatternUnit       -> []
  PatternPair l r -> case (Foil.assertDistinct l, Foil.assertExt r) of
    (Foil.Distinct, Foil.Ext) ->
      map (fmap Foil.sink) (patternNames l) <> patternNames r

-- * The signature

-- | The parameter of a λ: its modality, its domain (a type, or the cube of
-- a shape) and, for a λ over a shape, the shape as a tope family over the
-- cube.
data Param term = Param
  { paramModality :: TModality
    -- ^ the modality of the binder
  , paramDomain   :: term
    -- ^ the type of an ordinary binder, or the cube of a shaped binder
  , paramShape    :: Maybe term
    -- ^ the shape as a tope family over the cube, if the binder has one
  } deriving (Functor, Foldable, Traversable, Show)

instance ZipMatchK TModality where
  zipMatchWithK = zipMatchViaEq

instance ZipMatchK VarIdent where
  zipMatchWithK = zipMatchViaEq

-- | A hole's name is a whole field, matched as a constant.
instance ZipMatchK (Maybe VarIdent) where
  zipMatchWithK = zipMatchViaEq

deriveGenericK ''Param
instance ZipMatchK Param
instance ZipMatchFoldK Param

-- | The signature of the language, with neutral metadata supplied by @ann@.
-- Raw terms use 'Const' to omit neutral types; core terms and semantic values
-- use 'Identity' to make the type of every neutral elimination mandatory.
-- Scoped positions bind a 'Pattern'.
data Sig ann scope term
  = UniverseF
    -- ^ the universe @U@ of ordinary types
  | UniverseCubeF
    -- ^ the universe @CUBE@ of cubes
  | UniverseTopeF
    -- ^ the universe @TOPE@ of topes
  | CubeUnitF
    -- ^ the unit cube
  | CubeUnitStarF
    -- ^ the unique point of the unit cube
  | Cube2F
    -- ^ the directed interval cube @2@
  | Cube2_0F
    -- ^ the left endpoint of @2@
  | Cube2_1F
    -- ^ the right endpoint of @2@
  | CubeIF
    -- ^ the auxiliary directed interval cube @𝕀@
  | CubeI_0F
    -- ^ the left endpoint of @𝕀@
  | CubeI_1F
    -- ^ the right endpoint of @𝕀@
  | CubeProductF term term
    -- ^ a product of two cubes
  | CubeFlipF term
    -- ^ reversal of an interval point
  | CubeUnflipF term
    -- ^ the inverse operation to 'CubeFlipF'
  | CubeSupF term term
    -- ^ the join of two interval points
  | CubeInfF term term
    -- ^ the meet of two interval points
  | TopeTopF
    -- ^ the trivial tope @⊤@
  | TopeBottomF
    -- ^ the empty tope @⊥@
  | TopeEQF term term
    -- ^ equality of two cube points
  | TopeLEQF term term
    -- ^ the order relation on two directed-interval points
  | TopeAndF term term
    -- ^ conjunction of topes
  | TopeOrF term term
    -- ^ disjunction of topes
  | TopeInvF term
    -- ^ inversion of a tope under @ᵒᵖ@
  | TopeUninvF term
    -- ^ the inverse operation to 'TopeInvF'
  | RecBottomF
    -- ^ ex falso, @recBOT@
  | RecOrF (ann term) [(term, term)]
    -- ^ a raw @recOR@ has no metadata; an elaborated one carries its type,
    -- which a stuck semantic @recOR@ needs when it heads an elimination
  | TypeFunF TModality term (Maybe term) scope
    -- ^ a Π: the modality of the binder, its domain (a type, or the cube of
    -- a shape), the shape as a tope family over the cube, and the codomain
  | TypeShapeF TModality term (Maybe term)
    -- ^ the type of a tope family: @{t : cube | shape} → TOPE@, whose
    -- codomain is constant, so that the subtyping of families, which is
    -- covariant in the shape, can tell it from a Π
  | TypeSigmaF TModality term scope
    -- ^ a dependent pair type, with the modality and type of its first component
  | TypeIdF term (Maybe term) term
    -- ^ an identity type: the endpoints and, once elaborated, their carrier
  | AppF (ann term) term term
    -- ^ application of a function or a tope family; core and semantic
    -- applications carry their result type as metadata
  | LetF (Maybe term) term scope
    -- ^ a local definition: its optional type, value, and body
  | LambdaF (Maybe (Param term)) scope
    -- ^ a λ abstraction, with an optional parameter annotation and its body
  | PairF term term
    -- ^ a pair of terms or cube points
  | FirstF (ann term) term
    -- ^ the first projection of a pair, with its component type as metadata
  | SecondF (ann term) term
    -- ^ the second projection of a pair, with its component type as metadata
  | ReflF (Maybe (term, Maybe term))
    -- ^ reflexivity, optionally annotated by its element and carrier
  | IdJF (ann term) term term term term term term
    -- ^ path induction: its result type, carrier, base point, motive, base
    -- case, endpoint, and path
  | StuckAppF term term
    -- ^ a semantic application of a canonical non-function, stuck during
    -- conversion rather than a neutral with a known result type
  | StuckFirstF term
    -- ^ a semantic first projection stuck on a canonical non-pair
  | StuckSecondF term
    -- ^ a semantic second projection stuck on a canonical non-pair
  | StuckIdJF term term term term term term
    -- ^ semantic path induction stuck on a canonical non-path
  | MatchF TModality term (Maybe term) [(VarIdent, term)]
    -- ^ a match on a @#data@ value: the modality, the scrutinee, the
    -- @into@ motive, and the branches, each an arm chain
  | MatchArmF scope
    -- ^ one binder of a match branch
  | UnitF
    -- ^ the unique inhabitant of the unit type
  | TypeUnitF
    -- ^ the unit type
  | TypeAscF term term
    -- ^ a user ascription; evaluation erases it
  | TypeRestrictedF term [(term, term)]
    -- ^ an extension type: its underlying type and tope/boundary faces
  | TypeModalF TModality term
    -- ^ a type under a modality
  | ModAppF TModality term
    -- ^ introduction of a term under a modality
  | ModExtractF TModality TModality term
    -- ^ an internal modal elimination, with its outer and inner modalities
  | LetModF TModality TModality (Maybe term) (Maybe term) term scope
    -- ^ modal elimination by @let mod@, with modalities, optional type and
    -- motive, payload, and body
  | HoleF (Maybe VarIdent)
    -- ^ a user hole, with its optional name; not an internal unknown type
  deriving (Functor, Foldable, Traversable)

deriveBifunctor ''Sig
deriveBifoldable ''Sig
deriveBitraversable ''Sig
deriveGenericK ''Sig

instance ZipMatchK Identity where
  zipMatchWithK (g :^: M0) (Identity l) (Identity r) = Identity <$> g l r

instance ZipMatchFoldK Identity where
  zipMatchFoldK (g :%: F0) (Identity l) (Identity r) = Just (g l r)

instance ZipMatchK (Const ()) where
  zipMatchWithK _ (Const l) (Const _) = Just (Const l)

instance ZipMatchFoldK (Const ()) where
  zipMatchFoldK _ (Const _) (Const _) = Just mempty

instance ZipMatchK (Sig Identity)
instance ZipMatchFoldK (Sig Identity)
instance ZipMatchK (Sig (Const ()))
instance ZipMatchFoldK (Sig (Const ()))

-- * Terms

-- | Where a term was written: the annotation of a raw term, a phantom in the
-- node's term parameter.
newtype SrcPos term = SrcPos RzkPosition
  deriving (Functor, Foldable, Traversable)

-- | Two terms written in different places are the same term.
instance ZipMatchK SrcPos where
  zipMatchWithK _ (SrcPos pos) _ = Just (SrcPos pos)

-- | A raw term: the surface syntax converted, every node carrying where it
-- was written and no neutral carrying a type. What the checker takes.
type Raw = AST Pattern (AnnSig SrcPos (Sig (Const ())))

type ScopedRaw = ScopedAST Pattern (AnnSig SrcPos (Sig (Const ())))

-- | An unannotated core view, used for display and fingerprints.
type PlainCore = AST Pattern (Sig (Const ()))

type ScopedPlainCore = ScopedAST Pattern (Sig (Const ()))

-- | A core term: what the checker produces, what closures hold, and what
-- readback gives. Every neutral elimination carries its type.
type Core = AST Pattern (Sig Identity)

type ScopedCore = ScopedAST Pattern (Sig Identity)

-- | A raw node at a position.
rawNode :: RzkPosition -> Sig (Const ()) (ScopedRaw n) (Raw n) -> Raw n
rawNode pos sig = Node (AnnSig (SrcPos pos) sig)

-- | Fail on a case the checker's invariants rule out, asking for a bug report.
panicImpossible :: String -> a
panicImpossible msg = error $ unlines
  [ "PANIC! Impossible happened (" <> msg <> ")!"
  , "Please, report a bug at https://github.com/rzk-lang/rzk/issues"
  ]

-- | The position of a node the checker builds, which was written nowhere.
noPosition :: RzkPosition
noPosition = RzkPosition Nothing Nothing

-- | Where a raw term was written, when it came from a file. A variable is a
-- leaf and carries no position.
positionOfRaw :: Raw n -> Maybe RzkPosition
positionOfRaw (Var _) = Nothing
positionOfRaw (Node (AnnSig (SrcPos pos) _)) = case rzkLineCol pos of
  Nothing -> Nothing
  Just _  -> Just pos


-- | A raw term as a plain core term: the positions dropped.
stripPositions :: Raw n -> PlainCore n
stripPositions (Var x) = Var x
stripPositions (Node (AnnSig _ sig)) = Node (bimap scoped stripPositions (eraseNeutralTypes sig))
  where
    scoped (ScopedAST pat body) = ScopedAST pat (stripPositions body)

eraseNeutralTypes :: Sig ann a b -> Sig (Const ()) a b
eraseNeutralTypes = \case
  RecOrF _ rs      -> RecOrF (Const ()) rs
  AppF _ f x       -> AppF (Const ()) f x
  FirstF _ p       -> FirstF (Const ()) p
  SecondF _ p      -> SecondF (Const ()) p
  IdJF _ a b c d e f -> IdJF (Const ()) a b c d e f
  -- These constructors have no metadata, so their representation does not
  -- depend on @ann@.
  node             -> unsafeCoerce node

-- * Core patterns

pattern Universe :: AST Pattern (Sig ann) n
pattern Universe = Node UniverseF
pattern UniverseCube :: AST Pattern (Sig ann) n
pattern UniverseCube = Node UniverseCubeF
pattern UniverseTope :: AST Pattern (Sig ann) n
pattern UniverseTope = Node UniverseTopeF
pattern CubeUnit :: AST Pattern (Sig ann) n
pattern CubeUnit = Node CubeUnitF
pattern CubeUnitStar :: AST Pattern (Sig ann) n
pattern CubeUnitStar = Node CubeUnitStarF
pattern Cube2 :: AST Pattern (Sig ann) n
pattern Cube2 = Node Cube2F
pattern Cube2_0 :: AST Pattern (Sig ann) n
pattern Cube2_0 = Node Cube2_0F
pattern Cube2_1 :: AST Pattern (Sig ann) n
pattern Cube2_1 = Node Cube2_1F
pattern CubeI :: AST Pattern (Sig ann) n
pattern CubeI = Node CubeIF
pattern CubeI_0 :: AST Pattern (Sig ann) n
pattern CubeI_0 = Node CubeI_0F
pattern CubeI_1 :: AST Pattern (Sig ann) n
pattern CubeI_1 = Node CubeI_1F
pattern CubeProduct :: AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern CubeProduct l r = Node (CubeProductF l r)
pattern CubeFlip :: AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern CubeFlip t = Node (CubeFlipF t)
pattern CubeUnflip :: AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern CubeUnflip t = Node (CubeUnflipF t)
pattern CubeSup :: AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern CubeSup l r = Node (CubeSupF l r)
pattern CubeInf :: AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern CubeInf l r = Node (CubeInfF l r)
pattern TopeTop :: AST Pattern (Sig ann) n
pattern TopeTop = Node TopeTopF
pattern TopeBottom :: AST Pattern (Sig ann) n
pattern TopeBottom = Node TopeBottomF
pattern TopeEQ :: AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern TopeEQ l r = Node (TopeEQF l r)
pattern TopeLEQ :: AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern TopeLEQ l r = Node (TopeLEQF l r)
pattern TopeAnd :: AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern TopeAnd l r = Node (TopeAndF l r)
pattern TopeOr :: AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern TopeOr l r = Node (TopeOrF l r)
pattern TopeInv :: AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern TopeInv t = Node (TopeInvF t)
pattern TopeUninv :: AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern TopeUninv t = Node (TopeUninvF t)
pattern RecBottom :: AST Pattern (Sig ann) n
pattern RecBottom = Node RecBottomF
pattern RecOr :: ann (AST Pattern (Sig ann) n) -> [(AST Pattern (Sig ann) n, AST Pattern (Sig ann) n)] -> AST Pattern (Sig ann) n
pattern RecOr ty rs = Node (RecOrF ty rs)
pattern TypeFun :: TModality -> AST Pattern (Sig ann) n -> Maybe (AST Pattern (Sig ann) n) -> ScopedAST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern TypeFun md dom mshape ret = Node (TypeFunF md dom mshape ret)
pattern TypeShape :: TModality -> AST Pattern (Sig ann) n -> Maybe (AST Pattern (Sig ann) n) -> AST Pattern (Sig ann) n
pattern TypeShape md cube mshape = Node (TypeShapeF md cube mshape)
pattern TypeSigma :: TModality -> AST Pattern (Sig ann) n -> ScopedAST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern TypeSigma md a b = Node (TypeSigmaF md a b)
pattern TypeId :: AST Pattern (Sig ann) n -> Maybe (AST Pattern (Sig ann) n) -> AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern TypeId x mtA y = Node (TypeIdF x mtA y)
pattern App :: ann (AST Pattern (Sig ann) n) -> AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern App ty f x = Node (AppF ty f x)
pattern Let :: Maybe (AST Pattern (Sig ann) n) -> AST Pattern (Sig ann) n -> ScopedAST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern Let mty val body = Node (LetF mty val body)
pattern Lambda :: Maybe (Param (AST Pattern (Sig ann) n)) -> ScopedAST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern Lambda mparam body = Node (LambdaF mparam body)
pattern Pair :: AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern Pair l r = Node (PairF l r)
pattern First :: ann (AST Pattern (Sig ann) n) -> AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern First ty t = Node (FirstF ty t)
pattern Second :: ann (AST Pattern (Sig ann) n) -> AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern Second ty t = Node (SecondF ty t)
pattern Refl :: Maybe (AST Pattern (Sig ann) n, Maybe (AST Pattern (Sig ann) n)) -> AST Pattern (Sig ann) n
pattern Refl mx = Node (ReflF mx)
pattern IdJ :: ann (AST Pattern (Sig ann) n) -> AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern IdJ ty a b c d e f = Node (IdJF ty a b c d e f)
pattern Match :: TModality -> AST Pattern (Sig ann) n -> Maybe (AST Pattern (Sig ann) n) -> [(VarIdent, AST Pattern (Sig ann) n)] -> AST Pattern (Sig ann) n
pattern Match md scrut mmotive branches = Node (MatchF md scrut mmotive branches)
pattern MatchArm :: ScopedAST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern MatchArm arm = Node (MatchArmF arm)
pattern Unit :: AST Pattern (Sig ann) n
pattern Unit = Node UnitF
pattern TypeUnit :: AST Pattern (Sig ann) n
pattern TypeUnit = Node TypeUnitF
pattern TypeAsc :: AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern TypeAsc term ty = Node (TypeAscF term ty)

pattern TypeRestricted :: AST Pattern (Sig ann) n -> [(AST Pattern (Sig ann) n, AST Pattern (Sig ann) n)] -> AST Pattern (Sig ann) n
pattern TypeRestricted ty rs = Node (TypeRestrictedF ty rs)
pattern TypeModal :: TModality -> AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern TypeModal md ty = Node (TypeModalF md ty)
pattern ModApp :: TModality -> AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern ModApp md t = Node (ModAppF md t)
pattern ModExtract :: TModality -> TModality -> AST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern ModExtract app inn t = Node (ModExtractF app inn t)
pattern LetMod :: TModality -> TModality -> Maybe (AST Pattern (Sig ann) n) -> Maybe (AST Pattern (Sig ann) n) -> AST Pattern (Sig ann) n -> ScopedAST Pattern (Sig ann) n -> AST Pattern (Sig ann) n
pattern LetMod app inn mty mmotive val body = Node (LetModF app inn mty mmotive val body)
pattern Hole :: Maybe VarIdent -> AST Pattern (Sig ann) n
pattern Hole mname = Node (HoleF mname)

{-# COMPLETE Var, Universe, UniverseCube, UniverseTope, CubeUnit, CubeUnitStar,
  Cube2, Cube2_0, Cube2_1, CubeI, CubeI_0, CubeI_1, CubeProduct, CubeFlip,
  CubeUnflip, CubeSup, CubeInf, TopeTop, TopeBottom, TopeEQ, TopeLEQ, TopeAnd,
  TopeOr, TopeInv, TopeUninv, RecBottom, RecOr, TypeFun, TypeShape, TypeSigma, TypeId, App,
  Let, Lambda, Pair, First, Second, Refl, IdJ, Match, MatchArm, Unit, TypeUnit,
  TypeAsc, TypeRestricted, TypeModal, ModApp, ModExtract, LetMod, Hole #-}

-- * Raw patterns

-- | A raw node, matched without regard for where it was written.
pattern RawNode :: Sig (Const ()) (ScopedRaw n) (Raw n) -> Raw n
pattern RawNode sig <- Node (AnnSig _ sig)

{-# COMPLETE Var, RawNode #-}

-- | A raw node of each constructor, matched without regard for where it was
-- written: @RTypeFun@ for 'TypeFunF', and so on.
pattern RUniverse :: Raw n
pattern RUniverse <- RawNode UniverseF
pattern RUniverseCube :: Raw n
pattern RUniverseCube <- RawNode UniverseCubeF
pattern RUniverseTope :: Raw n
pattern RUniverseTope <- RawNode UniverseTopeF
pattern RCubeUnit :: Raw n
pattern RCubeUnit <- RawNode CubeUnitF
pattern RCubeUnitStar :: Raw n
pattern RCubeUnitStar <- RawNode CubeUnitStarF
pattern RCube2 :: Raw n
pattern RCube2 <- RawNode Cube2F
pattern RCube2_0 :: Raw n
pattern RCube2_0 <- RawNode Cube2_0F
pattern RCube2_1 :: Raw n
pattern RCube2_1 <- RawNode Cube2_1F
pattern RCubeI :: Raw n
pattern RCubeI <- RawNode CubeIF
pattern RCubeI_0 :: Raw n
pattern RCubeI_0 <- RawNode CubeI_0F
pattern RCubeI_1 :: Raw n
pattern RCubeI_1 <- RawNode CubeI_1F
pattern RCubeProduct :: Raw n -> Raw n -> Raw n
pattern RCubeProduct x1 x2 <- RawNode (CubeProductF x1 x2)
pattern RCubeFlip :: Raw n -> Raw n
pattern RCubeFlip x1 <- RawNode (CubeFlipF x1)
pattern RCubeUnflip :: Raw n -> Raw n
pattern RCubeUnflip x1 <- RawNode (CubeUnflipF x1)
pattern RCubeSup :: Raw n -> Raw n -> Raw n
pattern RCubeSup x1 x2 <- RawNode (CubeSupF x1 x2)
pattern RCubeInf :: Raw n -> Raw n -> Raw n
pattern RCubeInf x1 x2 <- RawNode (CubeInfF x1 x2)
pattern RTopeTop :: Raw n
pattern RTopeTop <- RawNode TopeTopF
pattern RTopeBottom :: Raw n
pattern RTopeBottom <- RawNode TopeBottomF
pattern RTopeEQ :: Raw n -> Raw n -> Raw n
pattern RTopeEQ x1 x2 <- RawNode (TopeEQF x1 x2)
pattern RTopeLEQ :: Raw n -> Raw n -> Raw n
pattern RTopeLEQ x1 x2 <- RawNode (TopeLEQF x1 x2)
pattern RTopeAnd :: Raw n -> Raw n -> Raw n
pattern RTopeAnd x1 x2 <- RawNode (TopeAndF x1 x2)
pattern RTopeOr :: Raw n -> Raw n -> Raw n
pattern RTopeOr x1 x2 <- RawNode (TopeOrF x1 x2)
pattern RTopeInv :: Raw n -> Raw n
pattern RTopeInv x1 <- RawNode (TopeInvF x1)
pattern RTopeUninv :: Raw n -> Raw n
pattern RTopeUninv x1 <- RawNode (TopeUninvF x1)
pattern RRecBottom :: Raw n
pattern RRecBottom <- RawNode RecBottomF
pattern RRecOr :: [(Raw n, Raw n)] -> Raw n
pattern RRecOr x1 <- RawNode (RecOrF (Const ()) x1)
pattern RTypeFun :: TModality -> Raw n -> Maybe (Raw n) -> ScopedRaw n -> Raw n
pattern RTypeFun x1 x2 x3 x4 <- RawNode (TypeFunF x1 x2 x3 x4)
pattern RTypeShape :: TModality -> Raw n -> Maybe (Raw n) -> Raw n
pattern RTypeShape x1 x2 x3 <- RawNode (TypeShapeF x1 x2 x3)
pattern RTypeSigma :: TModality -> Raw n -> ScopedRaw n -> Raw n
pattern RTypeSigma x1 x2 x3 <- RawNode (TypeSigmaF x1 x2 x3)
pattern RTypeId :: Raw n -> Maybe (Raw n) -> Raw n -> Raw n
pattern RTypeId x1 x2 x3 <- RawNode (TypeIdF x1 x2 x3)
pattern RApp :: Raw n -> Raw n -> Raw n
pattern RApp x1 x2 <- RawNode (AppF (Const ()) x1 x2)
pattern RLet :: Maybe (Raw n) -> Raw n -> ScopedRaw n -> Raw n
pattern RLet x1 x2 x3 <- RawNode (LetF x1 x2 x3)
pattern RLambda :: Maybe (Param (Raw n)) -> ScopedRaw n -> Raw n
pattern RLambda x1 x2 <- RawNode (LambdaF x1 x2)
pattern RPair :: Raw n -> Raw n -> Raw n
pattern RPair x1 x2 <- RawNode (PairF x1 x2)
pattern RFirst :: Raw n -> Raw n
pattern RFirst x1 <- RawNode (FirstF (Const ()) x1)
pattern RSecond :: Raw n -> Raw n
pattern RSecond x1 <- RawNode (SecondF (Const ()) x1)
pattern RRefl :: Maybe (Raw n, Maybe (Raw n)) -> Raw n
pattern RRefl x1 <- RawNode (ReflF x1)
pattern RIdJ :: Raw n -> Raw n -> Raw n -> Raw n -> Raw n -> Raw n -> Raw n
pattern RIdJ x1 x2 x3 x4 x5 x6 <- RawNode (IdJF (Const ()) x1 x2 x3 x4 x5 x6)
pattern RMatch :: TModality -> Raw n -> Maybe (Raw n) -> [(VarIdent, Raw n)] -> Raw n
pattern RMatch x1 x2 x3 x4 <- RawNode (MatchF x1 x2 x3 x4)
pattern RMatchArm :: ScopedRaw n -> Raw n
pattern RMatchArm x1 <- RawNode (MatchArmF x1)
pattern RUnit :: Raw n
pattern RUnit <- RawNode UnitF
pattern RTypeUnit :: Raw n
pattern RTypeUnit <- RawNode TypeUnitF
pattern RTypeAsc :: Raw n -> Raw n -> Raw n
pattern RTypeAsc x1 x2 <- RawNode (TypeAscF x1 x2)
pattern RTypeRestricted :: Raw n -> [(Raw n, Raw n)] -> Raw n
pattern RTypeRestricted x1 x2 <- RawNode (TypeRestrictedF x1 x2)
pattern RTypeModal :: TModality -> Raw n -> Raw n
pattern RTypeModal x1 x2 <- RawNode (TypeModalF x1 x2)
pattern RModApp :: TModality -> Raw n -> Raw n
pattern RModApp x1 x2 <- RawNode (ModAppF x1 x2)
pattern RModExtract :: TModality -> TModality -> Raw n -> Raw n
pattern RModExtract x1 x2 x3 <- RawNode (ModExtractF x1 x2 x3)
pattern RLetMod :: TModality -> TModality -> Maybe (Raw n) -> Maybe (Raw n) -> Raw n -> ScopedRaw n -> Raw n
pattern RLetMod x1 x2 x3 x4 x5 x6 <- RawNode (LetModF x1 x2 x3 x4 x5 x6)
pattern RHole :: Maybe VarIdent -> Raw n
pattern RHole x1 <- RawNode (HoleF x1)

{-# COMPLETE Var, RUniverse, RUniverseCube, RUniverseTope, RCubeUnit, RCubeUnitStar, RCube2, RCube2_0, RCube2_1, RCubeI, RCubeI_0, RCubeI_1, RCubeProduct, RCubeFlip, RCubeUnflip, RCubeSup, RCubeInf, RTopeTop, RTopeBottom, RTopeEQ, RTopeLEQ, RTopeAnd, RTopeOr, RTopeInv, RTopeUninv, RRecBottom, RRecOr, RTypeFun, RTypeShape, RTypeSigma, RTypeId, RApp, RLet, RLambda, RPair, RFirst, RSecond, RRefl, RIdJ, RMatch, RMatchArm, RUnit, RTypeUnit, RTypeAsc, RTypeRestricted, RTypeModal, RModApp, RModExtract, RLetMod, RHole #-}

-- * Queries

-- | The free names of a term, in order of mention, with repeats. Names bound
-- on the way down are removed with 'Foil.unsinkNamePattern'.
freeNamesOf :: Distinct n => AST Pattern (Sig ann) n -> [Foil.Name n]
freeNamesOf (Var x) = [x]
freeNamesOf (Node sig) = bifoldMap scoped freeNamesOf (eraseNeutralTypes sig)
  where
    scoped :: Distinct n' => ScopedAST Pattern (Sig ann) n' -> [Foil.Name n']
    scoped (ScopedAST pat body) = case Foil.assertDistinct pat of
      Foil.Distinct -> mapMaybe (Foil.unsinkNamePattern pat) (freeNamesOf body)

-- | A term without the types cached at its neutral eliminations.
stripAnnotations :: Core n -> PlainCore n
stripAnnotations = \case
  Var x    -> Var x
  Node sig -> Node (bimap (\(ScopedAST pat body) -> ScopedAST pat (stripAnnotations body)) stripAnnotations (eraseNeutralTypes sig))

-- | Replace a free name of a term by a term of the same scope.
substituteName :: Foil.Distinct n => Foil.Scope n -> Foil.Name n -> Core n -> Core n -> Core n
substituteName scope name value =
  substitute scope (singletonSubstitution name value)

-- | Abstract a free name out of a term: the pattern the continuation
-- receives binds what the name stood for, under the name's display name.
-- The name stays in the scope index, as a scope only grows, but the term
-- no longer mentions it.
abstractName
  :: Foil.Distinct n
  => Foil.Scope n -> Maybe VarIdent -> Foil.Name n -> Core n
  -> (forall l. (Foil.DExt n l, Foil.Distinct l) => Pattern n l -> Core l -> r)
  -> r
abstractName scope mname name term k = Foil.withFresh scope $ \binder ->
  let scope' = Foil.extendScope binder scope
      term' = substituteName scope' (Foil.sink name) (Var (Foil.nameOf binder)) (Foil.sink term)
  in k (PatternVar mname binder) term'

-- | Does the term contain a hole anywhere?
containsHole :: forall ann n. AST Pattern (Sig ann) n -> Bool
containsHole (Var _) = False
containsHole (Node (HoleF _)) = True
containsHole (Node sig) =
  bifoldr (\(ScopedAST _ body) acc -> containsHole body || acc)
          (\t acc -> containsHole t || acc) False (eraseNeutralTypes sig)

-- | The name of every hole in a term.
holeNamesOf :: forall ann n. AST Pattern (Sig ann) n -> [Maybe VarIdent]
holeNamesOf (Var _) = []
holeNamesOf (Node (HoleF mname)) = [mname]
holeNamesOf (Node sig) =
  bifoldr (\(ScopedAST _ body) acc -> holeNamesOf body <> acc)
          (\t acc -> holeNamesOf t <> acc) [] (eraseNeutralTypes sig)

-- | Is the term a hole, or an elimination headed by one (@? a@, @first ?@)?
-- Such a term has no shape of its own yet.
isHoleHeaded :: Core n -> Bool
isHoleHeaded = \case
  Hole _     -> True
  App _ f _  -> isHoleHeaded f
  First _ t  -> isHoleHeaded t
  Second _ t -> isHoleHeaded t
  _          -> False

-- | Syntactic equality of two core terms: conservative, as two α-equivalent
-- terms whose binders differ are not equal, and cheaper than α-equivalence.
eqCore :: Foil.Distinct n => Core n -> Core n -> Bool
eqCore l r = unsafeEqAST (stripAnnotations l) (stripAnnotations r)
