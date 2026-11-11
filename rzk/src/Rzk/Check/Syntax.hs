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
  , pattern Match, pattern MatchArm, pattern Unit, pattern TypeUnit, pattern TypeAsc, pattern Ann
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
  , pattern RAnn
  , pattern RTypeRestricted
  , pattern RTypeModal
  , pattern RModApp
  , pattern RModExtract
  , pattern RLetMod
  , pattern RHole
    -- * Queries
  , freeNamesOf
  , stripAnnotations
  , substituteName
  , abstractName
  , containsHole
  , holeNamesOf
  , isHoleHeaded
  , eqCore
  ) where

import           Control.Monad.Foil                (NameBinder, S)
import qualified Control.Monad.Foil                as Foil
import           Control.Monad.Foil.Internal       (Substitution (..))
import           Control.Monad.Free.Foil           (AST (..), ScopedAST (..),
                                                    substitute,
                                                    unsafeEqAST)
import           Control.Monad.Free.Foil.Annotated (AnnSig (..))
import           Data.Bifoldable                   (bifoldMap, bifoldr)
import           Data.Bifunctor                    (bimap)
import qualified Data.IntMap                       as IntMap
import           Data.Bifunctor.TH                 (deriveBifoldable,
                                                    deriveBifunctor,
                                                    deriveBitraversable)
import           Data.ZipMatchK                    (ZipMatchFoldK, ZipMatchK (..),
                                                    zipMatchViaEq)
import           Data.ZipMatchK.TH                 (deriveZipMatchK)
import           Generics.Kind.TH                  (deriveGenericK)
import           Unsafe.Coerce                     (unsafeCoerce)

import           Language.Rzk.Foil.Names           (RzkPosition (..),
                                                    TModality (..), VarIdent)

-- * Binders

-- | A binder. A variable carries the name the user wrote, if any (@_@ has
-- none); the unit pattern binds one name that nothing refers to; a pair
-- pattern binds the names of both components, the right one in the scope
-- of the left.
data Pattern (n :: S) (l :: S) where
  PatternVar  :: Maybe VarIdent -> NameBinder n l -> Pattern n l
  PatternUnit :: NameBinder n l -> Pattern n l
  PatternPair :: Pattern n i -> Pattern i l -> Pattern n l

deriveGenericK ''Pattern
instance Foil.SinkableK Pattern
instance Foil.CoSinkable Pattern
instance Foil.HasNameBinders Pattern
instance Foil.UnifiablePattern Pattern

-- | The names a pattern binds, in binding order, with their display names.
-- The names of the left component are names of the right one's scope too:
-- a name sinks by coercion, as free-foil's own 'Foil.sink' does.
patternNames :: Pattern n l -> [(Maybe VarIdent, Foil.Name l)]
patternNames = \case
  PatternVar x b  -> [(x, Foil.nameOf b)]
  PatternUnit b   -> [(Nothing, Foil.nameOf b)]
  PatternPair l r -> unsafeCoerce (patternNames l) <> patternNames r

-- * The signature

-- | The parameter of a λ: its modality, its domain (a type, or the cube of
-- a shape) and, for a λ over a shape, the shape as a tope family over the
-- cube.
data Param term = Param
  { paramModality :: TModality
  , paramDomain   :: term
  , paramShape    :: Maybe term
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

-- | The signature of the core: every construct of the language. Scoped
-- positions bind a 'Pattern'.
data Sig scope term
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
  | TypeFunF TModality term (Maybe term) scope
    -- ^ a Π: the modality of the binder, its domain (a type, or the cube of
    -- a shape), the shape as a tope family over the cube, and the codomain
  | TypeShapeF TModality term (Maybe term)
    -- ^ the type of a tope family: @{t : cube | shape} → TOPE@, whose
    -- codomain is constant, so that the subtyping of families, which is
    -- covariant in the shape, can tell it from a Π
  | TypeSigmaF TModality term scope
  | TypeIdF term (Maybe term) term
  | AppF term term
  | LetF (Maybe term) term scope
  | LambdaF (Maybe (Param term)) scope
  | PairF term term
  | FirstF term
  | SecondF term
  | ReflF (Maybe (term, Maybe term))
  | IdJF term term term term term term
  | MatchF TModality term (Maybe term) [(VarIdent, term)]
    -- ^ a match on a @#data@ value: the modality, the scrutinee, the
    -- @into@ motive, and the branches, each an arm chain
  | MatchArmF scope
    -- ^ one binder of a match branch
  | UnitF
  | TypeUnitF
  | TypeAscF term term
  | AnnF term term
    -- ^ a neutral value with its type, which evaluation builds so that the
    -- type of a neutral is read off it rather than computed from its head.
    -- Evaluating it drops it, the readbacks of "Rzk.Check.Value" strip it
    -- ('stripAnnotations'), and the type takes no part in conversion or in
    -- the free names of a term.
  | TypeRestrictedF term [(term, term)]
  | TypeModalF TModality term
  | ModAppF TModality term
  | ModExtractF TModality TModality term
  | LetModF TModality TModality (Maybe term) (Maybe term) term scope
  | HoleF (Maybe VarIdent)
  deriving (Functor, Foldable, Traversable)

deriveBifunctor ''Sig
deriveBifoldable ''Sig
deriveBitraversable ''Sig
deriveGenericK ''Sig

deriveZipMatchK ''Sig

-- * Terms

-- | Where a term was written: the annotation of a raw term, a phantom in the
-- node's term parameter.
newtype SrcPos term = SrcPos RzkPosition
  deriving (Functor, Foldable, Traversable)

-- | Two terms written in different places are the same term.
instance ZipMatchK SrcPos where
  zipMatchWithK _ (SrcPos pos) _ = Just (SrcPos pos)

-- | A raw term: the surface syntax converted, every node carrying where it
-- was written. What the checker takes.
type Raw = AST Pattern (AnnSig SrcPos Sig)

type ScopedRaw = ScopedAST Pattern (AnnSig SrcPos Sig)

-- | A core term: what the checker produces, what closures hold, and what
-- readback gives.
type Core = AST Pattern Sig

type ScopedCore = ScopedAST Pattern Sig

-- | A raw node at a position.
rawNode :: RzkPosition -> Sig (ScopedRaw n) (Raw n) -> Raw n
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


-- | A raw term as a core term: the positions dropped.
stripPositions :: Raw n -> Core n
stripPositions (Var x) = Var x
stripPositions (Node (AnnSig _ sig)) = Node (bimap scoped stripPositions sig)
  where
    scoped (ScopedAST pat body) = ScopedAST pat (stripPositions body)

-- * Core patterns

pattern Universe :: Core n
pattern Universe = Node UniverseF
pattern UniverseCube :: Core n
pattern UniverseCube = Node UniverseCubeF
pattern UniverseTope :: Core n
pattern UniverseTope = Node UniverseTopeF
pattern CubeUnit :: Core n
pattern CubeUnit = Node CubeUnitF
pattern CubeUnitStar :: Core n
pattern CubeUnitStar = Node CubeUnitStarF
pattern Cube2 :: Core n
pattern Cube2 = Node Cube2F
pattern Cube2_0 :: Core n
pattern Cube2_0 = Node Cube2_0F
pattern Cube2_1 :: Core n
pattern Cube2_1 = Node Cube2_1F
pattern CubeI :: Core n
pattern CubeI = Node CubeIF
pattern CubeI_0 :: Core n
pattern CubeI_0 = Node CubeI_0F
pattern CubeI_1 :: Core n
pattern CubeI_1 = Node CubeI_1F
pattern CubeProduct :: Core n -> Core n -> Core n
pattern CubeProduct l r = Node (CubeProductF l r)
pattern CubeFlip :: Core n -> Core n
pattern CubeFlip t = Node (CubeFlipF t)
pattern CubeUnflip :: Core n -> Core n
pattern CubeUnflip t = Node (CubeUnflipF t)
pattern CubeSup :: Core n -> Core n -> Core n
pattern CubeSup l r = Node (CubeSupF l r)
pattern CubeInf :: Core n -> Core n -> Core n
pattern CubeInf l r = Node (CubeInfF l r)
pattern TopeTop :: Core n
pattern TopeTop = Node TopeTopF
pattern TopeBottom :: Core n
pattern TopeBottom = Node TopeBottomF
pattern TopeEQ :: Core n -> Core n -> Core n
pattern TopeEQ l r = Node (TopeEQF l r)
pattern TopeLEQ :: Core n -> Core n -> Core n
pattern TopeLEQ l r = Node (TopeLEQF l r)
pattern TopeAnd :: Core n -> Core n -> Core n
pattern TopeAnd l r = Node (TopeAndF l r)
pattern TopeOr :: Core n -> Core n -> Core n
pattern TopeOr l r = Node (TopeOrF l r)
pattern TopeInv :: Core n -> Core n
pattern TopeInv t = Node (TopeInvF t)
pattern TopeUninv :: Core n -> Core n
pattern TopeUninv t = Node (TopeUninvF t)
pattern RecBottom :: Core n
pattern RecBottom = Node RecBottomF
pattern RecOr :: [(Core n, Core n)] -> Core n
pattern RecOr rs = Node (RecOrF rs)
pattern TypeFun :: TModality -> Core n -> Maybe (Core n) -> ScopedCore n -> Core n
pattern TypeFun md dom mshape ret = Node (TypeFunF md dom mshape ret)
pattern TypeShape :: TModality -> Core n -> Maybe (Core n) -> Core n
pattern TypeShape md cube mshape = Node (TypeShapeF md cube mshape)
pattern TypeSigma :: TModality -> Core n -> ScopedCore n -> Core n
pattern TypeSigma md a b = Node (TypeSigmaF md a b)
pattern TypeId :: Core n -> Maybe (Core n) -> Core n -> Core n
pattern TypeId x mtA y = Node (TypeIdF x mtA y)
pattern App :: Core n -> Core n -> Core n
pattern App f x = Node (AppF f x)
pattern Let :: Maybe (Core n) -> Core n -> ScopedCore n -> Core n
pattern Let mty val body = Node (LetF mty val body)
pattern Lambda :: Maybe (Param (Core n)) -> ScopedCore n -> Core n
pattern Lambda mparam body = Node (LambdaF mparam body)
pattern Pair :: Core n -> Core n -> Core n
pattern Pair l r = Node (PairF l r)
pattern First :: Core n -> Core n
pattern First t = Node (FirstF t)
pattern Second :: Core n -> Core n
pattern Second t = Node (SecondF t)
pattern Refl :: Maybe (Core n, Maybe (Core n)) -> Core n
pattern Refl mx = Node (ReflF mx)
pattern IdJ :: Core n -> Core n -> Core n -> Core n -> Core n -> Core n -> Core n
pattern IdJ a b c d e f = Node (IdJF a b c d e f)
pattern Match :: TModality -> Core n -> Maybe (Core n) -> [(VarIdent, Core n)] -> Core n
pattern Match md scrut mmotive branches = Node (MatchF md scrut mmotive branches)
pattern MatchArm :: ScopedCore n -> Core n
pattern MatchArm arm = Node (MatchArmF arm)
pattern Unit :: Core n
pattern Unit = Node UnitF
pattern TypeUnit :: Core n
pattern TypeUnit = Node TypeUnitF
pattern TypeAsc :: Core n -> Core n -> Core n
pattern TypeAsc term ty = Node (TypeAscF term ty)

pattern Ann :: Core n -> Core n -> Core n
pattern Ann term ty = Node (AnnF term ty)
pattern TypeRestricted :: Core n -> [(Core n, Core n)] -> Core n
pattern TypeRestricted ty rs = Node (TypeRestrictedF ty rs)
pattern TypeModal :: TModality -> Core n -> Core n
pattern TypeModal md ty = Node (TypeModalF md ty)
pattern ModApp :: TModality -> Core n -> Core n
pattern ModApp md t = Node (ModAppF md t)
pattern ModExtract :: TModality -> TModality -> Core n -> Core n
pattern ModExtract app inn t = Node (ModExtractF app inn t)
pattern LetMod :: TModality -> TModality -> Maybe (Core n) -> Maybe (Core n) -> Core n -> ScopedCore n -> Core n
pattern LetMod app inn mty mmotive val body = Node (LetModF app inn mty mmotive val body)
pattern Hole :: Maybe VarIdent -> Core n
pattern Hole mname = Node (HoleF mname)

{-# COMPLETE Var, Universe, UniverseCube, UniverseTope, CubeUnit, CubeUnitStar,
  Cube2, Cube2_0, Cube2_1, CubeI, CubeI_0, CubeI_1, CubeProduct, CubeFlip,
  CubeUnflip, CubeSup, CubeInf, TopeTop, TopeBottom, TopeEQ, TopeLEQ, TopeAnd,
  TopeOr, TopeInv, TopeUninv, RecBottom, RecOr, TypeFun, TypeShape, TypeSigma, TypeId, App,
  Let, Lambda, Pair, First, Second, Refl, IdJ, Match, MatchArm, Unit, TypeUnit,
  TypeAsc, Ann, TypeRestricted, TypeModal, ModApp, ModExtract, LetMod, Hole #-}

-- * Raw patterns

-- | A raw node, matched without regard for where it was written.
pattern RawNode :: Sig (ScopedRaw n) (Raw n) -> Raw n
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
pattern RRecOr x1 <- RawNode (RecOrF x1)
pattern RTypeFun :: TModality -> Raw n -> Maybe (Raw n) -> ScopedRaw n -> Raw n
pattern RTypeFun x1 x2 x3 x4 <- RawNode (TypeFunF x1 x2 x3 x4)
pattern RTypeShape :: TModality -> Raw n -> Maybe (Raw n) -> Raw n
pattern RTypeShape x1 x2 x3 <- RawNode (TypeShapeF x1 x2 x3)
pattern RTypeSigma :: TModality -> Raw n -> ScopedRaw n -> Raw n
pattern RTypeSigma x1 x2 x3 <- RawNode (TypeSigmaF x1 x2 x3)
pattern RTypeId :: Raw n -> Maybe (Raw n) -> Raw n -> Raw n
pattern RTypeId x1 x2 x3 <- RawNode (TypeIdF x1 x2 x3)
pattern RApp :: Raw n -> Raw n -> Raw n
pattern RApp x1 x2 <- RawNode (AppF x1 x2)
pattern RLet :: Maybe (Raw n) -> Raw n -> ScopedRaw n -> Raw n
pattern RLet x1 x2 x3 <- RawNode (LetF x1 x2 x3)
pattern RLambda :: Maybe (Param (Raw n)) -> ScopedRaw n -> Raw n
pattern RLambda x1 x2 <- RawNode (LambdaF x1 x2)
pattern RPair :: Raw n -> Raw n -> Raw n
pattern RPair x1 x2 <- RawNode (PairF x1 x2)
pattern RFirst :: Raw n -> Raw n
pattern RFirst x1 <- RawNode (FirstF x1)
pattern RSecond :: Raw n -> Raw n
pattern RSecond x1 <- RawNode (SecondF x1)
pattern RRefl :: Maybe (Raw n, Maybe (Raw n)) -> Raw n
pattern RRefl x1 <- RawNode (ReflF x1)
pattern RIdJ :: Raw n -> Raw n -> Raw n -> Raw n -> Raw n -> Raw n -> Raw n
pattern RIdJ x1 x2 x3 x4 x5 x6 <- RawNode (IdJF x1 x2 x3 x4 x5 x6)
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
pattern RAnn :: Raw n -> Raw n -> Raw n
pattern RAnn x1 x2 <- RawNode (AnnF x1 x2)
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

{-# COMPLETE Var, RUniverse, RUniverseCube, RUniverseTope, RCubeUnit, RCubeUnitStar, RCube2, RCube2_0, RCube2_1, RCubeI, RCubeI_0, RCubeI_1, RCubeProduct, RCubeFlip, RCubeUnflip, RCubeSup, RCubeInf, RTopeTop, RTopeBottom, RTopeEQ, RTopeLEQ, RTopeAnd, RTopeOr, RTopeInv, RTopeUninv, RRecBottom, RRecOr, RTypeFun, RTypeShape, RTypeSigma, RTypeId, RApp, RLet, RLambda, RPair, RFirst, RSecond, RRefl, RIdJ, RMatch, RMatchArm, RUnit, RTypeUnit, RTypeAsc, RAnn, RTypeRestricted, RTypeModal, RModApp, RModExtract, RLetMod, RHole #-}

-- * Queries

-- | The free names of a term, in order of mention, with repeats. A name
-- bound on the way down is dropped, which makes the coercion back to the
-- outer scope right.
freeNamesOf :: Core n -> [Foil.Name n]
freeNamesOf (Var x) = [x]
freeNamesOf (Ann t _) = freeNamesOf t
freeNamesOf (Node sig) = bifoldMap scoped freeNamesOf sig
  where
    scoped :: ScopedCore n' -> [Foil.Name n']
    scoped (ScopedAST pat body) =
      unsafeCoerce
        [ x
        | x <- freeNamesOf body
        , Foil.nameId x `notElem` bound ]
      where
        bound = map (Foil.nameId . snd) (patternNames pat)

-- | A term without the annotations of its neutrals, which the library's
-- readback keeps. The types are not looked at.
stripAnnotations :: Core n -> Core n
stripAnnotations = \case
  Var x    -> Var x
  Ann t _  -> stripAnnotations t
  Node sig -> Node (bimap (\(ScopedAST pat body) -> ScopedAST pat (stripAnnotations body)) stripAnnotations sig)

-- | Replace a free name of a term by a term of the same scope.
substituteName :: Foil.Distinct n => Foil.Scope n -> Foil.Name n -> Core n -> Core n -> Core n
substituteName scope name value =
  substitute scope (UnsafeSubstitution (IntMap.singleton (Foil.nameId name) value))

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
containsHole :: Core n -> Bool
containsHole (Var _) = False
containsHole (Node (HoleF _)) = True
containsHole (Node sig) =
  bifoldr (\(ScopedAST _ body) acc -> containsHole body || acc)
          (\t acc -> containsHole t || acc) False sig

-- | The name of every hole in a term.
holeNamesOf :: Core n -> [Maybe VarIdent]
holeNamesOf (Var _) = []
holeNamesOf (Node (HoleF mname)) = [mname]
holeNamesOf (Node sig) =
  bifoldr (\(ScopedAST _ body) acc -> holeNamesOf body <> acc)
          (\t acc -> holeNamesOf t <> acc) [] sig

-- | Is the term a hole, or an elimination headed by one (@? a@, @first ?@)?
-- Such a term has no shape of its own yet.
isHoleHeaded :: Core n -> Bool
isHoleHeaded = \case
  Hole _     -> True
  App f _    -> isHoleHeaded f
  First t    -> isHoleHeaded t
  Second t   -> isHoleHeaded t
  _          -> False

-- | Syntactic equality of two core terms: conservative, as two α-equivalent
-- terms whose binders differ are not equal, and cheaper than α-equivalence.
eqCore :: Foil.Distinct n => Core n -> Core n -> Bool
eqCore = unsafeEqAST
