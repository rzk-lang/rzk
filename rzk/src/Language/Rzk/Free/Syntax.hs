{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures -fno-warn-missing-signatures -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances    #-}
{-# LANGUAGE FlexibleInstances    #-}
module Language.Rzk.Free.Syntax where

import Data.String
import Data.Char (chr, ord)
import Data.Coerce
import Data.List ((\\))
import Data.Bifunctor.TH

import Free.Scoped
import Free.Scoped.TH

import qualified Language.Rzk.Syntax as Rzk

data TermF scope term
    = UniverseF
    | UniverseCubeF
    | UniverseTopeF
    | CubeUnitF
    | CubeUnitStarF
    | Cube2F
    | Cube2_0F
    | Cube2_1F
    | CubeProductF term term
    | TopeTopF
    | TopeBottomF
    | TopeEQF term term
    | TopeLEQF term term
    | TopeAndF term term
    | TopeOrF term term
    | RecBottomF
    | RecOrF [(term, term)]
    | TypeFunF (Maybe Rzk.VarIdent) term (Maybe scope) scope
    | TypeSigmaF (Maybe Rzk.VarIdent) term scope
    | TypeIdF term (Maybe term) term
    | AppF term term
    | LambdaF (Maybe Rzk.VarIdent) (Maybe (term, Maybe scope)) scope
    | PairF term term
    | FirstF term
    | SecondF term
    | ReflF (Maybe (term, Maybe term))
    | IdJF term term term term term term
    | TypeAscF term term
    | TypeRestrictedF term (term, term)
    deriving (Eq)
deriveBifunctor ''TermF
deriveBifoldable ''TermF
deriveBitraversable ''TermF
makePatternsAll ''TermF   -- declare all patterns using Template Haskell

newtype Type term = Type { getType :: term }
  deriving (Eq, Functor, Foldable, Traversable)

data TypeInfo term = TypeInfo
  { infoType :: term
  , infoWHNF :: Maybe term
  , infoNF   :: Maybe term
  } deriving (Eq, Functor, Foldable, Traversable)

type Term = FS TermF
type TermT = FS (AnnF TypeInfo TermF)

termIsWHNF :: TermT var -> TermT var
termIsWHNF t@Pure{} = t
termIsWHNF (Free (AnnF info f)) = t'
  where
    t' = Free (AnnF info { infoWHNF = Just t' } f)

termIsNF :: TermT var -> TermT var
termIsNF t@Pure{} = t
termIsNF (Free (AnnF info f)) = t'
  where
    t' = Free (AnnF info { infoWHNF = Just t', infoNF = Just t' } f)

invalidateWHNF :: TermT var -> TermT var
invalidateWHNF = transFS $ \(AnnF info f) ->
  AnnF info { infoWHNF = Nothing, infoNF = Nothing } f

substituteT :: TermT var -> Scope TermT var -> TermT var
substituteT x = substitute x . invalidateWHNF

type Term' = Term Rzk.VarIdent
type TermT' = TermT Rzk.VarIdent

freeVars :: Term a -> [a]
freeVars = foldMap pure

toTerm' :: Rzk.Term -> Term'
toTerm' = toTerm Pure

toScope :: Rzk.VarIdent -> (Rzk.VarIdent -> Term a) -> Rzk.Term -> Scope Term a
toScope x bvars = toTerm $ \z -> if x == z then Pure Z else S <$> bvars z

toScopePattern :: Rzk.Pattern -> (Rzk.VarIdent -> Term a) -> Rzk.Term -> Scope Term a
toScopePattern pat bvars = toTerm $ \z ->
  case lookup z (bindings pat (Pure Z)) of
    Just t -> t
    Nothing -> S <$> bvars z
  where
    bindings Rzk.PatternWildcard   _ = []
    bindings (Rzk.PatternVar x)    t = [(x, t)]
    bindings (Rzk.PatternPair l r) t = bindings l (First t) <> bindings r (Second t)

toTerm :: (Rzk.VarIdent -> Term a) -> Rzk.Term -> Term a
toTerm bvars = go
  where
    go = \case
      Rzk.Var x -> bvars x
      Rzk.Universe -> Universe

      Rzk.UniverseCube -> UniverseCube
      Rzk.UniverseTope -> UniverseTope
      Rzk.CubeUnit -> CubeUnit
      Rzk.CubeUnitStar -> CubeUnitStar
      Rzk.Cube2 -> Cube2
      Rzk.Cube2_0 -> Cube2_0
      Rzk.Cube2_1 -> Cube2_1
      Rzk.CubeProduct l r -> CubeProduct (go l) (go r)
      Rzk.TopeTop -> TopeTop
      Rzk.TopeBottom -> TopeBottom
      Rzk.TopeEQ l r -> TopeEQ (go l) (go r)
      Rzk.TopeLEQ l r -> TopeLEQ (go l) (go r)
      Rzk.TopeAnd l r -> TopeAnd (go l) (go r)
      Rzk.TopeOr l r -> TopeOr (go l) (go r)
      Rzk.RecBottom -> RecBottom
      Rzk.RecOr rs -> RecOr [ (go tope, go term) | Rzk.Restriction tope term <- rs ]
      Rzk.TypeId x tA y -> TypeId (go x) (Just (go tA)) (go y)
      Rzk.TypeIdSimple x y -> TypeId (go x) Nothing (go y)
      Rzk.App f x -> App (go f) (go x)
      Rzk.Pair l r -> Pair (go l) (go r)
      Rzk.First term -> First (go term)
      Rzk.Second term -> Second (go term)
      Rzk.Refl -> Refl Nothing
      Rzk.ReflTerm term -> Refl (Just (go term, Nothing))
      Rzk.ReflTermType x tA -> Refl (Just (go x, Just (go tA)))
      Rzk.IdJ a b c d e f -> IdJ (go a) (go b) (go c) (go d) (go e) (go f)
      Rzk.TypeAsc x t -> TypeAsc (go x) (go t)

      Rzk.TypeFun (Rzk.ParamVarType x arg) ret ->
        TypeFun (Just x) (go arg) Nothing (toScope x bvars ret)
      Rzk.TypeFun (Rzk.ParamVarShape pat cube tope) ret ->
        TypeFun (patternVar pat) (go cube) (Just (toScopePattern pat bvars tope)) (toScopePattern pat bvars ret)
      Rzk.TypeFun (Rzk.ParamWildcardType arg) ret ->
        TypeFun Nothing (go arg) Nothing (toTerm (fmap S <$> bvars) ret)
      Rzk.TypeFun (Rzk.ParamType arg) ret ->
        TypeFun Nothing (go arg) Nothing (toTerm (fmap S <$> bvars) ret)

      Rzk.TypeSigma pat tA tB ->
        TypeSigma (patternVar pat) (go tA) (toScopePattern pat bvars tB)

      Rzk.Lambda [] body -> go body
      Rzk.Lambda (Rzk.ParamPattern pat : params) body ->
        Lambda (patternVar pat) Nothing (toScopePattern pat bvars (Rzk.Lambda params body))
      Rzk.Lambda (Rzk.ParamPatternType pat ty : params) body ->
        Lambda (patternVar pat) (Just (go ty, Nothing))
          (toScopePattern pat bvars (Rzk.Lambda params body))
      Rzk.Lambda (Rzk.ParamPatternShape pat cube tope : params) body ->
        Lambda (patternVar pat) (Just (go cube, Just (toScopePattern pat bvars tope)))
          (toScopePattern pat bvars (Rzk.Lambda params body))

      Rzk.TypeRestricted ty (Rzk.Restriction tope term) ->
        TypeRestricted (go ty) (go tope, go term)

      Rzk.Hole{} -> error "holes are not supported"


    patternVar (Rzk.PatternVar x) = Just x
    patternVar _ = Nothing

fromTerm' :: Term' -> Rzk.Term
fromTerm' t = fromTermWith' vars (defaultVarIdents \\ vars) t
  where vars = freeVars t

fromScope' :: Rzk.VarIdent -> [Rzk.VarIdent] -> [Rzk.VarIdent] -> Scope Term Rzk.VarIdent -> Rzk.Term
fromScope' x used xs = fromTermWith' (x : used) xs . (>>= f)
  where
    f Z = Pure x
    f (S z) = Pure z

fromTermWith' :: [Rzk.VarIdent] -> [Rzk.VarIdent] -> Term' -> Rzk.Term
fromTermWith' used vars = go
  where
    withFresh Nothing f =
      case vars of
        x:xs -> f (x, xs)
        _ -> error "not enough fresh variables!"
    withFresh (Just z) f = f (z', filter (/= z') vars)    -- FIXME: very inefficient filter
      where
        z' = refreshVar used z

    go = \case
      Pure z -> Rzk.Var z

      Universe -> Rzk.Universe
      UniverseCube -> Rzk.UniverseCube
      UniverseTope -> Rzk.UniverseTope
      CubeUnit -> Rzk.CubeUnit
      CubeUnitStar -> Rzk.CubeUnitStar
      Cube2 -> Rzk.Cube2
      Cube2_0 -> Rzk.Cube2_0
      Cube2_1 -> Rzk.Cube2_1
      CubeProduct l r -> Rzk.CubeProduct (go l) (go r)
      TopeTop -> Rzk.TopeTop
      TopeBottom -> Rzk.TopeBottom
      TopeEQ l r -> Rzk.TopeEQ (go l) (go r)
      TopeLEQ l r -> Rzk.TopeLEQ (go l) (go r)
      TopeAnd l r -> Rzk.TopeAnd (go l) (go r)
      TopeOr l r -> Rzk.TopeOr (go l) (go r)
      RecBottom -> Rzk.RecBottom
      RecOr rs -> Rzk.RecOr [ Rzk.Restriction (go tope) (go term) | (tope, term) <- rs ]

      TypeFun z arg Nothing ret -> withFresh z $ \(x, xs) ->
        Rzk.TypeFun (Rzk.ParamVarType x (go arg)) (fromScope' x used xs ret)
      TypeFun z arg (Just tope) ret -> withFresh z $ \(x, xs) ->
        Rzk.TypeFun (Rzk.ParamVarShape (Rzk.PatternVar x) (go arg) (fromScope' x used xs tope)) (fromScope' x used xs ret)

      TypeSigma z a b -> withFresh z $ \(x, xs) ->
        Rzk.TypeSigma (Rzk.PatternVar x) (go a) (fromScope' x used xs b)
      TypeId l (Just tA) r -> Rzk.TypeId (go l) (go tA) (go r)
      TypeId l Nothing r -> Rzk.TypeIdSimple (go l) (go r)
      App l r -> Rzk.App (go l) (go r)

      Lambda z Nothing scope -> withFresh z $ \(x, xs) ->
        Rzk.Lambda [Rzk.ParamPattern (Rzk.PatternVar x)] (fromScope' x used xs scope)
      Lambda z (Just (ty, Nothing)) scope -> withFresh z $ \(x, xs) ->
        Rzk.Lambda [Rzk.ParamPatternType (Rzk.PatternVar x) (go ty)] (fromScope' x used xs scope)
      Lambda z (Just (cube, Just tope)) scope -> withFresh z $ \(x, xs) ->
        Rzk.Lambda [Rzk.ParamPatternShape (Rzk.PatternVar x) (go cube) (fromScope' x used xs tope)] (fromScope' x used xs scope)
      -- Lambda (Maybe (term, Maybe scope)) scope -> Rzk.Lambda (Maybe (term, Maybe scope)) scope

      Pair l r -> Rzk.Pair (go l) (go r)
      First term -> Rzk.First (go term)
      Second term -> Rzk.Second (go term)
      Refl Nothing -> Rzk.Refl
      Refl (Just (t, Nothing)) -> Rzk.ReflTerm (go t)
      Refl (Just (t, Just ty)) -> Rzk.ReflTermType (go t) (go ty)
      IdJ a b c d e f -> Rzk.IdJ (go a) (go b) (go c) (go d) (go e) (go f)
      TypeAsc l r -> Rzk.TypeAsc (go l) (go r)
      TypeRestricted ty (tope, term) ->
        Rzk.TypeRestricted (go ty) (Rzk.Restriction (go tope) (go term))

defaultVarIdents :: [Rzk.VarIdent]
defaultVarIdents = coerce [ "x" <> map digitToSub (show n) | n <- [1..] ]
  where
    digitToSub c = chr ((ord c - ord '0') + ord '₀')

-- | Given a list of used variable names in the current context,
-- generate a unique fresh name based on a given one.
--
-- >>> refreshVar ["x", "y", "x₁", "z"] "x"
-- x₂
refreshVar :: [Rzk.VarIdent] -> Rzk.VarIdent -> Rzk.VarIdent
refreshVar vars x
  | x `elem` vars = refreshVar vars (coerce incIndex x)
  | otherwise     = x

-- | Increment the subscript number at the end of the indentifier.
--
-- >>> incIndex "x"
-- x₁
-- >>> incIndex "x₁₉"
-- x₂₀
incIndex :: String -> String
incIndex s = name <> newIndex
  where
    digitsSub = "₀₁₂₃₄₅₆₇₈₉" :: String
    isDigitSub = (`elem` digitsSub)
    digitFromSub c = chr ((ord c - ord '₀') + ord '0')
    digitToSub c = chr ((ord c - ord '0') + ord '₀')
    (name, index) = break isDigitSub s
    oldIndexN = read ('0' : map digitFromSub index) -- FIXME: read
    newIndex = map digitToSub (show (oldIndexN + 1))

instance Show Term' where
  show = Rzk.printTree . fromTerm'

instance IsString Term' where
  fromString = toTerm' . fromRight . Rzk.parseTerm
    where
      fromRight (Left err) = error ("Parse error: " <> err)
      fromRight (Right t) = t

instance Show TermT' where
  show var@Pure{} = Rzk.printTree (fromTerm' (untyped var))
  show term@(Free (AnnF TypeInfo{..} _)) = termStr <> " : " <> typeStr
    where
      termStr = Rzk.printTree (fromTerm' (untyped term))
      typeStr = Rzk.printTree (fromTerm' (untyped infoType))
