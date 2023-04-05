{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures -fno-warn-missing-signatures -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.Rzk.Free.Syntax where

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
    | TypeIdF term term term
    | TypeIdSimpleF term term
    | AppF term term
    | LambdaF (Maybe Rzk.VarIdent) (Maybe (term, Maybe scope)) scope
    | PairF term term
    | FirstF term
    | SecondF term
    | ReflF
    | ReflTermF term
    | ReflTermTypeF term term
    | IdJF term term term term term term
    | TypeAscF term term

type Term = FS TermF
type TermT = FS (TypedF TermF)

deriveBifunctor ''TermF
deriveBifoldable ''TermF
deriveBitraversable ''TermF

makePatternsAll ''TermF   -- declare all patterns using Template Haskell

type Term' = Term Rzk.VarIdent

freeVars :: Term a -> [a]
freeVars = foldMap pure

toTerm' :: Rzk.Term -> Term'
toTerm' = toTerm Pure

toScope :: Rzk.VarIdent -> (Rzk.VarIdent -> Term a) -> Rzk.Term -> Scope Term a
toScope x bvars = toTerm $ \z -> if x == z then Pure Z else S <$> bvars z

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
      Rzk.TypeId x tA y -> TypeId (go x) (go tA) (go y)
      Rzk.TypeIdSimple x y -> TypeIdSimple (go x) (go y)
      Rzk.App f x -> App (go f) (go x)
      Rzk.Pair l r -> Pair (go l) (go r)
      Rzk.First term -> First (go term)
      Rzk.Second term -> Second (go term)
      Rzk.Refl -> Refl
      Rzk.ReflTerm term -> ReflTerm (go term)
      Rzk.ReflTermType x tA -> ReflTermType (go x) (go tA)
      Rzk.IdJ a b c d e f -> IdJ (go a) (go b) (go c) (go d) (go e) (go f)
      Rzk.TypeAsc x t -> TypeAsc (go x) (go t)

      Rzk.TypeFun (Rzk.ParamVarType x arg) ret ->
        TypeFun (Just x) (go arg) Nothing (toScope x bvars ret)
      Rzk.TypeFun (Rzk.ParamVarShape (Rzk.PatternVar x) cube tope) ret ->
        TypeFun (Just x) (go cube) (Just (toScope x bvars tope)) (toScope x bvars ret)
      Rzk.TypeFun (Rzk.ParamVarShape Rzk.PatternWildcard cube tope) ret ->
        TypeFun Nothing (go cube) (Just (toTerm (fmap S <$> bvars) tope)) (toTerm (fmap S <$> bvars) ret)
      Rzk.TypeFun (Rzk.ParamWildcardType arg) ret ->
        TypeFun Nothing (go arg) Nothing (toTerm (fmap S <$> bvars) ret)
      Rzk.TypeFun (Rzk.ParamType arg) ret ->
        TypeFun Nothing (go arg) Nothing (toTerm (fmap S <$> bvars) ret)
      Rzk.TypeFun (Rzk.ParamVarShape Rzk.PatternPair{} _cube _tope) _ret ->
        error "pattern pairs are not supported"

      Rzk.TypeSigma (Rzk.PatternVar x) tA tB ->
        TypeSigma (Just x) (go tA) (toScope x bvars tB)
      Rzk.TypeSigma Rzk.PatternWildcard tA tB ->
        TypeSigma Nothing (go tA) (toTerm (fmap S <$> bvars) tB)
      Rzk.TypeSigma Rzk.PatternPair{} _tA _tB ->
        error "pattern pairs are not supported"

      Rzk.Lambda (Rzk.ParamPattern Rzk.PatternWildcard) body ->
        Lambda Nothing Nothing (toTerm (fmap S <$> bvars) body)
      Rzk.Lambda (Rzk.ParamPattern (Rzk.PatternVar x)) body ->
        Lambda (Just x) Nothing (toScope x bvars body)
      Rzk.Lambda (Rzk.ParamPatternType Rzk.PatternWildcard ty) body ->
        Lambda Nothing (Just (go ty, Nothing)) (toTerm (fmap S <$> bvars) body)
      Rzk.Lambda (Rzk.ParamPatternType (Rzk.PatternVar x) ty) body ->
        Lambda (Just x) (Just (go ty, Nothing)) (toScope x bvars body)
      Rzk.Lambda (Rzk.ParamPatternShape Rzk.PatternWildcard cube tope) body ->
        Lambda Nothing (Just (go cube, Just (toTerm (fmap S <$> bvars) tope))) (toTerm (fmap S <$> bvars) body)
      Rzk.Lambda (Rzk.ParamPatternShape (Rzk.PatternVar x) cube tope) body ->
        Lambda (Just x) (Just (go cube, Just (toScope x bvars tope))) (toScope x bvars body)
      Rzk.Lambda (Rzk.ParamPattern Rzk.PatternPair{}) _body ->
        error "pattern pairs are not supported"
      Rzk.Lambda (Rzk.ParamPatternType Rzk.PatternPair{} _ty) _body ->
        error "pattern pairs are not supported"
      Rzk.Lambda (Rzk.ParamPatternShape Rzk.PatternPair{} _cube _tope) _body ->
        error "pattern pairs are not supported"

      Rzk.Hole{} -> error "holes are not supported"

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
      TypeId l tA r -> Rzk.TypeId (go l) (go tA) (go r)
      TypeIdSimple l r -> Rzk.TypeIdSimple (go l) (go r)
      App l r -> Rzk.App (go l) (go r)

      Lambda z Nothing scope -> withFresh z $ \(x, xs) ->
        Rzk.Lambda (Rzk.ParamPattern (Rzk.PatternVar x)) (fromScope' x used xs scope)
      Lambda z (Just (ty, Nothing)) scope -> withFresh z $ \(x, xs) ->
        Rzk.Lambda (Rzk.ParamPatternType (Rzk.PatternVar x) (go ty)) (fromScope' x used xs scope)
      Lambda z (Just (cube, Just tope)) scope -> withFresh z $ \(x, xs) ->
        Rzk.Lambda (Rzk.ParamPatternShape (Rzk.PatternVar x) (go cube) (fromScope' x used xs tope)) (fromScope' x used xs scope)
      -- Lambda (Maybe (term, Maybe scope)) scope -> Rzk.Lambda (Maybe (term, Maybe scope)) scope

      Pair l r -> Rzk.Pair (go l) (go r)
      First term -> Rzk.First (go term)
      Second term -> Rzk.Second (go term)
      Refl -> Rzk.Refl
      ReflTerm t -> Rzk.ReflTerm (go t)
      ReflTermType t ty -> Rzk.ReflTermType (go t) (go ty)
      IdJ a b c d e f -> Rzk.IdJ (go a) (go b) (go c) (go d) (go e) (go f)
      TypeAsc l r -> Rzk.TypeAsc (go l) (go r)

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
