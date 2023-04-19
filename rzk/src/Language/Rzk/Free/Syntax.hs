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
    | TypeRestrictedF term [(term, term)]
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
    bindings (Rzk.PatternWildcard _loc)   _ = []
    bindings (Rzk.PatternVar _loc x)    t = [(x, t)]
    bindings (Rzk.PatternPair _loc l r) t = bindings l (First t) <> bindings r (Second t)

toTerm :: (Rzk.VarIdent -> Term a) -> Rzk.Term -> Term a
toTerm bvars = go
  where
    go = \case
      Rzk.Var _loc x -> bvars x
      Rzk.Universe _loc -> Universe

      Rzk.UniverseCube _loc -> UniverseCube
      Rzk.UniverseTope _loc -> UniverseTope
      Rzk.CubeUnit _loc -> CubeUnit
      Rzk.CubeUnitStar _loc -> CubeUnitStar
      Rzk.Cube2 _loc -> Cube2
      Rzk.Cube2_0 _loc -> Cube2_0
      Rzk.Cube2_1 _loc -> Cube2_1
      Rzk.CubeProduct _loc l r -> CubeProduct (go l) (go r)
      Rzk.TopeTop _loc -> TopeTop
      Rzk.TopeBottom _loc -> TopeBottom
      Rzk.TopeEQ _loc l r -> TopeEQ (go l) (go r)
      Rzk.TopeLEQ _loc l r -> TopeLEQ (go l) (go r)
      Rzk.TopeAnd _loc l r -> TopeAnd (go l) (go r)
      Rzk.TopeOr _loc l r -> TopeOr (go l) (go r)
      Rzk.RecBottom _loc -> RecBottom
      Rzk.RecOr _loc rs -> RecOr [ (go tope, go term) | Rzk.Restriction _loc tope term <- rs ]
      Rzk.TypeId _loc x tA y -> TypeId (go x) (Just (go tA)) (go y)
      Rzk.TypeIdSimple _loc x y -> TypeId (go x) Nothing (go y)
      Rzk.App _loc f x -> App (go f) (go x)
      Rzk.Pair _loc l r -> Pair (go l) (go r)
      Rzk.First _loc term -> First (go term)
      Rzk.Second _loc term -> Second (go term)
      Rzk.Refl _loc -> Refl Nothing
      Rzk.ReflTerm _loc term -> Refl (Just (go term, Nothing))
      Rzk.ReflTermType _loc x tA -> Refl (Just (go x, Just (go tA)))
      Rzk.IdJ _loc a b c d e f -> IdJ (go a) (go b) (go c) (go d) (go e) (go f)
      Rzk.TypeAsc _loc x t -> TypeAsc (go x) (go t)

      Rzk.TypeFun _loc (Rzk.ParamVarType _ pat arg) ret ->
        TypeFun (patternVar pat) (go arg) Nothing (toScopePattern pat bvars ret)
      Rzk.TypeFun _loc (Rzk.ParamVarShape _ pat cube tope) ret ->
        TypeFun (patternVar pat) (go cube) (Just (toScopePattern pat bvars tope)) (toScopePattern pat bvars ret)
      Rzk.TypeFun _loc (Rzk.ParamWildcardType _ arg) ret ->
        TypeFun Nothing (go arg) Nothing (toTerm (fmap S <$> bvars) ret)
      Rzk.TypeFun _loc (Rzk.ParamType _ arg) ret ->
        TypeFun Nothing (go arg) Nothing (toTerm (fmap S <$> bvars) ret)

      Rzk.TypeSigma _loc pat tA tB ->
        TypeSigma (patternVar pat) (go tA) (toScopePattern pat bvars tB)

      Rzk.Lambda _loc [] body -> go body
      Rzk.Lambda _loc (Rzk.ParamPattern _ pat : params) body ->
        Lambda (patternVar pat) Nothing (toScopePattern pat bvars (Rzk.Lambda _loc params body))
      Rzk.Lambda _loc (Rzk.ParamPatternType _ [] _ty : params) body ->
        go (Rzk.Lambda _loc params body)
      Rzk.Lambda _loc (Rzk.ParamPatternType _ (pat:pats) ty : params) body ->
        Lambda (patternVar pat) (Just (go ty, Nothing))
          (toScopePattern pat bvars (Rzk.Lambda _loc (Rzk.ParamPatternType _loc pats ty : params) body))
      Rzk.Lambda _loc (Rzk.ParamPatternShape _ pat cube tope : params) body ->
        Lambda (patternVar pat) (Just (go cube, Just (toScopePattern pat bvars tope)))
          (toScopePattern pat bvars (Rzk.Lambda _loc params body))

      Rzk.TypeRestricted _loc ty rs ->
        TypeRestricted (go ty) (map (\(Rzk.Restriction _loc tope term) -> (go tope, go term)) rs)

      Rzk.Hole _loc _ident -> error "holes are not supported"


    patternVar (Rzk.PatternVar _loc x) = Just x
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

    loc = Nothing
    go = \case
      Pure z -> Rzk.Var loc z

      Universe -> Rzk.Universe loc
      UniverseCube -> Rzk.UniverseCube loc
      UniverseTope -> Rzk.UniverseTope loc
      CubeUnit -> Rzk.CubeUnit loc
      CubeUnitStar -> Rzk.CubeUnitStar loc
      Cube2 -> Rzk.Cube2 loc
      Cube2_0 -> Rzk.Cube2_0 loc
      Cube2_1 -> Rzk.Cube2_1 loc
      CubeProduct l r -> Rzk.CubeProduct loc (go l) (go r)
      TopeTop -> Rzk.TopeTop loc
      TopeBottom -> Rzk.TopeBottom loc
      TopeEQ l r -> Rzk.TopeEQ loc (go l) (go r)
      TopeLEQ l r -> Rzk.TopeLEQ loc (go l) (go r)
      TopeAnd l r -> Rzk.TopeAnd loc (go l) (go r)
      TopeOr l r -> Rzk.TopeOr loc (go l) (go r)
      RecBottom -> Rzk.RecBottom loc
      RecOr rs -> Rzk.RecOr loc [ Rzk.Restriction loc (go tope) (go term) | (tope, term) <- rs ]

      TypeFun z arg Nothing ret -> withFresh z $ \(x, xs) ->
        Rzk.TypeFun loc (Rzk.ParamVarType loc (Rzk.PatternVar loc x) (go arg)) (fromScope' x used xs ret)
      TypeFun z arg (Just tope) ret -> withFresh z $ \(x, xs) ->
        Rzk.TypeFun loc (Rzk.ParamVarShape loc (Rzk.PatternVar loc x) (go arg) (fromScope' x used xs tope)) (fromScope' x used xs ret)

      TypeSigma z a b -> withFresh z $ \(x, xs) ->
        Rzk.TypeSigma loc (Rzk.PatternVar loc x) (go a) (fromScope' x used xs b)
      TypeId l (Just tA) r -> Rzk.TypeId loc (go l) (go tA) (go r)
      TypeId l Nothing r -> Rzk.TypeIdSimple loc (go l) (go r)
      App l r -> Rzk.App loc (go l) (go r)

      Lambda z Nothing scope -> withFresh z $ \(x, xs) ->
        Rzk.Lambda loc [Rzk.ParamPattern loc (Rzk.PatternVar loc x)] (fromScope' x used xs scope)
      Lambda z (Just (ty, Nothing)) scope -> withFresh z $ \(x, xs) ->
        Rzk.Lambda loc [Rzk.ParamPatternType loc [Rzk.PatternVar loc x] (go ty)] (fromScope' x used xs scope)
      Lambda z (Just (cube, Just tope)) scope -> withFresh z $ \(x, xs) ->
        Rzk.Lambda loc [Rzk.ParamPatternShape loc (Rzk.PatternVar loc x) (go cube) (fromScope' x used xs tope)] (fromScope' x used xs scope)
      -- Lambda (Maybe (term, Maybe scope)) scope -> Rzk.Lambda loc (Maybe (term, Maybe scope)) scope

      Pair l r -> Rzk.Pair loc (go l) (go r)
      First term -> Rzk.First loc (go term)
      Second term -> Rzk.Second loc (go term)
      Refl Nothing -> Rzk.Refl loc
      Refl (Just (t, Nothing)) -> Rzk.ReflTerm loc (go t)
      Refl (Just (t, Just ty)) -> Rzk.ReflTermType loc (go t) (go ty)
      IdJ a b c d e f -> Rzk.IdJ loc (go a) (go b) (go c) (go d) (go e) (go f)
      TypeAsc l r -> Rzk.TypeAsc loc (go l) (go r)
      TypeRestricted ty rs ->
        Rzk.TypeRestricted loc (go ty) (map (\(tope, term) -> (Rzk.Restriction loc (go tope) (go term))) rs)

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
