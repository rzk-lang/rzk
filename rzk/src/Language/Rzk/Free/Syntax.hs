{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures -fno-warn-missing-signatures -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Rzk.Free.Syntax where

import           Data.Bifunctor.TH
import           Data.Char           (chr, ord)
import           Data.Coerce
import           Data.Function       (on)
import           Data.Functor        (void)
import           Data.List           (intercalate, nub, (\\))
import           Data.Maybe          (fromMaybe)
import           Data.String

import           Free.Scoped
import           Free.Scoped.TH

-- FIXME: use proper mechanisms for warnings
import           Debug.Trace

import qualified Language.Rzk.Syntax as Rzk

data RzkPosition = RzkPosition
  { rzkFilePath :: Maybe FilePath
  , rzkLineCol  :: Rzk.BNFC'Position
  }

ppRzkPosition :: RzkPosition -> String
ppRzkPosition RzkPosition{..} = intercalate ":" $ concat
  [ [fromMaybe "<stdin>" rzkFilePath]
  , foldMap (\(row, col) -> map show [row, col]) rzkLineCol]

newtype VarIdent = VarIdent { getVarIdent :: Rzk.VarIdent' RzkPosition }

instance Show VarIdent where
  show = Rzk.printTree . getVarIdent

instance Eq VarIdent where
  (==) = (==) `on` (void . getVarIdent)

instance IsString VarIdent where
  fromString s = VarIdent (Rzk.VarIdent (RzkPosition Nothing Nothing) (fromString s))

ppVarIdentWithLocation :: VarIdent -> String
ppVarIdentWithLocation (VarIdent var@(Rzk.VarIdent pos _ident)) =
  Rzk.printTree var <> " (" <> ppRzkPosition pos <> ")"

varIdent :: Rzk.VarIdent -> VarIdent
varIdent = varIdentAt Nothing

varIdentAt :: Maybe FilePath -> Rzk.VarIdent -> VarIdent
varIdentAt path (Rzk.VarIdent pos ident) = VarIdent (Rzk.VarIdent (RzkPosition path pos) ident)

fromVarIdent :: VarIdent -> Rzk.VarIdent
fromVarIdent (VarIdent (Rzk.VarIdent (RzkPosition _file pos) ident)) = Rzk.VarIdent pos ident

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
    | TypeFunF (Maybe VarIdent) term (Maybe scope) scope
    | TypeSigmaF (Maybe VarIdent) term scope
    | TypeIdF term (Maybe term) term
    | AppF term term
    | LambdaF (Maybe VarIdent) (Maybe (term, Maybe scope)) scope
    | PairF term term
    | FirstF term
    | SecondF term
    | ReflF (Maybe (term, Maybe term))
    | IdJF term term term term term term
    | UnitF
    | TypeUnitF
    | TypeAscF term term
    | TypeRestrictedF term [(term, term)]
    deriving (Eq, Functor, Foldable, Traversable)
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
invalidateWHNF tt = case tt of
  -- type layer terms that should not be evaluated further
  LambdaT{} -> tt
  ReflT{} -> tt
  TypeFunT{} -> tt
  TypeSigmaT{} -> tt
  TypeIdT{} -> tt
  RecBottomT{} -> tt
  TypeUnitT{} -> tt
  UnitT{} -> tt

  _ -> (`transFS` tt) $ \(AnnF info f) ->
          AnnF info { infoWHNF = Nothing, infoNF = Nothing } f

substituteT :: TermT var -> Scope TermT var -> TermT var
substituteT x = substitute x . invalidateWHNF

type Term' = Term VarIdent
type TermT' = TermT VarIdent

freeVars :: Term a -> [a]
freeVars = foldMap pure

-- FIXME: should be cached in TypeInfo?
partialFreeVarsT :: TermT a -> [a]
partialFreeVarsT (Pure x)             = [x]
partialFreeVarsT UniverseT{}          = []
partialFreeVarsT term@(Free (AnnF info _)) =
  -- FIXME: check correctness (is it ok to use untyped here?)
  foldMap (freeVars . untyped) [term, infoType info]

-- FIXME: should be cached in TypeInfo?
freeVarsT :: Eq a => (a -> TermT a) -> TermT a -> [a]
freeVarsT typeOfVar t = go [] (partialFreeVarsT t)
  where
    go vars latest
      | null new = vars
      | otherwise =
          go (new <> vars)
             (foldMap (partialFreeVarsT . typeOfVar) new)
      where
        new = nub latest \\ vars

toTerm' :: Rzk.Term -> Term'
toTerm' = toTerm Pure

toScope :: VarIdent -> (VarIdent -> Term a) -> Rzk.Term -> Scope Term a
toScope x bvars = toTerm $ \z -> if x == z then Pure Z else S <$> bvars z

toScopePattern :: Rzk.Pattern -> (VarIdent -> Term a) -> Rzk.Term -> Scope Term a
toScopePattern pat bvars = toTerm $ \z ->
  case lookup z (bindings pat (Pure Z)) of
    Just t  -> t
    Nothing -> S <$> bvars z
  where
    bindings (Rzk.PatternUnit _loc)     _ = []
    bindings (Rzk.PatternVar _loc (Rzk.VarIdent _ "_")) _ = []
    bindings (Rzk.PatternVar _loc x)    t = [(varIdent x, t)]
    bindings (Rzk.PatternPair _loc l r) t = bindings l (First t) <> bindings r (Second t)
    bindings (Rzk.PatternTuple loc p1 p2 ps) t = bindings (desugarTuple loc (reverse ps) p2 p1) t

desugarTuple loc ps p2 p1 =
  case ps of
    []          -> Rzk.PatternPair loc p1 p2
    pLast : ps' -> Rzk.PatternPair loc (desugarTuple loc ps' p2 p1) pLast

toTerm :: (VarIdent -> Term a) -> Rzk.Term -> Term a
toTerm bvars = go
  where
    deprecated t t' = trace msg (go t')
      where
        msg = unlines
          [ "[DEPRECATED]:" <> ppBNFC'Position (Rzk.hasPosition t)
          , "the following notation is deprecated and will be removed from future version of rzk:"
          , "  " <> Rzk.printTree t
          , "instead consider using the following notation:"
          , "  " <> Rzk.printTree t'
          ]

    ppBNFC'Position Nothing = ""
    ppBNFC'Position (Just (line_, col)) = " at line " <> show line_ <> " column " <> show col

    lint orig suggestion = trace $ unlines
      [ "[HINT]:" <> ppBNFC'Position (Rzk.hasPosition orig) <> " consider replacing"
      , "  " <> Rzk.printTree orig
      , "with the following"
      , "  " <> Rzk.printTree suggestion
      ]

    go = \case
      -- Depracations
      t@(Rzk.RecOrDeprecated loc psi phi a_psi a_phi) -> deprecated t
        (Rzk.RecOr loc [Rzk.Restriction loc psi a_psi, Rzk.Restriction loc phi a_phi])
      t@(Rzk.TypeExtensionDeprecated loc shape type_) -> deprecated t
        (Rzk.TypeFun loc shape type_)
      t@(Rzk.TypeFun loc (Rzk.ParamTermTypeDeprecated loc' pat type_) ret) -> deprecated t
        (Rzk.TypeFun loc (Rzk.ParamTermType loc' (patternToTerm pat) type_) ret)
      t@(Rzk.TypeFun loc (Rzk.ParamVarShapeDeprecated loc' pat cube tope) ret) -> deprecated t
        (Rzk.TypeFun loc (Rzk.ParamTermShape loc' (patternToTerm pat) cube tope) ret)
      t@(Rzk.Lambda loc ((Rzk.ParamPatternShapeDeprecated loc' pat cube tope):params) body) -> deprecated t
        (Rzk.Lambda loc ((Rzk.ParamPatternShape loc' [pat] cube tope):params) body)
      -- ASCII versions
      Rzk.ASCII_CubeUnitStar loc -> go (Rzk.CubeUnitStar loc)
      Rzk.ASCII_Cube2_0 loc -> go (Rzk.Cube2_0 loc)
      Rzk.ASCII_Cube2_1 loc -> go (Rzk.Cube2_1 loc)
      Rzk.ASCII_TopeTop loc -> go (Rzk.TopeTop loc)
      Rzk.ASCII_TopeBottom loc -> go (Rzk.TopeBottom loc)
      Rzk.ASCII_TopeEQ loc l r -> go (Rzk.TopeEQ loc l r)
      Rzk.ASCII_TopeLEQ loc l r -> go (Rzk.TopeLEQ loc l r)
      Rzk.ASCII_TopeAnd loc l r -> go (Rzk.TopeAnd loc l r)
      Rzk.ASCII_TopeOr loc l r -> go (Rzk.TopeOr loc l r)

      Rzk.ASCII_TypeFun loc param ret -> go (Rzk.TypeFun loc param ret)
      Rzk.ASCII_TypeSigma loc pat ty ret -> go (Rzk.TypeSigma loc pat ty ret)
      Rzk.ASCII_TypeSigmaTuple loc p ps tN -> go (Rzk.TypeSigmaTuple loc p ps tN)
      Rzk.ASCII_Lambda loc pat ret -> go (Rzk.Lambda loc pat ret)
      Rzk.ASCII_TypeExtensionDeprecated loc shape type_ -> go (Rzk.TypeExtensionDeprecated loc shape type_)
      Rzk.ASCII_First loc term -> go (Rzk.First loc term)
      Rzk.ASCII_Second loc term -> go (Rzk.Second loc term)


      Rzk.Var _loc x -> bvars (varIdent x)
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
      Rzk.RecOr _loc rs -> RecOr $ flip map rs $ \case
        Rzk.Restriction _loc tope term       -> (go tope, go term)
        Rzk.ASCII_Restriction _loc tope term -> (go tope, go term)
      Rzk.TypeId _loc x tA y -> TypeId (go x) (Just (go tA)) (go y)
      Rzk.TypeIdSimple _loc x y -> TypeId (go x) Nothing (go y)
      Rzk.TypeUnit _loc -> TypeUnit
      Rzk.Unit _loc -> Unit
      Rzk.App _loc f x -> App (go f) (go x)
      Rzk.Pair _loc l r -> Pair (go l) (go r)
      Rzk.Tuple _loc p1 p2 (p:ps) -> go (Rzk.Tuple _loc (Rzk.Pair _loc p1 p2) p ps)
      Rzk.Tuple _loc p1 p2 [] -> go (Rzk.Pair _loc p1 p2)
      Rzk.First _loc term -> First (go term)
      Rzk.Second _loc term -> Second (go term)
      Rzk.Refl _loc -> Refl Nothing
      Rzk.ReflTerm _loc term -> Refl (Just (go term, Nothing))
      Rzk.ReflTermType _loc x tA -> Refl (Just (go x, Just (go tA)))
      Rzk.IdJ _loc a b c d e f -> IdJ (go a) (go b) (go c) (go d) (go e) (go f)
      Rzk.TypeAsc _loc x t -> TypeAsc (go x) (go t)

      Rzk.TypeFun _loc (Rzk.ParamTermType _ patTerm arg) ret ->
        let pat = unsafeTermToPattern patTerm
         in TypeFun (patternVar pat) (go arg) Nothing (toScopePattern pat bvars ret)
      t@(Rzk.TypeFun loc (Rzk.ParamTermShape loc' patTerm cube tope) ret) ->
        let lint' = case tope of
              Rzk.App _loc fun arg | void arg == void patTerm ->
                lint t (Rzk.TypeFun loc (Rzk.ParamTermType loc' patTerm fun) ret)
              _ -> id
            pat = unsafeTermToPattern patTerm
         in lint' $ TypeFun (patternVar pat) (go cube) (Just (toScopePattern pat bvars tope)) (toScopePattern pat bvars ret)
      Rzk.TypeFun _loc (Rzk.ParamType _ arg) ret ->
        TypeFun Nothing (go arg) Nothing (toTerm (fmap S <$> bvars) ret)

      Rzk.TypeSigma _loc pat tA tB ->
        TypeSigma (patternVar pat) (go tA) (toScopePattern pat bvars tB)

      Rzk.TypeSigmaTuple _loc (Rzk.SigmaParam _ patA tA) ((Rzk.SigmaParam _ patB tB) : ps) tN ->
        go (Rzk.TypeSigmaTuple _loc (Rzk.SigmaParam _loc patX tX) ps tN)
        where
          patX = Rzk.PatternPair _loc patA patB
          tX = Rzk.TypeSigma _loc patA tA tB
      Rzk.TypeSigmaTuple _loc (Rzk.SigmaParam _ pat tA) [] tB -> go (Rzk.TypeSigma _loc pat tA tB)

      Rzk.Lambda _loc [] body -> go body
      Rzk.Lambda _loc (Rzk.ParamPattern _ pat : params) body ->
        Lambda (patternVar pat) Nothing (toScopePattern pat bvars (Rzk.Lambda _loc params body))
      Rzk.Lambda _loc (Rzk.ParamPatternType _ [] _ty : params) body ->
        go (Rzk.Lambda _loc params body)
      Rzk.Lambda _loc (Rzk.ParamPatternType _ (pat:pats) ty : params) body ->
        Lambda (patternVar pat) (Just (go ty, Nothing))
          (toScopePattern pat bvars (Rzk.Lambda _loc (Rzk.ParamPatternType _loc pats ty : params) body))
      Rzk.Lambda _loc (Rzk.ParamPatternShape _ [] _cube _tope : params) body ->
        go (Rzk.Lambda _loc params body)
      t@(Rzk.Lambda _loc (Rzk.ParamPatternShape _loc' (pat:pats) cube tope : params) body) ->
        let lint' = case tope of
              Rzk.App _loc fun arg
                | null pats && void arg == void (patternToTerm pat) ->
                    lint t (Rzk.Lambda _loc (Rzk.ParamPatternType _loc' [pat] fun : params) body)
              _ -> id
         in lint' $ Lambda (patternVar pat) (Just (go cube, Just (toScopePattern pat bvars tope)))
              (toScopePattern pat bvars (Rzk.Lambda _loc (Rzk.ParamPatternShape _loc' pats cube tope : params) body))

      Rzk.TypeRestricted _loc ty rs ->
        TypeRestricted (go ty) $ flip map rs $ \case
          Rzk.Restriction _loc tope term       -> (go tope, go term)
          Rzk.ASCII_Restriction _loc tope term -> (go tope, go term)

      Rzk.Hole _loc _ident -> error "holes are not supported"

    patternVar (Rzk.PatternVar _loc (Rzk.VarIdent _ "_")) = Nothing
    patternVar (Rzk.PatternVar _loc x)                    = Just (varIdent x)
    patternVar _                                          = Nothing

patternToTerm :: Rzk.Pattern -> Rzk.Term
patternToTerm = ptt
  where
    ptt = \case
      Rzk.PatternVar loc x    -> Rzk.Var loc x
      Rzk.PatternPair loc l r -> Rzk.Pair loc (ptt l) (ptt r)
      Rzk.PatternUnit loc     -> Rzk.Unit loc
      Rzk.PatternTuple loc p1 p2 ps -> patternToTerm (desugarTuple loc (reverse ps) p2 p1)

unsafeTermToPattern :: Rzk.Term -> Rzk.Pattern
unsafeTermToPattern = ttp
  where
    ttp = \case
      Rzk.Unit loc                        -> Rzk.PatternUnit loc
      Rzk.Var loc x                       -> Rzk.PatternVar loc x
      Rzk.Pair loc l r                    -> Rzk.PatternPair loc (ttp l) (ttp r)
      term -> error ("ERROR: expected a pattern but got\n  " ++ Rzk.printTree term)

fromTerm' :: Term' -> Rzk.Term
fromTerm' t = fromTermWith' vars (defaultVarIdents \\ vars) t
  where vars = freeVars t

fromScope' :: VarIdent -> [VarIdent] -> [VarIdent] -> Scope Term VarIdent -> Rzk.Term
fromScope' x used xs = fromTermWith' (x : used) xs . (>>= f)
  where
    f Z     = Pure x
    f (S z) = Pure z

fromTermWith' :: [VarIdent] -> [VarIdent] -> Term' -> Rzk.Term
fromTermWith' used vars = go
  where
    withFresh Nothing f =
      case vars of
        x:xs -> f (x, xs)
        _    -> error "not enough fresh variables!"
    withFresh (Just z) f = f (z', filter (/= z') vars)    -- FIXME: very inefficient filter
      where
        z' = refreshVar used z

    loc = Nothing

    go :: Term' -> Rzk.Term
    go = \case
      Pure z -> Rzk.Var loc (fromVarIdent z)

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
        Rzk.TypeFun loc (Rzk.ParamTermType loc (Rzk.Var loc (fromVarIdent x)) (go arg)) (fromScope' x used xs ret)
      TypeFun z arg (Just tope) ret -> withFresh z $ \(x, xs) ->
        Rzk.TypeFun loc (Rzk.ParamTermShape loc (Rzk.Var loc (fromVarIdent x)) (go arg) (fromScope' x used xs tope)) (fromScope' x used xs ret)

      TypeSigma z a b -> withFresh z $ \(x, xs) ->
        Rzk.TypeSigma loc (Rzk.PatternVar loc (fromVarIdent x)) (go a) (fromScope' x used xs b)
      TypeId l (Just tA) r -> Rzk.TypeId loc (go l) (go tA) (go r)
      TypeId l Nothing r -> Rzk.TypeIdSimple loc (go l) (go r)
      App l r -> Rzk.App loc (go l) (go r)

      Lambda z Nothing scope -> withFresh z $ \(x, xs) ->
        Rzk.Lambda loc [Rzk.ParamPattern loc (Rzk.PatternVar loc (fromVarIdent x))] (fromScope' x used xs scope)
      Lambda z (Just (ty, Nothing)) scope -> withFresh z $ \(x, xs) ->
        Rzk.Lambda loc [Rzk.ParamPatternType loc [Rzk.PatternVar loc (fromVarIdent x)] (go ty)] (fromScope' x used xs scope)
      Lambda z (Just (cube, Just tope)) scope -> withFresh z $ \(x, xs) ->
        Rzk.Lambda loc [Rzk.ParamPatternShape loc [Rzk.PatternVar loc (fromVarIdent x)] (go cube) (fromScope' x used xs tope)] (fromScope' x used xs scope)
      -- Lambda (Maybe (term, Maybe scope)) scope -> Rzk.Lambda loc (Maybe (term, Maybe scope)) scope

      Pair l r -> Rzk.Pair loc (go l) (go r)
      First term -> Rzk.First loc (go term)
      Second term -> Rzk.Second loc (go term)
      TypeUnit -> Rzk.TypeUnit loc
      Unit -> Rzk.Unit loc
      Refl Nothing -> Rzk.Refl loc
      Refl (Just (t, Nothing)) -> Rzk.ReflTerm loc (go t)
      Refl (Just (t, Just ty)) -> Rzk.ReflTermType loc (go t) (go ty)
      IdJ a b c d e f -> Rzk.IdJ loc (go a) (go b) (go c) (go d) (go e) (go f)
      TypeAsc l r -> Rzk.TypeAsc loc (go l) (go r)
      TypeRestricted ty rs ->
        Rzk.TypeRestricted loc (go ty) (map (\(tope, term) -> (Rzk.Restriction loc (go tope) (go term))) rs)

defaultVarIdents :: [VarIdent]
defaultVarIdents =
  [ fromString name
  | n <- [1..]
  , let name = "x" <> map digitToSub (show n) ]
  where
    digitToSub c = chr ((ord c - ord '0') + ord '₀')

-- | Given a list of used variable names in the current context,
-- generate a unique fresh name based on a given one.
--
-- >>> print $ refreshVar ["x", "y", "x₁", "z"] "x"
-- x₂
refreshVar :: [VarIdent] -> VarIdent -> VarIdent
refreshVar vars x
  | x `elem` vars = refreshVar vars (incVarIdentIndex x)
  | otherwise     = x

incVarIdentIndex :: VarIdent -> VarIdent
incVarIdentIndex (VarIdent (Rzk.VarIdent loc token)) =
  VarIdent (Rzk.VarIdent loc (coerce incIndex token))

-- | Increment the subscript number at the end of the indentifier.
--
-- >>> putStrLn $ incIndex "x"
-- x₁
-- >>> putStrLn $ incIndex "x₁₉"
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
      fromRight (Right t)  = t

instance Show TermT' where
  show var@Pure{} = Rzk.printTree (fromTerm' (untyped var))
  show term@(Free (AnnF TypeInfo{..} _)) = termStr <> " : " <> typeStr
    where
      termStr = Rzk.printTree (fromTerm' (untyped term))
      typeStr = Rzk.printTree (fromTerm' (untyped infoType))
