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

import           Data.Bifunctor      (bimap)
import           Data.Bifunctor.TH
import           Data.Char           (chr, ord)
import           Data.Coerce
import           Data.Function       (on)
import           Data.Functor        (void)
import           Data.List           (intercalate, nub, (\\))
import           Data.Maybe          (fromMaybe)
import           Data.String
import qualified Data.Text           as T

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

-- | The display name of a hole from its surface token text. The token includes
-- the leading @?@; an anonymous hole (bare @?@) has no name.
holeName :: T.Text -> Maybe VarIdent
holeName tok =
  case T.drop 1 tok of
    name | T.null name -> Nothing
         | otherwise   -> Just (fromString (T.unpack name))

-- | The surface token text (including the leading @?@) for a hole name.
holeIdentToken :: Maybe VarIdent -> T.Text
holeIdentToken Nothing  = "?"
holeIdentToken (Just x) = "?" <> T.pack (show x)

-- | The name(s) a binder introduces. A binder may name a single (possibly
-- anonymous) variable, or destructure a pair\/tuple via a pattern. The pattern
-- structure is kept around purely so that goals, holes and error messages can
-- show the user's original names (e.g. @t@ and @s@ for @\\ (t , s) -> …@)
-- instead of projections of a fresh variable (e.g. @π₁ x₄@ and @π₂ x₄@).
--
-- Operationally a pair pattern still binds a /single/ variable; the components
-- are projections of it (see 'toScopePattern'). 'Binder' only records the names
-- so they can be restored when rendering.
data Binder
  = BinderVar (Maybe VarIdent)   -- ^ a single variable (@Nothing@ for @_@)
  | BinderPair Binder Binder     -- ^ a pair pattern @(l , r)@
  | BinderUnit                   -- ^ the unit pattern @unit@
  deriving (Eq)

-- | The single name of a binder, if it binds exactly one named variable.
-- A pair\/unit pattern has no single name, so this is 'Nothing' for them.
-- Used wherever the old @Maybe VarIdent@ binder name is still sufficient.
binderName :: Binder -> Maybe VarIdent
binderName (BinderVar mname) = mname
binderName _                 = Nothing

data TModality = Sharp | Flat | Op | Id deriving (Eq, Show)

toModality :: Rzk.Modality -> TModality
toModality Rzk.Sharp{}       = Sharp
toModality Rzk.ASCII_Sharp{} = Sharp
toModality Rzk.Flat{}        = Flat
toModality Rzk.ASCII_Flat{}  = Flat
toModality Rzk.Op{}          = Op
toModality Rzk.ASCII_Op{}    = Op
toModality Rzk.Id{}          = Id

modCompToMods :: Rzk.ModComp -> (TModality, TModality)
modCompToMods (Rzk.Single _ m)      = (Id, toModality m)
modCompToMods (Rzk.Comp _ ext inn)  = (toModality ext, toModality inn)

fromMod :: TModality -> Rzk.Modality
fromMod Sharp = Rzk.Sharp Nothing
fromMod Flat  = Rzk.Flat Nothing
fromMod Op    = Rzk.Op Nothing
fromMod Id    = Rzk.Id Nothing

modsToModComp :: TModality -> TModality -> Rzk.ModComp
modsToModComp Id inn  = Rzk.Single Nothing (fromMod inn)
modsToModComp ext inn = Rzk.Comp Nothing (fromMod ext) (fromMod inn)

data TermF scope term
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
    | LambdaF Binder TModality (Maybe (term, Maybe scope)) scope
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
invalidateWHNF = transFS $ \(AnnF info f) ->
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
      Rzk.CubeI _loc -> CubeI
      Rzk.CubeI_0 _loc -> CubeI_0
      Rzk.CubeI_1 _loc -> CubeI_1
      Rzk.ASCII_CubeI _loc -> CubeI
      Rzk.ASCII_CubeI_0 _loc -> CubeI_0
      Rzk.ASCII_CubeI_1 _loc -> CubeI_1
      Rzk.CubeProduct _loc l r -> CubeProduct (go l) (go r)
      Rzk.TopeTop _loc -> TopeTop
      Rzk.TopeBottom _loc -> TopeBottom
      Rzk.TopeEQ _loc l r -> TopeEQ (go l) (go r)
      Rzk.TopeLEQ _loc l r -> TopeLEQ (go l) (go r)
      Rzk.TopeAnd _loc l r -> TopeAnd (go l) (go r)
      Rzk.TopeOr _loc l r -> TopeOr (go l) (go r)
      Rzk.TopeInv _loc t -> TopeInv (go t)
      Rzk.TopeUninv _loc t -> TopeUninv (go t)
      Rzk.CubeFlip _loc t -> CubeFlip (go t)
      Rzk.CubeUnflip _loc t -> CubeUnflip (go t)
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
      Rzk.TypeFun _loc (Rzk.ParamTermModalType _loc' patTerm mc ty) ret ->
        let pat = unsafeTermToPattern patTerm
            md  = modalColonToTModality mc
        in TypeFun (toBinder pat) md (go ty) Nothing (toScopePattern pat bvars ret)
      Rzk.TypeFun _loc (Rzk.ParamTermModalShape _loc' patTerm mc cube tope) ret ->
        let pat = unsafeTermToPattern patTerm
            md  = modalColonToTModality mc
        in TypeFun (toBinder pat) md (go cube) (Just (toScopePattern pat bvars tope)) (toScopePattern pat bvars ret)
      Rzk.TypeFun _loc (Rzk.ParamTermType _ patTerm arg) ret ->
        let pat = unsafeTermToPattern patTerm
        in TypeFun (toBinder pat) Id (go arg) Nothing (toScopePattern pat bvars ret)
      t@(Rzk.TypeFun loc (Rzk.ParamTermShape loc' patTerm cube tope) ret) ->
        let lint' = case tope of
              Rzk.App _loc fun arg | void arg == void patTerm ->
                lint t (Rzk.TypeFun loc (Rzk.ParamTermType loc' patTerm fun) ret)
              _ -> id
            pat = unsafeTermToPattern patTerm
        in lint' $ TypeFun (toBinder pat) Id (go cube) (Just (toScopePattern pat bvars tope)) (toScopePattern pat bvars ret)
      Rzk.TypeFun _loc (Rzk.ParamType _ arg) ret ->
        TypeFun (BinderVar Nothing) Id (go arg) Nothing (toTerm (fmap S <$> bvars) ret)

      Rzk.TypeSigma _loc pat tA tB ->
        TypeSigma (toBinder pat) Id (go tA) (toScopePattern pat bvars tB)

      Rzk.TypeSigmaModal _loc pat mc ty body ->
        let md = modalColonToTModality mc
        in TypeSigma (toBinder pat) md (go ty) (toScopePattern pat bvars body)

      Rzk.TypeSigmaTuple _loc (Rzk.SigmaParamModal _loc' pat mc ty) rest body ->
        let md = modalColonToTModality mc
            tailSigma = case rest of
              []       -> body
              [sp]     -> sigmaParamToTypeSigma _loc sp body
              (sp:sps) -> Rzk.TypeSigmaTuple _loc sp sps body
        in TypeSigma (toBinder pat) md (go ty) (toScopePattern pat bvars tailSigma)
      Rzk.TypeSigmaTuple _loc (Rzk.SigmaParam _ patA tA) (mp@(Rzk.SigmaParamModal{}) : rest) body ->
        go (Rzk.TypeSigma _loc patA tA (case rest of
              []  -> sigmaParamToTypeSigma _loc mp body
              _   -> Rzk.TypeSigmaTuple _loc mp rest body))
      Rzk.TypeSigmaTuple _loc (Rzk.SigmaParam _ patA tA) ((Rzk.SigmaParam _ patB tB) : ps) tN ->
        go (Rzk.TypeSigmaTuple _loc (Rzk.SigmaParam _loc patX tX) ps tN)
        where
          patX = Rzk.PatternPair _loc patA patB
          tX = Rzk.TypeSigma _loc patA tA tB
      Rzk.TypeSigmaTuple _loc (Rzk.SigmaParam _ pat tA) [] tB -> go (Rzk.TypeSigma _loc pat tA tB)
      Rzk.Lambda _loc (Rzk.ParamPatternModalType _ [] _mc _ty : params) body ->
        go (Rzk.Lambda _loc params body)
      Rzk.Lambda _loc (Rzk.ParamPatternModalType loc' (pat:pats) mc ty : params) body ->
        let md = modalColonToTModality mc
        in Lambda (toBinder pat) md (Just (go ty, Nothing))
             (toScopePattern pat bvars (Rzk.Lambda _loc (if null pats then params else Rzk.ParamPatternModalType loc' pats mc ty : params) body))
      Rzk.Lambda _loc (Rzk.ParamPatternModalShape _ [] _mc _cube _tope : params) body ->
        go (Rzk.Lambda _loc params body)
      Rzk.Lambda _loc (Rzk.ParamPatternModalShape loc' (pat:pats) mc cube tope : params) body ->
        let md = modalColonToTModality mc
        in Lambda (toBinder pat) md (Just (go cube, Just (toScopePattern pat bvars tope)))
             (toScopePattern pat bvars (Rzk.Lambda _loc (if null pats then params else Rzk.ParamPatternModalShape loc' pats mc cube tope : params) body))
      Rzk.Lambda _loc [] body -> go body
      Rzk.Lambda _loc (Rzk.ParamPattern _ pat : params) body ->
        Lambda (toBinder pat) Id Nothing (toScopePattern pat bvars (Rzk.Lambda _loc params body))
      Rzk.Lambda _loc (Rzk.ParamPatternType _ [] _ty : params) body ->
        go (Rzk.Lambda _loc params body)
      Rzk.Lambda _loc (Rzk.ParamPatternType _ (pat:pats) ty : params) body ->
        Lambda (toBinder pat) Id (Just (go ty, Nothing))
          (toScopePattern pat bvars (Rzk.Lambda _loc (Rzk.ParamPatternType _loc pats ty : params) body))
      Rzk.Lambda _loc (Rzk.ParamPatternShape _ [] _cube _tope : params) body ->
        go (Rzk.Lambda _loc params body)
      t@(Rzk.Lambda _loc (Rzk.ParamPatternShape _loc' (pat:pats) cube tope : params) body) ->
        let lint' = case tope of
              Rzk.App _loc fun arg
                | null pats && void arg == void (patternToTerm pat) ->
                    lint t (Rzk.Lambda _loc (Rzk.ParamPatternType _loc' [pat] fun : params) body)
              _ -> id
         in lint' $ Lambda (toBinder pat) Id (Just (go cube, Just (toScopePattern pat bvars tope)))
              (toScopePattern pat bvars (Rzk.Lambda _loc (Rzk.ParamPatternShape _loc' pats cube tope : params) body))
      Rzk.Let _loc (Rzk.BindPattern _ pat) val expr ->
        Let (toBinder pat) Nothing (go val) (toScopePattern pat bvars expr)
      Rzk.Let _loc (Rzk.BindPatternType _ pat ty) val expr -> 
        Let (toBinder pat) (Just (go ty)) (go val) (toScopePattern pat bvars expr)
      Rzk.TypeRestricted _loc ty rs ->
        TypeRestricted (go ty) $ flip map rs $ \case
          Rzk.Restriction _loc tope term       -> (go tope, go term)
          Rzk.ASCII_Restriction _loc tope term -> (go tope, go term)

      Rzk.Hole _loc (Rzk.HoleIdent _ (Rzk.HoleIdentToken tok)) ->
        Hole (holeName tok)
      Rzk.ModApp _loc md body -> ModApp (toModality md) (go body)
      Rzk.ModType _loc md ty -> TypeModal (toModality md) (go ty)
      Rzk.ModExtract{} -> error "$extract$ is an internal term and cannot appear in source"
      Rzk.LetMod _loc comp (Rzk.BindPattern _ pat) val body ->
        let (ext, inn) = modCompToMods comp
        in LetMod (toBinder pat) ext inn Nothing (go val) (toScopePattern pat bvars body)
      Rzk.LetMod _loc comp (Rzk.BindPatternType _ pat ty) val body ->
        let (ext, inn) = modCompToMods comp
        in LetMod (toBinder pat) ext inn (Just (go ty)) (go val) (toScopePattern pat bvars body)

    -- Translate a surface pattern into a 'Binder', keeping the pair\/tuple
    -- structure so the component names can be restored when rendering.
    toBinder (Rzk.PatternVar _loc (Rzk.VarIdent _ "_")) = BinderVar Nothing
    toBinder (Rzk.PatternVar _loc x)                    = BinderVar (Just (varIdent x))
    toBinder (Rzk.PatternUnit _loc)                     = BinderUnit
    toBinder (Rzk.PatternPair _loc l r)                 = BinderPair (toBinder l) (toBinder r)
    toBinder (Rzk.PatternTuple loc p1 p2 ps)            = toBinder (desugarTuple loc (reverse ps) p2 p1)

patternToTerm :: Rzk.Pattern -> Rzk.Term
patternToTerm = ptt
  where
    ptt = \case
      Rzk.PatternVar loc x    -> Rzk.Var loc x
      Rzk.PatternPair loc l r -> Rzk.Pair loc (ptt l) (ptt r)
      Rzk.PatternUnit loc     -> Rzk.Unit loc
      Rzk.PatternTuple loc p1 p2 ps -> patternToTerm (desugarTuple loc (reverse ps) p2 p1)


modalColonModality :: Rzk.ModalColon -> Rzk.Modality
modalColonModality = \case
  Rzk.ModalColonFlat loc        -> Rzk.Flat loc
  Rzk.ModalColonSharp loc       -> Rzk.Sharp loc
  Rzk.ModalColonOp loc          -> Rzk.Op loc
  Rzk.ModalColonId loc          -> Rzk.Id loc
  Rzk.ASCII_ModalColonFlat loc  -> Rzk.Flat loc
  Rzk.ASCII_ModalColonSharp loc -> Rzk.Sharp loc
  Rzk.ASCII_ModalColonOp loc    -> Rzk.Op loc

modalColonToTModality :: Rzk.ModalColon -> TModality
modalColonToTModality = toModality . modalColonModality

fromTModalityToModalColon :: TModality -> Rzk.ModalColon
fromTModalityToModalColon = \case
  Sharp -> Rzk.ModalColonSharp Nothing
  Flat  -> Rzk.ModalColonFlat Nothing
  Op    -> Rzk.ModalColonOp Nothing
  Id    -> Rzk.ModalColonId Nothing

unsafeTermToPattern :: Rzk.Term -> Rzk.Pattern
unsafeTermToPattern = ttp
  where
    ttp = \case
      Rzk.Unit loc                        -> Rzk.PatternUnit loc
      Rzk.Var loc x                       -> Rzk.PatternVar loc x
      Rzk.Pair loc l r                    -> Rzk.PatternPair loc (ttp l) (ttp r)
      Rzk.Tuple loc t1 t2 ts              -> Rzk.PatternTuple loc (ttp t1) (ttp t2) (map ttp ts)
      term -> error ("ERROR: expected a pattern but got\n  " ++ Rzk.printTree term)

sigmaParamToTypeSigma :: Rzk.BNFC'Position -> Rzk.SigmaParam -> Rzk.Term -> Rzk.Term
sigmaParamToTypeSigma loc sp body = case sp of
  Rzk.SigmaParam      _ pat ty      -> Rzk.TypeSigma      loc pat ty body
  Rzk.SigmaParamModal _ pat mc ty  -> Rzk.TypeSigmaModal loc pat mc ty body

-- | A projection step: first (@π₁@) or second (@π₂@) component.
data Proj = PFst | PSnd
  deriving (Eq)

-- | Render a 'Binder' as a surface pattern (used to display the binder itself,
-- e.g. @(t , s)@). Anonymous variables become @_@.
binderToPattern :: Binder -> Rzk.Pattern
binderToPattern (BinderVar Nothing)  = Rzk.PatternVar Nothing (fromVarIdent "_")
binderToPattern (BinderVar (Just x)) = Rzk.PatternVar Nothing (fromVarIdent x)
binderToPattern (BinderPair l r)     = Rzk.PatternPair Nothing (binderToPattern l) (binderToPattern r)
binderToPattern BinderUnit           = Rzk.PatternUnit Nothing

-- | A term that prints as the binder's surface pattern, e.g. the point
-- @(t , s)@. Used to render a /bare/ occurrence of a pattern binder's variable
-- (one not under a projection, e.g. the point in a shape tope @Δ² (t , s)@) as
-- the pattern itself rather than the underlying single variable. A
-- single-variable binder yields that variable.
binderToTerm :: Binder -> Term VarIdent
binderToTerm (BinderVar Nothing)  = Pure (fromString "_")
binderToTerm (BinderVar (Just x)) = Pure x
binderToTerm (BinderPair l r)     = Pair (binderToTerm l) (binderToTerm r)
binderToTerm BinderUnit           = Unit

-- | A 'VarIdent' that prints as the binder's surface pattern, e.g. @(t , s)@.
-- Used to display a pattern binder in a hole's local context as the pattern
-- itself rather than as the underlying single variable.
binderDisplayName :: Binder -> VarIdent
binderDisplayName = fromString . Rzk.printTree . binderToPattern

-- | The named leaves of a binder, each paired with the projection path that
-- reaches it from the bound variable. For example @(t , (a , b))@ yields
-- @[([PFst], t), ([PSnd, PFst], a), ([PSnd, PSnd], b)]@.
binderPaths :: Binder -> [([Proj], VarIdent)]
binderPaths (BinderVar (Just x)) = [([], x)]
binderPaths (BinderVar Nothing)  = []
binderPaths BinderUnit           = []
binderPaths (BinderPair l r)     =
  [ (PFst : p, n) | (p, n) <- binderPaths l ] ++
  [ (PSnd : p, n) | (p, n) <- binderPaths r ]

-- | The names appearing in a binder.
binderLeaves :: Binder -> [VarIdent]
binderLeaves = map snd . binderPaths

-- | Does this binder destructure a pair\/tuple (as opposed to naming a single
-- variable or @_@)?
binderIsCompound :: Binder -> Bool
binderIsCompound BinderVar{} = False
binderIsCompound _           = True

-- | Refresh the named leaves of a binder so they avoid the given names (and one
-- another). Anonymous leaves and the unit pattern are left unchanged.
freshenBinderLeaves :: [VarIdent] -> Binder -> Binder
freshenBinderLeaves used = snd . go used
  where
    go u (BinderVar (Just x)) = let x' = refreshVar u x in (x' : u, BinderVar (Just x'))
    go u b@(BinderVar Nothing) = (u, b)
    go u BinderUnit            = (u, BinderUnit)
    go u (BinderPair l r)      =
      let (u1, l') = go u l
          (u2, r') = go u1 r
      in (u2, BinderPair l' r')

-- | Decompose a chain of projections applied to a variable into the projection
-- path /from the variable outwards/, matching 'binderPaths'. The outermost
-- projection is applied last, so it goes at the /end/ of the path: e.g.
-- @π₂ (π₁ x)@ (select @π₁@ first, then @π₂@) becomes @Just ([PFst, PSnd], x)@.
projChain :: Term a -> Maybe ([Proj], a)
projChain (First t)  = (\(ps, r) -> (ps ++ [PFst], r)) <$> projChain t
projChain (Second t) = (\(ps, r) -> (ps ++ [PSnd], r)) <$> projChain t
projChain (Pure x)   = Just ([], x)
projChain _          = Nothing

-- | Replace projection chains rooted at a pattern binder with the binder's
-- component name. Given a map from a (bound) variable to the named leaves of
-- its binder, every @π₁/π₂@ chain that reaches a named leaf is rewritten to
-- that name. Ordinary projections (of variables not bound by a pattern, or
-- chains that do not reach a named leaf) are left untouched.
foldBinderProjections :: Eq a => [(a, [([Proj], a)])] -> Term a -> Term a
foldBinderProjections m = go
  where
    go t
      | Just (ps, root) <- projChain t
      , not (null ps)
      , Just leaves <- lookup root m
      , Just nm <- lookup ps leaves
      = Pure nm
    go (Free f) = Free (bimap goScope go f)
    go (Pure x) = Pure x
    goScope = foldBinderProjections (map liftEntry m)
    liftEntry (k, leaves) = (S k, map (fmap S) leaves)

-- | Replace bare uses of a pattern binder's variable with the pattern term
-- (e.g. a whole point @(t , s)@ rather than the underlying single variable, in
-- a tope @Δ² (t , s)@). Given a map from each (already display-named) variable
-- to its binder, every free occurrence of a /compound/ binder's variable is
-- expanded to its pattern. Complements 'foldBinderProjections', which folds
-- /projections/ of such a variable; run this /after/ folding, so projections
-- have already become component names and only bare uses remain.
restorePatternVars :: [(VarIdent, Binder)] -> Term VarIdent -> Term VarIdent
restorePatternVars binders = (>>= expand)
  where
    expand v = case lookup v binders of
      Just b | binderIsCompound b -> binderToTerm b
      _                           -> Pure v

-- | Like 'projChain', but for type-annotated terms.
projChainT :: TermT a -> Maybe ([Proj], a)
projChainT (FirstT _ t)  = (\(ps, r) -> (ps ++ [PFst], r)) <$> projChainT t
projChainT (SecondT _ t) = (\(ps, r) -> (ps ++ [PSnd], r)) <$> projChainT t
projChainT (Pure x)      = Just ([], x)
projChainT _             = Nothing

-- | Like 'foldBinderProjections', but for type-annotated terms (e.g. those
-- embedded in type errors). The annotation of a folded leaf is dropped, which
-- is harmless: the result is only rendered, and a bare variable needs none.
foldBinderProjectionsT :: Eq a => [(a, [([Proj], a)])] -> TermT a -> TermT a
foldBinderProjectionsT m = go
  where
    go t
      | Just (ps, root) <- projChainT t
      , not (null ps)
      , Just leaves <- lookup root m
      , Just nm <- lookup ps leaves
      = Pure nm
    go (Free (AnnF info f)) = Free (AnnF (fmap go info) (bimap goScope go f))
    go (Pure x) = Pure x
    goScope = foldBinderProjectionsT (map liftEntry m)
    liftEntry (k, leaves) = (S k, map (fmap S) leaves)

fromTerm' :: Term' -> Rzk.Term
fromTerm' t = fromTermWith' vars (defaultVarIdents \\ vars) t
  where vars = freeVars t

fromScope' :: VarIdent -> [VarIdent] -> [VarIdent] -> Scope Term VarIdent -> Rzk.Term
fromScope' x used xs = fromTermWith' (x : used) xs . (>>= f)
  where
    f Z     = Pure x
    f (S z) = Pure z

-- | Like 'fromScope'', but additionally restores pattern-binder component names
-- inside the scope: projections of the bound variable @x@ are folded back to
-- the names recorded in @binder@ (e.g. @π₁ x@ becomes @t@). For a binder that
-- names a single variable this is exactly 'fromScope''.
fromScopeBinder' :: Binder -> VarIdent -> [VarIdent] -> [VarIdent] -> Scope Term VarIdent -> Rzk.Term
fromScopeBinder' binder x used xs scope =
  fromTermWith' (x : used) xs
    (restorePattern (foldBinderProjections [(x, binderPaths binder)] (scope >>= f)))
  where
    f Z     = Pure x
    f (S z) = Pure z
    -- After projection chains have been folded to their component names, a bare
    -- use of a pattern binder's variable (the whole point, e.g. in a shape tope
    -- @Δ² (t , s)@) still reads as the placeholder; show it as the pattern.
    restorePattern
      | binderIsCompound binder = (>>= \v -> if v == x then binderToTerm binder else Pure v)
      | otherwise               = id

fromTermWith' :: [VarIdent] -> [VarIdent] -> Term' -> Rzk.Term
fromTermWith' used vars = go
  where
    -- Refresh a binder's named leaves against the names already in use and draw
    -- fresh names for anonymous leaves from the remaining supply.
    freshenBinder _    stream (BinderVar Nothing) =
      case stream of
        x:xs -> (BinderVar (Just x), xs)
        _    -> error "not enough fresh variables!"
    freshenBinder used' stream (BinderVar (Just z)) =
      (BinderVar (Just z'), filter (/= z') stream)
      where z' = refreshVar used' z
    freshenBinder _    stream BinderUnit = (BinderUnit, stream)
    freshenBinder used' stream (BinderPair l r) =
      let (l', s1) = freshenBinder used' stream l
          (r', s2) = freshenBinder (used' ++ binderLeaves l') s1 r
      in (BinderPair l' r', s2)

    -- Pick fresh names for a binder. Yields the bound variable's display name
    -- (used as the De Bruijn placeholder), the freshened binder (for the
    -- displayed pattern and for projection folding), and the remaining supply.
    withFreshBinder z f =
      case binder' of
        BinderVar (Just x) -> f (x, binder', stream)
        _ -> case stream of
               x:xs -> f (x, binder', xs)
               _    -> error "not enough fresh variables!"
      where
        (binder', stream) = freshenBinder used vars z

    loc = Nothing

    goMod :: TModality -> Rzk.Modality  
    goMod Sharp = Rzk.Sharp loc
    goMod Flat = Rzk.Flat loc 
    goMod Op = Rzk.Op loc
    goMod Id = Rzk.Id loc 


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
      CubeI -> Rzk.CubeI loc
      CubeI_0 -> Rzk.CubeI_0 loc
      CubeI_1 -> Rzk.CubeI_1 loc
      CubeProduct l r -> Rzk.CubeProduct loc (go l) (go r)
      TopeTop -> Rzk.TopeTop loc
      TopeBottom -> Rzk.TopeBottom loc
      TopeEQ l r -> Rzk.TopeEQ loc (go l) (go r)
      TopeLEQ l r -> Rzk.TopeLEQ loc (go l) (go r)
      TopeAnd l r -> Rzk.TopeAnd loc (go l) (go r)
      TopeOr l r -> Rzk.TopeOr loc (go l) (go r)
      TopeInv t -> Rzk.TopeInv loc (go t)
      TopeUninv t -> Rzk.TopeUninv loc (go t)
      CubeFlip t -> Rzk.CubeFlip loc (go t)
      CubeUnflip t -> Rzk.CubeUnflip loc (go t)
      RecBottom -> Rzk.RecBottom loc
      RecOr rs -> Rzk.RecOr loc [ Rzk.Restriction loc (go tope) (go term) | (tope, term) <- rs ]

      Hole mname -> Rzk.Hole loc (Rzk.HoleIdent loc (Rzk.HoleIdentToken (holeIdentToken mname)))

      TypeFun z Id arg Nothing ret -> withFreshBinder z $ \(x, z', xs) ->
        Rzk.TypeFun loc (Rzk.ParamTermType loc (patternToTerm (binderToPattern z')) (go arg)) (fromScopeBinder' z' x used xs ret)
      TypeFun z Id arg (Just tope) ret -> withFreshBinder z $ \(x, z', xs) ->
        Rzk.TypeFun loc (Rzk.ParamTermShape loc (patternToTerm (binderToPattern z')) (go arg) (fromScopeBinder' z' x used xs tope)) (fromScopeBinder' z' x used xs ret)
      TypeFun z md arg Nothing ret -> withFreshBinder z $ \(x, z', xs) ->
        Rzk.TypeFun loc (Rzk.ParamTermModalType loc (patternToTerm (binderToPattern z')) (fromTModalityToModalColon md) (go arg)) (fromScopeBinder' z' x used xs ret)
      TypeFun z md arg (Just tope) ret -> withFreshBinder z $ \(x, z', xs) ->
        Rzk.TypeFun loc (Rzk.ParamTermModalShape loc (patternToTerm (binderToPattern z')) (fromTModalityToModalColon md) (go arg) (fromScopeBinder' z' x used xs tope)) (fromScopeBinder' z' x used xs ret)

      TypeSigma z Id a b -> withFreshBinder z $ \(x, z', xs) ->
        Rzk.TypeSigma loc (binderToPattern z') (go a) (fromScopeBinder' z' x used xs b)
      TypeSigma z md a b -> withFreshBinder z $ \(x, z', xs) ->
        Rzk.TypeSigmaModal loc (binderToPattern z') (fromTModalityToModalColon md) (go a) (fromScopeBinder' z' x used xs b)
      TypeId l (Just tA) r -> Rzk.TypeId loc (go l) (go tA) (go r)
      TypeId l Nothing r -> Rzk.TypeIdSimple loc (go l) (go r)
      App l r -> Rzk.App loc (go l) (go r)

      Lambda z Id Nothing scope -> withFreshBinder z $ \(x, z', xs) ->
        Rzk.Lambda loc [Rzk.ParamPattern loc (binderToPattern z')] (fromScopeBinder' z' x used xs scope)
      Lambda z Id (Just (ty, Nothing)) scope -> withFreshBinder z $ \(x, z', xs) ->
        Rzk.Lambda loc [Rzk.ParamPatternType loc [binderToPattern z'] (go ty)] (fromScopeBinder' z' x used xs scope)
      Lambda z Id (Just (cube, Just tope)) scope -> withFreshBinder z $ \(x, z', xs) ->
        Rzk.Lambda loc [Rzk.ParamPatternShape loc [binderToPattern z'] (go cube) (fromScopeBinder' z' x used xs tope)] (fromScopeBinder' z' x used xs scope)
      Lambda z md Nothing scope -> withFreshBinder z $ \(x, z', xs) ->
        Rzk.Lambda loc [Rzk.ParamPattern loc (binderToPattern z')] (fromScopeBinder' z' x used xs scope)
      Lambda z md (Just (ty, Nothing)) scope -> withFreshBinder z $ \(x, z', xs) ->
        Rzk.Lambda loc [Rzk.ParamPatternModalType loc [binderToPattern z'] (fromTModalityToModalColon md) (go ty)] (fromScopeBinder' z' x used xs scope)
      Lambda z md (Just (cube, Just tope)) scope -> withFreshBinder z $ \(x, z', xs) ->
        Rzk.Lambda loc [Rzk.ParamPatternModalShape loc [binderToPattern z'] (fromTModalityToModalColon md) (go cube) (fromScopeBinder' z' x used xs tope)] (fromScopeBinder' z' x used xs scope)
      -- Lambda (Maybe (term, Maybe scope)) scope -> Rzk.Lambda loc (Maybe (term, Maybe scope)) scope
      Let z Nothing val scope -> withFreshBinder z $ \(x, z', xs) ->
        Rzk.Let loc (Rzk.BindPattern loc (binderToPattern z')) (go val) (fromScopeBinder' z' x used xs scope)
      Let z (Just ty) val scope -> withFreshBinder z $ \(x, z', xs) ->
        Rzk.Let loc (Rzk.BindPatternType loc (binderToPattern z') (go ty)) (go val) (fromScopeBinder' z' x used xs scope)
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
      TypeModal m ty -> Rzk.ModType loc (goMod m) (go ty)
      ModApp m ty -> Rzk.ModApp loc (goMod m) (go ty)
      ModExtract ma mb t -> Rzk.ModExtract loc (Rzk.Comp loc (goMod ma) (goMod mb)) (go t)
      LetMod z ext inn Nothing val scope -> withFreshBinder z $ \(x, z', xs) ->
        Rzk.LetMod loc (modsToModComp ext inn)
          (Rzk.BindPattern loc (binderToPattern z'))
          (go val) (fromScopeBinder' z' x used xs scope)
      LetMod z ext inn (Just ty) val scope -> withFreshBinder z $ \(x, z', xs) ->
        Rzk.LetMod loc (modsToModComp ext inn)
          (Rzk.BindPatternType loc (binderToPattern z') (go ty))
          (go val) (fromScopeBinder' z' x used xs scope)


defaultVarIdents :: [VarIdent]
defaultVarIdents =
  [ fromString name
  | n <- [1..]
  , let name = "x" <> map digitToSub (show n) ]
  where
    digitToSub c = chr ((ord c - ord '0') + ord '₀')

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text as T

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
-- >>> putStrLn $ T.unpack $ incIndex "x"
-- x₁
-- >>> putStrLn $ T.unpack $ incIndex "x₁₉"
-- x₂₀
incIndex :: T.Text -> T.Text
incIndex s = T.pack $ name <> newIndex
  where
    digitsSub = "₀₁₂₃₄₅₆₇₈₉" :: String
    isDigitSub = (`elem` digitsSub)
    digitFromSub c = chr ((ord c - ord '₀') + ord '0')
    digitToSub c = chr ((ord c - ord '0') + ord '₀')
    (name, index) = break isDigitSub (T.unpack s)
    oldIndexN = read ('0' : map digitFromSub index) -- FIXME: read
    newIndex = map digitToSub (show (oldIndexN + 1))

instance Show Term' where
  show = Rzk.printTree . fromTerm'

instance IsString Term' where
  fromString = toTerm' . fromRight . Rzk.parseTerm . T.pack
    where
      fromRight (Left err) = error (T.unpack $ "Parse error: " <> err)
      fromRight (Right t)  = t

instance Show TermT' where
  show var@Pure{} = Rzk.printTree (fromTerm' (untyped var))
  show term@(Free (AnnF TypeInfo{..} _)) = termStr <> " : " <> typeStr
    where
      termStr = Rzk.printTree (fromTerm' (untyped term))
      typeStr = Rzk.printTree (fromTerm' (untyped infoType))
