{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

-- | Surface names, binders and modalities.
--
-- These are what the checker shows the user, and they are independent of how a
-- term represents its variables: a 'VarIdent' is a surface identifier (with the
-- position of its defining occurrence), and a 'Binder' records the /names/ a binder
-- introduces — including a pair pattern, which still binds exactly one variable
-- whose components are projections of it.
module Language.Rzk.Foil.Names where

import           Data.Char           (chr, ord)
import           Data.Coerce         (coerce)
import           Data.List           (intercalate)
import           Data.Maybe          (fromMaybe)
import           Data.String         (IsString (..))
import           Data.Set            (Set)
import qualified Data.Set            as Set
import qualified Data.Text           as T

import qualified Language.Rzk.Syntax as Rzk

-- | An identifier that is not in scope becomes a hole under a /marked/ name.
--
-- A free-foil term refers to a variable by name, and an unresolved identifier has
-- none — so the term cannot represent it. Elaboration marks it instead, and the
-- checker reports it when it reaches it, which is what keeps the error where the
-- identifier was used (inside the binders and topes it was written under) rather
-- than at the top of the declaration.
--
-- The marker cannot be mistaken for a hole the user wrote: the grammar forbids @#@
-- in an identifier.
markUnresolved :: VarIdent -> VarIdent
markUnresolved x = fromString ('#' : show x)

unmarkUnresolved :: VarIdent -> Maybe VarIdent
unmarkUnresolved x = case show x of
  '#' : name -> Just (fromString name)
  _          -> Nothing

-- | What a bound name is shown as: the display name standing for the variable
-- itself, and the (freshened) binder, which gives the pattern to print and the
-- component names to fold projections back to.
type Display = (VarIdent, Binder)

-- | The annotation on every node of a typed term: its type, plus its memoised weak
-- head and normal forms.
data TypeInfo term = TypeInfo
  { infoType :: term
  , infoWHNF :: Maybe term
  , infoNF   :: Maybe term
  } deriving (Functor, Foldable, Traversable)

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

-- | Identifiers are equal when they are spelled the same, whatever their source
-- positions.
--
-- Written out rather than @(==) \`on\` (void . getVarIdent)@, which allocated a
-- position-free copy of the whole syntax node and compared that: identifier
-- equality is on the hot path (every name lookup, every refreshing of a display
-- name, every match of two terms that mention a hole), and profiling put it at 6%
-- of the checker's time.
instance Eq VarIdent where
  VarIdent (Rzk.VarIdent _ x) == VarIdent (Rzk.VarIdent _ y) = x == y

-- | Identifiers are ordered by name, ignoring the source position, so that the
-- order agrees with 'Eq'. Only used to key identifiers in a set or a map.
instance Ord VarIdent where
  compare (VarIdent (Rzk.VarIdent _ x)) (VarIdent (Rzk.VarIdent _ y)) = compare x y

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


-- | A tuple pattern is sugar for nested pairs.
desugarTuple :: Rzk.BNFC'Position -> [Rzk.Pattern] -> Rzk.Pattern -> Rzk.Pattern -> Rzk.Pattern
desugarTuple loc ps p2 p1 =
  case ps of
    []          -> Rzk.PatternPair loc p1 p2
    pLast : ps' -> Rzk.PatternPair loc (desugarTuple loc ps' p2 p1) pLast


toBinder :: Rzk.Pattern -> Binder
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

-- | Split a binder term into the individual variables it names. A multi-variable
-- binder like @(x y : A)@ is parsed as the application spine @x y@; this returns
-- @[x, y]@ so each can become its own nested binder. A single binder term (a
-- variable, a pair pattern, …) is returned unchanged as a singleton.
flattenBinderApp :: Rzk.Term -> [Rzk.Term]
flattenBinderApp = \case
  Rzk.App _loc f x -> flattenBinderApp f ++ [x]
  t                -> [t]

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

defaultVarIdents :: [VarIdent]
defaultVarIdents =
  [ fromString name
  | n <- [1 :: Int ..]
  , let name = "x" <> map digitToSub (show n) ]
  where
    digitToSub c = chr ((ord c - ord '0') + ord '₀')

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text as T
-- >>> import qualified Data.Set as Set

-- | Given a list of used variable names in the current context,
-- generate a unique fresh name based on a given one.
--
-- >>> print $ refreshVar ["x", "y", "x₁", "z"] "x"
-- x₂
refreshVar :: [VarIdent] -> VarIdent -> VarIdent
refreshVar vars x
  | x `elem` vars = refreshVar vars (incVarIdentIndex x)
  | otherwise     = x

-- | Refresh a name against a /set/ of taken ones.
--
-- The list version above is O(taken) per call, and naming a whole context calls it
-- once per entry, which made reading the naming off a context with every top-level
-- definition of a project in it quadratic.
--
-- >>> print $ refreshVarIn (Set.fromList ["x", "y", "x₁", "z"]) "x"
-- x₂
refreshVarIn :: Set VarIdent -> VarIdent -> VarIdent
refreshVarIn taken x
  | x `Set.member` taken = refreshVarIn taken (incVarIdentIndex x)
  | otherwise            = x

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
    oldIndexN = read ('0' : map digitFromSub index) :: Int -- FIXME: read
    newIndex = map digitToSub (show (oldIndexN + 1))
