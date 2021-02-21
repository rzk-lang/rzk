{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rzk.Simple.Syntax.Var where

import           Data.Char     (chr, ord)
import           Data.Coerce   (coerce)
import           Data.Function (on)
import           Data.Hashable (Hashable (..))
import           Data.String   (IsString (..))
import           Data.Text     (Text)
import qualified Data.Text     as Text
import           Text.Trifecta (Spanned (..))

-- | Standard type of variables.
newtype Var = Var { getVar :: Text }
  deriving (Eq, Ord, Hashable, IsString)

newtype SpannedVar = SpannedVar { getSpannedVar :: Spanned Var }
  deriving (Show)

unSpanVar :: SpannedVar -> Var
unSpanVar (SpannedVar (var :~ _span)) = var

instance Eq SpannedVar where (==) = (==) `on` unSpanVar
instance Ord SpannedVar where compare = compare `on` unSpanVar
instance Hashable SpannedVar where hashWithSalt salt = hashWithSalt salt . unSpanVar

instance Show Var where show = Text.unpack . getVar

appendIndexText :: Int -> Text -> Text
appendIndexText n s = s <> index
  where
    digitToSub c = chr ((ord c - ord '0') + ord '₀')
    index = Text.pack (map digitToSub (show n))

-- | Increment index for a variable.
--
-- >>> incVarIndex "x"
-- x₁
incVarIndex :: Var -> Var
incVarIndex = coerce incIndexText

-- | Increment the subscript number at the end of the indentifier.
--
-- >>> Var (incIndexText "x")
-- x₁
-- >>> Var (incIndexText "x₁₉")
-- x₂₀
incIndexText :: Text -> Text
incIndexText s = name <> newIndex
  where
    digitsSub = "₀₁₂₃₄₅₆₇₈₉" :: String
    isDigitSub = (`elem` digitsSub)
    digitFromSub c = chr ((ord c - ord '₀') + ord '0')
    digitToSub c = chr ((ord c - ord '0') + ord '₀')
    (name, index) = Text.break isDigitSub s
    oldIndexN = read ('0' : map digitFromSub (Text.unpack index)) -- FIXME: read
    newIndex = Text.pack (map digitToSub (show (oldIndexN + 1)))
