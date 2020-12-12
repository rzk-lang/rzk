{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rzk.Syntax.Var where

import           Data.Char   (chr, ord)
import           Data.Coerce (coerce)
import           Data.String (IsString (..))
import           Data.Text   (Text)
import qualified Data.Text   as Text

-- | Standard type of variables.
newtype Var = Var { getVar :: Text }
  deriving (Eq, IsString)

instance Show Var where show = Text.unpack . getVar

-- | 'Enum' instance allows to easily increment index for a variable:
--
-- >>> succ "x" :: Var
-- x₁
--
-- This can be useful in REPL when specifying a sequence of variables:
--
-- >>> take 10 (iterate succ "x") :: [Var]
-- [x,x₁,x₂,x₃,x₄,x₅,x₆,x₇,x₈,x₉]
instance Enum Var where
  succ = incVarIndex
  toEnum = error "toEnum is not implemented for Var"
  fromEnum = error "toEnum is not implemented for Var"

-- | Increment index for a variable.
--
-- >>> incVarIndex "x"
-- x₁
incVarIndex :: Var -> Var
incVarIndex = coerce incIndexText

-- | Given a list of used variable names in the current context,
-- generate a unique fresh name based on a given one.
--
-- >>> refreshVar ["x", "y", "x₁", "z"] "x" :: Var
-- x₂
refreshVar :: (Eq var, Enum var) => [var] -> var -> var
refreshVar vars x
  | x `elem` vars = refreshVar vars (succ x)
  | otherwise     = x

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
