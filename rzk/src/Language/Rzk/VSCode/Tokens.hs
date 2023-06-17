{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
module Language.Rzk.VSCode.Tokens where

import           Data.Aeson
import           Data.String  (IsString)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

-- | VS Code token.
data VSToken = VSToken
  { line           :: !Int
  , startCharacter :: !Int
  , length         :: !Int
  , tokenType      :: VSTokenType
  , tokenModifiers :: [VSTokenModifier]
  } deriving (Generic, ToJSON)

-- | VS Code token types. See https://code.visualstudio.com/api/language-extensions/semantic-highlight-guide#standard-token-types-and-modifiers.
newtype VSTokenType = VSTokenType Text
  deriving newtype (ToJSON, IsString)

-- | VS Code token modifiers. See https://code.visualstudio.com/api/language-extensions/semantic-highlight-guide#standard-token-types-and-modifiers.
newtype VSTokenModifier = VSTokenModifier Text
  deriving newtype (ToJSON, IsString)

