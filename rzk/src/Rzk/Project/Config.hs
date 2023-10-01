{-# LANGUAGE OverloadedStrings #-}

module Rzk.Project.Config where

import           Data.Yaml (FromJSON (..), (.!=), (.:), (.:?))
import qualified Data.Yaml as Y

data ProjectConfig = ProjectConfig
  { include :: [FilePath]
  , exclude :: [FilePath]
  } deriving (Eq, Show)

instance FromJSON ProjectConfig where
  parseJSON (Y.Object v) =
    ProjectConfig   <$>
    v .:  "include" <*>
    v .:? "exclude" .!= []
  parseJSON _ = fail "Expected config value to be an object"
