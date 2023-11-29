{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Language.Rzk.VSCode.Config (
  ServerConfig(..),
) where

import           Data.Aeson
import           Data.Default.Class (Default, def)

data ServerConfig = ServerConfig
  { formatEnabled          :: Bool
  } deriving Show

instance Default ServerConfig where
  def = ServerConfig
    { formatEnabled = True
    }

-- We need to derive the FromJSON instance manually in order to provide defaults
-- for absent fields.
instance FromJSON ServerConfig where
  -- Note: "configSection" in ServerDefinition already filters by the "rzk." prefix
  parseJSON = withObject "rzkSettings" $ \rzkSettings -> do
    formatSettings <- rzkSettings .: "format" -- TODO: how to make this optional?
    formatEnabled <- formatSettings .:? "enable" .!= formatEnabled def
    return ServerConfig { .. }

instance ToJSON ServerConfig where
  toJSON (ServerConfig { .. }) = object
      [ "format" .= object
          [ "enable" .= formatEnabled
          ]
      ]
