{-|
Module      : Rzk.VersionSpec
Description : Tests for the reported build description
-}
{-# LANGUAGE OverloadedStrings #-}
module Rzk.VersionSpec where

import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.List                  (isPrefixOf)
import           Data.Version               (showVersion)
import           Test.Hspec

import           Rzk.Version

spec :: Spec
spec = do
  describe "versionString" $ do
    -- Tools scrape `rzk version` for the version and nothing else: the VS Code
    -- extension takes the whole of stdout, trims it, and passes it to semver.
    -- Details belong in `rzk version --full` instead.
    it "is the bare version, with no prefix or extra words" $
      versionString `shouldBe` showVersion version

  describe "ppVersionInfo" $ do
    it "leads with the version" $
      ("rzk " ++ versionString) `shouldSatisfy` (`isPrefixOf` ppVersionInfo versionInfo)

    it "reports every field" $ do
      let rendered = ppVersionInfo versionInfo
      rendered `shouldContain` versionInfoCompiler versionInfo
      rendered `shouldContain` versionInfoPlatform versionInfo

  describe "the JSON encoding" $ do
    it "carries every field" $ do
      let json = BL8.unpack (encode versionInfo)
      mapM_ (shouldContain json)
        [ "\"version\":\"" <> versionString <> "\""
        , "\"compiler\":\"" <> versionInfoCompiler versionInfo <> "\""
        , "\"platform\":\"" <> versionInfoPlatform versionInfo <> "\""
        , "\"lsp\":"
        , "\"commit\":"
        ]
