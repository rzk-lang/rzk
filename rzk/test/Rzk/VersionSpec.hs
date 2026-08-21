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

    it "renders flags in Cabal's notation" $ do
      let rendered = ppVersionInfo versionInfo
            { versionInfoFlags = [BuildFlag "lsp" FlagOn, BuildFlag "fancy" FlagOff] }
      rendered `shouldContain` "flags:      +lsp -fancy"

    it "omits the commit date line when there is no date" $
      ppVersionInfo versionInfo { versionInfoCommitDate = Nothing }
        `shouldNotContain` "committed:"

  describe "isoCommitDate" $ do
    it "normalises git's default format" $
      isoCommitDate "Fri Aug 21 21:14:42 2026 +0300" `shouldBe` "2026-08-21"

    it "pads a single-digit day" $
      isoCommitDate "Mon Jan 5 09:00:00 2026 -0500" `shouldBe` "2026-01-05"

    it "passes an unrecognised date through unchanged" $
      isoCommitDate "2026-08-21" `shouldBe` "2026-08-21"

  describe "the JSON encoding" $ do
    it "carries every field" $ do
      let json = BL8.unpack (encode versionInfo)
      mapM_ (shouldContain json)
        [ "\"version\":\"" <> versionString <> "\""
        , "\"compiler\":\"" <> versionInfoCompiler versionInfo <> "\""
        , "\"platform\":\"" <> versionInfoPlatform versionInfo <> "\""
        , "\"flags\":[{\"enabled\":true,\"name\":\"lsp\"}]"
        , "\"commit\":"
        , "\"commitDate\":"
        ]
