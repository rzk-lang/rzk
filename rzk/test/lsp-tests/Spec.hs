{-# LANGUAGE OverloadedStrings #-}

import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Test

-- TODO: maybe integrate with Hspec?
-- https://github.com/haskell/lsp/blob/master/lsp-test/README.md#unit-tests-with-hspec


main :: IO ()
main = runSession "rzk lsp" fullCaps "test/lsp-tests/data" $ do
  doc <- openDoc "example.rzk" "rzk"
  noDiagnostics
