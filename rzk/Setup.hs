{-# LANGUAGE CPP #-}
-- Source: https://github.com/haskell/cabal/issues/6726#issuecomment-918663262

-- | Custom Setup that runs bnfc to generate the language sub-libraries
-- for the parsers included in Ogma.
module Main (main) where

import           Distribution.Simple         (defaultMainWithHooks,
                                              hookedPrograms, postConf,
                                              preBuild, simpleUserHooks)
import           Distribution.Simple.Program (Program (..), findProgramVersion,
                                              simpleProgram)
import           System.Process              (system)

-- | Run BNFC on the grammar before the actual build step.
--
-- All options for bnfc are hard-coded here.
main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
  { hookedPrograms = [ bnfcProgram ]
  , postConf       = \args flags packageDesc localBuildInfo -> do
#ifndef mingw32_HOST_OS
      _ <- system "bnfc -d -p Language.Rzk --generic --functor -o src/ grammar/Syntax.cf"
#endif
      postConf simpleUserHooks args flags packageDesc localBuildInfo
  }

-- | TODO: This should be in Cabal.Distribution.Simple.Program.Builtin.
bnfcProgram :: Program
bnfcProgram = (simpleProgram "bnfc")
  { programFindVersion = findProgramVersion "--version" id
  }
