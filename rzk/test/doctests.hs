-- doctests/Main.hs

import           Test.DocTest

-- This test suite exists only to add dependencies
main :: IO ()
main = doctest ["src"]
