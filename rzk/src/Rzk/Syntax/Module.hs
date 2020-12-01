module Rzk.Syntax.Module where

import           Rzk.Syntax.Decl

data Module var = Module
  { moduleDecls :: [Decl var]
  }
