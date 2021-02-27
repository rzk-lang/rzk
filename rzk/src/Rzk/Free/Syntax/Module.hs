module Rzk.Free.Syntax.Module where

import           Rzk.Free.Syntax.Decl

data Module b a = Module
  { moduleDecls :: [Decl b a]
  }
