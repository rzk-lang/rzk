module Rzk.Syntax.Decl where

import           Rzk.Syntax.Term

data Decl var = Decl
  { declName :: var
  , declType :: Term var
  , declBody :: Term var
  }
