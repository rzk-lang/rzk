module Rzk.Free.Syntax.Decl where

import           Rzk.Free.Syntax.Term

data Decl b a = Decl
  { declName :: a
  , declType :: Term b a
  , declBody :: Term b a
  }
