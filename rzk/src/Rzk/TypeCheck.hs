-- | The type checker.
--
-- This module is the public face of it: the judgements themselves live in
-- "Rzk.TypeCheck.Judgements", the module driver and the declarations in
-- "Rzk.TypeCheck.Decl", and so on. What used to be one 5,500-line module is now a
-- layer per concern, over the free-foil core in "Language.Rzk.Foil.Syntax".
module Rzk.TypeCheck (
  module Rzk.TypeCheck.Context,
  module Rzk.TypeCheck.Display,
  module Rzk.TypeCheck.Error,
  module Rzk.TypeCheck.Monad,
  module Rzk.TypeCheck.Eval,
  module Rzk.TypeCheck.Judgements,
  module Rzk.TypeCheck.Decl,
  module Rzk.TypeCheck.BinderTypes,
) where

import           Rzk.TypeCheck.BinderTypes
import           Rzk.TypeCheck.Context
import           Rzk.TypeCheck.Decl
import           Rzk.TypeCheck.Display
import           Rzk.TypeCheck.Error
import           Rzk.TypeCheck.Eval
import           Rzk.TypeCheck.Judgements
import           Rzk.TypeCheck.Monad
