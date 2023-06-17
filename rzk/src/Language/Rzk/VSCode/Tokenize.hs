{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Rzk.VSCode.Tokenize where

import           Language.Rzk.Syntax
import           Language.Rzk.Syntax.Print
import           Language.Rzk.VSCode.Tokens
import           Language.Rzk.VSCode.Tokens.Standard

tokenizeModule :: Module -> [VSToken]
tokenizeModule (Module _loc langDecl commands) = concat
  [ tokenizeLanguageDecl langDecl
  , foldMap tokenizeCommand commands
  ]

tokenizeLanguageDecl :: LanguageDecl -> [VSToken]
tokenizeLanguageDecl decl@(LanguageDecl loc lang) = concat
  [ mkToken loc decl vs_macro []
  , tokenizeLanguage lang ]

tokenizeLanguage :: Language -> [VSToken]
tokenizeLanguage lang@(Rzk1 loc) =
  mkToken loc lang vs_namespace []

tokenizeCommand :: Command -> [VSToken]
tokenizeCommand command = case command of
  CommandSetOption{}   -> []    -- FIXME: tokenize
  CommandUnsetOption{} -> []    -- FIXME: tokenize
  CommandCheck{}       -> []    -- FIXME: tokenize
  CommandCompute{}     -> []    -- FIXME: tokenize
  CommandComputeNF{}   -> []    -- FIXME: tokenize
  CommandComputeWHNF{} -> []    -- FIXME: tokenize

  CommandPostulate{}   -> []    -- FIXME: tokenize
  CommandDefine{}      -> []    -- FIXME: tokenize

  CommandAssume{}      -> []    -- FIXME: tokenize
  CommandSection{}     -> []    -- FIXME: tokenize


mkToken :: Print a => BNFC'Position -> a -> VSTokenType -> [VSTokenModifier] -> [VSToken]
mkToken Nothing _ _ _ = []
mkToken (Just (line, col)) x tokenType tokenModifiers =
  [ VSToken { startCharacter = col, length = Prelude.length (printTree x) , .. } ]
