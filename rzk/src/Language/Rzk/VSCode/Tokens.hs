{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
module Language.Rzk.VSCode.Tokens where

import           Data.Aeson
import           Data.String               (IsString)
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)

import           Language.Rzk.Syntax
import           Language.Rzk.Syntax.Print

tokenizeModule :: Module -> [VSToken]
tokenizeModule (Module _loc langDecl commands) = concat
  [ tokenizeLanguageDecl langDecl
  , foldMap tokenizeCommand commands
  ]

tokenizeLanguageDecl :: LanguageDecl -> [VSToken]
tokenizeLanguageDecl decl@(LanguageDecl loc lang) = concat
  [ mkToken loc decl VS_macro []
  , tokenizeLanguage lang ]

tokenizeLanguage :: Language -> [VSToken]
tokenizeLanguage lang@(Rzk1 loc) =
  mkToken loc lang VS_namespace []

tokenizeCommand :: Command -> [VSToken]
tokenizeCommand _ = []

mkToken :: Print a => BNFC'Position -> a -> VSTokenType -> [VSTokenModifier] -> [VSToken]
mkToken Nothing _ _ _ = []
mkToken (Just (line, col)) x tokenType tokenModifiers =
  [ VSToken { startCharacter = col, length = Prelude.length (printTree x) , .. } ]

data VSToken = VSToken
  { line           :: !Int
  , startCharacter :: !Int
  , length         :: !Int
  , tokenType      :: VSTokenType
  , tokenModifiers :: [VSTokenModifier]
  } deriving (Generic, ToJSON)

-- * VS Code token types

newtype VSTokenType = VSTokenType Text
  deriving newtype (ToJSON, IsString)

-- ** Standard token types

pattern VS_namespace     = VSTokenType "namespace" -- ^ For identifiers that declare or reference a namespace, module, or package.
pattern VS_class         = VSTokenType "class" -- ^ For identifiers that declare or reference a class type.
pattern VS_enum          = VSTokenType "enum" -- ^ For identifiers that declare or reference an enumeration type.
pattern VS_interface     = VSTokenType "interface" -- ^ For identifiers that declare or reference an interface type.
pattern VS_struct        = VSTokenType "struct" -- ^ For identifiers that declare or reference a struct type.
pattern VS_typeParameter = VSTokenType "typeParameter" -- ^ For identifiers that declare or reference a type parameter.
pattern VS_type          = VSTokenType "type" -- ^ For identifiers that declare or reference a type that is not covered above.
pattern VS_parameter     = VSTokenType "parameter" -- ^ For identifiers that declare or reference a function or method parameters.
pattern VS_variable      = VSTokenType "variable" -- ^ For identifiers that declare or reference a local or global variable.
pattern VS_property      = VSTokenType "property" -- ^ For identifiers that declare or reference a member property, member field, or member variable.
pattern VS_enumMember    = VSTokenType "enumMember" -- ^ For identifiers that declare or reference an enumeration property, constant, or member.
pattern VS_decorator     = VSTokenType "decorator" -- ^ For identifiers that declare or reference decorators and annotations.
pattern VS_event         = VSTokenType "event" -- ^ For identifiers that declare an event property.
pattern VS_function      = VSTokenType "function" -- ^ For identifiers that declare a function.
pattern VS_method        = VSTokenType "method" -- ^ For identifiers that declare a member function or method.
pattern VS_macro         = VSTokenType "macro" -- ^ For identifiers that declare a macro.
pattern VS_label         = VSTokenType "label" -- ^ For identifiers that declare a label.
pattern VS_comment       = VSTokenType "comment" -- ^ For tokens that represent a comment.
pattern VS_string        = VSTokenType "string" -- ^ For tokens that represent a string literal.
pattern VS_keyword       = VSTokenType "keyword" -- ^ For tokens that represent a language keyword.
pattern VS_number        = VSTokenType "number" -- ^ For tokens that represent a number literal.
pattern VS_regexp        = VSTokenType "regexp" -- ^ For tokens that represent a regular expression literal.
pattern VS_operator      = VSTokenType "operator" -- ^ For tokens that represent an operator.

-- * VS Code token modifiers

newtype VSTokenModifier = VSTokenModifier Text
  deriving newtype (ToJSON, IsString)

-- ** Standard token modifiers

pattern VS_declaration = VSTokenModifier "declaration" -- ^ For declarations of symbols.
pattern VS_definition = VSTokenModifier "definition" -- ^ For definitions of symbols, for example, in header files.
pattern VS_readonly = VSTokenModifier "readonly" -- ^ For readonly variables and member fields (constants).
pattern VS_static = VSTokenModifier "static" -- ^ For class members (static members).
pattern VS_deprecated = VSTokenModifier "deprecated" -- ^ For symbols that should no longer be used.
pattern VS_abstract = VSTokenModifier "abstract" -- ^ For types and member functions that are abstract.
pattern VS_async = VSTokenModifier "async" -- ^ For functions that are marked async.
pattern VS_modification = VSTokenModifier "modification" -- ^ For variable references where the variable is assigned to.
pattern VS_documentation = VSTokenModifier "documentation" -- ^ For occurrences of symbols in documentation.
pattern VS_defaultLibrary = VSTokenModifier "defaultLibrary" -- ^ For symbols that are part of the standard library.
