{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
module Language.Rzk.VSCode.Tokens where

import           Data.Aeson
import           Data.String  (IsString)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

-- | VS Code token.
data VSToken = VSToken
  { line           :: !Int
  , startCharacter :: !Int
  , length         :: !Int
  , tokenType      :: VSTokenType
  , tokenModifiers :: [VSTokenModifier]
  } deriving (Generic, ToJSON)

-- | VS Code token types. See https://code.visualstudio.com/api/language-extensions/semantic-highlight-guide#standard-token-types-and-modifiers.
newtype VSTokenType = VSTokenType Text
  deriving newtype (ToJSON, IsString)

-- | VS Code token modifiers. See https://code.visualstudio.com/api/language-extensions/semantic-highlight-guide#standard-token-types-and-modifiers.
newtype VSTokenModifier = VSTokenModifier Text
  deriving newtype (ToJSON, IsString)

-- * Standard token types

-- | For identifiers that declare or reference a namespace, module, or package.
vs_namespace :: VSTokenType
vs_namespace = VSTokenType "namespace"

-- | For identifiers that declare or reference a class type.
vs_class :: VSTokenType
vs_class = VSTokenType "class"

-- | For identifiers that declare or reference an enumeration type.
vs_enum :: VSTokenType
vs_enum = VSTokenType "enum"

-- | For identifiers that declare or reference an interface type.
vs_interface :: VSTokenType
vs_interface = VSTokenType "interface"

-- | For identifiers that declare or reference a struct type.
vs_struct :: VSTokenType
vs_struct = VSTokenType "struct"

-- | For identifiers that declare or reference a type parameter.
vs_typeParameter :: VSTokenType
vs_typeParameter = VSTokenType "typeParameter"

-- | For identifiers that declare or reference a type that is not covered above.
vs_type :: VSTokenType
vs_type = VSTokenType "type"

-- | For identifiers that declare or reference a function or method parameters.
vs_parameter :: VSTokenType
vs_parameter = VSTokenType "parameter"

-- | For identifiers that declare or reference a local or global variable.
vs_variable :: VSTokenType
vs_variable = VSTokenType "variable"

-- | For identifiers that declare or reference a member property, member field, or member variable.
vs_property :: VSTokenType
vs_property = VSTokenType "property"

-- | For identifiers that declare or reference an enumeration property, constant, or member.
vs_enumMember :: VSTokenType
vs_enumMember = VSTokenType "enumMember"

-- | For identifiers that declare or reference decorators and annotations.
vs_decorator :: VSTokenType
vs_decorator = VSTokenType "decorator"

-- | For identifiers that declare an event property.
vs_event :: VSTokenType
vs_event = VSTokenType "event"

-- | For identifiers that declare a function.
vs_function :: VSTokenType
vs_function = VSTokenType "function"

-- | For identifiers that declare a member function or method.
vs_method :: VSTokenType
vs_method = VSTokenType "method"

-- | For identifiers that declare a macro.
vs_macro :: VSTokenType
vs_macro = VSTokenType "macro"

-- | For identifiers that declare a label.
vs_label :: VSTokenType
vs_label = VSTokenType "label"

-- | For tokens that represent a comment.
vs_comment :: VSTokenType
vs_comment = VSTokenType "comment"

-- | For tokens that represent a string literal.
vs_string :: VSTokenType
vs_string = VSTokenType "string"

-- | For tokens that represent a language keyword.
vs_keyword :: VSTokenType
vs_keyword = VSTokenType "keyword"

-- | For tokens that represent a number literal.
vs_number :: VSTokenType
vs_number = VSTokenType "number"

-- | For tokens that represent a regular expression literal.
vs_regexp :: VSTokenType
vs_regexp = VSTokenType "regexp"

-- | For tokens that represent an operator.
vs_operator :: VSTokenType
vs_operator = VSTokenType "operator"

-- * Standard token modifiers

-- | For declarations of symbols.
vs_declaration :: VSTokenModifier
vs_declaration = VSTokenModifier "declaration"

-- | For definitions of symbols, for example, in header files.
vs_definition :: VSTokenModifier
vs_definition = VSTokenModifier "definition"

-- | For readonly variables and member fields (constants).
vs_readonly :: VSTokenModifier
vs_readonly = VSTokenModifier "readonly"

-- | For class members (static members).
vs_static :: VSTokenModifier
vs_static = VSTokenModifier "static"

-- | For symbols that should no longer be used.
vs_deprecated :: VSTokenModifier
vs_deprecated = VSTokenModifier "deprecated"

-- | For types and member functions that are abstract.
vs_abstract :: VSTokenModifier
vs_abstract = VSTokenModifier "abstract"

-- | For functions that are marked async.
vs_async :: VSTokenModifier
vs_async = VSTokenModifier "async"

-- | For variable references where the variable is assigned to.
vs_modification :: VSTokenModifier
vs_modification = VSTokenModifier "modification"

-- | For occurrences of symbols in documentation.
vs_documentation :: VSTokenModifier
vs_documentation = VSTokenModifier "documentation"

-- | For symbols that are part of the standard library.
vs_defaultLibrary :: VSTokenModifier
vs_defaultLibrary = VSTokenModifier "defaultLibrary"

