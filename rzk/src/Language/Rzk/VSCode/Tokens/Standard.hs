{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Rzk.VSCode.Tokens.Standard where

import           Language.Rzk.VSCode.Tokens

vs_namespace      = VSTokenType "namespace" -- ^ For identifiers that declare or reference a namespace, module, or package.
vs_class          = VSTokenType "class" -- ^ For identifiers that declare or reference a class type.
vs_enum           = VSTokenType "enum" -- ^ For identifiers that declare or reference an enumeration type.
vs_interface      = VSTokenType "interface" -- ^ For identifiers that declare or reference an interface type.
vs_struct         = VSTokenType "struct" -- ^ For identifiers that declare or reference a struct type.
vs_typeParameter  = VSTokenType "typeParameter" -- ^ For identifiers that declare or reference a type parameter.
vs_type           = VSTokenType "type" -- ^ For identifiers that declare or reference a type that is not covered above.
vs_parameter      = VSTokenType "parameter" -- ^ For identifiers that declare or reference a function or method parameters.
vs_variable       = VSTokenType "variable" -- ^ For identifiers that declare or reference a local or global variable.
vs_property       = VSTokenType "property" -- ^ For identifiers that declare or reference a member property, member field, or member variable.
vs_enumMember     = VSTokenType "enumMember" -- ^ For identifiers that declare or reference an enumeration property, constant, or member.
vs_decorator      = VSTokenType "decorator" -- ^ For identifiers that declare or reference decorators and annotations.
vs_event          = VSTokenType "event" -- ^ For identifiers that declare an event property.
vs_function       = VSTokenType "function" -- ^ For identifiers that declare a function.
vs_method         = VSTokenType "method" -- ^ For identifiers that declare a member function or method.
vs_macro          = VSTokenType "macro" -- ^ For identifiers that declare a macro.
vs_label          = VSTokenType "label" -- ^ For identifiers that declare a label.
vs_comment        = VSTokenType "comment" -- ^ For tokens that represent a comment.
vs_string         = VSTokenType "string" -- ^ For tokens that represent a string literal.
vs_keyword        = VSTokenType "keyword" -- ^ For tokens that represent a language keyword.
vs_number         = VSTokenType "number" -- ^ For tokens that represent a number literal.
vs_regexp         = VSTokenType "regexp" -- ^ For tokens that represent a regular expression literal.
vs_operator       = VSTokenType "operator" -- ^ For tokens that represent an operator.

vs_declaration    = VSTokenModifier "declaration" -- ^ For declarations of symbols.
vs_definition     = VSTokenModifier "definition" -- ^ For definitions of symbols, for example, in header files.
vs_readonly       = VSTokenModifier "readonly" -- ^ For readonly variables and member fields (constants).
vs_static         = VSTokenModifier "static" -- ^ For class members (static members).
vs_deprecated     = VSTokenModifier "deprecated" -- ^ For symbols that should no longer be used.
vs_abstract       = VSTokenModifier "abstract" -- ^ For types and member functions that are abstract.
vs_async          = VSTokenModifier "async" -- ^ For functions that are marked async.
vs_modification   = VSTokenModifier "modification" -- ^ For variable references where the variable is assigned to.
vs_documentation  = VSTokenModifier "documentation" -- ^ For occurrences of symbols in documentation.
vs_defaultLibrary = VSTokenModifier "defaultLibrary" -- ^ For symbols that are part of the standard library.
