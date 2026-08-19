{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Type errors, and how they are shown.
--
-- An error is captured /at its own scope/: it packages the context it was raised
-- in, existentially, so its scope index does not escape into the error type. The
-- old representation instead nested the error one @Inc@ deeper at every binder
-- (@ScopedTypeError@) and unwound the whole stack at printing time, inventing a
-- name per binder as it went. That unwinding loop, and both of its
-- @FIXME: very inefficient filter@ sites, are gone: the context already knows what
-- everything in scope is called (see "Rzk.TypeCheck.Display").
--
-- The error type is therefore /not/ scope-indexed, which is why entering a binder
-- no longer has to re-index the error channel.
module Rzk.TypeCheck.Error where

import           Control.Monad.Foil       (Distinct)
import qualified Control.Monad.Foil       as Foil
import           Data.List                (intercalate)

import           Language.Rzk.Foil.Syntax
import           Language.Rzk.Foil.Names (TModality (..), VarIdent, getVarIdent,
                                           ppVarIdentWithLocation)
import qualified Language.Rzk.Syntax      as Rzk
import           Rzk.TypeCheck.Context
import           Rzk.TypeCheck.Display

data TypeError n
  = TypeErrorOther String
  | TypeErrorUnify (TermT n) (TermT n) (TermT n)
  | TypeErrorUnifyTerms (TermT n) (TermT n)
  | TypeErrorNotPair (TermT n) (TermT n)
  | TypeErrorNotModal (Term n) TModality (TermT n)
  | TypeErrorModalityMismatch TModality TModality (Term n)
  | TypeErrorUnaccessibleVar (Foil.Name n) TModality TModality
  | TypeErrorNotTypeInModal (TermT n)
  | TypeErrorNotFunction (TermT n) (TermT n)
  | TypeErrorUnexpectedLambda (Term n) (TermT n)
  | TypeErrorUnexpectedPair (Term n) (TermT n)
  | TypeErrorUnexpectedRefl (Term n) (TermT n)
  | TypeErrorCannotInferBareLambda (Term n)
  | TypeErrorCannotInferBareRefl (Term n)
  | TypeErrorCannotInferHole (Term n)
  | TypeErrorUnsolvedHole (Maybe VarIdent) (TermT n)
  | TypeErrorUndefined VarIdent
  | TypeErrorTopeNotSatisfied [TermT n] (TermT n)
  | TypeErrorTopeContextDisjoint (TermT n) [TermT n]
  | TypeErrorTopesNotEquivalent (TermT n) (TermT n)
  | TypeErrorInvalidArgumentType (Term n) (TermT n)
  | TypeErrorDuplicateTopLevel [VarIdent] VarIdent
  | TypeErrorUnusedVariable (Foil.Name n) (TermT n)
  | TypeErrorUnusedUsedVariables [Foil.Name n] (Foil.Name n)
  | TypeErrorImplicitAssumption (Foil.Name n, TermT n) (Foil.Name n)
  | TypeErrorNotIntervalCube String (TermT n) (TermT n)
  | TypeErrorRepeatedBinder VarIdent [VarIdent]
  | TypeErrorMatchScrutineeNotData (TermT n) (TermT n)
  | TypeErrorMatchCannotInfer (Term n)
  | TypeErrorMatchMissingBranch VarIdent
  | TypeErrorMatchDuplicateBranch VarIdent
  | TypeErrorMatchUnknownBranch VarIdent [VarIdent]
  | TypeErrorMatchBranchArity VarIdent Int Int
  | TypeErrorReascribedTypeMismatch VarIdent (TermT n) (TermT n)

-- | An error, together with the context it was raised in.
--
-- The scope index is existential: an error raised under a binder is /already/
-- complete (its context says what its names are called), so it needs nothing
-- from the enclosing scope and can be thrown straight through it.
data TypeErrorInScopedContext where
  TypeErrorInScopedContext
    :: Distinct n => Context n -> TypeError n -> TypeErrorInScopedContext

ppModality :: TModality -> String
ppModality = \case
  Flat  -> "♭"
  Sharp -> "♯"
  Op    -> "ᵒᵖ"
  Id    -> "_id"

-- * Rendering

data OutputDirection = TopDown | BottomUp
  deriving (Eq)

block :: OutputDirection -> [String] -> String
block TopDown  = intercalate "\n"
block BottomUp = intercalate "\n" . reverse

namedBlock :: OutputDirection -> String -> [String] -> String
namedBlock dir name lines_ = block dir $
  name : map indent lines_
  where
    indent = intercalate "\n" . map ("  " ++) . lines

ppTypeError :: Naming n -> TypeError n -> String
ppTypeError naming = \case
  TypeErrorOther msg -> msg
  TypeErrorUnify term expected actual -> block TopDown
    [ "cannot unify expected type"
    , "  " <> ppU (untyped expected)
    , "with actual type"
    , "  " <> ppU (untyped actual)
    , "for term"
    , "  " <> ppU (untyped term) ]
  TypeErrorUnifyTerms expected actual -> block TopDown
    [ "cannot unify term"
    , "  " <> ppU (untyped expected)
    , "with term"
    , "  " <> ppU (untyped actual) ]
  TypeErrorNotPair term ty -> block TopDown
    [ "expected a cube product or dependent pair"
    , "but got type"
    , "  " <> ppU (untyped ty)
    , "for term"
    , "  " <> ppU (untyped term)
    , case ty of
        TypeFunT{} -> "\nPerhaps the term is applied to too few arguments?"
        _          -> ""
    ]
  TypeErrorNotModal term m ty -> block TopDown
    [ "expected modal type " <> ppModality m <> " ?"
    , "but got type"
    , "  " <> ppU (untyped ty)
    , "for term"
    , "  " <> ppU term
    ]
  TypeErrorModalityMismatch expected actual term -> block TopDown
    [ "modality mismatch"
    , "  expected " <> ppModality expected
    , "  but got  " <> ppModality actual
    , "for term"
    , "  " <> ppU term
    ]
  TypeErrorUnaccessibleVar _var varMod locks -> block TopDown
    [ "unaccessible var with modality " <> ppModality varMod
    , "  under locks " <> ppModality locks
    ]
  TypeErrorNotTypeInModal ty -> block TopDown
    [ "expected a type inside modal type"
    , "but got"
    , "  " <> ppU (untyped ty)
    ]

  TypeErrorUnexpectedLambda term ty -> block TopDown
    [ "unexpected lambda abstraction"
    , "  " <> ppU term
    , "when typechecking against a non-function type"
    , "  " <> ppTyped ty
    ]
  TypeErrorUnexpectedPair term ty -> block TopDown
    [ "unexpected pair"
    , "  " <> ppU term
    , "when typechecking against a type that is not a product or a dependent sum"
    , "  " <> ppTyped ty
    ]
  TypeErrorUnexpectedRefl term ty -> block TopDown
    [ "unexpected refl"
    , "  " <> ppU term
    , "when typechecking against a type that is not an identity type"
    , "  " <> ppTyped ty
    ]

  TypeErrorNotFunction term ty -> block TopDown
    [ "expected a function or extension type"
    , "but got type"
    , "  " <> ppU (untyped ty)
    , "for term"
    , "  " <> ppU (untyped term)
    , case term of
        AppT _ty f _x -> "\nPerhaps the term\n  " <> ppU (untyped f) <> "\nis applied to too many arguments?"
        _             -> ""
    ]
  TypeErrorCannotInferBareLambda term -> block TopDown
    [ "cannot infer the type of the argument"
    , "in lambda abstraction"
    , "  " <> ppU term
    ]
  TypeErrorCannotInferBareRefl term -> block TopDown
    [ "cannot infer the type of term"
    , "  " <> ppU term
    ]
  TypeErrorCannotInferHole term -> block TopDown
    [ "cannot infer the type of a hole"
    , "  " <> ppU term
    , "a hole is only allowed where its type is already known (checking position)"
    ]
  TypeErrorUnsolvedHole mname goal -> block TopDown
    [ "found an unsolved hole" <> maybe "" (\name -> " ?" <> show name) mname
    , "expected type (goal):"
    , "  " <> ppU (untyped goal)
    ]
  TypeErrorUndefined var -> block TopDown
    [ "undefined variable: " <> show var ]
  TypeErrorTopeNotSatisfied topes tope -> block TopDown
    [ "local context is not included in (does not entail) the tope"
    , "  " <> ppU (untyped tope)
    , "in local context (normalised)"
    , intercalate "\n" (map (("  " <>) . ppTyped) topes)] -- FIXME: remove
  TypeErrorTopeContextDisjoint tope topes -> block TopDown
    [ "the tope"
    , "  " <> ppU (untyped tope)
    , "is disjoint from the local tope context (their conjunction is the empty tope ⊥),"
    , "so this restriction face or recOR branch is vacuous everywhere"
    , "in local context (normalised)"
    , intercalate "\n" (map (("  " <>) . ppTyped) topes)]
  TypeErrorTopesNotEquivalent expected actual -> block TopDown
    [ "expected tope"
    , "  " <> ppU (untyped expected)
    , "but got"
    , "  " <> ppU (untyped actual) ]

  TypeErrorInvalidArgumentType argType argKind -> block TopDown
    [ "invalid function parameter type"
    , "  " <> ppU argType
    , "function parameter can be a cube, a shape, or a type"
    , "but given parameter type has type"
    , "  " <> ppU (untyped argKind)
    ]

  TypeErrorDuplicateTopLevel previous lastName -> block TopDown
    [ "duplicate top-level definition"
    , "  " <> ppVarIdentWithLocation lastName
    , "previous top-level definitions found at"
    , intercalate "\n"
      [ "  " <> ppVarIdentWithLocation name
      | name <- previous ]
    ]

  TypeErrorUnusedVariable name type_ -> block TopDown
    [ "unused variable"
    , "  " <> ppVar name <> " : " <> ppU (untyped type_)
    ]

  TypeErrorUnusedUsedVariables vars name -> block TopDown
    [ "unused variables"
    , "  " <> unwords (map ppVar vars)
    , "declared as used in definition of"
    , "  " <> ppVar name
    ]

  TypeErrorImplicitAssumption (a, aType) name -> block TopDown
    [ "implicit assumption"
    , "  " <> ppVar a <> " : " <> ppU (untyped aType)
    , "used in definition of"
    , "  " <> ppVar name
    ]

  TypeErrorNotIntervalCube op lType rType -> block TopDown
    [ op <> " expects both points in the same interval cube (2 or 𝕀)"
    , "but got a point of type"
    , "  " <> ppU (untyped lType)
    , "and a point of type"
    , "  " <> ppU (untyped rType)
    ]

  TypeErrorRepeatedBinder name previous -> block TopDown
    [ show name <> " is bound multiple times in one binder group"
    , "  " <> ppVarIdentWithLocation name
    , "already bound at"
    , intercalate "\n"
      [ "  " <> ppVarIdentWithLocation prev | prev <- previous ]
    ]

  TypeErrorMatchScrutineeNotData scrut ty -> block TopDown
    [ "match scrutinee"
    , "  " <> ppU (untyped scrut)
    , "is not of a #data type; its type is"
    , "  " <> ppU (untyped ty)
    ]
  TypeErrorMatchCannotInfer term -> block TopDown
    [ "cannot infer the type of match"
    , "  " <> ppU term
    , "a match without \"into\" is only allowed where its type is already known (checking position)"
    ]
  TypeErrorMatchMissingBranch con -> block TopDown
    [ "match has no branch for constructor " <> show con ]
  TypeErrorMatchDuplicateBranch con -> block TopDown
    [ "duplicate match branch for constructor " <> show con ]
  TypeErrorMatchUnknownBranch con cons -> block TopDown
    [ "match branch for " <> show con <> ", which is not a constructor of the scrutinee's type"
    , case cons of
        [] -> "the type has no constructors"
        _  -> "the constructors are: " <> unwords (map show cons)
    ]
  TypeErrorMatchBranchArity con expected actual -> block TopDown
    [ "match branch for constructor " <> show con
    , "binds " <> show actual <> " argument" <> (if actual == 1 then "" else "s")
        <> ", but its method takes " <> show expected
    ]
  TypeErrorReascribedTypeMismatch entryName canonical given -> block TopDown
    [ "the re-ascribed type of " <> show entryName
    , "  " <> ppU (untyped given)
    , "is not definitionally equal to its canonical type"
    , "  " <> ppU (untyped canonical)
    ]
  where
    ppU = ppTerm naming
    ppTyped = ppTermT naming
    ppVar = ppName naming

ppAction :: Naming n -> Int -> Action n -> String
ppAction naming n = unlines . map (replicate (2 * n) ' ' <>) . \case
  ActionTypeCheck term ty ->
    [ "typechecking"
    , "  " <> ppU term
    , "against type"
    , "  " <> ppU (untyped ty) ]

  ActionUnify term expected actual ->
    [ "unifying expected type"
    , "  " <> ppU (untyped expected)
    , "with actual type"
    , "  " <> ppU (untyped actual)
    , "for term"
    , "  " <> ppU (untyped term) ]

  ActionUnifyTerms expected actual ->
    [ "unifying term (expected)"
    , "  " <> ppTyped expected
    , "with term (actual)"
    , "  " <> ppTyped actual ]

  ActionInfer term ->
    [ "inferring type for term"
    , "  " <> ppU term ]

  ActionContextEntailedBy topes term ->
    [ "checking if local context"
    , intercalate "\n" (map (("  " <>) . ppU . untyped) topes)
    , "includes (is entailed by) restriction tope"
    , "  " <> ppU (untyped term) ]

  ActionContextEntails topes term ->
    [ "checking if local context"
    , intercalate "\n" (map (("  " <>) . ppU . untyped) topes)
    , "is included in (entails) the tope"
    , "  " <> ppU (untyped term) ]

  ActionContextEntailsUnion topes terms ->
    [ "checking if local context"
    , intercalate "\n" (map (("  " <>) . ppU . untyped) topes)
    , "is included in (entails) the union of the topes"
    , intercalate "\n" (map (("  " <>) . ppU . untyped) terms) ]

  ActionWHNF term ->
    [ "computing WHNF for term"
    , "  " <> ppTyped term ]

  ActionNF term ->
    [ "computing normal form for term"
    , "  " <> ppU (untyped term) ]

  ActionCheckCoherence (ltope, lterm) (rtope, rterm) ->
    [ "checking coherence for"
    , "  " <> ppU (untyped ltope)
    , "  |-> " <> ppU (untyped lterm)
    , "and"
    , "  " <> ppU (untyped rtope)
    , "  |-> " <> ppU (untyped rterm) ]

  ActionCloseSection Nothing ->
    [ "closing the file"
    , "and collecting assumptions (variables)" ]
  ActionCloseSection (Just sectionName) ->
    [ "closing #section " <> Rzk.printTree sectionName
    , "and collecting assumptions (variables)"]

  ActionCheckLetValue orig ->
    [ "checking the local definition "
        <> maybe "_" (Rzk.printTree . getVarIdent) orig ]
  where
    ppU = ppTerm naming
    ppTyped = ppTermT naming

-- | The context an error was raised in: where it happened, what was being
-- checked, the tope context, the stack of judgements, and the hypotheses.
ppContext :: OutputDirection -> Context n -> String
ppContext dir ctx@Context{..} = block dir $ dropWhile null
  [ block TopDown
    [ case ctxLocation of
        _ | dir == TopDown -> "" -- FIXME
        Just (LocationInfo (Just path) (Just lineNo) (Just col)) ->
          path <> " (line " <> show lineNo <> ", column " <> show col <> "):"
        Just (LocationInfo (Just path) (Just lineNo) Nothing) ->
          path <> " (line " <> show lineNo <> "):"
        Just (LocationInfo (Just path) _ _) ->
          path <> ":"
        _  -> ""
    , case ctxCurrentCommand of
        Just (Rzk.CommandDefine _loc name _vars _params _ty _term) ->
          "  Error occurred when checking\n    #define " <> Rzk.printTree name
        Just (Rzk.CommandPostulate _loc name _vars _params _ty ) ->
          "  Error occurred when checking\n    #postulate " <> Rzk.printTree name
        Just (Rzk.CommandData _loc name _vars _params _sort _body) ->
          "  Error occurred when checking\n    #data " <> Rzk.printTree name
        Just (Rzk.CommandCheck _loc term ty) ->
          "  Error occurred when checking\n    " <> Rzk.printTree term <> " : " <> Rzk.printTree ty
        Just (Rzk.CommandCompute _loc term) ->
          "  Error occurred when computing\n    " <> Rzk.printTree term
        Just (Rzk.CommandComputeNF _loc term) ->
          "  Error occurred when computing NF for\n    " <> Rzk.printTree term
        Just (Rzk.CommandComputeWHNF _loc term) ->
          "  Error occurred when computing WHNF for\n    " <> Rzk.printTree term
        Just (Rzk.CommandSetOption _loc optionName _optionValue) ->
          "  Error occurred when trying to set option\n    #set-option " <> show optionName
        Just command@Rzk.CommandUnsetOption{} ->
          "  Error occurred when trying to unset option\n    " <> Rzk.printTree command
        Just command@Rzk.CommandAssume{} ->
          "  Error occurred when checking assumption\n    " <> Rzk.printTree command
        Just (Rzk.CommandSection _loc name) ->
          "  Error occurred when checking\n    #section " <> Rzk.printTree name
        Just (Rzk.CommandSectionEnd _loc name) ->
          "  Error occurred when checking\n    #end " <> Rzk.printTree name
        Nothing -> "  Error occurred outside of any command!"
    ]
  , ""
  , case filter (not . isTopeTop) (availableTopes ctx) of
      [] -> "Local tope context is unrestricted (⊤)."
      topes -> namedBlock TopDown "Local tope context:"
        [ "  " <> ppU (untyped tope)
        | tope <- topes ]
  , ""
  , block dir
    [ "when " <> ppAction naming 0 action
    | action <- ctxActionStack ]
  , namedBlock TopDown "Definitions in context:"
    [ block dir
      [ ppName naming name <> " : " <> ppU (untyped (varType info))
      | (name, info) <- reverse (varsInScope ctx) ] ]
  ]
  where
    naming = namingOfContext ctx
    ppU = ppTerm naming
    isTopeTop TopeTopT{} = True
    isTopeTop _          = False

-- | An error, with the context it was raised in.
ppTypeErrorInScopedContext :: OutputDirection -> TypeErrorInScopedContext -> String
ppTypeErrorInScopedContext dir (TypeErrorInScopedContext ctx err) = block dir
  [ ppTypeError (namingOfContext ctx) err
  , ""
  , ppContext dir ctx
  ]
