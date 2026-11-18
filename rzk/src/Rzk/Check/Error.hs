{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- | Type errors, warnings and holes of the new checker, and how they are
-- shown.
--
-- An error is captured at its own scope, with the context it was raised
-- in, so that it can be shown with the names of that context. A warning
-- and a hole are recorded already rendered, so they are independent of the
-- scope they came from.
module Rzk.Check.Error
  ( TypeError (..)
  , typeErrorTag
  , ErrorInContext (..)
  , errorTag
  , errorLocation
  , ppTypeError
  , ppErrorInContext
  , ppAction
  , ppTope
  , ppPoint
    -- * Warnings
  , Warning (..)
  , warningTag
  , warningLocation
  , ppWarning
    -- * Hints
  , Hint (..)
  , hintTag
  , hintLocation
  , ppHint
    -- * Holes
  , HoleInfo (..)
  , ppHoleInfo
  ) where

import           Control.Monad.Foil       (Distinct, Scope)
import qualified Control.Monad.Foil       as Foil
import           Data.List                (intercalate)

import           Language.Rzk.Foil.Names  (TModality (..), VarIdent,
                                           fromVarIdent, ppVarIdentWithLocation)
import qualified Language.Rzk.Syntax      as Rzk
import           Rzk.Check.Context
import           Rzk.Check.Display
import           Rzk.Check.Syntax
import           Rzk.Check.Topes

data TypeError n
  = TypeErrorOther String
    -- ^ a diagnostic that does not yet have a dedicated constructor
  | TypeErrorUnsupported String
    -- ^ a construct the new checker does not cover yet
  | TypeErrorUnify (Core n) (Value n) (Value n)
    -- ^ the term, the expected type and its actual type
  | TypeErrorUnifyTerms (Value n) (Value n)
    -- ^ the expected and actual values that are not convertible
  | TypeErrorNotPair (Core n) (Value n)
    -- ^ a projected term and its non-pair type
  | forall l. TypeErrorUnexpectedPairPattern (Pattern n l) (Value n)
    -- ^ a pair pattern and its non-pair type
  | TypeErrorUnexpectedUnitPattern (Value n)
    -- ^ a unit pattern's non-unit type
  | TypeErrorNotFunction (Core n) (Value n)
    -- ^ an applied term and its non-function type
  | TypeErrorUnexpectedLambda (Raw n) (Value n)
    -- ^ a λ checked against a non-function type
  | TypeErrorUnexpectedPair (Raw n) (Value n)
    -- ^ a pair checked against a non-pair type
  | TypeErrorUnexpectedRefl (Raw n) (Value n)
    -- ^ reflexivity checked against a non-identity type
  | TypeErrorCannotInferBareLambda (Raw n)
    -- ^ a λ without a parameter annotation in inference position
  | TypeErrorCannotInferBareRefl (Raw n)
    -- ^ reflexivity without an element annotation in inference position
  | TypeErrorCannotInferHole (Raw n)
    -- ^ a user hole whose type cannot be inferred
  | TypeErrorUnsolvedHole (Maybe VarIdent) (Value n)
    -- ^ a user hole and the goal it leaves unsolved in strict mode
  | TypeErrorUndefined VarIdent
    -- ^ an unresolved surface identifier
  | TypeErrorTopeNotSatisfied [Tope n] (Tope n)
    -- ^ context topes and the goal they do not entail
  | TypeErrorTopeContextDisjoint (Tope n) [Tope n]
    -- ^ a tope disjoint from the listed context
  | TypeErrorInvalidArgumentType (Raw n) (Value n)
    -- ^ a function parameter and its invalid kind
  | TypeErrorDuplicateTopLevel [VarIdent] VarIdent
    -- ^ previous definitions and the duplicate name
  | TypeErrorUnusedVariable (Foil.Name n) (Value n)
    -- ^ an unused section assumption and its type
  | TypeErrorUnusedUsedVariables [Foil.Name n] (Foil.Name n)
    -- ^ assumptions listed as used but not needed by the declaration
  | TypeErrorImplicitAssumption (Foil.Name n, Value n) (Foil.Name n)
    -- ^ an assumption used implicitly, its type, and the declaration using it
  | TypeErrorNotIntervalCube String (Value n) (Value n)
    -- ^ an operation and the two operand types, which are not interval cubes
  | TypeErrorRepeatedBinder VarIdent [VarIdent]
    -- ^ a repeated binder and its previous occurrences

-- | The constructor name: the stable code of a diagnostic, and the tag the
-- fixtures pin.
typeErrorTag :: TypeError n -> String
typeErrorTag = \case
  TypeErrorOther{}                -> "TypeErrorOther"
  TypeErrorUnsupported{}          -> "TypeErrorUnsupported"
  TypeErrorUnify{}                -> "TypeErrorUnify"
  TypeErrorUnifyTerms{}           -> "TypeErrorUnifyTerms"
  TypeErrorNotPair{}              -> "TypeErrorNotPair"
  TypeErrorUnexpectedPairPattern{} -> "TypeErrorUnexpectedPairPattern"
  TypeErrorUnexpectedUnitPattern{} -> "TypeErrorUnexpectedUnitPattern"
  TypeErrorNotFunction{}          -> "TypeErrorNotFunction"
  TypeErrorUnexpectedLambda{}     -> "TypeErrorUnexpectedLambda"
  TypeErrorUnexpectedPair{}       -> "TypeErrorUnexpectedPair"
  TypeErrorUnexpectedRefl{}       -> "TypeErrorUnexpectedRefl"
  TypeErrorCannotInferBareLambda{} -> "TypeErrorCannotInferBareLambda"
  TypeErrorCannotInferBareRefl{}  -> "TypeErrorCannotInferBareRefl"
  TypeErrorCannotInferHole{}      -> "TypeErrorCannotInferHole"
  TypeErrorUnsolvedHole{}         -> "TypeErrorUnsolvedHole"
  TypeErrorUndefined{}            -> "TypeErrorUndefined"
  TypeErrorTopeNotSatisfied{}     -> "TypeErrorTopeNotSatisfied"
  TypeErrorTopeContextDisjoint{}  -> "TypeErrorTopeContextDisjoint"
  TypeErrorInvalidArgumentType{}  -> "TypeErrorInvalidArgumentType"
  TypeErrorDuplicateTopLevel{}    -> "TypeErrorDuplicateTopLevel"
  TypeErrorUnusedVariable{}       -> "TypeErrorUnusedVariable"
  TypeErrorUnusedUsedVariables{}  -> "TypeErrorUnusedUsedVariables"
  TypeErrorImplicitAssumption{}   -> "TypeErrorImplicitAssumption"
  TypeErrorNotIntervalCube{}      -> "TypeErrorNotIntervalCube"
  TypeErrorRepeatedBinder{}       -> "TypeErrorRepeatedBinder"

-- | An error with the context it was raised in. The scope is existential:
-- the context says what the names are called.
data ErrorInContext where
  ErrorInContext :: Distinct n => ContextWithScope n -> TypeError n -> ErrorInContext

errorTag :: ErrorInContext -> String
errorTag (ErrorInContext _ err) = typeErrorTag err

errorLocation :: ErrorInContext -> Maybe LocationInfo
errorLocation (ErrorInContext (ContextWithScope _ ctx) _) = ctxLocation ctx

ppPattern :: Pattern n l -> String
ppPattern = \case
  PatternVar mname _ -> maybe "_" (Rzk.printTree . fromVarIdent) mname
  PatternWildcard _  -> "_"
  PatternUnit        -> "unit"
  PatternPair l r    -> "(" <> ppPattern l <> ", " <> ppPattern r <> ")"

ppTypeError :: Distinct n => Scope n -> Context n -> TypeError n -> String
ppTypeError scope ctx = \case
  TypeErrorOther msg -> msg
  TypeErrorUnsupported what -> "not supported by the new checker yet: " <> what
  TypeErrorUnify term expected actual -> block
    [ "cannot unify expected type"
    , "  " <> ppV expected
    , "with actual type"
    , "  " <> ppV actual
    , "for term"
    , "  " <> ppC term ]
  TypeErrorUnifyTerms expected actual -> block
    [ "cannot unify term"
    , "  " <> ppV expected
    , "with term"
    , "  " <> ppV actual ]
  TypeErrorNotPair term ty -> block
    [ "expected a cube product or dependent pair"
    , "but got type"
    , "  " <> ppV ty
    , "for term"
    , "  " <> ppC term ]
  TypeErrorUnexpectedPairPattern pat ty -> block
    [ "unexpected pair pattern"
    , "  " <> ppPattern pat
    , "when typechecking against a type that is not a dependent pair or cube product"
    , "  " <> ppV ty ]
  TypeErrorUnexpectedUnitPattern ty -> block
    [ "unexpected unit pattern"
    , "  unit"
    , "when typechecking against a non-unit type"
    , "  " <> ppV ty ]
  TypeErrorNotFunction term ty -> block
    [ "expected a function or extension type"
    , "but got type"
    , "  " <> ppV ty
    , "for term"
    , "  " <> ppC term ]
  TypeErrorUnexpectedLambda term ty -> block
    [ "unexpected lambda abstraction"
    , "  " <> ppR term
    , "when typechecking against a non-function type"
    , "  " <> ppV ty ]
  TypeErrorUnexpectedPair term ty -> block
    [ "unexpected pair"
    , "  " <> ppR term
    , "when typechecking against a type that is not a product or a dependent sum"
    , "  " <> ppV ty ]
  TypeErrorUnexpectedRefl term ty -> block
    [ "unexpected refl"
    , "  " <> ppR term
    , "when typechecking against a type that is not an identity type"
    , "  " <> ppV ty ]
  TypeErrorCannotInferBareLambda term -> block
    [ "cannot infer the type of the argument"
    , "in lambda abstraction"
    , "  " <> ppR term ]
  TypeErrorCannotInferBareRefl term -> block
    [ "cannot infer the type of term"
    , "  " <> ppR term ]
  TypeErrorCannotInferHole term -> block
    [ "cannot infer the type of a hole"
    , "  " <> ppR term
    , "a hole is only allowed where its type is already known (checking position)" ]
  TypeErrorUnsolvedHole mname goal -> block
    [ "found an unsolved hole" <> maybe "" (\name -> " ?" <> show name) mname
    , "expected type (goal):"
    , "  " <> ppV goal ]
  TypeErrorUndefined var -> "undefined variable: " <> show var
  TypeErrorTopeNotSatisfied topes tope -> block
    [ "local context is not included in (does not entail) the tope"
    , "  " <> ppTope naming tope
    , "in local context (normalised)"
    , intercalate "\n" (map (("  " <>) . ppTope naming) topes) ]
  TypeErrorTopeContextDisjoint tope topes -> block
    [ "the tope"
    , "  " <> ppTope naming tope
    , "is disjoint from the local tope context (their conjunction is the empty tope ⊥),"
    , "so this restriction face or recOR branch is vacuous everywhere"
    , "in local context (normalised)"
    , intercalate "\n" (map (("  " <>) . ppTope naming) topes) ]
  TypeErrorInvalidArgumentType argType argKind -> block
    [ "invalid function parameter type"
    , "  " <> ppR argType
    , "function parameter can be a cube, a shape, or a type"
    , "but given parameter type has type"
    , "  " <> ppV argKind ]
  TypeErrorDuplicateTopLevel previous lastName -> block
    [ "duplicate top-level definition"
    , "  " <> ppVarIdentWithLocation lastName
    , "previous top-level definitions found at"
    , intercalate "\n" [ "  " <> ppVarIdentWithLocation name | name <- previous ] ]
  TypeErrorUnusedVariable name ty -> block
    [ "unused variable"
    , "  " <> ppName naming name <> " : " <> ppV ty ]
  TypeErrorUnusedUsedVariables vars name -> block
    [ "unused variables"
    , "  " <> unwords (map (ppName naming) vars)
    , "declared as used in definition of"
    , "  " <> ppName naming name ]
  TypeErrorImplicitAssumption (a, aType) name -> block
    [ "implicit assumption"
    , "  " <> ppName naming a <> " : " <> ppV aType
    , "used in definition of"
    , "  " <> ppName naming name ]
  TypeErrorNotIntervalCube op lType rType -> block
    [ op <> " expects both points in the same interval cube (2 or 𝕀)"
    , "but got a point of type"
    , "  " <> ppV lType
    , "and a point of type"
    , "  " <> ppV rType ]
  TypeErrorRepeatedBinder name previous -> block
    [ show name <> " is bound multiple times in one binder group"
    , "  " <> ppVarIdentWithLocation name
    , "already bound at"
    , intercalate "\n" [ "  " <> ppVarIdentWithLocation prev | prev <- previous ] ]
  where
    naming = namingOfContext ctx
    ppV = ppValueIn scope ctx
    ppC = ppCore naming
    ppR = ppCore naming . stripPositions
    block = intercalate "\n"

-- | An error, with the context it was raised in.
ppErrorInContext :: ErrorInContext -> String
ppErrorInContext (ErrorInContext (ContextWithScope scope ctx) err) = intercalate "\n"
  [ ppTypeError scope ctx err
  , ""
  , ppContext scope ctx ]

-- | The context an error was raised in: where, the tope context, the stack
-- of judgements, and the hypotheses.
ppContext :: Distinct n => Scope n -> Context n -> String
ppContext scope ctx = intercalate "\n" $ filter (not . null)
  [ case ctxLocation ctx of
      Just (LocationInfo (Just path) (Just lineNo) (Just col)) ->
        path <> " (line " <> show lineNo <> ", column " <> show col <> "):"
      Just (LocationInfo (Just path) (Just lineNo) Nothing) ->
        path <> " (line " <> show lineNo <> "):"
      Just (LocationInfo (Just path) _ _) -> path <> ":"
      _ -> ""
  , case filter ((/= TTop) . tTope) (contextTopes (ctxTopes ctx)) of
      [] -> "Local tope context is unrestricted (⊤)."
      topes -> intercalate "\n" $ "Local tope context:" :
        [ "  " <> (if isAccessible mt then "" else "× ") <> ppTope naming (tTope mt)
        | mt <- topes ]
  , intercalate "\n"
      [ "when " <> ppAction scope ctx action
      | action <- reverse (diagActions (ctxDiag ctx)) ]
  , intercalate "\n" $ "Definitions in context:" :
      [ "  " <> ppName naming name <> " : " <> ppValueIn scope ctx (entryType entry)
      | name <- ctxBound ctx, let entry = lookupEntry name ctx ]
  ]
  where
    naming = namingOfContext ctx

ppAction :: Distinct n => Scope n -> Context n -> Action n -> String
ppAction scope ctx = \case
  ActionCheck term ty -> block
    [ "typechecking"
    , "  " <> ppCore naming (stripPositions term)
    , "against type"
    , "  " <> ppValueIn scope ctx ty ]
  ActionInfer term -> block
    [ "inferring type for term"
    , "  " <> ppCore naming (stripPositions term) ]
  ActionConvert expected actual -> block
    [ "comparing expected"
    , "  " <> ppValueIn scope ctx expected
    , "with actual"
    , "  " <> ppValueIn scope ctx actual ]
  ActionEntails topes tope -> block
    [ "checking if local context"
    , intercalate "\n" (map (("  " <>) . ppTope naming) topes)
    , "is included in (entails) the tope"
    , "  " <> ppTope naming tope ]
  ActionEntailsUnion topes union -> block
    [ "checking if local context"
    , intercalate "\n" (map (("  " <>) . ppTope naming) topes)
    , "is included in (entails) the union of the topes"
    , intercalate "\n" (map (("  " <>) . ppTope naming) union) ]
  ActionCheckLetValue mname ->
    "checking the local definition " <> maybe "_" (Rzk.printTree . fromVarIdent) mname
  ActionCloseSection Nothing -> "closing the file and collecting assumptions (variables)"
  ActionCloseSection (Just name) -> "closing #section " <> name <> " and collecting assumptions (variables)"
  where
    naming = namingOfContext ctx
    block = intercalate "\n"

-- | A tope of the solver, shown with the context's names.
ppTope :: Naming n -> Tope n -> String
ppTope naming = go
  where
    go = \case
      TTop -> "⊤"
      TBot -> "⊥"
      TEq x y -> ppPoint naming x <> " ≡ " <> ppPoint naming y
      TLeq x y -> ppPoint naming x <> " ≤ " <> ppPoint naming y
      TAnd l r -> paren (go l) <> " ∧ " <> paren (go r)
      TOr l r -> paren (go l) <> " ∨ " <> paren (go r)
      TInv t -> "inv " <> paren (go t)
      TUninv t -> "uninv " <> paren (go t)
      TModal md t -> ppModality md <> " " <> paren (go t)
      TModApp md t -> "mod " <> ppModality md <> " " <> paren (go t)
      TNeutral p -> ppPoint naming p
      THole m -> maybe "?" (("?" <>) . show) m
    paren s = if ' ' `elem` s then "(" <> s <> ")" else s

ppPoint :: Naming n -> Point n -> String
ppPoint naming = go
  where
    go = \case
      PVar x _ -> ppName naming x
      PStar -> "*₁"
      P2_0 -> "0₂"
      P2_1 -> "1₂"
      PI_0 -> "0ᵢ"
      PI_1 -> "1ᵢ"
      PPair a b -> "(" <> go a <> ", " <> go b <> ")"
      PFst p -> "π₁ " <> paren (go p)
      PSnd p -> "π₂ " <> paren (go p)
      PSup a b -> "sup " <> paren (go a) <> " " <> paren (go b)
      PInf a b -> "inf " <> paren (go a) <> " " <> paren (go b)
      PFlip p -> "flip " <> paren (go p)
      PUnflip p -> "unflip " <> paren (go p)
      PModApp md p -> "mod " <> ppModality md <> " " <> paren (go p)
      PModExtract app inn p -> "$extract$ " <> ppModality app <> "/" <> ppModality inn <> " " <> paren (go p)
      PApp _ f a -> go f <> " " <> paren (go a)
      PHole m -> maybe "?" (("?" <>) . show) m
      POpaque _ -> "…"
    paren s = if ' ' `elem` s then "(" <> s <> ")" else s

ppModality :: TModality -> String
ppModality = \case
  Flat  -> "♭"
  Sharp -> "♯"
  Op    -> "ᵒᵖ"
  Id    -> "_id"

-- * Warnings

-- | A warning, rendered.
data Warning
  = OverhangWarning String String [String] (Maybe LocationInfo)
    -- ^ a restriction face or a recOR branch guard, the tope, the local
    -- tope context
  | TopeFamilyDomainWarning String String (Maybe LocationInfo)
    -- ^ the family, its declared domain
  | ShadowingWarning String [String] (Maybe LocationInfo)
    -- ^ a binder, and the binding it shadows, each with where it was written
  | UnsupportedOptionWarning String (Maybe LocationInfo)
    -- ^ an option that the new checker accepts but does not implement
  deriving (Eq, Show)

-- | The stable code of a warning.
warningTag :: Warning -> String
warningTag = \case
  OverhangWarning{}         -> "OverhangWarning"
  TopeFamilyDomainWarning{} -> "TopeFamilyDomainWarning"
  ShadowingWarning{}        -> "ShadowingWarning"
  UnsupportedOptionWarning{} -> "UnsupportedOptionWarning"

warningLocation :: Warning -> Maybe LocationInfo
warningLocation = \case
  OverhangWarning _ _ _ loc     -> loc
  TopeFamilyDomainWarning _ _ loc -> loc
  ShadowingWarning _ _ loc        -> loc
  UnsupportedOptionWarning _ loc   -> loc

ppWarning :: Warning -> String
ppWarning = \case
  OverhangWarning what tope topes _ -> intercalate "\n" $
    [ "Warning: " <> what <> " overhangs the local tope context"
    , "  " <> tope
    , "local tope context:" ] <> map ("  " <>) topes
  TopeFamilyDomainWarning family domain _ -> intercalate "\n"
    [ "Warning: the tope family"
    , "  " <> family
    , "is not included in its declared domain"
    , "  " <> domain
    , "and is read as their intersection" ]
  ShadowingWarning binder previous _ -> intercalate "\n" $
    [ "Warning: a binder shadows a name in scope"
    , "  " <> binder
    , "previously bound at" ] <> map ("  " <>) previous
  UnsupportedOptionWarning name _ ->
    "Warning: option " <> show name <> " is not supported by the new checker yet and has no effect"

-- * Hints

-- | A hint, rendered and independent of the scope where it was found.
data Hint
  = SimplificationHint String String (Maybe LocationInfo)
    -- ^ the original and suggested terms, rendered, and the source location
  deriving (Eq, Show)

-- | The stable code of a hint.
hintTag :: Hint -> String
hintTag = \case
  SimplificationHint{} -> "SimplificationHint"

hintLocation :: Hint -> Maybe LocationInfo
hintLocation = \case
  SimplificationHint _ _ loc -> loc

ppHint :: Hint -> String
ppHint = \case
  SimplificationHint original suggestion _ -> intercalate "\n"
    [ "Hint: consider replacing"
    , "  " <> original
    , "with the following"
    , "  " <> suggestion ]

-- * Holes

-- | The goal and the context at a hole, rendered.
data HoleInfo = HoleInfo
  { holeName     :: Maybe VarIdent
    -- ^ the optional user-supplied name of the hole
  , holeGoal     :: String
    -- ^ the rendered type expected at the hole
  , holeTermVars :: [(String, String)]
    -- ^ rendered names and types of local term variables
  , holeCubeVars :: [(String, String)]
    -- ^ rendered names and types of local cube variables
  , holeTopes    :: [String]
    -- ^ the rendered local tope context
  , holeLocation :: Maybe LocationInfo
    -- ^ where the hole occurs
  } deriving (Eq, Show)

ppHoleInfo :: HoleInfo -> String
ppHoleInfo info = unlines $
  [ "Hole" <> maybe "" ((" ?" <>) . show) (holeName info) <> maybe "" ppLoc (holeLocation info)
  , "  Goal: " <> holeGoal info ]
  <> [ "  " <> x <> " : " <> ty | (x, ty) <- holeCubeVars info <> holeTermVars info ]
  <> [ "  tope: " <> t | t <- holeTopes info ]
  where
    ppLoc (LocationInfo path line col) =
      " at " <> maybe "" id path <> maybe "" ((":" <>) . show) line <> maybe "" ((":" <>) . show) col
