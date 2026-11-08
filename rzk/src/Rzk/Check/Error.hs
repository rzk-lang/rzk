{-# LANGUAGE DataKinds         #-}
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
    -- * Holes
  , HoleInfo (..)
  , ppHoleInfo
  ) where

import           Control.Monad.Foil       (Distinct)
import qualified Control.Monad.Foil       as Foil
import           Control.Monad.Foil.Internal (NameMap (..))
import qualified Data.IntMap              as IntMap
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
  | TypeErrorUnsupported String
    -- ^ a construct the new checker does not cover yet
  | TypeErrorUnify (Core n) (Value n) (Value n)
    -- ^ the term, the expected type and its actual type
  | TypeErrorUnifyTerms (Value n) (Value n)
  | TypeErrorNotPair (Core n) (Value n)
  | TypeErrorNotFunction (Core n) (Value n)
  | TypeErrorUnexpectedLambda (Raw n) (Value n)
  | TypeErrorUnexpectedPair (Raw n) (Value n)
  | TypeErrorUnexpectedRefl (Raw n) (Value n)
  | TypeErrorCannotInferBareLambda (Raw n)
  | TypeErrorCannotInferBareRefl (Raw n)
  | TypeErrorCannotInferHole (Raw n)
  | TypeErrorUnsolvedHole (Maybe VarIdent) (Value n)
  | TypeErrorUndefined VarIdent
  | TypeErrorTopeNotSatisfied [Tope] Tope
  | TypeErrorTopeContextDisjoint Tope [Tope]
  | TypeErrorInvalidArgumentType (Raw n) (Value n)
  | TypeErrorDuplicateTopLevel [VarIdent] VarIdent
  | TypeErrorUnusedVariable (Foil.Name n) (Value n)
  | TypeErrorUnusedUsedVariables [Foil.Name n] (Foil.Name n)
  | TypeErrorImplicitAssumption (Foil.Name n, Value n) (Foil.Name n)
  | TypeErrorNotIntervalCube String (Value n) (Value n)
  | TypeErrorRepeatedBinder VarIdent [VarIdent]

-- | The constructor name: the stable code of a diagnostic, and the tag the
-- fixtures pin.
typeErrorTag :: TypeError n -> String
typeErrorTag = \case
  TypeErrorOther{}                -> "TypeErrorOther"
  TypeErrorUnsupported{}          -> "TypeErrorUnsupported"
  TypeErrorUnify{}                -> "TypeErrorUnify"
  TypeErrorUnifyTerms{}           -> "TypeErrorUnifyTerms"
  TypeErrorNotPair{}              -> "TypeErrorNotPair"
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
  ErrorInContext :: Distinct n => Context n -> TypeError n -> ErrorInContext

errorTag :: ErrorInContext -> String
errorTag (ErrorInContext _ err) = typeErrorTag err

errorLocation :: ErrorInContext -> Maybe LocationInfo
errorLocation (ErrorInContext ctx _) = ctxLocation ctx

ppTypeError :: Distinct n => Context n -> TypeError n -> String
ppTypeError ctx = \case
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
    ppV = ppValueIn ctx
    ppC = ppCore naming
    ppR = ppCore naming . stripPositions
    block = intercalate "\n"

-- | An error, with the context it was raised in.
ppErrorInContext :: ErrorInContext -> String
ppErrorInContext (ErrorInContext ctx err) = intercalate "\n"
  [ ppTypeError ctx err
  , ""
  , ppContext ctx ]

-- | The context an error was raised in: where, the tope context, the stack
-- of judgements, and the hypotheses.
ppContext :: Distinct n => Context n -> String
ppContext ctx = intercalate "\n" $ filter (not . null)
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
      [ "when " <> ppAction ctx action
      | action <- reverse (diagActions (ctxDiag ctx)) ]
  , intercalate "\n" $ "Definitions in context:" :
      [ "  " <> ppName naming name <> " : " <> ppValueIn ctx (entryType entry)
      | (name, entry) <- reverse (entriesInScope ctx) ]
  ]
  where
    naming = namingOfContext ctx

ppAction :: Distinct n => Context n -> Action n -> String
ppAction ctx = \case
  ActionCheck term ty -> block
    [ "typechecking"
    , "  " <> ppCore naming (stripPositions term)
    , "against type"
    , "  " <> ppValueIn ctx ty ]
  ActionInfer term -> block
    [ "inferring type for term"
    , "  " <> ppCore naming (stripPositions term) ]
  ActionConvert expected actual -> block
    [ "comparing expected"
    , "  " <> ppValueIn ctx expected
    , "with actual"
    , "  " <> ppValueIn ctx actual ]
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
ppTope :: Naming n -> Tope -> String
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

ppPoint :: Naming n -> Point -> String
ppPoint naming = go
  where
    go = \case
      PVar i _ -> nameById i
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
    nameById i = case IntMap.lookup i m of
      Just x  -> Rzk.printTree (fromVarIdent x)
      Nothing -> "x" <> show i
      where NameMap m = namingOf naming

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
  deriving (Eq, Show)

-- | The stable code of a warning.
warningTag :: Warning -> String
warningTag = \case
  OverhangWarning{}         -> "OverhangWarning"
  TopeFamilyDomainWarning{} -> "TopeFamilyDomainWarning"
  ShadowingWarning{}        -> "ShadowingWarning"

warningLocation :: Warning -> Maybe LocationInfo
warningLocation = \case
  OverhangWarning _ _ _ loc     -> loc
  TopeFamilyDomainWarning _ _ loc -> loc
  ShadowingWarning _ _ loc        -> loc

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

-- * Holes

-- | The goal and the context at a hole, rendered.
data HoleInfo = HoleInfo
  { holeName     :: Maybe VarIdent
  , holeGoal     :: String
  , holeTermVars :: [(String, String)]
  , holeCubeVars :: [(String, String)]
  , holeTopes    :: [String]
  , holeLocation :: Maybe LocationInfo
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
