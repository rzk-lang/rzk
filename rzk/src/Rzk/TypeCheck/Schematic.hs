{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Check schematic dRzk declarations without treating universes as object types.
-- Family arguments are checked at their indices. Schematic rules,
-- such as function extensionality, may also be parameters of a declaration.
-- Postulates supply trusted rules; checking their statements does not prove them.
module Rzk.TypeCheck.Schematic
  ( SchematicStatement, CheckedSchematicDeclaration, checkSchematicDeclaration
  , recordSchematicDeclaration
  ) where

import Control.Monad (forM_, unless, void, when)
import Control.Monad.Except (catchError)
import Control.Monad.Reader (asks, local)
import Control.Monad.State.Strict (gets)
import Data.Bifoldable (bifoldMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (isJust)
import Control.Monad.Foil (Distinct)
import qualified Control.Monad.Foil as Foil
import Control.Monad.Free.Foil (AST (..))
import Control.Monad.Free.Foil.Annotated (AnnSig (..))
import Language.Rzk.Foil.Names (TModality (..), TypeInfo (..), VarIdent)
import Language.Rzk.Foil.Syntax
import Rzk.TypeCheck.Context
import Rzk.TypeCheck.Display (namingOfContext, ppTerm)
import Rzk.TypeCheck.Error
import Rzk.TypeCheck.Eval
import Rzk.TypeCheck.Fragment.RSTT (fragmentHead)
import Rzk.TypeCheck.Monad

-- Constructors stay private: a statement alone does not certify its proof.
newtype SchematicStatement n = SchematicStatement (TermT n)
data CheckedSchematicDeclaration n = CheckedSchematicDeclaration
  (SchematicStatement n) (Maybe (TermT n))

-- | Roles of elaborated type and domain expressions in schematic declarations.
-- These are not universe levels. The fragment's formation conditions are
-- checked separately in "Rzk.TypeCheck.Fragment.RSTT".
data Sort
  = ObjectType
    -- ^ Ordinary types that may instantiate @(A : U)@, e.g. @Unit@ or @A -> A@.
  | ParameterKind
    -- ^ Kinds of schematic data: @U@, @CUBE@, @TOPE@, family kinds such as
    -- @A -> U@, and dependent sums containing non-object components.
  | SchematicRule
    -- ^ Function signatures ending in an object type and quantifying over
    -- kinds or rules, e.g. @(A : U) -> A -> A@.
  | Shape
    -- ^ Cube or tope expressions, e.g. @2@ or @TOP@; these are not object types.
  deriving (Eq)

-- | Validate an elaborated declaration and the definitions used by its proof.
-- This supplements dRzk typing with kind checks; it is not a separate RSTT kernel.
checkSchematicDeclaration
  :: Distinct n => TermT n -> Maybe (TermT n)
  -> TypeCheck n (CheckedSchematicDeclaration n)
checkSchematicDeclaration ty value = do
  declaration 256 IntSet.empty ty value
  pure (CheckedSchematicDeclaration (SchematicStatement ty) value)

-- | Apply the selected safe-mode policy and return whether validation succeeded.
recordSchematicDeclaration
  :: Distinct n => VarIdent -> TermT n -> Maybe (TermT n) -> TypeCheck n SchematicStatus
recordSchematicDeclaration name ty value = do
  enabled <- asks rsttSafeEnabled
  if not enabled then pure SchematicUnchecked else
    (checkSchematicDeclaration ty value >> pure SchematicChecked) `catchError` \err -> do
      case err of
        TypeErrorInScopedContext _ (TypeErrorSchematic problem reason) -> do
          loc <- asks ctxLocation
          warnings <- gets logWarningsRev
          -- Source checking already gives more precise locations for these.
          let alreadyReported = problem == SchematicUnsupported && any (sameDeclaration loc) warnings
          unless alreadyReported $ recordCheckWarning (RSTTScopeWarning RSTTSchematic
            (reason <> " (in " <> show name <> ")") loc)
        _ -> reportIncompleteRSTTCheck ("schematic check incomplete in " <> show name) err
      pure SchematicRejected

sameDeclaration :: Maybe LocationInfo -> CheckWarning -> Bool
sameDeclaration loc warning = withinDeclaration && case warning of
  RSTTHoleWarning{} -> True
  RSTTScopeWarning RSTTModal _ _ -> True
  RSTTScopeWarning RSTTInductive _ _ -> True
  RSTTScopeWarning RSTTInterval _ _ -> True
  RSTTScopeWarning RSTTUnsupported _ _ -> True
  _ -> False
  where
    withinDeclaration = case (loc, warningLocation warning) of
      (Just start, Just at) -> locationFilePath start == locationFilePath at &&
        locationLine at >= locationLine start
      _ -> loc == warningLocation warning

invalid :: Distinct n => String -> TermT n -> TypeCheck n a
invalid = schematicError SchematicKindMismatch

schematicError :: Distinct n => SchematicProblem -> String -> TermT n -> TypeCheck n a
schematicError problem reason t = do
  naming <- asks namingOfContext
  issueTypeError (TypeErrorSchematic problem (reason <> ": " <> ppTerm naming (untyped t)))

budget :: Distinct n => Int -> TypeCheck n ()
budget fuel = when (fuel <= 0) $
  issueTypeError (TypeErrorOther "schematic validation limit reached")

-- Check a declaration telescope separately: a definition may name a family kind
-- or a schema, whereas an argument to a type parameter must be an object type.
declaration :: Distinct n => Int -> IntSet.IntSet -> TermT n -> Maybe (TermT n) -> TypeCheck n ()
declaration fuel seen ty value = do
  budget fuel
  -- Check dependencies before reduction can obscure their failures.
  dependencies ty
  signature fuel seen ty
  mapM_ (\t -> dependencies t >> term fuel seen t) value
  where
    dependencies = mapM_ (uses fuel seen . Var) . freeVarsOfTermT

signature :: Distinct n => Int -> IntSet.IntSet -> TermT n -> TypeCheck n ()
signature fuel seen ty = do
  budget fuel
  uses fuel seen ty
  inspectRedex fuel seen ty
  fragmentHead fuel ty >>= \case
    TypeFunT _ orig md dom guard body -> do
      void (classify (fuel - 1) seen dom)
      mapM_ (\g -> inScope orig md dom g (term (fuel - 1) seen)) guard
      inScope orig md dom body (signature (fuel - 1) seen)
    t -> void (classify (fuel - 1) seen t)

-- Check each dependency before using its signature, even if safe mode was off
-- when it was declared. Successful earlier checks are cached in the context.
-- Check variables during traversal instead of rescanning every subtree.
uses :: Distinct n => Int -> IntSet.IntSet -> TermT n -> TypeCheck n ()
uses fuel seen (Var v) = do
  info <- infoOfVar id v
  cached <- gets (IntSet.member (Foil.nameId v) . logSchematicCache)
  when (varIsTopLevel info && varSchematicStatus info /= SchematicChecked && not cached) $ do
    when (varSchematicStatus info == SchematicRejected) $
      schematicError SchematicDependency "dependency failed schematic validation" (Var v)
    when (isJust (varDataRole info)) $
      schematicError SchematicUnsupported "inductive dependency" (Var v)
    budget fuel
    when (IntSet.member (Foil.nameId v) seen) $
      issueTypeError (TypeErrorOther "cyclic schematic dependency")
    -- A caller's tope assumptions cannot justify a reusable dependency check.
    local withoutCallerTopes $ declaration (fuel - 1) (IntSet.insert (Foil.nameId v) seen)
      (varType info) (varValue info)
    modifyLog $ \checkLog' -> checkLog'
      { logSchematicCache = IntSet.insert (Foil.nameId v) (logSchematicCache checkLog') }
uses _ _ _ = pure ()

withoutCallerTopes :: Context n -> Context n
withoutCallerTopes ctx = ctx
  { ctxDiscreteTopes = [], ctxTopes = [], ctxTopesNF = []
  , ctxTopesNFUnion = [[]], ctxTopesEntailBottom = Just False
  , ctxTopesSaturated = SaturationUncached
  }

-- Check erased arguments and annotations before exposing the result.
inspectRedex :: Distinct n => Int -> IntSet.IntSet -> TermT n -> TypeCheck n ()
inspectRedex fuel seen original = case original of
  LetT{} -> term fuel seen original
  TypeAscT{} -> term fuel seen original
  FirstT{} -> term fuel seen original
  SecondT{} -> term fuel seen original
  IdJT{} -> term fuel seen original
  RecOrT{} -> term fuel seen original
  _ -> applications fuel seen original

classify :: Distinct n => Int -> IntSet.IntSet -> TermT n -> TypeCheck n Sort
classify fuel seen original = do
  budget fuel
  uses fuel seen original
  inspectRedex fuel seen original
  knownObjectFamily fuel seen original >>= \case
    True -> pure ObjectType
    False -> classifyHead fuel seen original

classifyHead :: Distinct n => Int -> IntSet.IntSet -> TermT n -> TypeCheck n Sort
classifyHead fuel seen original =
  fragmentHead fuel original >>= \case
    UniverseT{} -> pure ParameterKind
    UniverseCubeT{} -> pure ParameterKind
    UniverseTopeT{} -> pure ParameterKind
    TypeFunT _ orig md dom guard body -> do
      d <- classify (fuel - 1) seen dom
      mapM_ (\g -> inScope orig md dom g (term (fuel - 1) seen)) guard
      r <- inScope orig md dom body (classify (fuel - 1) seen)
      case (d, r) of
        (ObjectType, ObjectType) -> pure ObjectType
        (Shape, ObjectType) -> pure ObjectType
        (_, ParameterKind) -> pure ParameterKind
        (_, Shape) -> invalid "cube or shape family" original
        _ -> pure SchematicRule
    TypeSigmaT _ orig md a b -> do
      d <- classify (fuel - 1) seen a
      r <- inScope orig md a b (classify (fuel - 1) seen)
      pure (if d == ObjectType && r == ObjectType then ObjectType else ParameterKind)
    TypeIdT _ a mt b -> do
      ty <- maybe (schematicTypeOf fuel a) pure mt
      object (fuel - 1) seen ty
      term (fuel - 1) seen a
      term (fuel - 1) seen b
      pure ObjectType
    TypeUnitT{} -> pure ObjectType
    RecBottomT{} -> pure ObjectType
    RecOrT _ branches -> do
      sorts <- mapM (\(p, v) -> localTope p (classify (fuel - 1) seen v)) branches
      case sorts of
        s : rest | all (== s) rest -> pure s
        _ -> invalid "case split between different schematic kinds" original
    TypeRestrictedT _ ty rs -> do
      s <- classify (fuel - 1) seen ty
      forM_ rs $ \(p, v) -> term (fuel - 1) seen p >> argument (fuel - 1) seen ty v
      pure s
    CubeUnitT{} -> pure Shape
    Cube2T{} -> pure Shape
    CubeProductT _ a b -> term (fuel - 1) seen a >> term (fuel - 1) seen b >> pure Shape
    t -> do
      termNeutral (fuel - 1) seen t
      schematicTypeOf fuel t >>= fragmentHead fuel >>= \case
        UniverseT{} -> pure ObjectType
        UniverseCubeT{} -> pure Shape
        UniverseTopeT{} -> pure Shape
        _ -> invalid "expected an object type, parameter kind, or schematic rule" t

-- Reuse only universally object-valued families, after checking actual arguments.
-- Other families retain reduction-based classification in the caller's context.
knownObjectFamily :: Distinct n => Int -> IntSet.IntSet -> TermT n -> TypeCheck n Bool
knownObjectFamily fuel seen = spine 0
  where
    spine arity (AppT _ f _) = spine (arity + 1) f
    spine arity (Var v) = do
      info <- infoOfVar id v
      if not (varIsTopLevel info) then pure False else do
        let key = Foil.nameId v
        cached <- gets (IntMap.lookup key . logSchematicObjectFamilies)
        summary <- case cached of
          Just result -> pure result
          Nothing -> do
            cache key Nothing
            result <- (local withoutCallerTopes $
              objectFamilyArity fuel seen 0 (varType info) (Var v))
              `catchError` \_ -> pure Nothing
            cache key result
            pure result
        pure (summary == Just arity)
    spine _ _ = pure False
    cache key result = modifyLog $ \checkLog' -> checkLog'
      { logSchematicObjectFamilies = IntMap.insert key result (logSchematicObjectFamilies checkLog') }

-- Probe at fresh parameters; never generalise a concrete instance.
objectFamilyArity :: Distinct n => Int -> IntSet.IntSet -> Int -> TermT n -> TermT n -> TypeCheck n (Maybe Int)
objectFamilyArity fuel seen arity ty value = do
  budget fuel
  fragmentHead fuel ty >>= \case
    TypeFunT _ orig md dom _ body ->
      inScope orig md dom body $ \cod -> do
        bound <- asks ctxBound
        case bound of
          v : _ -> objectFamilyArity (fuel - 1) seen (arity + 1) cod
            (appT cod (Foil.sink value) (Var v))
          [] -> issueTypeError (TypeErrorOther "missing schematic index")
    UniverseT{} -> do
      s <- classify (fuel - 1) seen value
      pure (if s == ObjectType then Just arity else Nothing)
    _ -> pure Nothing

object :: Distinct n => Int -> IntSet.IntSet -> TermT n -> TypeCheck n ()
object fuel seen t = do
  s <- classify (fuel - 1) seen t
  unless (s == ObjectType) (invalid "expected an object type" t)

-- A type-family argument is checked pointwise. In particular U, A -> U, and
-- (A : U) -> A cannot instantiate a parameter declared simply as A : U.
argument :: Distinct n => Int -> IntSet.IntSet -> TermT n -> TermT n -> TypeCheck n ()
argument fuel seen dom arg = do
  budget fuel
  functionHead fuel dom >>= \case
    UniverseT{} -> object (fuel - 1) seen arg
    TypeFunT _ orig md a guard body -> do
      s <- classify (fuel - 1) seen dom
      if s == ParameterKind then do
        -- Eta-expand only for checking; the ordinary checker has checked the
        -- argument's type. The body is inspected with the same fresh index.
        inScope orig md a body $ \cod -> do
          ctx <- asks id
          case ctxBound ctx of
            v : _ -> argument (fuel - 1) seen cod (appT cod (Foil.sink arg) (Var v))
            [] -> issueTypeError (TypeErrorOther "missing schematic index")
        mapM_ (\g -> inScope orig md a g (term (fuel - 1) seen)) guard
      else term (fuel - 1) seen arg
    _ -> term (fuel - 1) seen arg

-- Section abstraction can leave application annotations describing an earlier
-- telescope. Derive elimination types from the current signatures instead.
-- TODO: Share this reconstruction with Eval when repairing annotations there.
schematicTypeOf :: Distinct n => Int -> TermT n -> TypeCheck n (TermT n)
schematicTypeOf fuel t = do
  budget fuel
  case t of
    AppT _ f x -> schematicTypeOf (fuel - 1) f >>= functionHead fuel >>= \case
      TypeFunT _ _ _ _ _ ret -> instantiate ret x
      _ -> invalid "application without a function type" f
    FirstT _ p -> schematicTypeOf (fuel - 1) p >>= functionHead fuel >>= \case
      TypeSigmaT _ _ _ a _ -> pure a
      CubeProductT _ a _ -> pure a
      _ -> invalid "projection without a product type" p
    SecondT _ p -> schematicTypeOf (fuel - 1) p >>= functionHead fuel >>= \case
      TypeSigmaT _ _ _ a b -> instantiate b (firstT a p)
      CubeProductT _ _ b -> pure b
      _ -> invalid "projection without a product type" p
    _ -> typeOfUncomputed t

-- Boundaries constrain a function without changing its parameter kinds.
functionHead :: Distinct n => Int -> TermT n -> TypeCheck n (TermT n)
functionHead fuel t = do
  budget fuel
  fragmentHead fuel t >>= \case
    TypeRestrictedT _ base _ -> functionHead (fuel - 1) base
    head' -> pure head'

applications :: Distinct n => Int -> IntSet.IntSet -> TermT n -> TypeCheck n ()
applications fuel seen (AppT _ f x) = do
  budget fuel
  term (fuel - 1) seen f
  schematicTypeOf fuel f >>= functionHead fuel >>= \case
    TypeFunT _ _ _ dom _ _ -> argument (fuel - 1) seen dom x
    _ -> invalid "application without a schematic or object function type" f
applications _ _ _ = pure ()

term :: Distinct n => Int -> IntSet.IntSet -> TermT n -> TypeCheck n ()
term fuel seen t = do
  budget fuel
  uses fuel seen t
  case t of
    TypeFunT{} -> void (classify (fuel - 1) seen t)
    TypeSigmaT{} -> void (classify (fuel - 1) seen t)
    TypeIdT{} -> void (classify (fuel - 1) seen t)
    TypeRestrictedT{} -> void (classify (fuel - 1) seen t)
    LambdaT info orig param body -> do
      (md, dom) <- case param of
        Just (LambdaParam md dom guard) -> do
          mapM_ (\g -> inScope orig md dom g (term (fuel - 1) seen)) guard
          pure (md, dom)
        Nothing -> functionHead fuel (infoType info) >>= \case
          TypeFunT _ _ md dom _ _ -> pure (md, dom)
          _ -> invalid "lambda without a function type" t
      void (classify (fuel - 1) seen dom)
      inScope orig md dom body (term (fuel - 1) seen)
    AppT{} -> applications fuel seen t
    LetT _ orig annotation value body -> do
      mapM_ (signature (fuel - 1) seen) annotation
      term (fuel - 1) seen value
      ty <- maybe (schematicTypeOf fuel value) pure annotation
      inScopeWith orig Id ty (Just value) body (term (fuel - 1) seen)
    TypeAscT _ value ty -> signature (fuel - 1) seen ty >> term (fuel - 1) seen value
    IdJT _ a x motive d y p -> do
      object (fuel - 1) seen a
      -- J eliminates equality into object types, not into schematic kinds.
      motiveType <- schematicTypeOf fuel motive
      argument (fuel - 1) seen motiveType motive
      mapM_ (term (fuel - 1) seen) [x, motive, d, y, p]
    _ -> termNeutral fuel seen t

-- This is deliberately an allow-list. New syntax cannot inherit certification.
termNeutral :: Distinct n => Int -> IntSet.IntSet -> TermT n -> TypeCheck n ()
termNeutral fuel seen t = case t of
  Var{} -> pure ()
  UniverseT{} -> pure ()
  UniverseCubeT{} -> pure ()
  UniverseTopeT{} -> pure ()
  CubeUnitT{} -> pure ()
  CubeUnitStarT{} -> pure ()
  Cube2T{} -> pure ()
  Cube2_0T{} -> pure ()
  Cube2_1T{} -> pure ()
  TypeUnitT{} -> pure ()
  UnitT{} -> pure ()
  TopeTopT{} -> pure ()
  TopeBottomT{} -> pure ()
  CubeProductT{} -> children
  CubeSupT{} -> children
  CubeInfT{} -> children
  TopeEQT{} -> children
  TopeLEQT{} -> children
  TopeAndT{} -> children
  TopeOrT{} -> children
  PairT _ x y -> do
    ty <- schematicTypeOf fuel t
    functionHead fuel ty >>= \case
      TypeSigmaT _ _ _ a b -> do
        argument (fuel - 1) seen a x
        cod <- instantiate b x
        argument (fuel - 1) seen cod y
      CubeProductT{} -> children
      _ -> invalid "pair without a product type" t
  FirstT{} -> children
  SecondT{} -> children
  ReflT _ mx -> forM_ mx $ \(x, mty) -> do
    ty <- maybe (schematicTypeOf fuel x) pure mty
    object (fuel - 1) seen ty
    term (fuel - 1) seen x
  -- Elaboration checks the absurd tope context; recBOT has a cyclic type annotation.
  RecBottomT{} -> pure ()
  RecOrT{} -> children
  AppT{} -> applications fuel seen t
  _ -> schematicError SchematicUnsupported "unsupported schematic expression" t
  where
    children = case t of
      Node (AnnSig _ sig) -> mapM_ (term (fuel - 1) seen) (bifoldMap (const []) (:[]) sig)
      Var{} -> pure ()
