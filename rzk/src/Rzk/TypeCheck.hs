{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-orphans #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Rzk.TypeCheck where

import           Control.Applicative      ((<|>))
import           Control.Monad            (forM, forM_, join, unless, void, when)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer.Strict (Writer, runWriter, tell)
import qualified Control.Monad.State.Strict as State
import           Data.Bifoldable          (bifoldMap, bifoldr)
import           Data.Bifunctor           (first)
import           Data.List                (intercalate, intersect, nub, tails,
                                           (\\))
import           Data.Maybe               (catMaybes, fromMaybe, isNothing,
                                           mapMaybe)
import           Data.String              (IsString (..))
import           Data.Tuple               (swap)

import           Free.Scoped
import           Rzk.Declaration
import           Language.Rzk.Free.Syntax
import qualified Language.Rzk.Syntax      as Rzk

import           Debug.Trace
import           Unsafe.Coerce

-- $setup
-- >>> :set -XOverloadedStrings

-- | Parse and 'unsafeInferStandalone''.
instance IsString TermT' where
  fromString = unsafeInferStandalone' . fromString

defaultTypeCheck
  :: TypeCheck var a
  -> Either (TypeErrorInScopedContext var) a
defaultTypeCheck tc =
  let (result, warnings) = defaultTypeCheckWithWarnings tc
  in foldr (trace . ("Warning: " <>) . ppCheckWarning) result warnings

-- | Run the checker and return warnings separately.
defaultTypeCheckWithWarnings
  :: TypeCheck var a
  -> (Either (TypeErrorInScopedContext var) a, [CheckWarning])
defaultTypeCheckWithWarnings tc =
  let ((result, _), warnings) = runWriter (State.runStateT (runExceptT (runReaderT tc emptyContext)) emptySchematicCache)
  in (result, nub warnings)

-- The tope-family warning and its deduplication are adapted from
-- https://github.com/rzk-lang/rzk/pull/362.
data CheckWarning
  = TopeFamilyDomainWarning String String (Maybe LocationInfo)
  | MetaPrefixWarning VarIdent VarIdent Int Int MetaPrefixRule (Maybe LocationInfo)
  | FreeStandingRestrictionWarning VarIdent String FragmentUse (Maybe LocationInfo)
  | MetaBinderWarning VarIdent VarIdent String (Maybe LocationInfo)
  | ShapeDependencyWarning String (Maybe LocationInfo)
  | RSTTIncompleteWarning String (Maybe LocationInfo)
  | RSTTSchematicWarning String (Maybe LocationInfo)
  deriving (Eq)

data FragmentUse = UseBinder | UseMotive | UseData | UseIdentity | UseConcluded
  deriving (Eq, Show)

data MetaPrefixRule = MetaPrefixBoth | MetaPrefixStrictOnly
  deriving (Eq, Show)

data MetaPrefixSensitivity = MetaPrefixOff | MetaPrefixStructural | MetaPrefixStrict
  deriving (Eq, Show)

ppCheckWarning :: CheckWarning -> String
ppCheckWarning (TopeFamilyDomainWarning family domain _) =
  "tope family " <> family <> " is not included in its declared domain "
    <> domain <> ". It is read as its intersection with the domain"
ppCheckWarning (MetaPrefixWarning defName usedName supplied required rule _) =
  "meta-parameter layer: " <> show usedName <> " is used with "
    <> show supplied <> " of its " <> show required <> " meta-prefix arguments"
    <> (case rule of
          MetaPrefixBoth -> " at an object-level position"
          MetaPrefixStrictOnly -> " outside another declaration's meta prefix")
    <> " (in " <> show defName <> ")"
ppCheckWarning (FreeStandingRestrictionWarning defName ty use _) =
  "free-standing restriction " <> ty <> " in " <> place use
    <> " (in " <> show defName <> ")"
  where
    place UseBinder = "a binder type"
    place UseMotive = "an eliminator motive"
    place UseData = "a type passed as data"
    place UseIdentity = "an identity type"
    place UseConcluded = "a concluded type, off the spine of codomains"
ppCheckWarning (MetaBinderWarning defName name ty _) =
  "schematic binder " <> show name <> " : " <> ty
    <> " inside a term (in " <> show defName <> ")"
ppCheckWarning (ShapeDependencyWarning reason _) = reason
ppCheckWarning (RSTTIncompleteWarning reason _) = reason
ppCheckWarning (RSTTSchematicWarning reason _) = reason

-- FIXME: merge with VarInfo
data Decl var = Decl
  { declName         :: var
  , declType         :: TermT var
  , declValue        :: Maybe (TermT var)
  , declIsAssumption :: Bool
  , declUsedVars     :: [var]
  , declLocation     :: Maybe LocationInfo
  } deriving Eq

type Decl' = Decl VarIdent

typecheckModulesWithLocationIncremental
  :: [(FilePath, [Decl'])]    -- ^ Cached declarations (only those that do not need rechecking).
  -> [(FilePath, Rzk.Module)] -- ^ New modules to check
  -> TypeCheck VarIdent ([(FilePath, [Decl'])], [TypeErrorInScopedContext VarIdent])
typecheckModulesWithLocationIncremental cached modulesToTypecheck = do
  let decls = foldMap snd cached
  localDeclsPrepared decls $ do
    (checked, errors) <- typecheckModulesWithLocation' modulesToTypecheck
    return (cached <> checked, errors)

typecheckModulesWithLocation' :: [(FilePath, Rzk.Module)] -> TypeCheck VarIdent ([(FilePath, [Decl'])], [TypeErrorInScopedContext VarIdent])
typecheckModulesWithLocation' = \case
  [] -> return ([], [])
  m@(path, _) : ms -> do
    (decls, errs) <- typecheckModuleWithLocation m
    case errs of
      _:_ -> return ([(path, decls)], errs)
      _ -> do
        localDeclsPrepared decls $ do
          (decls', errors) <- typecheckModulesWithLocation' ms
          return ((path, decls) : decls', errors)

typecheckModulesWithLocation :: [(FilePath, Rzk.Module)] -> TypeCheck VarIdent [(FilePath, [Decl'])]
typecheckModulesWithLocation = \case
  [] -> return []
  m@(path, _) : ms -> do
    (decls, errs) <- typecheckModuleWithLocation m
    case errs of
      err : _ -> do
        throwError err
      [] -> localDeclsPrepared decls $
        ((path, decls) :) <$> typecheckModulesWithLocation ms

typecheckModules :: [Rzk.Module] -> TypeCheck VarIdent [Decl']
typecheckModules = \case
  [] -> return []
  m : ms -> do
    (decls, errs) <- typecheckModule Nothing m
    case errs of
      err : _ -> do
        throwError err
      _ -> do
        localDeclsPrepared decls $
          (decls <>) <$> typecheckModules ms

typecheckModuleWithLocation :: (FilePath, Rzk.Module) -> TypeCheck VarIdent ([Decl'], [TypeErrorInScopedContext VarIdent])
typecheckModuleWithLocation (path, module_) = do
  traceTypeCheck Normal ("Checking module from " <> path) $ do
    withLocation (LocationInfo { locationFilePath = Just path, locationLine = Nothing }) $
      typecheckModule (Just path) module_

countCommands :: Integral a => [Rzk.Command] -> a
countCommands = fromIntegral . length

typecheckModule :: Maybe FilePath -> Rzk.Module -> TypeCheck VarIdent ([Decl'], [TypeErrorInScopedContext VarIdent])
typecheckModule path (Rzk.Module _moduleLoc _lang commands) =
  withSection Nothing (go 1 commands) $ -- FIXME: use module name? or anonymous section?
    return ([], [])
  where
    totalCommands = countCommands commands

    go :: Integer -> [Rzk.Command] -> TypeCheck VarIdent ([Decl'], [TypeErrorInScopedContext VarIdent])
    go _i [] = return ([], [])

    go  i (command@(Rzk.CommandUnsetOption _loc optionName) : moreCommands) = do
      traceTypeCheck Normal ("[ " <> show i <> " out of " <> show totalCommands <> " ]"
          <> " Unsetting option " <> optionName) $ do
        withCommand command $ do
          unsetOption optionName $
            go (i + 1) moreCommands

    go  i (command@(Rzk.CommandSetOption _loc optionName optionValue) : moreCommands) = do
      traceTypeCheck Normal ("[ " <> show i <> " out of " <> show totalCommands <> " ]"
          <> " Setting option " <> optionName <> " = " <> optionValue ) $ do
        withCommand command $ do
          setOption optionName optionValue $
            go (i + 1) moreCommands

    go  i (command@(Rzk.CommandDefine _loc name (Rzk.DeclUsedVars _ vars) params ty term) : moreCommands) =
      traceTypeCheck Normal ("[ " <> show i <> " out of " <> show totalCommands <> " ]"
          <> " Checking #define " <> Rzk.printTree name ) $ do
        withCommand command $ do
          mapM_ checkDefinedVar (varIdentAt path <$> vars)
          paramDecls <- concat <$> mapM paramToParamDecl params
          checkedType <- typecheck (toTerm' (addParamDecls paramDecls ty)) universeT
          ty' <- whnfT checkedType
          checkedTerm <- typecheck (toTerm' (addParams params term)) ty'
          recordMetaPrefixUses (varIdentAt path name) checkedType (Just checkedTerm)
          recordFragmentUses (varIdentAt path name) checkedType (Just checkedTerm) False
          recordSchematicDeclaration (varIdentAt path name) checkedType (Just checkedTerm)
          term' <- memoizeWHNF checkedTerm
          loc <- asks location
          let decl = Decl (varIdentAt path name) (cacheWHNF checkedType ty') (Just term') False (varIdentAt path <$> vars) loc
          fmap (first (decl :)) $
            localDeclPrepared decl $ do
              Context{..} <- ask
              termSVG <-
                case renderBackend of
                  Just RenderSVG -> renderTermSVG (Pure (varIdentAt path name))
                  Just RenderLaTeX -> issueTypeError $ TypeErrorOther "\"latex\" rendering is not yet supported"
                  Nothing -> pure Nothing
              maybe id trace termSVG $ do
                go (i + 1) moreCommands

    go  i (command@(Rzk.CommandPostulate _loc name (Rzk.DeclUsedVars _ vars) params ty) : moreCommands) =
      traceTypeCheck Normal ("[ " <> show i <> " out of " <> show totalCommands <> " ]"
          <> " Checking #postulate " <> Rzk.printTree name) $ do
        withCommand command $ do
          mapM_ checkDefinedVar (varIdentAt path <$> vars)
          paramDecls <- concat <$> mapM paramToParamDecl params
          checkedType <- typecheck (toTerm' (addParamDecls paramDecls ty)) universeT
          recordMetaPrefixUses (varIdentAt path name) checkedType Nothing
          recordFragmentUses (varIdentAt path name) checkedType Nothing True
          recordSchematicDeclaration (varIdentAt path name) checkedType Nothing
          ty' <- whnfT checkedType
          loc <- asks location
          let decl = Decl (varIdentAt path name) (cacheWHNF checkedType ty') Nothing False (varIdentAt path <$> vars) loc
          fmap (first (decl :)) $
            localDeclPrepared decl $
              go (i + 1) moreCommands

    go  i (command@(Rzk.CommandCheck _loc term ty) : moreCommands) =
      traceTypeCheck Normal ("[ " <> show i <> " out of " <> show totalCommands <> " ]"
          <> " Checking " <> Rzk.printTree term <> " : " <> Rzk.printTree ty ) $ do
        withCommand command $ do
          checkedType <- typecheck (toTerm' ty) universeT
          ty' <- whnfT checkedType
          term' <- typecheck (toTerm' term) ty'
          recordMetaPrefixUses "#check" checkedType (Just term')
          recordFragmentUses "#check" checkedType (Just term') False
          recordSchematicDeclaration "#check" checkedType (Just term')
          go (i + 1) moreCommands

    go  i (Rzk.CommandCompute loc term : moreCommands) =
      go i (Rzk.CommandComputeWHNF loc term : moreCommands)

    go  i (command@(Rzk.CommandComputeNF _loc term) : moreCommands) =
      traceTypeCheck Normal ("[ " <> show i <> " out of " <> show totalCommands <> " ]"
          <> " Computing NF for " <> Rzk.printTree term) $ do
        withCommand command $ do
          checkedTerm <- infer (toTerm' term)
          checkedType <- typeOfUncomputed checkedTerm
          recordMetaPrefixUses "#compute-nf" checkedType (Just checkedTerm)
          recordFragmentUses "#compute-nf" checkedType (Just checkedTerm) False
          recordSchematicDeclaration "#compute-nf" checkedType (Just checkedTerm)
          term' <- nfT checkedTerm
          traceTypeCheck Normal ("  " <> show (untyped term')) $ do
            go (i + 1) moreCommands

    go  i (command@(Rzk.CommandComputeWHNF _loc term) : moreCommands) =
      traceTypeCheck Normal ("[ " <> show i <> " out of " <> show totalCommands <> " ]"
          <> " Computing WHNF for " <> Rzk.printTree term) $ do
        withCommand command $ do
          checkedTerm <- infer (toTerm' term)
          checkedType <- typeOfUncomputed checkedTerm
          recordMetaPrefixUses "#compute" checkedType (Just checkedTerm)
          recordFragmentUses "#compute" checkedType (Just checkedTerm) False
          recordSchematicDeclaration "#compute" checkedType (Just checkedTerm)
          term' <- whnfT checkedTerm
          traceTypeCheck Normal ("  " <> show (untyped term')) $ do
            go (i + 1) moreCommands

    go  i (command@(Rzk.CommandAssume _loc names ty) : moreCommands) =
      traceTypeCheck Normal ("[ " <> show i <> " out of " <> show totalCommands <> " ]"
          <> " Checking #assume " <> intercalate " " [ Rzk.printTree name | name <- names ] ) $ do
        withCommand command $ do
          ty' <- typecheck (toTerm' ty) universeT
          forM_ names $ \name -> recordMetaPrefixUses (varIdentAt path name) ty' Nothing
          forM_ names $ \name -> recordFragmentUses (varIdentAt path name) ty' Nothing True
          forM_ names $ \name -> recordSchematicDeclaration (varIdentAt path name) ty' Nothing
          loc <- asks location
          let decls = [ Decl (varIdentAt path name) ty' Nothing True [] loc | name <- names ]
          fmap (first (decls <>)) $
            localDeclsPrepared decls $
              go (i + 1) moreCommands

    go  i (command@(Rzk.CommandSection _loc name) : moreCommands) = do
      withCommand command $ do
        (sectionCommands, moreCommands') <- splitSectionCommands name moreCommands
        withSection (Just name) (go i sectionCommands) $ do
          go (i + countCommands sectionCommands) moreCommands'

    go  _i (command@(Rzk.CommandSectionEnd _loc endName) : _moreCommands) = do
      withCommand command $
        issueTypeError $ TypeErrorOther $
          "unexpected #end " <> Rzk.printTree endName <> ", no section was declared!"


splitSectionCommands :: Rzk.SectionName -> [Rzk.Command] -> TypeCheck var ([Rzk.Command], [Rzk.Command])
splitSectionCommands name [] =
  issueTypeError (TypeErrorOther $ "Section " <> Rzk.printTree name <> " is not closed with an #end")
splitSectionCommands name (Rzk.CommandSection _loc name' : moreCommands) = do
  (cs1, cs2) <- splitSectionCommands name' moreCommands
  (cs3, cs4) <- splitSectionCommands name cs2
  return (cs1 <> cs3, cs4)
splitSectionCommands name (Rzk.CommandSectionEnd _loc endName : moreCommands) = do
  when (Rzk.printTree name /= Rzk.printTree endName) $
    issueTypeError $ TypeErrorOther $
      "unexpected #end " <> Rzk.printTree endName <> ", expecting #end " <> Rzk.printTree name
  return ([], moreCommands)
splitSectionCommands name (command : moreCommands) = do
  (cs1, cs2) <- splitSectionCommands name moreCommands
  return (command : cs1, cs2)

setOption :: String -> String -> TypeCheck var a -> TypeCheck var a
setOption "rstt-safe" = \case
  "off" -> localRSTTSafeMode RSTTSafeOff
  "warn" -> localRSTTSafeMode RSTTSafeWarn
  "error" -> localRSTTSafeMode RSTTSafeError
  _ -> const $ issueTypeError $ TypeErrorOther "unknown rstt-safe mode (use off, warn, or error)"
setOption "verbosity" = \case
  "debug"   -> localVerbosity Debug
  "normal"  -> localVerbosity Normal
  "silent"  -> localVerbosity Silent
  _ -> const $
    issueTypeError $ TypeErrorOther "unknown verbosity level (use \"debug\", \"normal\", or \"silent\")"
setOption "render" = \case
  "svg"   -> localRenderBackend (Just RenderSVG)
  "latex" -> localRenderBackend (Just RenderLaTeX)
  "none"  -> localRenderBackend Nothing
  _ -> const $
    issueTypeError $ TypeErrorOther "unknown render backend (use \"svg\", \"latex\", or \"none\")"
setOption "warn-tope-family-domain" = \case
  "yes" -> localWarnTopeFamilyDomain True
  "no"  -> localWarnTopeFamilyDomain False
  _ -> const $
    issueTypeError $ TypeErrorOther "unknown value for \"warn-tope-family-domain\" (use \"yes\" or \"no\")"
setOption "warn-meta-prefix" = \case
  "off" -> localMetaPrefixSensitivity MetaPrefixOff
  "structural" -> localMetaPrefixSensitivity MetaPrefixStructural
  "strict" -> localMetaPrefixSensitivity MetaPrefixStrict
  _ -> const $
    issueTypeError $ TypeErrorOther "unknown value for \"warn-meta-prefix\" (use \"off\", \"structural\", or \"strict\")"
setOption "warn-free-standing-restriction" = \case
  "yes" -> localWarnFreeStandingRestriction True
  "no" -> localWarnFreeStandingRestriction False
  _ -> const $
    issueTypeError $ TypeErrorOther "unknown value for \"warn-free-standing-restriction\" (use \"yes\" or \"no\")"
setOption "warn-shape-dependency" = \case
  "yes" -> localWarnShapeDependency True
  "no" -> localWarnShapeDependency False
  _ -> const $
    issueTypeError $ TypeErrorOther "unknown value for \"warn-shape-dependency\" (use \"yes\" or \"no\")"
setOption "warn-meta-binder" = \case
  "yes" -> localWarnMetaBinder True
  "no" -> localWarnMetaBinder False
  _ -> const $
    issueTypeError $ TypeErrorOther "unknown value for \"warn-meta-binder\" (use \"yes\" or \"no\")"
setOption optionName = const $ const $
  issueTypeError $ TypeErrorOther ("unknown option " <> show optionName)

unsetOption :: String -> TypeCheck var a -> TypeCheck var a
unsetOption "rstt-safe" = localRSTTSafeMode RSTTSafeOff
unsetOption "verbosity" = localVerbosity (verbosity emptyContext)
unsetOption "warn-tope-family-domain" =
  localWarnTopeFamilyDomain (warnTopeFamilyDomain emptyContext)
unsetOption "warn-meta-prefix" =
  localMetaPrefixSensitivity (metaPrefixSensitivity emptyContext)
unsetOption "warn-free-standing-restriction" = localWarnFreeStandingRestriction (warnFreeStandingRestriction emptyContext)
unsetOption "warn-shape-dependency" = localWarnShapeDependency (warnShapeDependency emptyContext)
unsetOption "warn-meta-binder" = localWarnMetaBinder (warnMetaBinder emptyContext)
unsetOption optionName = const $
  issueTypeError $ TypeErrorOther ("unknown option " <> show optionName)

paramToParamDecl :: Rzk.Param -> TypeCheck var [Rzk.ParamDecl]
paramToParamDecl (Rzk.ParamPatternShapeDeprecated loc pat cube tope) = pure
  [ Rzk.ParamTermShape loc (patternToTerm pat) cube tope ]
paramToParamDecl (Rzk.ParamPatternShape loc pats cube tope) = pure
  [ Rzk.ParamTermShape loc (patternToTerm pat) cube tope | pat <- pats]
paramToParamDecl (Rzk.ParamPatternType loc pats ty) = pure
  [ Rzk.ParamTermType loc (patternToTerm pat) ty | pat <- pats ]
paramToParamDecl Rzk.ParamPattern{} = issueTypeError $
  TypeErrorOther "untyped pattern in parameters"

addParamDecls :: [Rzk.ParamDecl] -> Rzk.Term -> Rzk.Term
addParamDecls [] = id
addParamDecls (paramDecl : paramDecls)
  = Rzk.TypeFun Nothing paramDecl . addParamDecls paramDecls

addParams :: [Rzk.Param] -> Rzk.Term -> Rzk.Term
addParams []     = id
addParams params = Rzk.Lambda Nothing params

data SchematicProblem = SchematicKindMismatch | SchematicDependency | SchematicUnsupported
  deriving (Eq, Show)

data TypeError var
  = TypeErrorOther String
  | TypeErrorSchematic SchematicProblem String
  | TypeErrorRSTT String
  | TypeErrorUnify (TermT var) (TermT var) (TermT var)
  | TypeErrorUnifyTerms (TermT var) (TermT var)
  | TypeErrorNotPair (TermT var) (TermT var)
  | TypeErrorNotFunction (TermT var) (TermT var)
  | TypeErrorUnexpectedLambda (Term var) (TermT var)
  | TypeErrorUnexpectedPair (Term var) (TermT var)
  | TypeErrorUnexpectedRefl (Term var) (TermT var)
  | TypeErrorCannotInferBareLambda (Term var)
  | TypeErrorCannotInferBareRefl (Term var)
  | TypeErrorUndefined var
  | TypeErrorTopeNotSatisfied [TermT var] (TermT var)
  | TypeErrorTopeContextDisjoint (TermT var) [TermT var]
  | TypeErrorTopesNotEquivalent (TermT var) (TermT var)
  | TypeErrorInvalidArgumentType (Term var) (TermT var)
  | TypeErrorDuplicateTopLevel [VarIdent] VarIdent
  | TypeErrorUnusedVariable var (TermT var)
  | TypeErrorUnusedUsedVariables [var] var
  | TypeErrorImplicitAssumption (var, TermT var) var
  deriving (Functor, Foldable)

data TypeErrorInContext var = TypeErrorInContext
  { typeErrorError   :: TypeError var
  , typeErrorContext :: Context var
  } deriving (Functor, Foldable)

data TypeErrorInScopedContext var
  = PlainTypeError (TypeErrorInContext var)
  | ScopedTypeError (Maybe VarIdent) (TypeErrorInScopedContext (Inc var))
  deriving (Functor, Foldable)

type TypeError' = TypeError VarIdent

ppTypeError' :: TypeError' -> String
ppTypeError' = \case
  TypeErrorOther msg -> msg
  TypeErrorSchematic _ msg -> msg
  TypeErrorRSTT msg -> "RSTT-safe check failed: " <> msg
  TypeErrorUnify term expected actual -> block TopDown
    [ "cannot unify expected type"
    , "  " <> show (untyped expected)
    , "with actual type"
    , "  " <> show (untyped actual)
    , "for term"
    , "  " <> show (untyped term) ]
  TypeErrorUnifyTerms expected actual -> block TopDown
    [ "cannot unify term"
    , "  " <> show (untyped expected)
    , "with term"
    , "  " <> show (untyped actual) ]
  TypeErrorNotPair term ty -> block TopDown
    [ "expected a cube product or dependent pair"
    , "but got type"
    , "  " <> show (untyped ty)
    , "for term"
    , "  " <> show (untyped term)
    , case ty of
        TypeFunT{} -> "\nPerhaps the term is applied to too few arguments?"
        _          -> ""
    ]

  TypeErrorUnexpectedLambda term ty -> block TopDown
    [ "unexpected lambda abstraction"
    , "  " <> show term
    , "when typechecking against a non-function type"
    , "  " <> show ty
    ]
  TypeErrorUnexpectedPair term ty -> block TopDown
    [ "unexpected pair"
    , "  " <> show term
    , "when typechecking against a type that is not a product or a dependent sum"
    , "  " <> show ty
    ]
  TypeErrorUnexpectedRefl term ty -> block TopDown
    [ "unexpected refl"
    , "  " <> show term
    , "when typechecking against a type that is not an identity type"
    , "  " <> show ty
    ]

  TypeErrorNotFunction term ty -> block TopDown
    [ "expected a function or extension type"
    , "but got type"
    , "  " <> show (untyped ty)
    , "for term"
    , "  " <> show (untyped term)
    , case term of
        AppT _ty f _x -> "\nPerhaps the term\n  " <> show (untyped f) <> "\nis applied to too many arguments?"
        _ -> ""
    ]
  TypeErrorCannotInferBareLambda term -> block TopDown
    [ "cannot infer the type of the argument"
    , "in lambda abstraction"
    , "  " <> show term
    ]
  TypeErrorCannotInferBareRefl term -> block TopDown
    [ "cannot infer the type of term"
    , "  " <> show term
    ]
  TypeErrorUndefined var -> block TopDown
    [ "undefined variable: " <> show (Pure var :: Term') ]
  TypeErrorTopeNotSatisfied topes tope -> block TopDown
    [ "local context is not included in (does not entail) the tope"
    , "  " <> show (untyped tope)
    , "in local context (normalised)"
    , intercalate "\n" (map ("  " <>) (map show topes))
    , intercalate "\n" (map ("  " <>) (map show (generateTopesForPoints (allTopePoints tope))))] -- FIXME: remove
  TypeErrorTopeContextDisjoint tope topes -> block TopDown
    [ "the tope"
    , "  " <> show (untyped tope)
    , "is disjoint from the local tope context (their conjunction is the empty tope ⊥),"
    , "so this restriction face is vacuous everywhere"
    , "in local context (normalised)"
    , intercalate "\n" (map ("  " <>) (map show topes))]
  TypeErrorTopesNotEquivalent expected actual -> block TopDown
    [ "expected tope"
    , "  " <> show (untyped expected)
    , "but got"
    , "  " <> show (untyped actual) ]

  TypeErrorInvalidArgumentType argType argKind -> block TopDown
    [ "invalid function parameter type"
    , "  " <> show argType
    , "function parameter can be a cube, a shape, or a type"
    , "but given parameter type has type"
    , "  " <> show (untyped argKind)
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
    , "  " <> Rzk.printTree (getVarIdent name) <> " : " <> show (untyped type_)
    ]

  TypeErrorUnusedUsedVariables vars name -> block TopDown
    [ "unused variables"
    , "  " <> intercalate " " (map (Rzk.printTree . getVarIdent) vars)
    , "declared as used in definition of"
    , "  " <> Rzk.printTree (getVarIdent name)
    ]

  TypeErrorImplicitAssumption (a, aType) name -> block TopDown
    [ "implicit assumption"
    , "  " <> Rzk.printTree (getVarIdent a) <> " : " <> show (untyped aType)
    , "used in definition of"
    , "  " <> Rzk.printTree (getVarIdent name)
    ]

ppTypeErrorInContext :: OutputDirection -> TypeErrorInContext VarIdent -> String
ppTypeErrorInContext dir TypeErrorInContext{..} = block dir
  [ ppTypeError' typeErrorError
  , ""
  , ppContext' dir typeErrorContext
  ]

ppTypeErrorInScopedContextWith'
  :: OutputDirection
  -> [VarIdent]
  -> [VarIdent]
  -> TypeErrorInScopedContext VarIdent
  -> String
ppTypeErrorInScopedContextWith' dir used vars = \case
  PlainTypeError err -> ppTypeErrorInContext dir err
  ScopedTypeError orig err -> withFresh orig $ \(x, xs) ->
    ppTypeErrorInScopedContextWith' dir (x:used) xs $ fmap (g x) err
  where
    g x Z     = x
    g _ (S y) = y

    withFresh Nothing f =
      case vars of
        x:xs -> f (x, xs)
        _    -> panicImpossible "not enough fresh variables"
    withFresh (Just z) f = f (z', filter (/= z') vars)    -- FIXME: very inefficient filter
      where
        z' = refreshVar used z -- FIXME: inefficient

ppTypeErrorInScopedContext' :: OutputDirection -> TypeErrorInScopedContext VarIdent -> String
ppTypeErrorInScopedContext' dir err =
  ppTypeErrorInScopedContextWith' dir vars (defaultVarIdents \\ vars) err
  where
    vars = nub (foldMap pure err)

issueWarning :: String -> TypeCheck var ()
issueWarning message = do
  trace ("Warning: " <> message) $
    return ()

fromTypeError :: TypeError var -> TypeCheck var (TypeErrorInScopedContext var)
fromTypeError err = do
  context <- ask
  return $ PlainTypeError $ TypeErrorInContext
    { typeErrorError = err
    , typeErrorContext = context
    }

issueTypeError :: TypeError var -> TypeCheck var a
issueTypeError err = fromTypeError err >>= throwError

panicImpossible :: String -> a
panicImpossible msg = error $ unlines
  [ "PANIC! Impossible happened (" <> msg <> ")!"
  , "Please, report a bug at https://github.com/rzk-lang/rzk/issues"
    -- TODO: add details and/or instructions how to produce an artifact for reproducing
  ]

data Action var
  = ActionTypeCheck (Term var) (TermT var)
  | ActionUnify (TermT var) (TermT var) (TermT var)
  | ActionUnifyTerms (TermT var) (TermT var)
  | ActionInfer (Term var)
  | ActionContextEntailedBy [TermT var] (TermT var)
  | ActionContextEntails [TermT var] (TermT var)
  | ActionContextEquiv [TermT var] [TermT var]
  | ActionWHNF (TermT var)
  | ActionNF (TermT var)
  | ActionCheckCoherence (TermT var, TermT var) (TermT var, TermT var)
  | ActionCloseSection (Maybe Rzk.SectionName)
  deriving (Functor, Foldable)

type Action' = Action VarIdent

ppTermInContext :: Eq var => TermT var -> TypeCheck var String
ppTermInContext term =  do
  vars <- freeVarsT_ term
  let mapping = zip vars defaultVarIdents
      toRzkVarIdent origs var = fromMaybe "_" $
        join (lookup var origs) <|> lookup var mapping
  origs <- asks varOrigs
  return (show (untyped (toRzkVarIdent origs <$> term)))

ppSomeAction :: Eq var => [(var, Maybe VarIdent)] -> Int -> Action var -> String
ppSomeAction origs n action = ppAction n (toRzkVarIdent <$> action)
  where
    vars = nub (foldMap pure action)
    mapping = zip vars defaultVarIdents
    toRzkVarIdent var = fromMaybe "_" $
      join (lookup var origs) <|> lookup var mapping

ppAction :: Int -> Action' -> String
ppAction n = unlines . map (replicate (2 * n) ' ' <>) . \case
  ActionTypeCheck term ty ->
    [ "typechecking"
    , "  " <> show term
    , "against type"
    , "  " <> show (untyped ty) ]

  ActionUnify term expected actual ->
    [ "unifying expected type"
    , "  " <> show (untyped expected)
    , "with actual type"
    , "  " <> show (untyped actual)
    , "for term"
    , "  " <> show (untyped term) ]

  ActionUnifyTerms expected actual ->
    [ "unifying term (expected)"
    , "  " <> show expected
    , "with term (actual)"
    , "  " <> show actual ]

  ActionInfer term ->
    [ "inferring type for term"
    , "  " <> show term ]

  ActionContextEntailedBy ctxTopes term ->
    [ "checking if local context"
    , intercalate "\n" (map (("  " <>) . show . untyped) ctxTopes)
    , "includes (is entailed by) restriction tope"
    , "  " <> show (untyped term) ]

  ActionContextEntails ctxTopes term ->
    [ "checking if local context"
    , intercalate "\n" (map (("  " <>) . show . untyped) ctxTopes)
    , "is included in (entails) the tope"
    , "  " <> show (untyped term) ]

  ActionContextEquiv ctxTopes terms ->
    [ "checking if local context"
    , intercalate "\n" (map (("  " <>) . show . untyped) ctxTopes)
    , "is equivalent to the union of the topes"
    , intercalate "\n" (map (("  " <>) . show . untyped) terms) ]

  ActionWHNF term ->
    [ "computing WHNF for term"
    , "  " <> show term ]

  ActionNF term ->
    [ "computing normal form for term"
    , "  " <> show (untyped term) ]

  ActionCheckCoherence (ltope, lterm) (rtope, rterm) ->
    [ "checking coherence for"
    , "  " <> show (untyped ltope)
    , "  |-> " <> show (untyped lterm)
    , "and"
    , "  " <> show (untyped rtope)
    , "  |-> " <> show (untyped rterm) ]

  ActionCloseSection Nothing ->
    [ "closing the file"
    , "and collecting assumptions (variables)" ]
  ActionCloseSection (Just sectionName) ->
    [ "closing #section " <> Rzk.printTree sectionName
    , "and collecting assumptions (variables)"]


traceAction' :: Int -> Action' -> a -> a
traceAction' n action = trace ("[debug]\n" <> ppAction n action)

unsafeTraceAction' :: Int -> Action var -> a -> a
unsafeTraceAction' n = traceAction' n . unsafeCoerce

data LocationInfo = LocationInfo
  { locationFilePath :: Maybe FilePath
  , locationLine     :: Maybe Int
  } deriving (Eq)

data Verbosity
  = Debug
  | Normal
  | Silent
  deriving (Eq, Ord)

trace' :: Verbosity -> Verbosity -> String -> a -> a
trace' msgLevel currentLevel
  | currentLevel <= msgLevel = trace
  | otherwise                = const id

traceTypeCheck :: Verbosity -> String -> TypeCheck var a -> TypeCheck var a
traceTypeCheck msgLevel msg tc = do
  Context{..} <- ask
  trace' msgLevel verbosity msg tc

localVerbosity :: Verbosity -> TypeCheck var a -> TypeCheck var a
localVerbosity v = local $ \Context{..} -> Context { verbosity = v, .. }

localWarnTopeFamilyDomain :: Bool -> TypeCheck var a -> TypeCheck var a
localWarnTopeFamilyDomain enabled = local $ \context ->
  context { warnTopeFamilyDomain = enabled }

localRenderBackend :: Maybe RenderBackend -> TypeCheck var a -> TypeCheck var a
localRenderBackend v = local $ \Context{..} -> Context { renderBackend = v, .. }

data Covariance
  = Covariant     -- ^ Positive position.
  | Contravariant -- ^ Negative position.
  | Invariant     -- ^ Unknown position.

data RenderBackend
  = RenderSVG
  | RenderLaTeX

data ScopeInfo var = ScopeInfo
  { scopeName :: Maybe Rzk.SectionName
  , scopeVars :: [(var, VarInfo var)]
  } deriving (Functor, Foldable)

addVarToScope :: var -> VarInfo var -> ScopeInfo var -> ScopeInfo var
addVarToScope var info ScopeInfo{..} = ScopeInfo
  { scopeVars = (var, info) : scopeVars, .. }

data VarInfo var = VarInfo
  { varType                :: TermT var
  , varValue               :: Maybe (TermT var)
  , varOrig                :: Maybe VarIdent
  , varIsTopLevel          :: Bool
  , varMetaPrefix          :: Int
  , varDeclarationId       :: Maybe DeclarationId
    -- ^ Identity of this installed declaration. Local binders have none.
  , varIsAssumption        :: Bool -- FIXME: perhaps, introduce something like decl kind?
  , varDeclaredAssumptions :: [var]
  , varLocation            :: Maybe LocationInfo
  } deriving (Functor, Foldable)

data Context var = Context
  { localScopes            :: [ScopeInfo var]
  , localTopes             :: [TermT var]
  , localTopesNF           :: [TermT var]
  , localTopesNFUnion      :: [[TermT var]]
  , localTopesEntailBottom :: Bool
  , actionStack            :: [Action var]
  , currentCommand         :: Maybe Rzk.Command
  , location               :: Maybe LocationInfo
  , verbosity              :: Verbosity
  , covariance             :: Covariance
  , rsttSafeSetting :: RSTTSafeMode
  , rsttSafeOverride :: Maybe RSTTSafeMode
  , metaPrefixSensitivity  :: MetaPrefixSensitivity
  , warnTopeFamilyDomain   :: Bool
  , warnFreeStandingRestriction :: Bool
  , warnShapeDependency :: Bool
  , warnMetaBinder :: Bool
  , renderBackend          :: Maybe RenderBackend
  } deriving (Functor, Foldable)

addVarInCurrentScope :: var -> VarInfo var -> Context var -> Context var
addVarInCurrentScope var info Context{..} = Context
  { localScopes =
      case localScopes of
        []             -> [ScopeInfo Nothing [(var, info)]]
        scope : scopes -> addVarToScope var info scope : scopes
  , .. }

emptyContext :: Context var
emptyContext = Context
  { localScopes = [ScopeInfo Nothing []]
  , localTopes = [topeTopT]
  , localTopesNF = [topeTopT]
  , localTopesNFUnion = [[topeTopT]]
  , localTopesEntailBottom = False
  , actionStack = []
  , currentCommand = Nothing
  , location = Nothing
  , verbosity = Normal
  , covariance = Covariant
  , rsttSafeSetting = RSTTSafeOff
  , rsttSafeOverride = Nothing
  , metaPrefixSensitivity = MetaPrefixStrict
  , warnTopeFamilyDomain = True
  , warnFreeStandingRestriction = False
  , warnShapeDependency = False
  , warnMetaBinder = False
  , renderBackend = Nothing
  }

askCurrentScope :: TypeCheck var (ScopeInfo var)
askCurrentScope = asks localScopes >>= \case
  []              -> panicImpossible "no current scope available"
  scope : _scopes -> pure scope

varInfos :: Context var -> [(var, VarInfo var)]
varInfos Context{..} = concatMap scopeVars localScopes

varTypes :: Context var -> [(var, TermT var)]
varTypes = map (fmap varType) . varInfos

varValues :: Context var -> [(var, Maybe (TermT var))]
varValues = map (fmap varValue) . varInfos

varOrigs :: Context var -> [(var, Maybe VarIdent)]
varOrigs = map (fmap varOrig) . varInfos

withPartialDecls
  :: TypeCheck VarIdent ([Decl'], [err])
  -> TypeCheck VarIdent ([Decl'], [err])
  -> TypeCheck VarIdent ([Decl'], [err])
withPartialDecls tc next = do
  (decls, errs) <- tc
  if null errs
    then first (decls <>)
      <$> localDeclsPrepared decls next
    else return (decls, errs)

withSection
  :: Maybe Rzk.SectionName
  -> TypeCheck VarIdent ([Decl VarIdent], [TypeErrorInScopedContext VarIdent])
  -> TypeCheck VarIdent ([Decl VarIdent], [TypeErrorInScopedContext VarIdent])
  -> TypeCheck VarIdent ([Decl VarIdent], [TypeErrorInScopedContext VarIdent])
withSection name sectionBody =
  withPartialDecls $ startSection name $ do
    (decls, errs) <- sectionBody
    localDeclsPrepared decls $
      performing (ActionCloseSection name) $ do
        result <- (Right <$> endSection errs) `catchError` (return . Left)
        case result of
          Left err              -> return ([], errs <> [err])
          Right (decls', errs') -> return (decls', errs')
        -- (\ decls' -> (decls', errs)) <$> endSection errs

startSection :: Maybe Rzk.SectionName -> TypeCheck VarIdent a -> TypeCheck VarIdent a
startSection name = local $ \Context{..} -> Context
  { localScopes = ScopeInfo { scopeName = name, scopeVars = [] } : localScopes
  , .. }

endSection :: [TypeErrorInScopedContext VarIdent] -> TypeCheck VarIdent ([Decl'], [TypeErrorInScopedContext VarIdent])
endSection errs = askCurrentScope >>= scopeToDecls errs

scopeToDecls :: Eq var => [TypeErrorInScopedContext var] -> ScopeInfo var -> TypeCheck var ([Decl var], [TypeErrorInScopedContext var])
scopeToDecls errs ScopeInfo{..} = do
  (decls, errs') <- collectScopeDecls errs [] scopeVars
  -- only issue unused variable errors if there were no errors prior in the section
  -- when (null errs) $ do
  unusedErrors <- forM decls $ \Decl{..} -> do
    let unusedUsedVars = declUsedVars `intersect` map fst scopeVars
    if null errs && not (null unusedUsedVars)
      then do
        err <- local (\c -> c { location = declLocation }) $
          fromTypeError (TypeErrorUnusedUsedVariables unusedUsedVars declName)
        return [err]
      else return []
  return (decls, errs' <> concat unusedErrors)

insertExplicitAssumptionFor
  :: Eq var => var -> (var, VarInfo var) -> TermT var -> TermT var
insertExplicitAssumptionFor a (declName, VarInfo{..}) term =
  term >>= \case
    y | y == declName -> appT varType (Pure declName) (Pure a)
      | otherwise     -> Pure y

insertExplicitAssumptionFor'
  :: Eq var => var -> (var, VarInfo var) -> VarInfo var -> VarInfo var
insertExplicitAssumptionFor' a decl VarInfo{..}
  | varIsAssumption = VarInfo{..}
  | otherwise = VarInfo
      { varType = insertExplicitAssumptionFor a decl varType
      , varValue = insertExplicitAssumptionFor a decl <$> varValue
      , varIsAssumption = varIsAssumption
      , varIsTopLevel = varIsTopLevel
      , varMetaPrefix = varMetaPrefix
      , varDeclarationId = varDeclarationId
      , varOrig = varOrig
      , varDeclaredAssumptions = varDeclaredAssumptions
      , varLocation = varLocation
      }

makeAssumptionExplicit
  :: Eq var
  => (var, VarInfo var)
  -> [(var, VarInfo var)]
  -> TypeCheck var (Bool, [(var, VarInfo var)])
makeAssumptionExplicit _ [] = pure (False {- UNUSED -}, [])
makeAssumptionExplicit assumption@(a, aInfo) ((x, xInfo) : xs) = do
  varsInType <- freeVarsT_ (varType xInfo)
  varsInBody <- concat <$> traverse freeVarsT_ (varValue xInfo)
  let xFreeVars = varsInBody <> varsInType
  let hasAssumption = a `elem` xFreeVars
  xType <- typeOfVar x
  xValue <- valueOfVar x
  let assumptionInType = a `elem` freeVars (untyped xType)
      assumptionInBody = a `elem` foldMap (freeVars . untyped) xValue
      implicitAssumption = and
        [ hasAssumption
        , not (assumptionInType || assumptionInBody)
        , a `notElem` varDeclaredAssumptions xInfo ]
  if hasAssumption
     then do
       when implicitAssumption $ do
         issueTypeError $ TypeErrorImplicitAssumption (a, varType aInfo) x
       (_used, xs'') <- makeAssumptionExplicit (a, aInfo) xs'
       return (True {- USED -}, (x, xInfo') : xs'')
     else do
       (used, xs'') <- makeAssumptionExplicit assumption xs
       return (used, (x, xInfo) : xs'')
  where
    xType' = typeFunT (varOrig aInfo) (varType aInfo) Nothing (abstract a (varType xInfo))
    xInfo' = VarInfo
      { varType = xType'
      , varValue = fmap (lambdaT xType' (varOrig aInfo) Nothing . abstract a) (varValue xInfo)
      , varIsAssumption = varIsAssumption xInfo
      , varIsTopLevel = varIsTopLevel xInfo
      , varMetaPrefix = varMetaPrefix xInfo
      , varDeclarationId = varDeclarationId xInfo
      , varOrig = varOrig xInfo
      , varDeclaredAssumptions = varDeclaredAssumptions xInfo \\ [a]
      , varLocation = varLocation xInfo
      }
    xs' = map (fmap (insertExplicitAssumptionFor' a (x, xInfo))) xs

collectScopeDecls :: Eq var => [TypeErrorInScopedContext var] -> [(var, VarInfo var)] -> [(var, VarInfo var)] -> TypeCheck var ([Decl var], [TypeErrorInScopedContext var])
collectScopeDecls errs recentVars (decl@(var, VarInfo{..}) : vars)
  | varIsAssumption = do
      (used, recentVars') <- makeAssumptionExplicit decl recentVars
      -- only issue unused vars error if there were no other errors previously
      -- when (null errs) $ do
      unusedErr <-
        if null errs && not used
          then local (\c -> c { location = varLocation }) $
            pure <$> fromTypeError (TypeErrorUnusedVariable var varType)
          else return []
      collectScopeDecls (errs <> unusedErr) recentVars' vars
  | otherwise = do
      collectScopeDecls errs (decl : recentVars) vars
collectScopeDecls errs recentVars [] = do
  loc <- asks location
  return (toDecl loc <$> recentVars, errs)
  where
    toDecl loc (var, VarInfo{..}) = Decl
      { declName = var
      , declType = varType
      , declValue = varValue
      , declIsAssumption = varIsAssumption
      , declUsedVars = varDeclaredAssumptions
      , declLocation = updatePosition (varOrig >>= fmap fst . Rzk.hasPosition . fromVarIdent) <$> loc -- FIXME
      }
    updatePosition Nothing l       = l
    updatePosition (Just lineNo) l = l { locationLine = Just lineNo }

abstractAssumption :: Eq var => (var, VarInfo var) -> Decl var -> Decl var
abstractAssumption (var, VarInfo{..}) Decl{..} = Decl
  { declName = declName
  , declType = typeFunT varOrig varType Nothing (abstract var declType)
  , declValue = (\body -> lambdaT newDeclType varOrig Nothing (abstract var body)) <$> declValue
  , declIsAssumption = declIsAssumption
  , declUsedVars = declUsedVars
  , declLocation = declLocation
  }
  where
    newDeclType = typeFunT varOrig varType Nothing (abstract var declType)

data OutputDirection = TopDown | BottomUp
  deriving (Eq)

block :: OutputDirection -> [String] -> String
block TopDown  = intercalate "\n"
block BottomUp = intercalate "\n" . reverse

namedBlock :: OutputDirection -> String -> [String] -> String
namedBlock dir name lines_ = block dir $
  name : map indent lines_
  where
    indent = intercalate "\n" . (map ("  " ++)) . lines

ppContext' :: OutputDirection -> Context VarIdent -> String
ppContext' dir ctx@Context{..} = block dir $ dropWhile null
  [ block TopDown
    [ case location of
        _ | dir == TopDown -> "" -- FIXME
        Just (LocationInfo (Just path) (Just lineNo)) ->
          path <> " (line " <> show lineNo <> "):"
        Just (LocationInfo (Just path) _) ->
          path <> ":"
        _  -> ""
    , case currentCommand of
        Just (Rzk.CommandDefine _loc name _vars _params _ty _term) ->
          "  Error occurred when checking\n    #define " <> Rzk.printTree name
        Just (Rzk.CommandPostulate _loc name _vars _params _ty ) ->
          "  Error occurred when checking\n    #postulate " <> Rzk.printTree name
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
  , case filter (/= topeTopT) localTopes of
      [] -> "Local tope context is unrestricted (⊤)."
      localTopes' -> namedBlock TopDown "Local tope context:"
        [ "  " <> show (untyped tope)
        | tope <- localTopes' ]
  , ""
  , block dir
    [ "when " <> ppAction 0 action
    | action <- actionStack ]
  , namedBlock TopDown "Definitions in context:"
    [ block dir
      [ show (Pure x :: Term') <> " : " <> show (untyped ty)
      | (x, ty) <- reverse (varTypes ctx) ] ]
  ]

doesShadowName :: VarIdent -> TypeCheck var [VarIdent]
doesShadowName name = asks $ \ctx ->
  filter (name ==) (mapMaybe snd (varOrigs ctx))

checkTopLevelDuplicate :: VarIdent -> TypeCheck var ()
checkTopLevelDuplicate name = do
  doesShadowName name >>= \case
    []         -> return ()
    collisions -> issueTypeError $
      TypeErrorDuplicateTopLevel collisions name

checkNameShadowing :: VarIdent -> TypeCheck var ()
checkNameShadowing name = do
  doesShadowName name >>= \case
    [] -> return ()
    collisions -> issueWarning $
      Rzk.printTree (getVarIdent name) <> " shadows an existing definition:"
      <> unlines
        [ "  " <> ppVarIdentWithLocation name
        , "previous top-level definitions found at"
        , intercalate "\n"
          [ "  " <> ppVarIdentWithLocation prev | prev <- collisions ] ]

withLocation :: LocationInfo -> TypeCheck var a -> TypeCheck var a
withLocation loc = local $ \Context{..} -> Context { location = Just loc, .. }

withCommand :: Rzk.Command -> TypeCheck VarIdent ([Decl'], [TypeErrorInScopedContext VarIdent]) -> TypeCheck VarIdent ([Decl'], [TypeErrorInScopedContext VarIdent])
withCommand command tc = local f $ do
  result <- (Right <$> tc) `catchError` (return . Left)
  case result of
    Left err            -> return ([], [err])
    Right (decls, errs) -> return (decls, errs)
  where
    f Context{..} = Context
      { currentCommand = Just command
      , location = updatePosition (Rzk.hasPosition command) <$> location
      , .. }
    updatePosition pos loc = loc { locationLine = fst <$> pos }

localDecls :: [Decl VarIdent] -> TypeCheck VarIdent a -> TypeCheck VarIdent a
localDecls []             = id
localDecls (decl : decls) = localDecl decl . localDecls decls

localDeclsPrepared :: [Decl VarIdent] -> TypeCheck VarIdent a -> TypeCheck VarIdent a
localDeclsPrepared [] = id
localDeclsPrepared (decl : decls) = localDeclPrepared decl . localDeclsPrepared decls

localDecl :: Decl VarIdent -> TypeCheck VarIdent a -> TypeCheck VarIdent a
localDecl (Decl x ty term isAssumption vars loc) tc = do
  ty' <- memoizeWHNF ty
  term' <- traverse memoizeWHNF term
  localDeclPrepared (Decl x ty' term' isAssumption vars loc) tc

localDeclPrepared :: Decl VarIdent -> TypeCheck VarIdent a -> TypeCheck VarIdent a
localDeclPrepared (Decl x ty term isAssumption vars loc) tc = do
  checkTopLevelDuplicate x
  -- Section closing may have added schematic parameters to this type.
  prefix <- metaPrefixOf ty
  key <- State.gets schematicNextId
  State.modify' $ \cache -> cache { schematicNextId = nextDeclarationId key }
  local (update key prefix) tc
  where
    update key prefix = addVarInCurrentScope x VarInfo
      { varType = ty
      , varValue = term
      , varOrig = Just x
      , varIsAssumption = isAssumption
      , varIsTopLevel = True
      , varMetaPrefix = prefix
      , varDeclarationId = Just key
      , varDeclaredAssumptions = vars
      , varLocation = loc
      }

type TypeCheck var = ReaderT (Context var) (ExceptT (TypeErrorInScopedContext var) (State.StateT SchematicCache (Writer [CheckWarning])))

-- | Memoised schematic checks for one checker run. Entries may outlive the
-- declaration's scope. Every installation receives a fresh identity, so section
-- generalisation cannot reuse results for an earlier telescope.
data SchematicCache = SchematicCache
  { schematicNextId :: DeclarationId
    -- ^ Next unused identity in this run.
  , schematicValidated :: DeclarationSet
    -- ^ Declarations whose dependency validation has succeeded.
  , schematicFamilies :: DeclarationMap DeclarationSort
    -- ^ Family summaries. A missing entry has not been probed yet.
  }

emptySchematicCache :: SchematicCache
emptySchematicCache = SchematicCache firstDeclarationId emptyDeclarationSet emptyDeclarationMap

-- Bundle controls follow https://github.com/rzk-lang/rzk/pull/364.
-- Safe mode is opt-in because the additional traversals can be expensive.
data RSTTSafeMode = RSTTSafeOff | RSTTSafeWarn | RSTTSafeError
  deriving (Eq, Show, Read)

rsttSafeMode :: Context var -> RSTTSafeMode
rsttSafeMode ctx = fromMaybe (rsttSafeSetting ctx) (rsttSafeOverride ctx)

localRSTTSafeMode :: RSTTSafeMode -> TypeCheck var a -> TypeCheck var a
localRSTTSafeMode mode = local $ \ctx -> ctx { rsttSafeSetting = mode }

-- | Apply a run-wide choice that source options cannot override.
localRSTTSafeOverride :: Maybe RSTTSafeMode -> TypeCheck var a -> TypeCheck var a
localRSTTSafeOverride mode = local $ \ctx -> ctx { rsttSafeOverride = mode }

effectiveMetaPrefixSensitivity :: Context var -> MetaPrefixSensitivity
effectiveMetaPrefixSensitivity ctx
  | rsttSafeMode ctx /= RSTTSafeOff = MetaPrefixStrict
  | otherwise = metaPrefixSensitivity ctx

effectiveFragmentCheck :: (Context var -> Bool) -> Context var -> Bool
effectiveFragmentCheck standalone ctx = rsttSafeMode ctx /= RSTTSafeOff || standalone ctx

recordCheckWarning :: CheckWarning -> TypeCheck var ()
recordCheckWarning warning = do
  tell [warning]
  mode <- asks rsttSafeMode
  when (mode == RSTTSafeError) $ case warning of
    TopeFamilyDomainWarning{} -> pure ()
    _ -> issueTypeError (TypeErrorRSTT (ppCheckWarning warning))

isRSTTError :: TypeErrorInScopedContext var -> Bool
isRSTTError (ScopedTypeError _ err) = isRSTTError err
isRSTTError (PlainTypeError err) = case typeErrorError err of
  TypeErrorRSTT{} -> True
  _ -> False

recordSchematicDeclaration :: Eq var => VarIdent -> TermT var -> Maybe (TermT var) -> TypeCheck var ()
recordSchematicDeclaration name ty value = do
  mode <- asks rsttSafeMode
  when (mode /= RSTTSafeOff) $
    checkSchematicDeclaration ty value `catchError` report
  where
    report err = case schematicReason err of
      Just message -> do
        loc <- asks location
        recordCheckWarning (RSTTSchematicWarning (message <> " (in " <> show name <> ")") loc)
      Nothing -> reportIncompleteRSTTCheck ("RSTT schematic check incomplete in " <> show name) err
    schematicReason :: TypeErrorInScopedContext v -> Maybe String
    schematicReason (ScopedTypeError _ err) = schematicReason err
    schematicReason (PlainTypeError err) = case typeErrorError err of
      TypeErrorSchematic _ message -> Just message
      _ -> Nothing

-- The meta-prefix check follows https://github.com/rzk-lang/rzk/pull/312,
-- including the incomplete-check reporting in
-- https://github.com/rzk-lang/rzk/pull/364.
-- A prefix ends at the last schematic parameter. This check does not
-- establish universe levels or validate schematic instantiations.
localMetaPrefixSensitivity :: MetaPrefixSensitivity -> TypeCheck var a -> TypeCheck var a
localMetaPrefixSensitivity sensitivity = local $ \ctx ->
  ctx { metaPrefixSensitivity = sensitivity }

-- | Recognise universes and functions quantifying over or into them.
isMetaType :: Eq var => TermT var -> TypeCheck var Bool
isMetaType ty = metaHeadView ty >>= \case
  UniverseT{} -> pure True
  UniverseCubeT{} -> pure True
  UniverseTopeT{} -> pure True
  TypeFunT _ orig param _mtope ret -> do
    paramMeta <- isMetaType param
    if paramMeta then pure True else enterScope orig param (isMetaType ret)
  _ -> pure False

-- | Count parameters through the last schematic parameter.
metaPrefixOf :: Eq var => TermT var -> TypeCheck var Int
metaPrefixOf ty = go 1 0 ty `catchError` \err -> do
  sensitivity <- asks effectiveMetaPrefixSensitivity
  when (sensitivity /= MetaPrefixOff) $
    reportIncompleteRSTTCheck "RSTT meta-prefix classification incomplete" err
  pure 0
  where
    go :: Eq v => Int -> Int -> TermT v -> TypeCheck v Int
    go pos acc t = metaHeadView t >>= \case
      TypeFunT _ orig param _mtope ret -> do
        meta <- isMetaType param
        enterScope orig param (go (pos + 1) (if meta then pos else acc) ret)
      _ -> pure acc

metaHeadView :: Eq var => TermT var -> TypeCheck var (TermT var)
metaHeadView ty = case stripTypeRestrictions ty of
  t@UniverseT{} -> pure t
  t@UniverseCubeT{} -> pure t
  t@UniverseTopeT{} -> pure t
  t@TypeFunT{} -> pure t
  t@TypeSigmaT{} -> pure t
  t@TypeIdT{} -> pure t
  t -> stripTypeRestrictions <$> whnfT t

reportIncompleteRSTTCheck :: String -> TypeErrorInScopedContext var -> TypeCheck var ()
reportIncompleteRSTTCheck what err = do
  loc <- asks location
  if isRSTTError err then throwError err
    else recordCheckWarning (RSTTIncompleteWarning (what <> ": " <> reason err) loc)
  where
    reason :: TypeErrorInScopedContext v -> String
    reason (ScopedTypeError _ nested) = reason nested
    reason (PlainTypeError e) = case typeErrorError e of
      TypeErrorOther message -> message
      _ -> "could not inspect a type"

data MetaPosKind = MetaPos | ObjectPos
  deriving (Eq)

data MetaPositions = MetaPositions
  { posStructural :: MetaPosKind
  , posStrict :: MetaPosKind
  }

metaPositions, objectPositions :: MetaPositions
metaPositions = MetaPositions MetaPos MetaPos
objectPositions = MetaPositions ObjectPos ObjectPos

-- | Report top-level uses that supply too few schematic arguments.
recordMetaPrefixUses :: Eq var => VarIdent -> TermT var -> Maybe (TermT var) -> TypeCheck var ()
recordMetaPrefixUses defName ty mval = asks effectiveMetaPrefixSensitivity >>= \case
  MetaPrefixOff -> pure ()
  _ -> localVerbosity Silent $
    (go metaPositions ty >> mapM_ (go metaPositions) mval) `catchError`
      reportIncompleteRSTTCheck ("RSTT meta-parameter check incomplete in " <> show defName)
  where
    go :: Eq v => MetaPositions -> TermT v -> TypeCheck v ()
    go pos t = case t of
      Pure v -> checkHead pos v 0
      AppT{} -> do
        let (headTerm, args) = collectMetaSpine t []
        prefix <- case headTerm of
          Pure v -> do
            info <- metaVarInfo v
            checkHead pos v (length args)
            pure (if varIsTopLevel info then Just (varMetaPrefix info) else Nothing)
          _ -> go objectPositions headTerm >> pure Nothing
        forM_ (zip [1 :: Int ..] args) $ \(i, (fun, arg)) -> do
          domMeta <- typeOfUncomputed fun >>= metaHeadView >>= \case
            TypeFunT _ _ dom _ _ -> isMetaType dom
            _ -> pure False
          let strict = case prefix of
                Just k | domMeta && i <= k -> MetaPos
                _ -> ObjectPos
          go (MetaPositions (if domMeta then MetaPos else ObjectPos) strict) arg
      LambdaT info orig mparam body -> do
        dom <- case mparam of
          Just (param, mtope) -> do
            go metaPositions param
            forM_ mtope $ \tope -> enterScope orig param (go objectPositions tope)
            pure param
          Nothing -> metaHeadView (infoType info) >>= \case
            TypeFunT _ _ param _ _ -> pure param
            _ -> pure universeT
        enterScope orig dom (go pos body)
      TypeFunT _ orig param mtope ret -> do
        go metaPositions param
        forM_ mtope $ \tope -> enterScope orig param (go objectPositions tope)
        enterScope orig param (go metaPositions ret)
      TypeSigmaT _ orig param ret -> do
        go metaPositions param
        enterScope orig param (go metaPositions ret)
      TypeIdT _ left mty right -> do
        go objectPositions left
        mapM_ (go metaPositions) mty
        go objectPositions right
      TypeRestrictedT _ base restrictions -> do
        go metaPositions base
        forM_ restrictions $ \(tope, term) -> do
          go objectPositions tope
          go objectPositions term
      Free (AnnF _ node) ->
        mapM_ (go objectPositions) (bifoldr (\_ acc -> acc) (:) [] node)

    checkHead :: Eq v => MetaPositions -> v -> Int -> TypeCheck v ()
    checkHead pos v supplied = do
      info <- metaVarInfo v
      let required = varMetaPrefix info
      when (varIsTopLevel info && required > supplied) $ do
        sensitivity <- asks effectiveMetaPrefixSensitivity
        let rule = case (posStructural pos, posStrict pos) of
              (ObjectPos, _) -> Just MetaPrefixBoth
              (MetaPos, ObjectPos) | sensitivity == MetaPrefixStrict -> Just MetaPrefixStrictOnly
              _ -> Nothing
        forM_ rule $ \r -> do
          loc <- asks location
          recordCheckWarning (MetaPrefixWarning defName (fromMaybe "_" (varOrig info)) supplied required r loc)

metaVarInfo :: Eq var => var -> TypeCheck var (VarInfo var)
metaVarInfo v = asks (lookup v . varInfos) >>= \case
  Just info -> pure info
  Nothing -> issueTypeError (TypeErrorUndefined v)

collectMetaSpine :: TermT var -> [(TermT var, TermT var)] -> (TermT var, [(TermT var, TermT var)])
collectMetaSpine (AppT _ fun arg) args = collectMetaSpine fun ((fun, arg) : args)
collectMetaSpine headTerm args = (headTerm, args)

-- Fragment diagnostics follow https://github.com/rzk-lang/rzk/pull/364.
-- Distinguish concluded types, assumed types, and terms (including type-valued data).
data CheckedPosition
  = InTail
  | InAssumption FragmentUse
  | InTerm PrefixPosition

-- A schematic argument may have its own parameter prefix.
data PrefixPosition = InParameterPrefix | OutsideParameterPrefix
  deriving (Eq)

-- | Report restriction and schematic-binder violations in a declaration.
recordFragmentUses
  :: Eq n
  => VarIdent          -- ^ the declaration
  -> TermT n           -- ^ its type
  -> Maybe (TermT n)   -- ^ its value, for a definition
  -> Bool              -- ^ does it supply an assumption rather than a proof?
  -> TypeCheck n ()
recordFragmentUses defName ty mval isAssumption = do
  restrictions <- asks (effectiveFragmentCheck warnFreeStandingRestriction)
  binders <- asks (effectiveFragmentCheck warnMetaBinder)
  shapes <- asks (effectiveFragmentCheck warnShapeDependency)
  when (restrictions || binders || shapes) $
    localVerbosity Silent $
      forM_ [SourceTypes, ComputedTypes] $ \view ->
        checkFragmentUses view defName ty mval isAssumption `catchError`
          reportIncompleteRSTTCheck ("RSTT fragment check incomplete in " <> show defName)

-- Keep source checks even when reduction discards an argument or annotation.
data FragmentView = SourceTypes | ComputedTypes
  deriving (Eq)

checkFragmentUses
  :: Eq n
  => FragmentView -> VarIdent -> TermT n -> Maybe (TermT n) -> Bool -> TypeCheck n ()
checkFragmentUses view defName ty mval isAssumption = do
  go (if isAssumption then InAssumption UseBinder else InTail) ty
  mapM_ (go (InTerm InParameterPrefix)) mval
  where
    go :: Eq l => CheckedPosition -> TermT l -> TypeCheck l ()
    go (InTerm prefixPosition) t = goTerm prefixPosition t
    go InTail t = goTail t
    go (InAssumption use) t = goAssumed use t

    expose :: Eq l => TermT l -> TypeCheck l (TermT l)
    expose = case view of
      SourceTypes -> pure
      ComputedTypes -> fragmentHead 256

    -- Allow restrictions along the spine of concluded codomains.
    goTail :: Eq l => TermT l -> TypeCheck l ()
    goTail original = expose original >>= \t -> case t of
      TypeRestrictedT _ ty' rs -> do
        goTail ty'
        forM_ rs $ \(tope, term) -> do
          goTerm OutsideParameterPrefix tope
          goTerm OutsideParameterPrefix term
      TypeFunT _ orig param mtope ret -> do
        checkShapeDependencies InTail t
        goAssumed UseBinder param
        checkDomain orig param mtope
        enterScope orig param (goTail ret)
      RecOrT _ rs -> forM_ rs $ \(tope, term) -> do
        goTerm OutsideParameterPrefix tope
        goTail term
      _ -> goAssumed UseConcluded t

    -- Assumed restrictions must sit directly under a shape-Π.
    goAssumed :: Eq l => FragmentUse -> TermT l -> TypeCheck l ()
    goAssumed = goAssumedAt OutsideParameterPrefix

    goAssumedAt
      :: Eq l
      => PrefixPosition -> FragmentUse -> TermT l -> TypeCheck l ()
    goAssumedAt prefixPosition use original = expose original >>= \t -> case t of
      TypeRestrictedT _ ty' rs -> do
        reportFreeStanding use t
        goAssumed use ty'
        forM_ rs $ \(tope, term) -> do
          goTerm OutsideParameterPrefix tope
          goTerm OutsideParameterPrefix term
      TypeFunT _ orig param mtope ret -> do
        checkShapeDependencies (InAssumption use) t
        goAssumed UseBinder param
        checkDomain orig param mtope
        shape <- isShapeBinder (maybe False (const True) mtope) param
        enterScope orig param $
          if shape then goExtCodomain use ret else goAssumed use ret
      TypeSigmaT _ orig a b -> do
        goAssumed UseBinder a
        enterScope orig a (goAssumed use b)
      -- Check the identity type separately from its endpoints.
      TypeIdT _ a mtA b -> do
        goTerm OutsideParameterPrefix a
        mapM_ (goAssumed UseIdentity) mtA
        goTerm OutsideParameterPrefix b
      LambdaT info orig mparam body -> do
        forM_ mparam $ \(param, mtope) -> do
          goAssumed UseBinder param
          checkDomain orig param mtope
          when (prefixPosition == OutsideParameterPrefix) $ reportMetaBinder orig param
        dom <- binderType info mparam
        enterScope orig dom (goAssumedAt prefixPosition use body)
      RecOrT _ rs -> forM_ rs $ \(tope, term) -> do
        goTerm OutsideParameterPrefix tope
        goAssumed use term
      Pure{} -> pure ()
      _ -> goTerm OutsideParameterPrefix t

    -- Direct shape codomains admit boundary clipping to the binder's domain.
    goExtCodomain
      :: Eq l => FragmentUse -> TermT l -> TypeCheck l ()
    goExtCodomain use original = expose original >>= \t -> case t of
      TypeRestrictedT _ ty' rs -> do
        goAssumed use ty'
        forM_ rs $ \(face, term) -> do
          goTerm OutsideParameterPrefix face
          goTerm OutsideParameterPrefix term
      _ -> goAssumed use t

    -- Topes may contain applications with their own binders and assumed types.
    checkDomain
      :: Eq l
      => Maybe VarIdent -> TermT l -> Maybe (Scope TermT l) -> TypeCheck l ()
    checkDomain orig param =
      mapM_ (\tope -> enterScope orig param (goTerm OutsideParameterPrefix tope))

    -- RS17 Appendix A.2 requires shapes independent of ambient cube points.
    -- Schematic families are judgements over cube contexts, not extension types.
    checkShapeDependencies :: Eq l => CheckedPosition -> TermT l -> TypeCheck l ()
    checkShapeDependencies position t = do
      enabled <- asks (effectiveFragmentCheck warnShapeDependency)
      when enabled $ case t of
        TypeFunT _ orig param mtope ret -> do
          schematic <- isMetaType t
          shape <- isShapeBinder (maybe False (const True) mtope) param
          when shape $ enterScope orig param $ do
            reportShapeDependency "cube domain" Z (S <$> param)
            unless schematic $ do
              forM_ mtope $ reportShapeDependency "shape domain" Z
              expose ret >>= \body -> case (position, body) of
                (InAssumption _, TypeRestrictedT _ _ rs) ->
                  forM_ rs $ \(face, _) -> reportShapeDependency "restriction boundary" Z face
                _ -> pure ()
        _ -> pure ()

    reportShapeDependency
      :: Eq l => String -> l -> TermT l -> TypeCheck l ()
    reportShapeDependency what point tope = do
      points <- outerPoints point tope
      unless (null points) $ do
        rendered <- ppTermInContext tope
        names <- mapM (ppTermInContext . Pure) points
        loc <- asks location
        recordCheckWarning (ShapeDependencyWarning
          ("context-dependent " <> what <> " " <> rendered
            <> " (outer cube points: " <> intercalate ", " names
            <> ", in " <> show defName <> ")") loc)

    -- Follow aliases and variable types without reducing away dependencies.
    outerPoints :: Eq l => l -> TermT l -> TypeCheck l [l]
    outerPoints point t = walk [point] (freeVars (untyped t))
      where
        walk _ [] = pure []
        walk seen (v : vs)
          | v `elem` seen = walk seen vs
          | otherwise = do
              varTy <- typeOfVar v
              value <- valueOfVar v
              cube <- typeOfUncomputed varTy >>= whnfT
              let here = case (value, cube) of
                    (Nothing, UniverseCubeT{}) -> [v]
                    _ -> []
                  dependencies = freeVars (untyped varTy) <> foldMap (freeVars . untyped) value
              rest <- walk (v : seen) (dependencies <> vs)
              pure (here <> rest)

    -- Check term binders and route type-valued bodies through goData.
    goTerm :: Eq l => PrefixPosition -> TermT l -> TypeCheck l ()
    goTerm InParameterPrefix t | not (isLambda t) = goDataAt InParameterPrefix t
    goTerm prefixPosition t = case t of
      Pure{} -> pure ()

      -- Function types returned by branches still introduce shapes.
      TypeFunT{} -> goAssumed UseData t

      -- The stored type is synthesised by elaboration.
      ReflT _ mx -> forM_ mx $ \(x, _mxty) -> goTerm OutsideParameterPrefix x

      TypeAscT _ term' ty' -> do
        goTerm OutsideParameterPrefix term'
        goTail ty'

      LambdaT info orig mparam body -> do
        checkShapeDependencies InTail (infoType info)
        forM_ mparam $ \(param, mtope) -> do
          goAssumed UseBinder param
          checkDomain orig param mtope
          when (prefixPosition == OutsideParameterPrefix) $ reportMetaBinder orig param
        dom <- binderType info mparam
        enterScope orig dom (goTerm prefixPosition body)

      AppT{} -> do
        let (h, args) = collectFragmentSpine t []
        goTerm OutsideParameterPrefix h
        forM_ args $ \(fnode, arg) -> do
          -- Substitution can create a forbidden type in a later argument domain.
          when (view == ComputedTypes) $
            typeOfUncomputed fnode >>= funDomain >>= mapM_ (goAssumed UseBinder)
          -- Schema arguments may bind their own schematic parameters.
          plumbing <- domainIsMeta fnode
          if plumbing then goTerm InParameterPrefix arg else goData arg

      -- The motive is assumed: the eliminator binds its variables at it.
      IdJT _ tA a tC d x p -> do
        goData tA
        goTerm OutsideParameterPrefix a
        goAssumed UseMotive tC
        goTerm OutsideParameterPrefix d
        goTerm OutsideParameterPrefix x
        goTerm OutsideParameterPrefix p

      PairT _ l r -> do
        goData l
        goData r

      Free (AnnF _ f) ->
        mapM_ (goTerm OutsideParameterPrefix) (bifoldMap (const []) (:[]) f)

    -- Type-valued data may later become binder or motive types. A proof
    -- quantified over a universe must still be walked as a term.
    goData :: Eq l => TermT l -> TypeCheck l ()
    goData = goDataAt OutsideParameterPrefix

    goDataAt :: Eq l => PrefixPosition -> TermT l -> TypeCheck l ()
    goDataAt prefixPosition t = do
      isType <- landsInUniverse t
      if isType then goAssumedAt prefixPosition UseData t else goTerm OutsideParameterPrefix t

    isLambda :: TermT l -> Bool
    isLambda LambdaT{} = True
    isLambda _         = False

    binderType
      :: Eq l
      => TypeInfo (TermT l) -> Maybe (TermT l, Maybe (Scope TermT l))
      -> TypeCheck l (TermT l)
    binderType _info (Just (param, _mtope)) = pure param
    binderType info Nothing = fromMaybe universeT <$> funDomain (infoType info)

    -- A cube domain without a tope denotes the unrestricted shape.
    isShapeBinder :: Eq l => Bool -> TermT l -> TypeCheck l Bool
    isShapeBinder True _ = pure True
    isShapeBinder False param =
      typeOfUncomputed param >>= whnfT >>= \case
        UniverseCubeT{} -> pure True
        _               -> pure False

    -- Is the Π-domain of this function node schematic?
    domainIsMeta :: Eq l => TermT l -> TypeCheck l Bool
    domainIsMeta f = do
      tf <- typeOfUncomputed f
      whnfT (stripTypeRestrictions tf) >>= \case
        TypeFunT _ _ dom _ _ -> isMetaType dom
        _                      -> pure False

    -- Recognise types and type families by their final codomain.
    landsInUniverse :: Eq l => TermT l -> TypeCheck l Bool
    landsInUniverse t =
      typeOfUncomputed t >>= finalCodomain
      where
        finalCodomain :: Eq k => TermT k -> TypeCheck k Bool
        finalCodomain ty' = whnfT (stripTypeRestrictions ty') >>= \case
          UniverseT{}     -> pure True
          UniverseCubeT{} -> pure True
          UniverseTopeT{} -> pure True
          TypeFunT _ orig param _mtope ret -> enterScope orig param (finalCodomain ret)
          _               -> pure False

    funDomain :: Eq l => TermT l -> TypeCheck l (Maybe (TermT l))
    funDomain tf =
      whnfT (stripTypeRestrictions tf) >>= \case
        TypeFunT _ _ dom _ _ -> pure (Just dom)
        _                      -> pure Nothing

    reportMetaBinder :: Eq l => Maybe VarIdent -> TermT l -> TypeCheck l ()
    reportMetaBinder orig param = do
      enabled <- asks (effectiveFragmentCheck warnMetaBinder)
      when enabled $ do
        meta <- isMetaType param
        when meta $ do
          rendered <- ppTermInContext param
          loc <- asks location
          recordCheckWarning (MetaBinderWarning defName (fromMaybe "_" orig) rendered loc)

    reportFreeStanding :: Eq l => FragmentUse -> TermT l -> TypeCheck l ()
    reportFreeStanding use t = do
      enabled <- asks (effectiveFragmentCheck warnFreeStandingRestriction)
      when enabled $ do
        rendered <- ppTermInContext t
        loc <- asks location
        recordCheckWarning (FreeStandingRestrictionWarning defName rendered use loc)

-- Expose type constructors without consulting cached normal forms or simplifying
-- their guards. Keep this reducer in step with the object-level rules in whnfT.
fragmentHead :: Eq n => Int -> TermT n -> TypeCheck n (TermT n)
fragmentHead fuel t
  | fuel <= 0 = issueTypeError (TypeErrorOther "RSTT fragment reduction limit reached")
  | otherwise = case t of
      Pure v -> valueOfVar v >>= maybe (neutral t) step
      AppT info f x -> step f >>= \case
        LambdaT _ _ _ body -> step (substituteT x body)
        f' -> neutral (AppT info f' x)
      TypeAscT _ value _ -> step value
      FirstT info pair -> step pair >>= \case
        PairT _ a _ -> step a
        pair' -> neutral (FirstT info pair')
      SecondT info pair -> step pair >>= \case
        PairT _ _ b -> step b
        pair' -> neutral (SecondT info pair')
      IdJT info a x motive d y proof -> step proof >>= \case
        ReflT{} -> step d
        proof' -> neutral (IdJT info a x motive d y proof')
      RecOrT _ rs -> firstMatchingFragment rs >>= maybe (pure t) step
      -- In particular, stop at TypeRestrictedT before reducing its topes.
      _ -> pure t
  where
    step = fragmentHead (fuel - 1)
    neutral term = do
      ty <- typeOfUncomputed term >>= step
      tryRestriction ty >>= maybe (pure term) step

collectFragmentSpine :: TermT n -> [(TermT n, TermT n)] -> (TermT n, [(TermT n, TermT n)])
collectFragmentSpine (AppT _ f x) acc = collectFragmentSpine f ((f, x) : acc)
collectFragmentSpine h acc            = (h, acc)

firstMatchingFragment :: Eq var => [(TermT var, TermT var)] -> TypeCheck var (Maybe (TermT var))
firstMatchingFragment [] = pure Nothing
firstMatchingFragment ((guard, value) : rest) = checkTope guard >>= \case
  True -> pure (Just value)
  False -> firstMatchingFragment rest

localWarnFreeStandingRestriction :: Bool -> TypeCheck var a -> TypeCheck var a
localWarnFreeStandingRestriction enabled = local $ \ctx -> ctx { warnFreeStandingRestriction = enabled }

localWarnShapeDependency :: Bool -> TypeCheck var a -> TypeCheck var a
localWarnShapeDependency enabled = local $ \ctx -> ctx { warnShapeDependency = enabled }

localWarnMetaBinder :: Bool -> TypeCheck var a -> TypeCheck var a
localWarnMetaBinder enabled = local $ \ctx -> ctx { warnMetaBinder = enabled }

-- Schematic validation follows https://github.com/rzk-lang/rzk/pull/364.
-- It checks family arguments at fresh indices and keeps universes separate
-- from object types. Postulates remain trusted rules.
-- | Roles of elaborated type and domain expressions in schematic declarations.
-- These are not universe levels. The fragment's formation conditions are
-- checked separately by recordFragmentUses.
data SchematicSort
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
-- This supplements dRzk typing with kind checks.
checkSchematicDeclaration :: Eq var => TermT var -> Maybe (TermT var) -> TypeCheck var ()
checkSchematicDeclaration = schematicDeclaration 256 emptyDeclarationSet

schematicInvalid :: Eq n => String -> TermT n -> TypeCheck n a
schematicInvalid = schematicError SchematicKindMismatch

schematicError :: Eq n => SchematicProblem -> String -> TermT n -> TypeCheck n a
schematicError problem reason t = do
  rendered <- ppTermInContext t
  issueTypeError (TypeErrorSchematic problem (reason <> ": " <> rendered))

schematicBudget :: Int -> TypeCheck n ()
schematicBudget fuel = when (fuel <= 0) $
  issueTypeError (TypeErrorOther "schematic validation limit reached")

-- Check a declaration telescope separately: a definition may name a family kind
-- or a schema, whereas an argument to a type parameter must be an object type.
schematicDeclaration :: Eq n => Int -> DeclarationSet -> TermT n -> Maybe (TermT n) -> TypeCheck n ()
schematicDeclaration fuel seen ty value = do
  schematicBudget fuel
  -- Check dependencies before reduction can obscure their failures.
  dependencies ty
  schematicSignature fuel seen ty
  mapM_ (\t -> dependencies t >> schematicTerm fuel seen t) value
  where
    dependencies = mapM_ (schematicUses fuel seen . Pure) . (freeVars . untyped)

schematicSignature :: Eq n => Int -> DeclarationSet -> TermT n -> TypeCheck n ()
schematicSignature fuel seen ty = do
  schematicBudget fuel
  schematicUses fuel seen ty
  schematicInspectRedex fuel seen ty
  fragmentHead fuel ty >>= \case
    TypeFunT _ orig dom guard body -> do
      void (schematicClassify (fuel - 1) seen dom)
      mapM_ (\g -> enterScope orig dom (schematicTerm (fuel - 1) seen g)) guard
      enterScope orig dom (schematicSignature (fuel - 1) seen body)
    t -> void (schematicClassify (fuel - 1) seen t)

-- Check each dependency before using its signature, even if safe mode was off
-- when it was declared. Successful earlier checks are cached in checker state.
-- Check variables during traversal instead of rescanning every subtree.
schematicUses :: Eq n => Int -> DeclarationSet -> TermT n -> TypeCheck n ()
schematicUses fuel seen (Pure v) = do
  info <- metaVarInfo v
  case varDeclarationId info of
    Nothing -> pure ()
    Just key -> do
      cached <- State.gets (memberDeclarationSet key . schematicValidated)
      unless cached $ do
        schematicBudget fuel
        when (memberDeclarationSet key seen) $
          issueTypeError (TypeErrorOther "cyclic schematic dependency")
        -- A caller's tope assumptions cannot justify a reusable dependency check.
        local withoutCallerTopes $ schematicDeclaration (fuel - 1) (insertDeclarationSet key seen)
          (varType info) (varValue info)
        State.modify' $ \cache -> cache
          { schematicValidated = insertDeclarationSet key (schematicValidated cache) }
schematicUses _ _ _ = pure ()

withoutCallerTopes :: Context n -> Context n
withoutCallerTopes ctx = ctx
  { localTopes = [], localTopesNF = []
  , localTopesNFUnion = [[]], localTopesEntailBottom = False
  }

-- Check erased arguments and annotations before exposing the result.
schematicInspectRedex :: Eq n => Int -> DeclarationSet -> TermT n -> TypeCheck n ()
schematicInspectRedex fuel seen original = case original of
  TypeAscT{} -> schematicTerm fuel seen original
  FirstT{} -> schematicTerm fuel seen original
  SecondT{} -> schematicTerm fuel seen original
  IdJT{} -> schematicTerm fuel seen original
  RecOrT{} -> schematicTerm fuel seen original
  _ -> schematicApplications fuel seen original

schematicClassify :: Eq n => Int -> DeclarationSet -> TermT n -> TypeCheck n SchematicSort
schematicClassify fuel seen original = do
  schematicBudget fuel
  schematicUses fuel seen original
  schematicInspectRedex fuel seen original
  schematicKnownObjectFamily fuel seen original >>= \case
    True -> pure ObjectType
    False -> schematicClassifyHead fuel seen original

schematicClassifyHead :: Eq n => Int -> DeclarationSet -> TermT n -> TypeCheck n SchematicSort
schematicClassifyHead fuel seen original =
  fragmentHead fuel original >>= \case
    UniverseT{} -> pure ParameterKind
    UniverseCubeT{} -> pure ParameterKind
    UniverseTopeT{} -> pure ParameterKind
    TypeFunT _ orig dom guard body -> do
      d <- schematicClassify (fuel - 1) seen dom
      mapM_ (\g -> enterScope orig dom (schematicTerm (fuel - 1) seen g)) guard
      r <- enterScope orig dom (schematicClassify (fuel - 1) seen body)
      case (d, r) of
        (ObjectType, ObjectType) -> pure ObjectType
        (Shape, ObjectType) -> pure ObjectType
        (_, ParameterKind) -> pure ParameterKind
        (_, Shape) -> schematicInvalid "cube or shape family" original
        _ -> pure SchematicRule
    TypeSigmaT _ orig a b -> do
      d <- schematicClassify (fuel - 1) seen a
      r <- enterScope orig a (schematicClassify (fuel - 1) seen b)
      pure (if d == ObjectType && r == ObjectType then ObjectType else ParameterKind)
    TypeIdT _ a mt b -> do
      ty <- maybe (schematicTypeOf fuel a) pure mt
      schematicObject (fuel - 1) seen ty
      schematicTerm (fuel - 1) seen a
      schematicTerm (fuel - 1) seen b
      pure ObjectType
    TypeUnitT{} -> pure ObjectType
    RecBottomT{} -> pure ObjectType
    RecOrT _ branches -> do
      sorts <- mapM (\(p, v) -> localTope p (schematicClassify (fuel - 1) seen v)) branches
      case sorts of
        s : rest | all (== s) rest -> pure s
        _ -> schematicInvalid "case split between different schematic kinds" original
    TypeRestrictedT _ ty rs -> do
      s <- schematicClassify (fuel - 1) seen ty
      forM_ rs $ \(p, v) -> schematicTerm (fuel - 1) seen p >> schematicArgument (fuel - 1) seen ty v
      pure s
    CubeUnitT{} -> pure Shape
    Cube2T{} -> pure Shape
    CubeProductT _ a b -> schematicTerm (fuel - 1) seen a >> schematicTerm (fuel - 1) seen b >> pure Shape
    t -> do
      schematicTermNeutral (fuel - 1) seen t
      schematicTypeOf fuel t >>= fragmentHead fuel >>= \case
        UniverseT{} -> pure ObjectType
        UniverseCubeT{} -> pure Shape
        UniverseTopeT{} -> pure Shape
        _ -> schematicInvalid "expected an object type, parameter kind, or schematic rule" t

-- Reuse only universally object-valued families, after checking actual arguments.
-- Other families retain reduction-based classification in the caller's context.
schematicKnownObjectFamily :: Eq n => Int -> DeclarationSet -> TermT n -> TypeCheck n Bool
schematicKnownObjectFamily fuel seen = spine 0
  where
    spine arity (AppT _ f _) = spine (arity + 1) f
    spine arity (Pure v) = do
      info <- metaVarInfo v
      case varDeclarationId info of
        Nothing -> pure False
        Just key -> do
          cached <- State.gets (lookupDeclarationMap key . schematicFamilies)
          summary <- case cached of
            Just result -> pure result
            Nothing -> do
              -- Recursive probes must fall back to ordinary classification.
              cache key DeclarationUnclassified
              result <- (local withoutCallerTopes $
                schematicObjectFamilySort fuel seen 0 (varType info) (Pure v))
                `catchError` \_ -> pure DeclarationUnclassified
              cache key result
              pure result
          pure (summary == DeclarationObjectValuedAfter arity)
    spine _ _ = pure False
    cache :: DeclarationId -> DeclarationSort -> TypeCheck var ()
    cache key result = State.modify' $ \checkLog' -> checkLog'
      { schematicFamilies = insertDeclarationMap key result (schematicFamilies checkLog') }

-- Probe at fresh parameters to avoid generalising a concrete instance.
schematicObjectFamilySort :: Eq n => Int -> DeclarationSet -> Int -> TermT n -> TermT n -> TypeCheck n DeclarationSort
schematicObjectFamilySort fuel seen arity ty value = do
  schematicBudget fuel
  fragmentHead fuel ty >>= \case
    TypeFunT _ orig dom _ body ->
      enterScope orig dom $
        schematicObjectFamilySort (fuel - 1) seen (arity + 1) body
          (appT body (S <$> value) (Pure Z))
    UniverseT{} -> do
      s <- schematicClassify (fuel - 1) seen value
      pure $ case s of
        ObjectType -> DeclarationObjectValuedAfter arity
        ParameterKind -> DeclarationMetaValued
        _ -> DeclarationUnclassified
    _ -> pure DeclarationUnclassified

schematicObject :: Eq n => Int -> DeclarationSet -> TermT n -> TypeCheck n ()
schematicObject fuel seen t = do
  s <- schematicClassify (fuel - 1) seen t
  unless (s == ObjectType) (schematicInvalid "expected an object type" t)

-- A type-family argument is checked pointwise. In particular U, A -> U, and
-- (A : U) -> A cannot instantiate a parameter declared simply as A : U.
schematicArgument :: Eq n => Int -> DeclarationSet -> TermT n -> TermT n -> TypeCheck n ()
schematicArgument fuel seen dom arg = do
  schematicBudget fuel
  schematicFunctionHead fuel dom >>= \case
    UniverseT{} -> schematicObject (fuel - 1) seen arg
    TypeFunT _ orig a guard body -> do
      s <- schematicClassify (fuel - 1) seen dom
      if s == ParameterKind then do
        -- Eta-expand only for checking. The ordinary checker has checked the
        -- argument's type. The body is inspected with the same fresh index.
        enterScope orig a $
          schematicArgument (fuel - 1) seen body (appT body (S <$> arg) (Pure Z))
        mapM_ (\g -> enterScope orig a (schematicTerm (fuel - 1) seen g)) guard
      else schematicTerm (fuel - 1) seen arg
    _ -> schematicTerm (fuel - 1) seen arg

-- Section abstraction can leave application annotations describing an earlier
-- telescope. Derive elimination types from the current signatures instead.
schematicTypeOf :: Eq n => Int -> TermT n -> TypeCheck n (TermT n)
schematicTypeOf fuel t = do
  schematicBudget fuel
  case t of
    AppT _ f x -> schematicTypeOf (fuel - 1) f >>= schematicFunctionHead fuel >>= \case
      TypeFunT _ _ _ _ ret -> pure (substituteT x ret)
      _ -> schematicInvalid "application without a function type" f
    FirstT _ p -> schematicTypeOf (fuel - 1) p >>= schematicFunctionHead fuel >>= \case
      TypeSigmaT _ _ a _ -> pure a
      CubeProductT _ a _ -> pure a
      _ -> schematicInvalid "projection without a product type" p
    SecondT _ p -> schematicTypeOf (fuel - 1) p >>= schematicFunctionHead fuel >>= \case
      TypeSigmaT _ _ a b -> pure (substituteT (firstT a p) b)
      CubeProductT _ _ b -> pure b
      _ -> schematicInvalid "projection without a product type" p
    _ -> typeOfUncomputed t

-- Boundaries constrain a function without changing its parameter kinds.
schematicFunctionHead :: Eq n => Int -> TermT n -> TypeCheck n (TermT n)
schematicFunctionHead fuel t = do
  schematicBudget fuel
  fragmentHead fuel t >>= \case
    TypeRestrictedT _ base _ -> schematicFunctionHead (fuel - 1) base
    head' -> pure head'

schematicApplications :: Eq n => Int -> DeclarationSet -> TermT n -> TypeCheck n ()
schematicApplications fuel seen (AppT _ f x) = do
  schematicBudget fuel
  schematicTerm (fuel - 1) seen f
  schematicTypeOf fuel f >>= schematicFunctionHead fuel >>= \case
    TypeFunT _ _ dom _ _ -> schematicArgument (fuel - 1) seen dom x
    _ -> schematicInvalid "application without a schematic or object function type" f
schematicApplications _ _ _ = pure ()

schematicTerm :: Eq n => Int -> DeclarationSet -> TermT n -> TypeCheck n ()
schematicTerm fuel seen t = do
  schematicBudget fuel
  schematicUses fuel seen t
  case t of
    TypeFunT{} -> void (schematicClassify (fuel - 1) seen t)
    TypeSigmaT{} -> void (schematicClassify (fuel - 1) seen t)
    TypeIdT{} -> void (schematicClassify (fuel - 1) seen t)
    TypeRestrictedT{} -> void (schematicClassify (fuel - 1) seen t)
    LambdaT info orig param body -> do
      dom <- case param of
        Just (dom, guard) -> do
          mapM_ (\g -> enterScope orig dom (schematicTerm (fuel - 1) seen g)) guard
          pure dom
        Nothing -> schematicFunctionHead fuel (infoType info) >>= \case
          TypeFunT _ _ dom _ _ -> pure dom
          _ -> schematicInvalid "lambda without a function type" t
      void (schematicClassify (fuel - 1) seen dom)
      enterScope orig dom (schematicTerm (fuel - 1) seen body)
    AppT{} -> schematicApplications fuel seen t
    TypeAscT _ value ty -> schematicSignature (fuel - 1) seen ty >> schematicTerm (fuel - 1) seen value
    IdJT _ a x motive d y p -> do
      schematicObject (fuel - 1) seen a
      -- J eliminates equality into object types, not into schematic kinds.
      motiveType <- schematicTypeOf fuel motive
      schematicArgument (fuel - 1) seen motiveType motive
      mapM_ (schematicTerm (fuel - 1) seen) [x, motive, d, y, p]
    _ -> schematicTermNeutral fuel seen t

-- This is deliberately an allow-list. New syntax cannot inherit certification.
schematicTermNeutral :: Eq n => Int -> DeclarationSet -> TermT n -> TypeCheck n ()
schematicTermNeutral fuel seen t = case t of
  Pure{} -> pure ()
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
  TopeEQT{} -> children
  TopeLEQT{} -> children
  TopeAndT{} -> children
  TopeOrT{} -> children
  PairT _ x y -> do
    ty <- schematicTypeOf fuel t
    schematicFunctionHead fuel ty >>= \case
      TypeSigmaT _ _ a b -> do
        schematicArgument (fuel - 1) seen a x
        cod <- pure (substituteT x b)
        schematicArgument (fuel - 1) seen cod y
      CubeProductT{} -> children
      _ -> schematicInvalid "pair without a product type" t
  FirstT{} -> children
  SecondT{} -> children
  ReflT _ mx -> forM_ mx $ \(x, mty) -> do
    ty <- maybe (schematicTypeOf fuel x) pure mty
    schematicObject (fuel - 1) seen ty
    schematicTerm (fuel - 1) seen x
  -- Elaboration checks the absurd tope context. Its type annotation is cyclic.
  RecBottomT{} -> pure ()
  RecOrT{} -> children
  AppT{} -> schematicApplications fuel seen t
  _ -> schematicError SchematicUnsupported "unsupported schematic expression" t
  where
    children = case t of
      Free (AnnF _ sig) -> mapM_ (schematicTerm (fuel - 1) seen) (bifoldMap (const []) (:[]) sig)
      Pure{} -> pure ()
-- Retain source expressions for later fragment checks, including checks of
-- dependencies declared while safe mode was off.
cacheWHNF :: TermT var -> TermT var -> TermT var
cacheWHNF (Free (AnnF info sig)) reduced = Free (AnnF info { infoWHNF = Just reduced } sig)
cacheWHNF t _ = t

memoizeWHNF :: Eq var => TermT var -> TypeCheck var (TermT var)
memoizeWHNF t = cacheWHNF t <$> whnfT t

freeVarsT_ :: Eq var => TermT var -> TypeCheck var [var]
freeVarsT_ term = do
  types <- asks varTypes
  let typeOfVar' x =
        case lookup x types of
          Nothing -> panicImpossible "undefined variable"
          Just ty -> ty
  return (freeVarsT typeOfVar' term)

traceStartAndFinish :: Show a => String -> a -> a
traceStartAndFinish tag = trace ("start [" <> tag <> "]") .
  (\x -> trace ("finish [" <> tag <> "] with " <> show x) x)

entail :: Eq var => [TermT var] -> TermT var -> Bool
entail topes tope = all (`solveRHS` tope) $
  saturateTopes (allTopePoints tope) <$>
    simplifyLHSwithDisjunctions topes'
  where
    topes' = nubTermT (topes <> generateTopesForPoints (allTopePoints tope))

entailM :: Eq var => [TermT var] -> TermT var -> TypeCheck var Bool
entailM topes tope = do
  -- genTopes <- generateTopesForPointsM (allTopePoints tope)
  let topes'    = nubTermT topes -- (topes <> genTopes)
      topes''   = simplifyLHSwithDisjunctions topes'
      topes'''  = saturateTopes (allTopePoints tope) <$> topes''
  prettyTopes <- mapM ppTermInContext (saturateTopes (allTopePoints tope) (simplifyLHS topes'))
  prettyTope <- ppTermInContext tope
  traceTypeCheck Debug
    ("entail " <> intercalate ", " prettyTopes <> " |- " <> prettyTope) $
      and <$> mapM (`solveRHSM` tope) topes'''

entailTraceM :: Eq var => [TermT var] -> TermT var -> TypeCheck var Bool
entailTraceM topes tope = do
  topes' <- mapM ppTermInContext topes
  tope' <- ppTermInContext tope
  result <- trace ("entail " <> intercalate ", " topes' <> " |- " <> tope') $
        topes `entailM` tope
  return $ trace ("  " <> show result) result

nubTermT :: Eq var => [TermT var] -> [TermT var]
nubTermT []     = []
nubTermT (t:ts) = t : nubTermT (filter (/= t) ts)

saturateTopes :: Eq var => [TermT var] -> [TermT var] -> [TermT var]
saturateTopes _points topes = saturateWith
  (\tope ts -> tope `elem` ts)
  generateTopes
  topes

-- FIXME: cleanup
saturateWith :: (a -> [a] -> Bool) -> ([a] -> [a] -> [a]) -> [a] -> [a]
saturateWith elem' step zs = go (nub' zs) []
  where
    go lastNew xs
      | null new = lastNew
      | otherwise = lastNew <> go new xs'
      where
        xs' = lastNew <> xs
        new = filter (not . (`elem'` xs')) (nub' $ step lastNew xs)
    nub' []     = []
    nub' (x:xs) = x : nub' (filter (not . (`elem'` [x])) xs)

generateTopes :: Eq var => [TermT var] -> [TermT var] -> [TermT var]
generateTopes newTopes oldTopes
  | topeBottomT `elem` newTopes = []
  | topeEQT cube2_0T cube2_1T `elem` newTopes = [topeBottomT]
  | length oldTopes > 100 = []    -- FIXME
  | otherwise = concat
      [  -- symmetry EQ
        [ topeEQT y x | TopeEQT _ty x y <- newTopes ]
        -- transitivity EQ (1)
      , [ topeEQT x z
        | TopeEQT _ty x y : newTopes' <- tails newTopes
        , TopeEQT _ty y' z <- newTopes' <> oldTopes
        , y == y' ]
        -- transitivity EQ (2)
      , [ topeEQT x z
        | TopeEQT _ty y z : newTopes' <- tails newTopes
        , TopeEQT _ty x y' <- newTopes' <> oldTopes
        , y == y' ]

        -- transitivity LEQ (1)
      , [ topeLEQT x z
        | TopeLEQT _ty x y : newTopes' <- tails newTopes
        , TopeLEQT _ty y' z <- newTopes' <> oldTopes
        , y == y' ]
        -- transitivity LEQ (2)
      , [ topeLEQT x z
        | TopeLEQT _ty y z : newTopes' <- tails newTopes
        , TopeLEQT _ty x y' <- newTopes' <> oldTopes
        , y == y' ]

        -- antisymmetry LEQ
      , [ topeEQT x y
        | TopeLEQT _ty x y : newTopes' <- tails newTopes
        , TopeLEQT _ty y' x' <- newTopes' <> oldTopes
        , y == y'
        , x == x' ]

        -- FIXME: special case of substitution of EQ
        -- transitivity EQ-LEQ (1)
      , [ topeLEQT x z
        | TopeEQT  _ty y z : newTopes' <- tails newTopes
        , TopeLEQT _ty x y' <- newTopes' <> oldTopes
        , y == y' ]

        -- FIXME: special case of substitution of EQ
        -- transitivity EQ-LEQ (2)
      , [ topeLEQT x z
        | TopeEQT  _ty x y : newTopes' <- tails newTopes
        , TopeLEQT _ty y' z <- newTopes' <> oldTopes
        , y == y' ]

        -- FIXME: special case of substitution of EQ
        -- transitivity EQ-LEQ (3)
      , [ topeLEQT x z
        | TopeLEQT  _ty y z : newTopes' <- tails newTopes
        , TopeEQT _ty x y' <- newTopes' <> oldTopes
        , y == y' ]

        -- FIXME: special case of substitution of EQ
        -- transitivity EQ-LEQ (4)
      , [ topeLEQT x z
        | TopeLEQT  _ty x y : newTopes' <- tails newTopes
        , TopeEQT _ty y' z <- newTopes' <> oldTopes
        , y == y' ]

        -- FIXME: consequence of LEM for LEQ and antisymmetry for LEQ
      , [ topeEQT x y | TopeLEQT _ty x y@Cube2_0T{} <- newTopes ]
        -- FIXME: consequence of LEM for LEQ and antisymmetry for LEQ
      , [ topeEQT x y | TopeLEQT _ty x@Cube2_1T{} y <- newTopes ]
      ]

-- | Linearity topes @(x ≤ y) ∨ (y ≤ x)@ for pairs of points. This is sound
-- only for points of the directed interval 𝟚; being pure, this function cannot
-- check that, so it is unsound-in-general and 'generateTopesForPointsM' (which
-- guards on @cube2T@) should be preferred wherever the 'TypeCheck' monad is
-- available. The only solving path that calls this, the pure 'entail', is
-- invoked solely for the ⊥-collapse check (@localTopesEntailBottom@), whose
-- goal ⊥ has no points, so no linearity tope is ever actually generated there.
generateTopesForPoints :: Eq var => [TermT var] -> [TermT var]
generateTopesForPoints points = nubTermT $ concat
  [ [ topeOrT (topeLEQT x y) (topeLEQT y x)
    | x : points' <- tails (filter (`notElem` [cube2_0T, cube2_1T]) points)
    , y <- points'
    , x /= y ]
  ]

generateTopesForPointsM :: Eq var => [TermT var] -> TypeCheck var [TermT var]
generateTopesForPointsM points = do
  let pairs = nub $ concat
        [ [ (x, y)
          | x : points' <- tails (filter (`notElem` [cube2_0T, cube2_1T]) points)
          , y <- points'
          , x /= y ]
        ]
  topes <- forM pairs $ \(x, y) -> do
    xType <- typeOf x
    yType <- typeOf y
    return $ if (xType == cube2T) && (yType == cube2T)
      then [topeOrT (topeLEQT x y) (topeLEQT y x)]
      else []
  return (concat topes)

allTopePoints :: Eq var => TermT var -> [TermT var]
allTopePoints = nubTermT . foldMap subPoints . nubTermT . topePoints

topePoints :: TermT var -> [TermT var]
topePoints = \case
  TopeTopT{}     -> []
  TopeBottomT{}  -> []
  TopeAndT _ l r -> topePoints l <> topePoints r
  TopeOrT  _ l r -> topePoints l <> topePoints r
  TopeEQT  _ x y -> [x, y]
  TopeLEQT _ x y -> [x, y]
  _              -> []

subPoints :: TermT var -> [TermT var]
subPoints = \case
  p@(PairT _ x y) -> p : foldMap subPoints [x, y]
  p@Pure{} -> [p]
  p@(Free (AnnF TypeInfo{..} _))
    | Cube2T{} <- infoType -> [p]
  _ -> []

-- | Simplify the context, including disjunctions. See also 'simplifyLHS'.
simplifyLHSwithDisjunctions :: Eq var => [TermT var] -> [[TermT var]]
simplifyLHSwithDisjunctions topes = map nubTermT $
  case topes of
    [] -> [[]]
    TopeTopT{} : topes' -> simplifyLHSwithDisjunctions topes'
    TopeBottomT{} : _  -> [[topeBottomT]]
    TopeAndT _ l r : topes' -> simplifyLHSwithDisjunctions (l : r : topes')

    -- NOTE: it is inefficient to expand disjunctions immediately
    TopeOrT  _ l r : topes' -> simplifyLHSwithDisjunctions (l : topes') <> simplifyLHSwithDisjunctions (r : topes')

    TopeEQT  _ (PairT _ x y) (PairT _ x' y') : topes' ->
      simplifyLHSwithDisjunctions (topeEQT x x' : topeEQT y y' : topes')
    t : topes' -> map (t :) (simplifyLHSwithDisjunctions topes')

-- | Simplify the context, except disjunctions. See also 'simplifyLHSwithDisjunctions'.
simplifyLHS :: Eq var => [TermT var] -> [TermT var]
simplifyLHS topes = nubTermT $
  case topes of
    [] -> []
    TopeTopT{} : topes' -> simplifyLHS topes'
    TopeBottomT{} : _  -> [topeBottomT]
    TopeAndT _ l r : topes' -> simplifyLHS (l : r : topes')

    -- NOTE: it is inefficient to expand disjunctions immediately
    -- TopeOrT  _ l r : topes' -> simplifyLHS (l : topes') <> simplifyLHS (r : topes')

    TopeEQT  _ (PairT _ x y) (PairT _ x' y') : topes' ->
      simplifyLHS (topeEQT x x' : topeEQT y y' : topes')
    t : topes' -> t : simplifyLHS topes'

solveRHSM :: Eq var => [TermT var] -> TermT var -> TypeCheck var Bool
solveRHSM topes tope =
  case tope of
    _ | topeBottomT `elem` topes -> return True
    TopeTopT{}     -> return True
    TopeEQT  _ty (PairT _ty1 x y) (PairT _ty2 x' y') ->
      solveRHSM topes $ topeAndT
        (topeEQT x x')
        (topeEQT y y')
    TopeEQT  _ty (PairT TypeInfo{ infoType = CubeProductT _ cubeI cubeJ } x y) r ->
      solveRHSM topes $ topeAndT
        (topeEQT x (firstT cubeI r))
        (topeEQT y (secondT cubeJ r))
    TopeEQT  _ty l (PairT TypeInfo{ infoType = CubeProductT _ cubeI cubeJ } x y) ->
      solveRHSM topes $ topeAndT
        (topeEQT (firstT cubeI l) x)
        (topeEQT (secondT cubeJ l) y)
    TopeEQT  _ty l r
      | or
          [ l == r
          , tope `elem` topes
          , topeEQT r l `elem` topes
          ] -> return True
    TopeLEQT _ty l r
      | l == r -> return True
      | solveRHS topes (topeEQT l r) -> return True
      | solveRHS topes (topeEQT l cube2_0T) -> return True
      | solveRHS topes (topeEQT r cube2_1T) -> return True
    TopeAndT _ l r -> (&&)
      <$> solveRHSM topes l
      <*> solveRHSM topes r
    _ | tope `elem` topes -> return True
    TopeOrT  _ l r -> do
      l' <- solveRHSM topes l
      r' <- solveRHSM topes r
      if (l' || r')
        then return True
        else do
          lems <- generateTopesForPointsM (allTopePoints tope)
          let lems' = [ lem | lem@(TopeOrT _ t1 t2) <- lems, all (`notElem` topes) [t1, t2] ]
          case lems' of
            TopeOrT _ t1 t2 : _ -> do
              l'' <- solveRHSM (saturateTopes [] (t1 : topes)) tope
              r'' <- solveRHSM (saturateTopes [] (t2 : topes)) tope
              return (l'' && r'')
            _ -> return False
    _ -> return False

solveRHS :: Eq var => [TermT var] -> TermT var -> Bool
solveRHS topes tope =
  case tope of
    _ | topeBottomT `elem` topes -> True
    TopeTopT{}     -> True
    TopeEQT  _ty (PairT _ty1 x y) (PairT _ty2 x' y')
      | solveRHS topes (topeEQT x x') && solveRHS topes (topeEQT y y') -> True
    TopeEQT  _ty (PairT TypeInfo{ infoType = CubeProductT _ cubeI cubeJ } x y) r
      | solveRHS topes (topeEQT x (firstT cubeI r)) && solveRHS topes (topeEQT y (secondT cubeJ r)) -> True
    TopeEQT  _ty l (PairT TypeInfo{ infoType = CubeProductT _ cubeI cubeJ } x y)
      | solveRHS topes (topeEQT (firstT cubeI l) x) && solveRHS topes (topeEQT (secondT cubeJ l) y) -> True
    TopeEQT  _ty l r -> or
      [ l == r
      , tope `elem` topes
      , topeEQT r l `elem` topes
      ]
    TopeLEQT _ty l r
      | l == r -> True
      | solveRHS topes (topeEQT l r) -> True
      | solveRHS topes (topeEQT l cube2_0T) -> True
      | solveRHS topes (topeEQT r cube2_1T) -> True
    -- TopeBottomT{}  -> solveLHS topes tope
    TopeAndT _ l r -> solveRHS topes l && solveRHS topes r
    TopeOrT  _ l r -> solveRHS topes l || solveRHS topes r
    _ -> tope `elem` topes

checkTope :: Eq var => TermT var -> TypeCheck var Bool
checkTope tope = do
  ctxTopes <- asks localTopes
  performing (ActionContextEntails ctxTopes tope) $ do
    topes' <- asks localTopesNF
    tope' <- nfTope tope
    topes' `entailM` tope'

checkTopeEntails :: Eq var => TermT var -> TypeCheck var Bool
checkTopeEntails tope = do
  ctxTopes <- asks localTopes
  performing (ActionContextEntailedBy ctxTopes tope) $ do
    contextTopes <- asks localTopesNF
    restrictionTope <- nfTope tope
    let contextTopesRHS = foldr topeAndT topeTopT contextTopes
    [restrictionTope] `entailM` contextTopesRHS

checkEntails :: Eq var => TermT var -> TermT var -> TypeCheck var Bool
checkEntails l r = do  -- FIXME: add action
  l' <- nfTope l
  r' <- nfTope r
  [l'] `entailM` r'

contextEntailedBy :: Eq var => TermT var -> TypeCheck var ()
contextEntailedBy tope = do
  ctxTopes <- asks localTopes
  performing (ActionContextEntailedBy ctxTopes tope) $ do
    contextTopes <- asks localTopesNF
    restrictionTope <- nfTope tope
    let contextTopesRHS = foldr topeOrT topeBottomT contextTopes
    [restrictionTope] `entailM` contextTopesRHS >>= \case
      False -> issueTypeError $ TypeErrorTopeNotSatisfied [restrictionTope] contextTopesRHS
      True -> return ()

-- | Diagnose a restriction face against the local tope context. Three cases,
-- by how the face tope relates to the context:
--
--   * DISJOINT — the tope and a consistent context have empty overlap (their
--     conjunction is ⊥). The face is then vacuous everywhere, so this is a hard
--     error.
--   * OVERHANG — the tope is not entailed by the context but still overlaps it.
--     This is allowed and often intentional (e.g. restricting with an
--     already-defined shape, whose faces live on the whole cube rather than being
--     relativised to the context), so we only emit a non-fatal hint. The hint is
--     gated at 'Normal' verbosity, hence silent under 'Silent' (e.g. in tests).
--   * CONTAINED — the tope entails the context: nothing to report.
checkTopeAgainstContext :: Eq var => String -> TermT var -> TypeCheck var ()
checkTopeAgainstContext what tope = do
  ctxEntailsBottom <- asks localTopesEntailBottom
  unless ctxEntailsBottom $ do          -- a contradictory context is handled elsewhere (recBOT)
    ctxTopes <- asks (filter (/= topeTopT) . localTopesNF)
    disjoint <- (tope : ctxTopes) `entailM` topeBottomT
    if disjoint
      then issueTypeError (TypeErrorTopeContextDisjoint tope ctxTopes)
      else do
        entailed <- checkTopeEntails tope     -- tope |- AND(localTopesNF)
        unless entailed $ do
          topeStr <- ppTermInContext tope
          ctxStrs <- mapM ppTermInContext ctxTopes
          traceTypeCheck Normal
            (intercalate "\n" $
              [ "Warning: " <> what <> " overhangs the local tope context"
              , "  " <> topeStr
              , "is not entailed by the local context (normalised)"
              ] <> map ("  " <>) ctxStrs)
            (return ())

contextEntails :: Eq var => TermT var -> TypeCheck var ()
contextEntails tope = do
  ctxTopes <- asks localTopes
  performing (ActionContextEntails ctxTopes tope) $ do
    topeIsEntailed <- checkTope tope
    topes' <- asks localTopesNF
    unless topeIsEntailed $
      issueTypeError $ TypeErrorTopeNotSatisfied topes' tope

topesEquiv :: Eq var => TermT var -> TermT var -> TypeCheck var Bool
topesEquiv expected actual = performing (ActionUnifyTerms expected actual) $ do
  expected' <- nfT expected
  actual' <- nfT actual
  (&&)
    <$> [expected'] `entailM` actual'
    <*> [actual'] `entailM` expected'

contextEquiv :: Eq var => [TermT var] -> TypeCheck var ()
contextEquiv topes = do
  ctxTopes <- asks localTopes
  performing (ActionContextEquiv ctxTopes topes) $ do
    contextTopes <- asks localTopesNF
    recTopes <- mapM nfTope topes
    let contextTopesRHS = foldr topeOrT topeBottomT contextTopes
        recTopesRHS     = foldr topeOrT topeBottomT recTopes
    contextTopes `entailM` recTopesRHS >>= \case
      False -> issueTypeError $ TypeErrorTopeNotSatisfied contextTopes recTopesRHS
      True -> return ()
    recTopes `entailM` contextTopesRHS >>= \case
      False -> issueTypeError $ TypeErrorTopeNotSatisfied recTopes contextTopesRHS
      True -> return ()

switchVariance :: TypeCheck var a -> TypeCheck var a
switchVariance = local $ \Context{..} -> Context
  { covariance = switch covariance, .. }
    where
      switch Covariant     = Contravariant
      switch Contravariant = Covariant
      switch Invariant     = Invariant

setVariance :: Covariance -> TypeCheck var a -> TypeCheck var a
setVariance variance = local $ \Context{..} -> Context
  { covariance = variance, .. }

-- | Orient a subtyping check on (supertype, subtype) by the ambient variance.
-- Adapted from https://github.com/rzk-lang/rzk/pull/347.
bySubtyping
  :: (TermT var -> TermT var -> TypeCheck var ())
  -> TermT var -> TermT var -> TypeCheck var ()
bySubtyping check expected actual = asks covariance >>= \case
  Covariant     -> check expected actual
  Contravariant -> check actual expected
  Invariant     -> check expected actual >> check actual expected

-- | The subtype's domain must hold wherever the supertype's domain does.
domainEntails :: Eq var => TermT var -> TermT var -> TypeCheck var ()
domainEntails superTope subTope = localTope superTope $ contextEntails subTope

enterScopeContext :: Maybe VarIdent -> TermT var -> Context var -> Context (Inc var)
enterScopeContext orig ty context =
  addVarInCurrentScope Z VarInfo
    { varType   = S <$> ty
    , varValue  = Nothing
    , varOrig   = orig
    , varIsAssumption = False
    , varIsTopLevel = False
    , varMetaPrefix = 0
    , varDeclarationId = Nothing
    , varDeclaredAssumptions = []
    , varLocation = location context
    }
    (S <$> context)

enterScope :: Maybe VarIdent -> TermT var -> TypeCheck (Inc var) b -> TypeCheck var b
enterScope orig ty action = do
  newContext <- asks (enterScopeContext orig ty)
  lift $ withExceptT (ScopedTypeError orig) $
    runReaderT action newContext

performing :: Eq var => Action var -> TypeCheck var a -> TypeCheck var a
performing action tc = do
  ctx@Context{..} <- ask
  unless (length actionStack < 1000) $  -- FIXME: which depth is reasonable? factor out into a parameter
    issueTypeError $ TypeErrorOther "maximum depth reached"
  traceTypeCheck Debug (ppSomeAction (varOrigs ctx) (length actionStack) action) $
    local (const Context { actionStack = action : actionStack, .. }) $ tc

stripTypeRestrictions :: TermT var -> TermT var
stripTypeRestrictions (TypeRestrictedT _ty ty _restriction) = stripTypeRestrictions ty
stripTypeRestrictions t = t

-- | Perform at most one \(\eta\)-expansion at the top-level to assist unification.
etaMatch :: Eq var => Maybe (TermT var) -> TermT var -> TermT var -> TypeCheck var (TermT var, TermT var)
-- FIXME: double check the next 3 rules
etaMatch _mterm expected@TypeRestrictedT{} actual@TypeRestrictedT{} = pure (expected, actual)
etaMatch  mterm expected (TypeRestrictedT _ty ty _rs) = etaMatch mterm expected ty
etaMatch (Just term) expected@TypeRestrictedT{} actual =
  etaMatch (Just term) expected (typeRestrictedT actual [(topeTopT, term)])
-- ------------------------------------
etaMatch _mterm expected@LambdaT{} actual@LambdaT{} = pure (expected, actual)
etaMatch _mterm expected@PairT{}   actual@PairT{}   = pure (expected, actual)
etaMatch _mterm expected@LambdaT{} actual = do
  actual' <- etaExpand actual
  pure (expected, actual')
etaMatch _mterm expected actual@LambdaT{} = do
  expected' <- etaExpand expected
  pure (expected', actual)
etaMatch _mterm expected@PairT{} actual = do
  actual' <- etaExpand actual
  pure (expected, actual')
etaMatch _mterm expected actual@PairT{} = do
  expected' <- etaExpand expected
  pure (expected', actual)
etaMatch _mterm expected actual = pure (expected, actual)

etaExpand :: Eq var => TermT var -> TypeCheck var (TermT var)
etaExpand term@LambdaT{} = pure term
etaExpand term@PairT{} = pure term
etaExpand term = do
  ty <- typeOf term
  case stripTypeRestrictions ty of
    TypeFunT _ty orig param mtope ret -> pure $
      lambdaT ty orig (Just (param, mtope))
        (appT ret (S <$> term) (Pure Z))

    TypeSigmaT _ty _orig a b -> pure $
      pairT ty
        (firstT a term)
        (secondT (substituteT (firstT a term) b) term)

    CubeProductT _ty a b -> pure $
      pairT ty
        (firstT a term)
        (secondT b term)

    _ -> pure term

inCubeLayer :: Eq var => TermT var -> TypeCheck var Bool
inCubeLayer = \case
  RecBottomT{}    -> pure False
  UniverseT{}     -> pure False

  UniverseCubeT{} -> pure True
  CubeProductT{}  -> pure True
  CubeUnitT{}     -> pure True
  CubeUnitStarT{} -> pure True
  Cube2T{}        -> pure True
  Cube2_0T{}      -> pure True
  Cube2_1T{}      -> pure True

  t               -> typeOf t >>= inCubeLayer

inTopeLayer :: Eq var => TermT var -> TypeCheck var Bool
inTopeLayer = \case
  RecBottomT{} -> pure False
  UniverseT{} -> pure False

  UniverseCubeT{} -> pure True
  UniverseTopeT{} -> pure True

  CubeProductT{} -> pure True
  CubeUnitT{} -> pure True
  CubeUnitStarT{} -> pure True
  Cube2T{} -> pure True
  Cube2_0T{} -> pure True
  Cube2_1T{} -> pure True

  TopeTopT{} -> pure True
  TopeBottomT{} -> pure True
  TopeAndT{} -> pure True
  TopeOrT{} -> pure True
  TopeEQT{} -> pure True
  TopeLEQT{} -> pure True

  TypeFunT _ty orig param _mtope ret -> do
    enterScope orig param $ inTopeLayer ret

  t -> typeOfUncomputed t >>= inTopeLayer

tryRestriction :: Eq var => TermT var -> TypeCheck var (Maybe (TermT var))
tryRestriction = \case
  TypeRestrictedT _ _ rs -> do
    let go [] = pure Nothing
        go ((tope, term') : rs') = do
          checkTope tope >>= \case
            True  -> pure (Just term')
            False -> go rs'
    go rs
  _ -> pure Nothing

-- | Compute a typed term to its WHNF.
--
-- >>> unsafeTypeCheck' $ whnfT "(\\ (x : Unit) -> x) unit"
-- unit : Unit
whnfT :: Eq var => TermT var -> TypeCheck var (TermT var)
whnfT tt = performing (ActionWHNF tt) $ case tt of
  -- use cached result if it exists
  Free (AnnF info _)
    | Just tt' <- infoWHNF info -> pure tt'

  -- universe constants
  UniverseT{} -> pure tt
  UniverseCubeT{} -> pure tt
  UniverseTopeT{} -> pure tt

  -- cube layer (except vars, pairs, and applications)
  CubeProductT{} -> nfTope tt
  CubeUnitT{} -> pure tt
  CubeUnitStarT{} -> pure tt
  Cube2T{} -> pure tt
  Cube2_0T{} -> pure tt
  Cube2_1T{} -> pure tt

  -- tope layer (except vars, pairs of points, and applications)
  TopeTopT{} -> pure tt
  TopeBottomT{} -> pure tt
  TopeAndT{} -> nfTope tt
  TopeOrT{} -> nfTope tt
  TopeEQT{} -> nfTope tt
  TopeLEQT{} -> nfTope tt

  -- type layer terms that should not be evaluated further
  LambdaT{} -> pure tt
  PairT{} -> pure tt
  ReflT{} -> pure tt
  TypeFunT{} -> pure tt
  TypeSigmaT{} -> pure tt
  TypeIdT{} -> pure tt
  RecBottomT{} -> pure tt
  TypeUnitT{} -> pure tt
  UnitT{} -> pure tt

  -- type ascriptions are ignored, since we already have a typechecked term
  TypeAscT _ty term _ty' -> whnfT term

  -- check if we have cube or a tope term (if so, compute NF)
  _ -> typeOf tt >>= \case
    UniverseCubeT{} -> nfTope tt
    UniverseTopeT{} -> nfTope tt

    -- CubeUnitT{} -> pure cubeUnitStarT -- compute an expression of 1 cube to its only point
    TypeUnitT{} -> pure unitT -- compute an expression of Unit type to unit
    -- FIXME: next line is ad hoc, should be improved!
    TypeRestrictedT _info TypeUnitT{} _rs -> pure unitT -- compute an expression of Unit type to unit

    -- check if we have cube point term (if so, compute NF)
    typeOf_tt -> typeOf typeOf_tt >>= \case
      UniverseCubeT{} -> nfTope tt

      -- now we are in the type layer
      _ -> fmap termIsWHNF $ do
        -- check if we are in the empty context
        inBottom <- asks localTopesEntailBottom
        if inBottom
           then pure recBottomT -- if so, reduce to recBOT
           else tryRestriction typeOf_tt >>= \case
            Just tt' -> whnfT tt'
            Nothing -> case tt of
              t@(Pure var) ->
                valueOfVar var >>= \case
                  Nothing   -> pure t
                  Just term -> whnfT term

              AppT ty f x ->
                whnfT f >>= \case
                  LambdaT _ty _orig _arg body ->
                    whnfT (substituteT x body)
                  f' -> typeOf f' >>= \case
                    TypeFunT _ty _orig _param (Just tope) UniverseTopeT{} -> do
                      topeAndT
                        <$> (AppT ty <$> nfT f' <*> nfT x)
                        <*> nfT (substituteT x tope)
                    -- FIXME: this seems to be a hack, and will not work in all situations!
                    -- FIXME: need to check performance of this code thoroughly
                    -- FIXME: for now, it seems to add ~2x slowdown
                    TypeFunT info _orig _param _mtope ret@TypeRestrictedT{}
                      | TypeRestrictedT{} <- infoType info -> pure (AppT ty f' x)
                      | otherwise -> do
                          let ret' = substituteT x ret
                          tryRestriction ret' >>= \case -- FIXME: to many unnecessary checks?
                            Nothing  -> pure (AppT ty { infoType = ret' } f' x)
                            Just tt' -> whnfT tt'
                    _ -> pure (AppT ty f' x)

              FirstT ty t ->
                whnfT t >>= \case
                  PairT _ l _r -> whnfT l
                  t'           -> pure (FirstT ty t')

              SecondT ty t ->
                whnfT t >>= \case
                  PairT _ _l r -> whnfT r
                  t'           -> pure (SecondT ty t')
              IdJT ty tA a tC d x p ->
                whnfT p >>= \case
                  ReflT{} -> whnfT d
                  p'      -> pure (IdJT ty tA a tC d x p')

              RecOrT _ty rs -> do
                let go [] = pure Nothing
                    go ((tope, tt') : rs') = do
                      checkTope tope >>= \case
                        True  -> pure (Just tt')
                        False -> go rs'
                go rs >>= \case
                  Just tt' -> whnfT tt'
                  Nothing
                    | [tt'] <- nubTermT (map snd rs) -> whnfT tt'
                    | otherwise -> pure tt

              TypeRestrictedT ty type_ rs -> do
                rs' <- traverse (\(tope, term) -> (,) <$> nfT tope <*> pure term) rs
                case filter ((/= topeBottomT) . fst) rs' of
                  []   -> whnfT type_  -- get rid of restrictions at BOT
                  rs'' -> TypeRestrictedT ty <$> whnfT type_ <*> pure rs''

nfTope :: Eq var => TermT var -> TypeCheck var (TermT var)
nfTope tt = performing (ActionNF tt) $ fmap termIsNF $ case tt of
  Pure var ->
    valueOfVar var >>= \case
      Nothing   -> return tt
      Just term -> nfTope term

  -- see if normal form is already available
  Free (AnnF info _) | Just tt' <- infoNF info -> pure tt'

  -- universe constants
  UniverseT{} -> pure tt
  UniverseCubeT{} -> pure tt
  UniverseTopeT{} -> pure tt

  -- cube layer constants
  CubeUnitT{} -> pure tt
  CubeUnitStarT{} -> pure tt
  Cube2T{} -> pure tt
  Cube2_0T{} -> pure tt
  Cube2_1T{} -> pure tt

  -- type layer constants
  TypeUnitT{} -> pure tt
  UnitT{} -> pure tt

  -- cube layer with computation
  CubeProductT _ty l r -> cubeProductT <$> nfTope l <*> nfTope r

  -- tope layer constants
  TopeTopT{} -> pure tt
  TopeBottomT{} -> pure tt

  -- tope layer with computation
  TopeAndT ty l r ->
    nfTope l >>= \case
      TopeBottomT{} -> pure topeBottomT
      l' -> nfTope r >>= \case
        TopeBottomT{} -> pure topeBottomT
        r'            -> pure (TopeAndT ty l' r')

  TopeOrT  ty l r -> do
    l' <- nfTope l
    r' <- nfTope r
    case (l', r') of
      (TopeBottomT{}, _) -> pure r'
      (_, TopeBottomT{}) -> pure l'
      _                  -> pure (TopeOrT ty l' r')

  TopeEQT  ty l r -> TopeEQT  ty <$> nfTope l <*> nfTope r
  TopeLEQT ty l r -> TopeLEQT ty <$> nfTope l <*> nfTope r

  -- type ascriptions are ignored, since we already have a typechecked term
  TypeAscT _ty term _ty' -> nfTope term

  PairT ty l r -> PairT ty <$> nfTope l <*> nfTope r

  AppT ty f x ->
    nfTope f >>= \case
      LambdaT _ty _orig _arg body ->
        nfTope (substituteT x body)
      f' -> typeOfUncomputed f' >>= \case
        TypeFunT _ty _orig _param (Just tope) UniverseTopeT{} -> do
          topeAndT
            <$> (AppT ty f' <$> nfTope x)
            <*> nfTope (substituteT x tope)
        _ -> AppT ty f' <$> nfTope x

  FirstT ty t ->
    nfTope t >>= \case
      PairT _ty x _y -> pure x
      t'             -> pure (FirstT ty t')

  SecondT ty t ->
    nfTope t >>= \case
      PairT _ty _x y -> pure y
      t'             -> pure (SecondT ty t')

  LambdaT ty orig _mparam body
    | TypeFunT _ty _origF param mtope _ret <- infoType ty ->
        LambdaT ty orig (Just (param, mtope)) <$> enterScope orig param (nfTope body)
  LambdaT{} -> panicImpossible "lambda with a non-function type in the tope layer"

  TypeFunT{} -> panicImpossible "exposed function type in the tope layer"
  TypeSigmaT{} -> panicImpossible "dependent sum type in the tope layer"
  TypeIdT{} -> panicImpossible "identity type in the tope layer"
  ReflT{} -> panicImpossible "refl in the tope layer"
  IdJT{} -> panicImpossible "idJ eliminator in the tope layer"
  TypeRestrictedT{} -> panicImpossible "extension types in the tope layer"

  RecOrT{} -> panicImpossible "recOR in the tope layer"
  RecBottomT{} -> panicImpossible "recBOT in the tope layer"

-- | Compute a typed term to its NF.
--
-- >>> unsafeTypeCheck' $ nfT "(\\ (x : Unit) -> x) unit"
-- unit : Unit
nfT :: Eq var => TermT var -> TypeCheck var (TermT var)
nfT tt = performing (ActionNF tt) $ case tt of
  -- universe constants
  UniverseT{} -> pure tt
  UniverseCubeT{} -> pure tt
  UniverseTopeT{} -> pure tt

  -- cube layer constants
  CubeUnitT{} -> pure tt
  CubeUnitStarT{} -> pure tt
  Cube2T{} -> pure tt
  Cube2_0T{} -> pure tt
  Cube2_1T{} -> pure tt

  -- cube layer with computation
  CubeProductT{} -> nfTope tt

  -- tope layer constants
  TopeTopT{} -> pure tt
  TopeBottomT{} -> pure tt

  -- tope layer with computation
  TopeAndT{} -> nfTope tt
  TopeOrT{} -> nfTope tt
  TopeEQT{} -> nfTope tt
  TopeLEQT{} -> nfTope tt

  -- type layer constants
  ReflT ty _x -> pure (ReflT ty Nothing)
  RecBottomT{} -> pure tt
  TypeUnitT{} -> pure tt
  UnitT{} -> pure tt

  -- type ascriptions are ignored, since we already have a typechecked term
  TypeAscT _ty term _ty' -> nfT term

  -- now we are in the type layer
  _ -> do
    -- check if we are in the empty context
    inBottom <- asks localTopesEntailBottom
    if inBottom
       then pure recBottomT -- if so, reduce to recBOT
       else typeOf tt >>= tryRestriction >>= \case
        Just tt' -> nfT tt'
        Nothing -> case tt of
          t@(Pure var) ->
            valueOfVar var >>= \case
              Nothing   -> pure t
              Just term -> nfT term

          TypeFunT ty orig param mtope ret -> do
            param' <- nfT param
            enterScope orig param' $ do
              mtope' <- traverse nfT mtope
              maybe id localTope mtope' $
                TypeFunT ty orig param' mtope' <$> nfT ret
          AppT ty f x ->
            whnfT f >>= \case
              LambdaT _ty _orig _arg body ->
                nfT (substituteT x body)
              f' -> typeOf f' >>= \case
                TypeFunT _ty _orig _param (Just tope) UniverseTopeT{} -> do
                  topeAndT
                    <$> (AppT ty <$> nfT f' <*> nfT x)
                    <*> nfT (substituteT x tope)
                _ -> AppT ty <$> nfT f' <*> nfT x
          LambdaT ty orig _mparam body -> do
            case stripTypeRestrictions (infoType ty) of
              TypeFunT _ty _orig param mtope _ret -> do
                param' <- nfT param
                enterScope orig param' $ do
                  mtope' <- traverse nfT mtope
                  maybe id localTope mtope' $
                    LambdaT ty orig (Just (param', mtope')) <$> nfT body
              _ -> panicImpossible "lambda with a non-function type"


          TypeSigmaT ty orig a b -> do
            a' <- nfT a
            enterScope orig a' $ do
              TypeSigmaT ty orig a' <$> nfT b
          PairT ty l r -> PairT ty <$> nfT l <*> nfT r
          FirstT ty t ->
            whnfT t >>= \case
              PairT _ l _r -> nfT l
              t'           -> FirstT ty <$> nfT t'
          SecondT ty t ->
            whnfT t >>= \case
              PairT _ _l r -> nfT r
              t'           -> SecondT ty <$> nfT t'

          TypeIdT ty x _tA y -> TypeIdT ty <$> nfT x <*> pure Nothing <*> nfT y
          IdJT ty tA a tC d x p ->
            whnfT p >>= \case
              ReflT{} -> nfT d
              p' -> IdJT ty <$> nfT tA <*> nfT a <*> nfT tC <*> nfT d <*> nfT x <*> nfT p'

          RecOrT _ty rs -> do
            let go [] = pure Nothing
                go ((tope, tt') : rs') = do
                  checkTope tope >>= \case
                    True  -> pure (Just tt')
                    False -> go rs'
            go rs >>= \case
              Just tt' -> nfT tt'
              Nothing
                | [tt'] <- nubTermT (map snd rs) -> nfT tt'
                | otherwise -> pure tt


          TypeRestrictedT ty type_ rs -> do
            rs' <- forM rs $ \(tope, term) -> do
              nfTope tope >>= \case
                TopeBottomT{} -> pure Nothing
                tope' -> do
                  term' <- localTope tope' $
                    nfT term
                  return (Just (tope', term'))
            case catMaybes rs' of
              []   -> nfT type_
              rs'' -> TypeRestrictedT ty <$> nfT type_ <*> pure rs''

checkDefinedVar :: VarIdent -> TypeCheck VarIdent ()
checkDefinedVar x = asks (lookup x . varInfos) >>= \case
  Nothing  -> issueTypeError $ TypeErrorUndefined x
  Just _ty -> return ()

valueOfVar :: Eq var => var -> TypeCheck var (Maybe (TermT var))
valueOfVar x = asks (lookup x . varValues) >>= \case
  Nothing -> issueTypeError $ TypeErrorUndefined x
  Just ty -> return ty

typeOfVar :: Eq var => var -> TypeCheck var (TermT var)
typeOfVar x = asks (lookup x . varTypes) >>= \case
  Nothing -> issueTypeError $ TypeErrorUndefined x
  Just ty -> return ty

typeOfUncomputed :: Eq var => TermT var -> TypeCheck var (TermT var)
typeOfUncomputed = \case
  Pure x                     -> typeOfVar x
  Free (AnnF TypeInfo{..} _) -> pure infoType

typeOf :: Eq var => TermT var -> TypeCheck var (TermT var)
typeOf t = typeOfUncomputed t >>= whnfT

unifyTopes :: Eq var => TermT var -> TermT var -> TypeCheck var ()
unifyTopes l r = do
  equiv <- (&&)
    <$> [l] `entailM` r
    <*> [r] `entailM` l
  unless equiv $
    issueTypeError (TypeErrorTopesNotEquivalent l r)

inAllSubContexts :: TypeCheck var () -> TypeCheck var () -> TypeCheck var ()
inAllSubContexts handleSingle tc = do
  topeSubContexts <- asks localTopesNFUnion
  case topeSubContexts of
    [] -> panicImpossible "empty set of alternative contexts"
    [_] -> handleSingle
    _:_:_ -> do
      forM_ topeSubContexts $ \topes' -> do
        local (\Context{..} -> Context
            { localTopes = topes'
            , localTopesNF = topes'
            , localTopesNFUnion = [topes']
            , .. }) $
          tc

unify :: Eq var => Maybe (TermT var) -> TermT var -> TermT var -> TypeCheck var ()
unify mterm expected actual = performUnification `catchError` \typeError -> do
  inAllSubContexts (throwError typeError) performUnification
  where
    performUnification = unifyInCurrentContext mterm expected actual

unifyViaDecompose :: Eq var => TermT var -> TermT var -> TypeCheck var ()
unifyViaDecompose expected actual | expected == actual = return ()
unifyViaDecompose (AppT _ f x) (AppT _ g y) = do
  unifyTerms f g
  unifyTerms x y
unifyViaDecompose _ _ = issueTypeError (TypeErrorOther "cannot decompose")

unifyInCurrentContext :: Eq var => Maybe (TermT var) -> TermT var -> TermT var -> TypeCheck var ()
unifyInCurrentContext mterm expected actual = performing action $
  asks localTopesEntailBottom >>= \inBottom -> unless inBottom $
  unifyViaDecompose expected actual `catchError` \_ -> do      -- NOTE: this gives a small, but noticeable speedup
    expectedVal <- whnfT expected
    actualVal <- whnfT actual
    mea <- asks covariance >>= \case
      Covariant     -> Just <$> etaMatch mterm expectedVal actualVal
      Contravariant -> Just . swap <$> etaMatch mterm actualVal expectedVal
      Invariant     -> traceTypeCheck Debug "invariant" $ do
        -- FIXME: inefficient
        traceTypeCheck Debug "invariant->covariant" $ 
          setVariance Covariant     $ unifyInCurrentContext mterm expectedVal actualVal
        traceTypeCheck Debug "invariant->contravariant" $ 
          setVariance Contravariant $ unifyInCurrentContext mterm expectedVal actualVal
        return Nothing
    case mea of
      Nothing -> return ()
      Just (expected', actual') ->
        unless (expected' == actual') $ do  -- NOTE: this gives a small, but noticeable speedup
          case actual' of
            RecBottomT{} -> return ()
            RecOrT _ty rs' ->
              case expected' of
                RecOrT _ty rs -> forM_ [(l, r) | l <- rs, r <- rs'] $
                  \((ltope, lterm), (rtope, rterm)) ->
                    localTope (topeAndT ltope rtope) $
                      unify mterm lterm rterm
                _ -> do
                  forM_ rs' $ \(tope, term) ->
                    localTope tope $
                      unify mterm expected' term
            _ -> typeOf expected' >>= typeOf >>= \case
              UniverseCubeT{} -> contextEntails (topeEQT expected' actual')
              _ -> do
                let def = unless (expected' == actual') err
                    err =
                      case mterm of
                        Nothing   -> issueTypeError (TypeErrorUnifyTerms expected' actual')
                        Just term -> issueTypeError (TypeErrorUnify term expected' actual')
                    errS = do
                      let expectedS = S <$> expected'
                          actualS = S <$> actual'
                      case mterm of
                        Nothing   -> issueTypeError (TypeErrorUnifyTerms expectedS actualS)
                        Just term -> issueTypeError (TypeErrorUnify (S <$> term) expectedS actualS)
                case expected' of
                  Pure{} -> def

                  UniverseT{} -> def
                  UniverseCubeT{} -> def
                  UniverseTopeT{} -> def

                  TypeUnitT{} -> def
                  UnitT{} -> return ()  -- Unit always unifies!

                  CubeUnitT{} -> def
                  CubeUnitStarT{} -> def
                  Cube2T{} -> def
                  Cube2_0T{} -> def
                  Cube2_1T{} -> def
                  CubeProductT _ l r ->
                    case actual' of
                      CubeProductT _ l' r' -> do
                        unifyTerms l l'
                        unifyTerms r r'
                      _ -> err

                  PairT _ty l r ->
                    case actual' of
                      PairT _ty' l' r' -> do
                        unifyTerms l l'
                        unifyTerms r r'

                      -- one part of eta-expansion for pairs
                      -- FIXME: add symmetric version!
                      _ -> err

                  FirstT _ty t ->
                    case actual' of
                      FirstT _ty' t' -> unifyTerms t t'
                      _              -> err

                  SecondT _ty t ->
                    case actual' of
                      SecondT _ty' t' -> unifyTerms t t'
                      _               -> err

                  TopeTopT{}    -> unifyTopes expected' actual'
                  TopeBottomT{} -> unifyTopes expected' actual'
                  TopeEQT{}     -> unifyTopes expected' actual'
                  TopeLEQT{}    -> unifyTopes expected' actual'
                  TopeAndT{}    -> unifyTopes expected' actual'
                  TopeOrT{}     -> unifyTopes expected' actual'

                  RecBottomT{} -> return () -- unifies with anything
                  RecOrT _ty rs ->
                    case actual' of
                      -- ----------------------------------------------
                      -- IMPORTANT: this pattern matching is redundant,
                      -- but it is not obvious, so
                      -- take care when refactoring!
                      -- ----------------------------------------------
      --                RecOrT _ty rs' -> sequence_ $
      --                  checkCoherence <$> rs <*> rs'
                      -- ----------------------------------------------
                      _ -> do
                        forM_ rs $ \(tope, term) ->
                          localTope tope $
                            unify mterm term actual'

                  TypeFunT _ty _orig cube mtope ret ->
                    case actual' of
                      TypeFunT _ty' orig' cube' mtope' ret' -> do
                        switchVariance $  -- unifying in the negative position!
                          unify Nothing cube cube' -- FIXME: unifyCubes
                        enterScope orig' cube $ do
                          variance <- asks covariance
                          case ret' of
                            UniverseTopeT{} -> do
                              -- This is the case for tope families (shapes)
                              --
                              -- (Λ → TOPE) <: (Δ → TOPE)
                              -- since if φ : Λ → TOPE
                              -- then φ ⊢ Δ
                              --
                              -- we DO NOT take tope context Φ into account!
                              expectedTopeNF <- fromMaybe topeTopT <$> traverse nfT mtope
                              actualTopeNF   <- fromMaybe topeTopT <$> traverse nfT mtope'
                              let superEntailedBySub superNF subNF = do
                                    entails <- [subNF] `entailM` superNF
                                    unless entails $
                                      issueTypeError (TypeErrorTopeNotSatisfied [subNF] superNF)
                              bySubtyping superEntailedBySub expectedTopeNF actualTopeNF
                            _ -> do
                              -- this is the case for Π-types and extension types
                              --
                              -- Ξ | Φ | Γ   ⊢   {t : I | φ} → A t   <:   {s : J | ψ} → B s
                              -- when
                              -- Ξ | Φ, ψ ⊢ φ
                              expectedTopeNF <- fromMaybe topeTopT <$> traverse nfT mtope
                              actualTopeNF   <- fromMaybe topeTopT <$> traverse nfT mtope'
                              bySubtyping domainEntails expectedTopeNF actualTopeNF
                          -- Codomains are compared on the supertype's domain.
                          -- Invariant comparison runs both directional checks.
                          let domain = case variance of
                                Covariant -> mtope
                                Contravariant -> mtope'
                                Invariant -> mtope
                          maybe id localTope domain $ case mterm of
                            Nothing -> unify Nothing ret ret'
                            Just term -> unifyTypes (appT ret' (S <$> term) (Pure Z)) ret ret'
                      _ -> err

                  TypeSigmaT _ty _orig a b ->
                    case actual' of
                      TypeSigmaT _ty' orig' a' b' -> do
                        unify Nothing a a'
                        enterScope orig' a' $ unify Nothing b b'
                      _ -> err

                  TypeIdT _ty x tA y ->
                    case actual' of
                      TypeIdT _ty' x' tA' y' -> do
                        -- The underlying types must be compared: without this
                        -- check the routine equates identity types over
                        -- different types whenever the endpoints unify,
                        -- accepting e.g. a free homotopy (a path in the type
                        -- of functions) where an endpoint-fixing one (a path
                        -- in a hom-type) is expected. Compared invariantly:
                        -- subtyping between the underlying types must not
                        -- leak into equality of identity types over them.
                        --
                        -- The stored type annotation may be absent (nfT erases
                        -- it); recover it from an endpoint's type in that case,
                        -- so the comparison is robust however the identity type
                        -- was produced.
                        t1 <- maybe (typeOf x)  pure tA
                        t2 <- maybe (typeOf x') pure tA'
                        unifyTerms t1 t2
                        unifyTerms x x'
                        unifyTerms y y'
                      _ -> err

                  AppT _ty f x ->
                    case actual' of
                      AppT _ty' f' x' -> do
                        unifyTerms f f'
                        unifyTerms x x'
                      _ -> err

                  LambdaT ty _orig _mparam body ->
                    case stripTypeRestrictions (infoType ty) of
                      TypeFunT _ty _origF param mtope _ret ->
                        case actual' of
                          LambdaT ty' orig' _mparam' body' -> do
                            case stripTypeRestrictions (infoType ty') of
                              TypeFunT _ty' _origF' param' mtope' _ret' -> do
                                unifyTerms param param' -- we (should) have already checked this in types!
                                enterScope orig' param $ do
                                  case (mtope, mtope') of
                                    (Just tope, Just tope') -> do
                                      -- η-expansion uses the function's own domain.
                                      -- Compare domains as for Π-types, then compare
                                      -- bodies where both functions are defined.
                                      bySubtyping domainEntails tope tope'
                                      localTope (topeAndT tope tope') $
                                        unifyTerms body body'
                                    (Nothing, Nothing) -> do
                                      unifyTerms body body'
                                    _ -> errS
                              _ -> err
                          _ -> err
                      _ -> err

                  ReflT ty _x | TypeIdT _ty x _tA y <- infoType ty ->
                    case actual' of
                      ReflT ty' _x' | TypeIdT _ty' x' _tA' y' <- infoType ty' -> do
                        -- unifyTerms tA tA' -- TODO: do we need this check?
                        unifyTerms x x'
                        unifyTerms y y'
                      _ -> err
                  ReflT{} -> panicImpossible "refl with a non-identity type!"

                  IdJT _ty a b c d e f ->
                    case actual' of
                      IdJT _ty' a' b' c' d' e' f' -> do
                        unifyTerms a a'
                        unifyTerms b b'
                        unifyTerms c c'
                        unifyTerms d d'
                        unifyTerms e e'
                        unifyTerms f f'
                      _ -> err

                  TypeAscT{} -> panicImpossible "type ascription at the root of WHNF"

                  TypeRestrictedT _ty ty rs ->
                    case actual' of
                      TypeRestrictedT _ty' ty' rs' -> do
                        unify mterm ty ty'
                        -- The faces of the supertype must be covered by the faces of
                        -- the subtype (the subtype is at least as specified), with
                        -- the boundary terms agreeing on overlaps. Which side is the
                        -- subtype depends on the ambient variance.
                        variance <- asks covariance
                        let subCoversSuper subRs superRs = sequence_
                              [ localTope tope $ do
                                  -- FIXME: can do less entails checks?
                                  contextEntails (foldr topeOrT topeBottomT (map fst subRs))
                                  forM_ subRs $ \(tope', term') -> do
                                    localTope tope' $
                                      unifyTerms term term'
                              | (tope, term) <- superRs
                              ]
                        case variance of
                          Covariant     -> subCoversSuper rs' rs
                          Contravariant -> subCoversSuper rs rs'
                          Invariant     -> do
                            subCoversSuper rs' rs
                            subCoversSuper rs rs'
                      _ -> err    -- FIXME: need better unification for restrictions
  where
    action = case mterm of
               Nothing   -> ActionUnifyTerms expected actual
               Just term -> ActionUnify term expected actual

unifyTypes :: Eq var => TermT var -> TermT var -> TermT var -> TypeCheck var ()
unifyTypes = unify . Just

-- | Compare terms by conversion. Types used as terms must also be convertible.
-- Use 'unify' for directional comparisons of types.
unifyTerms :: Eq var => TermT var -> TermT var -> TypeCheck var ()
unifyTerms expected actual = setVariance Invariant (unify Nothing expected actual)

localTope :: Eq var => TermT var -> TypeCheck var a -> TypeCheck var a
localTope tope tc = do
  Context{..} <- ask
  tope' <- nfTope tope
  -- A small optimisation to help unify terms faster
  let refine = case tope' of
        TopeEQT _ x y | x == y -> const tc          -- no new information added!
        _ | tope' `elem` localTopes -> const tc     -- no new information added!
          | otherwise -> id
  refine $ do
    local (f tope' localTopesNF) tc
  where
    f tope' localTopes' Context{..} = Context
      { localTopes = tope : localTopes
      , localTopesNF = tope' : localTopesNF
      , localTopesNFUnion = map nubTermT
          [ new <> old
          | new <- simplifyLHSwithDisjunctions [tope']
          , old <- localTopesNFUnion ]
      , localTopesEntailBottom = entailsBottom
      , .. }
      where
        entailsBottom = (tope' : localTopes') `entail` topeBottomT

universeT :: TermT var
universeT = iterate f (panicImpossible msg) !! 30
  where
    msg = "going too high up the universe levels"
    f t = UniverseT TypeInfo
      { infoType = t
      , infoNF = Just universeT
      , infoWHNF = Just universeT }

cubeT :: TermT var
cubeT = UniverseCubeT TypeInfo
  { infoType = universeT
  , infoNF = Just cubeT
  , infoWHNF = Just cubeT }

topeT :: TermT var
topeT = UniverseTopeT TypeInfo
  { infoType = universeT
  , infoNF = Just topeT
  , infoWHNF = Just topeT }

topeEQT :: TermT var -> TermT var -> TermT var
topeEQT l r = TopeEQT info l r
  where
    info = TypeInfo
      { infoType = topeT
      , infoNF = Nothing
      , infoWHNF = Nothing
      }

topeLEQT :: TermT var -> TermT var -> TermT var
topeLEQT l r = TopeLEQT info l r
  where
    info = TypeInfo
      { infoType = topeT
      , infoNF = Nothing
      , infoWHNF = Nothing
      }

topeOrT :: TermT var -> TermT var -> TermT var
topeOrT l r = TopeOrT info l r
  where
    info = TypeInfo
      { infoType = topeT
      , infoNF = Nothing
      , infoWHNF = Nothing
      }

topeAndT :: TermT var -> TermT var -> TermT var
topeAndT l r = TopeAndT info l r
  where
    info = TypeInfo
      { infoType = topeT
      , infoNF = Nothing
      , infoWHNF = Nothing
      }

cubeProductT :: TermT var -> TermT var -> TermT var
cubeProductT l r = t
  where
    t = CubeProductT info l r
    info = TypeInfo
      { infoType  = cubeT
      , infoNF    = Nothing
      , infoWHNF  = Nothing
      }

cubeUnitT :: TermT var
cubeUnitT = CubeUnitT TypeInfo
  { infoType = cubeT
  , infoNF = Just cubeUnitT
  , infoWHNF = Just cubeUnitT }

cubeUnitStarT :: TermT var
cubeUnitStarT = CubeUnitStarT TypeInfo
  { infoType = cubeUnitT
  , infoNF = Just cubeUnitStarT
  , infoWHNF = Just cubeUnitStarT }

typeUnitT :: TermT var
typeUnitT = TypeUnitT TypeInfo
  { infoType = universeT
  , infoNF = Just typeUnitT
  , infoWHNF = Just typeUnitT }

unitT :: TermT var
unitT = UnitT TypeInfo
  { infoType = typeUnitT
  , infoNF = Just unitT
  , infoWHNF = Just unitT }

cube2T :: TermT var
cube2T = Cube2T TypeInfo
  { infoType = cubeT
  , infoNF = Just cube2T
  , infoWHNF = Just cube2T }

cube2_0T :: TermT var
cube2_0T = Cube2_0T TypeInfo
  { infoType = cube2T
  , infoNF = Just cube2_0T
  , infoWHNF = Just cube2_0T }

cube2_1T :: TermT var
cube2_1T = Cube2_1T TypeInfo
  { infoType = cube2T
  , infoNF = Just cube2_1T
  , infoWHNF = Just cube2_1T }

topeTopT :: TermT var
topeTopT = TopeTopT TypeInfo
  { infoType = topeT
  , infoNF = Just topeTopT
  , infoWHNF = Just topeTopT }

topeBottomT :: TermT var
topeBottomT = TopeBottomT TypeInfo
  { infoType = topeT
  , infoNF = Just topeBottomT
  , infoWHNF = Just topeBottomT }

recBottomT :: TermT var
recBottomT = RecBottomT TypeInfo
  { infoType = recBottomT
  , infoNF = Just recBottomT
  , infoWHNF = Just recBottomT }

typeRestrictedT :: TermT var -> [(TermT var, TermT var)] -> TermT var
typeRestrictedT ty rs = t
  where
    t = TypeRestrictedT info ty rs
    info = TypeInfo
      { infoType  = universeT
      , infoNF    = Nothing
      , infoWHNF  = Nothing
      }

lambdaT
  :: TermT var
  -> Maybe VarIdent
  -> Maybe (TermT var, Maybe (Scope TermT var))
  -> Scope TermT var
  -> TermT var
lambdaT ty orig mparam body = t
  where
    t = LambdaT info orig mparam body
    info = TypeInfo
      { infoType  = ty
      , infoNF    = Nothing
      , infoWHNF  = Just t
      }

appT :: TermT var -> TermT var -> TermT var -> TermT var
appT ty f x = t
  where
    t = AppT info f x
    info = TypeInfo
      { infoType  = ty
      , infoNF    = Nothing
      , infoWHNF  = Nothing
      }

pairT :: TermT var -> TermT var -> TermT var -> TermT var
pairT ty l r = t
  where
    t = PairT info l r
    info = TypeInfo
      { infoType  = ty
      , infoNF    = Nothing
      , infoWHNF  = Just t
      }

firstT :: TermT var -> TermT var -> TermT var
firstT ty arg = t
  where
    t = FirstT info arg
    info = TypeInfo
      { infoType  = ty
      , infoNF    = Nothing
      , infoWHNF  = Nothing
      }

secondT :: TermT var -> TermT var -> TermT var
secondT ty arg = t
  where
    t = SecondT info arg
    info = TypeInfo
      { infoType  = ty
      , infoNF    = Nothing
      , infoWHNF  = Nothing
      }

reflT
  :: TermT var
  -> Maybe (TermT var, Maybe (TermT var))
  -> TermT var
reflT ty mx = t
  where
    t = ReflT info mx
    info = TypeInfo
      { infoType  = ty
      , infoNF    = Just (ReflT info Nothing)
      , infoWHNF  = Just (ReflT info Nothing)
      }

typeFunT
  :: Maybe VarIdent
  -> TermT var
  -> Maybe (Scope TermT var)
  -> Scope TermT var
  -> TermT var
typeFunT orig cube mtope ret = t
  where
    t = TypeFunT info orig cube mtope ret
    info = TypeInfo
      { infoType  = universeT
      , infoNF    = Nothing
      , infoWHNF  = Just t
      }

typeSigmaT
  :: Maybe VarIdent
  -> TermT var
  -> Scope TermT var
  -> TermT var
typeSigmaT orig a b = t
  where
    t = TypeSigmaT info orig a b
    info = TypeInfo
      { infoType  = universeT
      , infoNF    = Nothing
      , infoWHNF  = Just t
      }

recOrT
  :: TermT var
  -> [(TermT var, TermT var)]
  -> TermT var
recOrT ty rs = t
  where
    t = RecOrT info rs
    info = TypeInfo
      { infoType  = ty
      , infoNF    = Nothing
      , infoWHNF  = Nothing
      }

typeIdT :: TermT var -> Maybe (TermT var) -> TermT var -> TermT var
typeIdT x tA y = t
  where
    t = TypeIdT info x tA y
    info = TypeInfo
      { infoType  = universeT
      , infoNF    = Nothing
      , infoWHNF  = Just t
      }

idJT
  :: TermT var
  -> TermT var
  -> TermT var
  -> TermT var
  -> TermT var
  -> TermT var
  -> TermT var
  -> TermT var
idJT ty tA a tC d x p = t
  where
    t = IdJT info tA a tC d x p
    info = TypeInfo
      { infoType  = ty
      , infoNF    = Nothing
      , infoWHNF  = Nothing
      }

typeAscT :: TermT var -> TermT var -> TermT var
typeAscT x ty = t
  where
    t = TypeAscT info x ty
    info = TypeInfo
      { infoType  = ty
      , infoNF    = Nothing
      , infoWHNF  = Nothing
      }

typecheck :: Eq var => Term var -> TermT var -> TypeCheck var (TermT var)
typecheck term ty = performing (ActionTypeCheck term ty) $ do
  whnfT ty >>= \case

    RecBottomT{} -> do
      return recBottomT

    TypeRestrictedT _ty ty' rs -> do
      term' <- typecheck term ty'
      contextEntailedBy (foldr topeOrT topeBottomT (map fst rs))
      forM_ rs $ \(tope, rterm) -> do
        checkTopeAgainstContext "restriction face" tope
        localTope tope $
          unifyTerms rterm term'
      return term'    -- FIXME: correct?

    ty' -> case term of
      Lambda orig mparam body ->
        case ty' of
          TypeFunT _ty _orig' param' mtope' ret -> do
            case mparam of
              Nothing -> return ()
              Just (param, Nothing) -> do
                (paramType, mtope) <- do
                  paramType <- infer param
                  typeOf paramType >>= \case
                    -- an argument can be a shape
                    TypeFunT _ty _orig cube _mtope UniverseTopeT{} -> do
                      mapM_ checkNameShadowing orig
                      enterScope orig cube $ do
                        let tope' = appT topeT (S <$> paramType) (Pure Z)  -- eta expand ty'
                        return (cube, Just tope')
                    _kind -> return (paramType, Nothing)
                unifyTerms param' paramType
                mapM_ checkNameShadowing orig
                enterScope orig param' $ do
                  mapM_ (unifyTerms (fromMaybe topeTopT mtope')) mtope
              Just (param, mtope) -> do
                param'' <- typecheck param =<< typeOf param'
                unifyTerms param' param''
                mapM_ checkNameShadowing orig
                enterScope orig param' $ do
                  mtope'' <- typecheck (fromMaybe TopeTop mtope) topeT
                  unifyTerms (fromMaybe topeTopT mtope') mtope''

            mapM_ checkNameShadowing orig
            origs <- asks varOrigs
            let fallback = zip (nub (freeVars term)) defaultVarIdents
                name var = fromMaybe "_" (join (lookup var origs) <|> lookup var fallback)
                family = show (name <$> term)
            enterScope orig param' $ do
              maybe id localTope mtope' $ do
                body' <- typecheck body ret
                -- As in rzk-lang/rzk#359, a concrete tope family carries its
                -- declared domain, matching the domain conjunct in nfTope.
                -- Test inclusion without assuming the domain itself.
                body'' <- case (ret, mtope') of
                  (UniverseTopeT{}, Just domain) -> do
                    bodyNF <- nfT body'
                    domainNF <- nfT domain
                    included <- [bodyNF] `entailM` domainNF
                    if included then pure body' else do
                      warn <- asks warnTopeFamilyDomain
                      when warn $ do
                        domainText <- ppTermInContext domainNF
                        loc <- asks location
                        tell [TopeFamilyDomainWarning family domainText loc]
                      pure (topeAndT domain body')
                  _ -> pure body'
                return (lambdaT ty' orig (Just (param', mtope')) body'')

          _ -> issueTypeError $ TypeErrorUnexpectedLambda term ty

      Pair l r ->
        case ty' of
          CubeProductT _ty a b -> do
            l' <- typecheck l a
            r' <- typecheck r b
            return (pairT ty' l' r')
          TypeSigmaT _ty _orig a b -> do
            l' <- typecheck l a
            r' <- typecheck r (substituteT l' b)
            return (pairT ty' l' r')
          _ -> issueTypeError $ TypeErrorUnexpectedPair term ty

      Refl mx ->
        case ty' of
          TypeIdT _ty y _tA z -> do
            tA <- typeOf y
            forM_ mx $ \(x, mxty) -> do
              forM_ mxty $ \xty -> do
                xty' <- typecheck xty universeT
                unifyTerms tA xty'
              x' <- typecheck x tA
              unifyTerms x' y
              unifyTerms x' z
            when (isNothing mx) $
              unifyTerms y z
            return (reflT ty' (Just (y, Just tA)))
          _ -> issueTypeError $ TypeErrorUnexpectedRefl term ty

        -- FIXME: this does not make typechecking faster, why?
--      RecOr rs -> do
--        rs' <- forM rs $ \(tope, rterm) -> do
--          tope' <- typecheck tope topeT
--          contextEntailedBy tope'
--          localTope tope' $ do
--            rterm' <- typecheck rterm ty
--            return (tope', rterm')
--        return (recOrT ty rs')

      _ -> do
        term' <- infer term
        inferredType <- typeOf term'
        unifyTypes term' ty' inferredType
        return term'

inferAs :: Eq var => TermT var -> Term var -> TypeCheck var (TermT var)
inferAs expectedKind term = do
  term' <- infer term
  ty <- typeOf term'
  kind <- typeOf ty
  unifyTypes ty expectedKind kind
  return term'

infer :: Eq var => Term var -> TypeCheck var (TermT var)
infer tt = performing (ActionInfer tt) $ case tt of
  Pure x -> pure (Pure x)

  Universe     -> pure universeT
  UniverseCube -> pure cubeT
  UniverseTope -> pure topeT

  CubeUnit      -> pure cubeUnitT
  CubeUnitStar  -> pure cubeUnitStarT

  Cube2 -> pure cube2T
  Cube2_0 -> pure cube2_0T
  Cube2_1 -> pure cube2_1T

  CubeProduct l r -> do
    l' <- typecheck l cubeT
    r' <- typecheck r cubeT
    return (cubeProductT l' r')

  Pair l r -> do
    l' <- infer l
    r' <- infer r
    lt <- typeOf l'
    rt <- typeOf r'
    typeOf lt >>= \case
      --    Γ ⊢ l ⇒ (I : CUBE)
      --    Γ ⊢ r ⇒ (J : CUBE)
      -- ———————————————————————————
      -- Γ ⊢ (l, r) ⇒ (I × J : CUBE)
      UniverseCubeT{} -> return (pairT (cubeProductT lt rt) l' r')
      --    Γ ⊢ l ⇒ (A : U)
      --    Γ ⊢ r ⇒ (B : U)
      -- ———————————————————————————
      -- Γ ⊢ (l, r) ⇒ (A × B : U)             where A × B = Σ (_ : A), B
      _ -> do
        -- NOTE: infer as a non-dependent pair!
        return (pairT (typeSigmaT Nothing lt (S <$> rt)) l' r')

  First t -> do
    t' <- infer t
    fmap stripTypeRestrictions (typeOf t') >>= \case
      RecBottomT{} -> pure recBottomT -- FIXME: is this ok?
      TypeSigmaT _ty _orig lt _rt ->
        return (firstT lt t')
      CubeProductT _ty l _r ->
        return (firstT l t')
      ty -> issueTypeError $ TypeErrorNotPair t' ty

  Second t -> do
    t' <- infer t
    fmap stripTypeRestrictions (typeOf t') >>= \case
      RecBottomT{} -> pure recBottomT -- FIXME: is this ok?
      TypeSigmaT _ty _orig lt rt ->
        return (secondT (substituteT (firstT lt t') rt) t')
      CubeProductT _ty _l r ->
        return (secondT r t')
      ty -> issueTypeError $ TypeErrorNotPair t' ty

  TypeUnit -> pure typeUnitT
  Unit -> pure unitT

  TopeTop -> pure topeTopT
  TopeBottom -> pure topeBottomT

  TopeEQ l r -> do
    l' <- inferAs cubeT l
    lt <- typeOf l'
    r' <- typecheck r lt
    return (topeEQT l' r')

  TopeLEQ l r -> do
    l' <- typecheck l cube2T
    r' <- typecheck r cube2T
    return (topeLEQT l' r')

  TopeAnd l r -> do
    l' <- typecheck l topeT
    r' <- typecheck r topeT
    return (topeAndT l' r')

  TopeOr l r -> do
    l' <- typecheck l topeT
    r' <- typecheck r topeT
    return (topeOrT l' r')

  RecBottom -> do
    contextEntails topeBottomT
    return recBottomT

  -- Γ ⊢ t ⇒ (T : K)
  -- Γ ⊢ K ≡ U
  -- —————————————
  -- Γ ⊢ t ⇒ T ⇐ U

  RecOr rs -> do
    ttts <- forM rs $ \(tope, term) -> do
      tope' <- typecheck tope topeT
      contextEntailedBy tope'
      localTope tope' $ do
        term' <- inferAs universeT term
        ty <- typeOf term'
        return (tope', (term', ty))
    let rs' = map (fmap fst) ttts
        ts  = map (fmap snd) ttts
    sequence_ [ checkCoherence l r | l:rs'' <- tails rs', r <- rs'' ]
    contextEquiv (map fst ttts)
    return (recOrT (recOrT universeT ts) rs')

  TypeFun orig a Nothing b -> do
    a' <- infer a
    typeOf a' >>= \case
      -- an argument can be a type
      UniverseT{} ->
        case a' of
          -- except if its a TOPE universe
          UniverseTopeT{} ->
            issueTypeError $ TypeErrorOther "tope params are illegal"
          _ -> do
            mapM_ checkNameShadowing orig
            b' <- enterScope orig a' $ typecheck b universeT
            return (typeFunT orig a' Nothing b')
      -- an argument can be a cube
      UniverseCubeT{} -> do
        mapM_ checkNameShadowing orig
        b' <- enterScope orig a' $ typecheck b universeT
        return (typeFunT orig a' Nothing b')
      -- an argument can be a shape
      TypeFunT _ty _orig cube mtope UniverseTopeT{} -> do
        mapM_ checkNameShadowing orig
        enterScope orig cube $ do
          let tope' = appT topeT (S <$> a') (Pure Z)  -- eta expand a'
          localTope tope' $ do
            b' <- typecheck b universeT
            case mtope of
              Nothing -> return (typeFunT orig cube (Just tope') b')
              Just tope'' -> return (typeFunT orig cube (Just (topeAndT tope'' tope')) b')
      ty -> issueTypeError $ TypeErrorInvalidArgumentType a ty

  TypeFun orig cube (Just tope) ret -> do
    cube' <- typecheck cube cubeT
    mapM_ checkNameShadowing orig
    enterScope orig cube' $ do
      tope' <- typecheck tope topeT
      localTope tope' $ do
        ret' <- typecheck ret universeT
        return (typeFunT orig cube' (Just tope') ret')

  TypeSigma orig a b -> do
    a' <- typecheck a universeT
    mapM_ checkNameShadowing orig
    b' <- enterScope orig a' $ typecheck b universeT
    return (typeSigmaT orig a' b')

  TypeId x (Just tA) y -> do
    tA' <- typecheck tA universeT
    x' <- typecheck x tA'
    y' <- typecheck y tA'
    return (typeIdT x' (Just tA') y')

  TypeId x Nothing y -> do
    x' <- inferAs universeT x
    tA <- typeOf x'
    y' <- typecheck y tA
    return (typeIdT x' (Just tA) y')

  App f x -> do
    f' <- inferAs universeT f
    fmap stripTypeRestrictions (typeOf f') >>= \case
      TypeFunT _ty _orig a mtope b -> do
        x' <- typecheck x a
        let result = appT (substituteT x' b) f' x'
        case b of
          UniverseTopeT{} -> do
            case mtope of
              Nothing -> return result
              Just tope -> do
                return (topeAndT (substituteT x' tope) result)
          _               -> do
            mapM_ (contextEntails . substituteT x') mtope   -- FIXME: need to check?
            return result
      ty -> issueTypeError $ TypeErrorNotFunction f' ty

  Lambda _orig Nothing _body -> do
    issueTypeError $ TypeErrorCannotInferBareLambda tt
  Lambda orig (Just (ty, Nothing)) body -> do
    ty' <- infer ty
    mtope <- typeOf ty' >>= \case
      -- an argument can be a type
      UniverseT{} ->
        case ty' of
          -- except if its a TOPE universe
          UniverseTopeT{} ->
            issueTypeError $ TypeErrorOther "tope params are illegal"
          _ -> return Nothing
      -- an argument can be a cube
      UniverseCubeT{} -> return Nothing
      -- an argument can be a shape
      TypeFunT _ty _orig cube _mtope UniverseTopeT{} -> do
        mapM_ checkNameShadowing orig
        enterScope orig cube $ do
          let tope' = appT topeT (S <$> ty') (Pure Z)  -- eta expand ty'
          return (Just tope')
      kind -> issueTypeError $ TypeErrorInvalidArgumentType ty kind
    mapM_ checkNameShadowing orig
    enterScope orig ty' $ do
      maybe id localTope mtope $ do
        body' <- infer body
        ret <- typeOf body'
        return (lambdaT (typeFunT orig ty' mtope ret) orig (Just (ty', mtope)) body')
  Lambda orig (Just (cube, Just tope)) body -> do
    cube' <- typecheck cube cubeT
    mapM_ checkNameShadowing orig
    enterScope orig cube' $ do
      tope' <- infer tope
      body' <- localTope tope' $ infer body
      ret <- typeOf body'
      return (lambdaT (typeFunT orig cube' (Just tope') ret) orig (Just (cube', Just tope')) body')

  Refl Nothing -> issueTypeError $ TypeErrorCannotInferBareRefl tt
  Refl (Just (x, Nothing)) -> do
    x' <- inferAs universeT x
    ty <- typeOf x'
    return (reflT (typeIdT x' (Just ty) x') (Just (x', Just ty)))
  Refl (Just (x, Just ty)) -> do
    ty' <- typecheck ty universeT
    x' <- typecheck x ty'
    return (reflT (typeIdT x' (Just ty') x') (Just (x', Just ty')))

  IdJ tA a tC d x p -> do
    tA' <- typecheck tA universeT
    a' <- typecheck a tA'
    let typeOf_C =
          typeFunT Nothing tA' Nothing $
            typeFunT Nothing (typeIdT (S <$> a') (Just (S <$> tA')) (Pure Z)) Nothing $
              universeT
    tC' <- typecheck tC typeOf_C
    let typeOf_d =
          appT universeT
            (appT (typeFunT Nothing (typeIdT a' (Just tA') a') Nothing universeT)
              tC' a')
            (reflT (typeIdT a' (Just tA') a') Nothing)
    d' <- typecheck d typeOf_d
    x' <- typecheck x tA'
    p' <- typecheck p (typeIdT a' (Just tA') x')
    let ret =
          appT universeT
            (appT (typeFunT Nothing (typeIdT a' (Just tA') x') Nothing universeT)
              tC' x')
            p'
    return (idJT ret tA' a' tC' d' x' p')

  TypeAsc term ty -> do
    ty' <- inferAs universeT ty -- this works on types AND cubes
    term' <- typecheck term ty'
    return (typeAscT term' ty')

  TypeRestricted ty rs -> do
    ty' <- typecheck ty universeT
    rs' <- forM rs $ \(tope, term) -> do
      tope' <- typecheck tope topeT
      term' <- localTope tope' $ typecheck term ty'
      return (tope', term')
    sequence_ [ checkCoherence l r | l:rs'' <- tails rs', r <- rs'' ]
    return (typeRestrictedT ty' rs')

checkCoherence
  :: Eq var
  => (TermT var, TermT var)
  -> (TermT var, TermT var)
  -> TypeCheck var ()
checkCoherence (ltope, lterm) (rtope, rterm) =
  performing (ActionCheckCoherence (ltope, lterm) (rtope, rterm)) $ do
    localTope (topeAndT ltope rtope) $ do
      ltype <- stripTypeRestrictions <$> typeOf lterm   -- FIXME: why strip?
      rtype <- stripTypeRestrictions <$> typeOf rterm   -- FIXME: why strip?
      -- FIXME: do we need to unify types here or is it included in unification of terms?
      unifyTerms ltype rtype
      unifyTerms lterm rterm

inferStandalone :: Eq var => Term var -> Either (TypeErrorInScopedContext var) (TermT var)
inferStandalone term = defaultTypeCheck (infer term)

unsafeInferStandalone' :: Term' -> TermT'
unsafeInferStandalone' term = unsafeTypeCheck' (infer term)

unsafeTypeCheck' :: TypeCheck VarIdent a -> a
unsafeTypeCheck' tc =
  case defaultTypeCheck tc of
    Left err     -> error $ ppTypeErrorInScopedContext' BottomUp err
    Right result -> result

type PointId = String
type ShapeId = [PointId]

cube2powerT :: Int -> TermT var
cube2powerT 1   = cube2T
cube2powerT dim = cubeProductT (cube2powerT (dim - 1)) cube2T

splits :: [a] -> [([a], [a])]
splits [] = [([], [])]
splits (x:xs) = ([], x:xs) : [ (x : before, after) | (before, after) <- splits xs ]

verticesFrom :: [TermT var] -> [(ShapeId, TermT var)]
verticesFrom ts = combine <$> mapM mk ts
  where
    mk t = [("0", topeEQT t cube2_0T), ("1", topeEQT t cube2_1T)]
    combine xs = ([concat (map fst xs)], foldr1 topeAndT (map snd xs))

subTopes2 :: Int -> TermT var -> [(ShapeId, TermT var)]
-- 1-dim
subTopes2 1 t =
  [ (words "0", topeEQT t cube2_0T)
  , (words "1", topeEQT t cube2_1T)
  , (words "0 1", topeTopT) ]
-- 2-dim
subTopes2 2 ts =
  -- vertices
  [ (words "00", topeEQT t cube2_0T `topeAndT` topeEQT s cube2_0T)
  , (words "01", topeEQT t cube2_0T `topeAndT` topeEQT s cube2_1T)
  , (words "10", topeEQT t cube2_1T `topeAndT` topeEQT s cube2_0T)
  , (words "11", topeEQT t cube2_1T `topeAndT` topeEQT s cube2_1T)
  -- edges and the diagonal
  , (words "00 01", topeEQT t cube2_0T)
  , (words "10 11", topeEQT t cube2_1T)
  , (words "00 10", topeEQT s cube2_0T)
  , (words "01 11", topeEQT s cube2_1T)
  , (words "00 11", topeEQT s t)
  -- triangles
  , (words "00 01 11", topeLEQT t s)
  , (words "00 10 11", topeLEQT s t)
  ]
  where
    t = firstT cube2T ts
    s = secondT cube2T ts
-- 3-dim
subTopes2 3 t =
  -- vertices
  [ (words "000", topeEQT t1 cube2_0T `topeAndT` topeEQT t2 cube2_0T `topeAndT` topeEQT t3 cube2_0T)
  , (words "001", topeEQT t1 cube2_0T `topeAndT` topeEQT t2 cube2_0T `topeAndT` topeEQT t3 cube2_1T)
  , (words "010", topeEQT t1 cube2_0T `topeAndT` topeEQT t2 cube2_1T `topeAndT` topeEQT t3 cube2_0T)
  , (words "011", topeEQT t1 cube2_0T `topeAndT` topeEQT t2 cube2_1T `topeAndT` topeEQT t3 cube2_1T)
  , (words "100", topeEQT t1 cube2_1T `topeAndT` topeEQT t2 cube2_0T `topeAndT` topeEQT t3 cube2_0T)
  , (words "101", topeEQT t1 cube2_1T `topeAndT` topeEQT t2 cube2_0T `topeAndT` topeEQT t3 cube2_1T)
  , (words "110", topeEQT t1 cube2_1T `topeAndT` topeEQT t2 cube2_1T `topeAndT` topeEQT t3 cube2_0T)
  , (words "111", topeEQT t1 cube2_1T `topeAndT` topeEQT t2 cube2_1T `topeAndT` topeEQT t3 cube2_1T)
  -- edges
  , (words "000 001", topeEQT t1 cube2_0T `topeAndT` topeEQT t2 cube2_0T)
  , (words "010 011", topeEQT t1 cube2_0T `topeAndT` topeEQT t2 cube2_1T)
  , (words "000 010", topeEQT t1 cube2_0T `topeAndT` topeEQT t3 cube2_0T)
  , (words "001 011", topeEQT t1 cube2_0T `topeAndT` topeEQT t3 cube2_1T)
  , (words "100 101", topeEQT t1 cube2_1T `topeAndT` topeEQT t2 cube2_0T)
  , (words "110 111", topeEQT t1 cube2_1T `topeAndT` topeEQT t2 cube2_1T)
  , (words "100 110", topeEQT t1 cube2_1T `topeAndT` topeEQT t3 cube2_0T)
  , (words "101 111", topeEQT t1 cube2_1T `topeAndT` topeEQT t3 cube2_1T)
  , (words "000 100", topeEQT t2 cube2_0T `topeAndT` topeEQT t3 cube2_0T)
  , (words "001 101", topeEQT t2 cube2_0T `topeAndT` topeEQT t3 cube2_1T)
  , (words "010 110", topeEQT t2 cube2_1T `topeAndT` topeEQT t3 cube2_0T)
  , (words "011 111", topeEQT t2 cube2_1T `topeAndT` topeEQT t3 cube2_1T)
  -- face diagonals
  , (words "000 011", topeEQT t1 cube2_0T `topeAndT` topeEQT t2 t3)
  , (words "100 111", topeEQT t1 cube2_1T `topeAndT` topeEQT t2 t3)
  , (words "000 101", topeEQT t2 cube2_0T `topeAndT` topeEQT t1 t3)
  , (words "010 111", topeEQT t2 cube2_1T `topeAndT` topeEQT t1 t3)
  , (words "000 110", topeEQT t3 cube2_0T `topeAndT` topeEQT t1 t2)
  , (words "001 111", topeEQT t3 cube2_1T `topeAndT` topeEQT t1 t2)
  -- the long diagonal
  , (words "000 111", topeEQT t3 t2 `topeAndT` topeEQT t2 t1)
  -- face triangles
  , (words "000 001 011", topeEQT t1 cube2_0T `topeAndT` topeLEQT t2 t3)
  , (words "000 010 011", topeEQT t1 cube2_0T `topeAndT` topeLEQT t3 t2)
  , (words "100 101 111", topeEQT t1 cube2_1T `topeAndT` topeLEQT t2 t3)
  , (words "100 110 111", topeEQT t1 cube2_1T `topeAndT` topeLEQT t3 t2)
  , (words "000 001 101", topeEQT t2 cube2_0T `topeAndT` topeLEQT t1 t3)
  , (words "000 100 101", topeEQT t2 cube2_0T `topeAndT` topeLEQT t3 t1)
  , (words "010 011 111", topeEQT t2 cube2_1T `topeAndT` topeLEQT t1 t3)
  , (words "010 110 111", topeEQT t2 cube2_1T `topeAndT` topeLEQT t3 t1)
  , (words "000 010 110", topeEQT t3 cube2_0T `topeAndT` topeLEQT t1 t2)
  , (words "000 100 110", topeEQT t3 cube2_0T `topeAndT` topeLEQT t2 t1)
  , (words "001 011 111", topeEQT t3 cube2_1T `topeAndT` topeLEQT t1 t2)
  , (words "001 101 111", topeEQT t3 cube2_1T `topeAndT` topeLEQT t2 t1)
  -- diagonal triangles
  , (words "000 001 111", topeEQT t1 t2 `topeAndT` topeLEQT t2 t3)
  , (words "000 010 111", topeEQT t1 t3 `topeAndT` topeLEQT t1 t2)
  , (words "000 100 111", topeEQT t2 t3 `topeAndT` topeLEQT t2 t1)
  , (words "000 011 111", topeLEQT t1 t2 `topeAndT` topeEQT t2 t3)
  , (words "000 101 111", topeLEQT t2 t1 `topeAndT` topeEQT t1 t3)
  , (words "000 110 111", topeLEQT t3 t1 `topeAndT` topeEQT t1 t2)
  -- tetrahedra
  , (words "000 001 011 111", topeLEQT t1 t2 `topeAndT` topeLEQT t2 t3)
  , (words "000 010 011 111", topeLEQT t1 t3 `topeAndT` topeLEQT t3 t2)
  , (words "000 001 101 111", topeLEQT t2 t1 `topeAndT` topeLEQT t1 t3)
  , (words "000 100 101 111", topeLEQT t2 t3 `topeAndT` topeLEQT t3 t1)
  , (words "000 010 110 111", topeLEQT t3 t1 `topeAndT` topeLEQT t1 t2)
  , (words "000 100 110 111", topeLEQT t3 t2 `topeAndT` topeLEQT t2 t1)
  ]
  where
    t1 = firstT  cube2T (firstT (cube2powerT 2) t)
    t2 = secondT cube2T (firstT (cube2powerT 2) t)
    t3 = secondT cube2T t
subTopes2 dim _ = error (show dim <> " dimensions are not supported")

cubeSubTopes :: [(ShapeId, TermT (Inc var))]
cubeSubTopes = subTopes2 3 (Pure Z)

limitLength :: Int -> String -> String
limitLength n s
  | length s > n = take (n - 1) s <> "…"
  | otherwise    = s

renderObjectsFor
  :: Eq var
  => String
  -> Int
  -> TermT var
  -> TermT var
  -> TypeCheck var [(ShapeId, RenderObjectData)]
renderObjectsFor mainColor dim t term = fmap catMaybes $ do
  forM (subTopes2 dim t) $ \(shapeId, tope) -> do
    checkTopeEntails tope >>= \case
      False -> return Nothing
      True -> typeOf term >>= \case
        UniverseTopeT{} -> localTope term $ checkTopeEntails tope >>= \case
          False -> return Nothing
          True -> return $ Just (shapeId, RenderObjectData
            { renderObjectDataLabel = ""
            , renderObjectDataFullLabel = ""
            , renderObjectDataColor = "orange"  -- FIXME: orange for topes?
            })
        _ -> do
          origs <- asks varOrigs
          term' <- localTope tope $ whnfT term
          label <-
            case term' of
              AppT _ (Pure z) arg
                | Just (Just "_") <- lookup z origs -> return ""
                | null (nub (freeVars (untyped arg)) \\ nub (freeVars (untyped t))) ->
                    ppTermInContext (Pure z)
              _ -> ppTermInContext term'
          return $ Just (shapeId, RenderObjectData
            { renderObjectDataLabel = label
            , renderObjectDataFullLabel = label
            , renderObjectDataColor =
                case term' of
                  Pure{} -> "purple"
                  AppT _ (Pure x) arg
                    | Just (Just "_") <- lookup x origs -> mainColor
                    | null (nub (freeVars (untyped arg)) \\ nub (freeVars (untyped t)))  -> "purple"
                  _ -> mainColor
            })

componentWiseEQT :: Int -> TermT var -> TermT var -> TermT var
componentWiseEQT 1 t s = topeEQT t s
componentWiseEQT 2 t s = topeAndT
  (componentWiseEQT 1 (firstT  cube2T t) (firstT  cube2T s))
  (componentWiseEQT 1 (secondT cube2T t) (secondT cube2T s))
componentWiseEQT 3 t s = topeAndT
  (componentWiseEQT 2 (firstT  (cube2powerT 2) t) (firstT (cube2powerT 2) s))
  (componentWiseEQT 1 (secondT cube2T t) (secondT cube2T s))
componentWiseEQT dim _ _ = error ("cannot work with " <> show dim <> " dimensions")

renderObjectsInSubShapeFor
  :: Eq var
  => String
  -> Int
  -> [var]
  -> var
  -> TermT var
  -> TermT var
  -> TermT var
  -> TypeCheck var [(ShapeId, RenderObjectData)]
renderObjectsInSubShapeFor mainColor dim sub super retType f x = fmap catMaybes $ do
  let reduceContext
        = foldr topeOrT topeBottomT
        . map (foldr topeAndT topeTopT)
        . map (filter (\tope -> all (`notElem` tope) sub))
        . map (saturateTopes [])
        . simplifyLHSwithDisjunctions
  contextTopes  <- asks (reduceContext . localTopesNF)
  contextTopes' <- localTope (componentWiseEQT dim (Pure super) x) $ asks (reduceContext . localTopesNF)
  forM (subTopes2 dim (Pure super)) $ \(shapeId, tope) -> do
    checkEntails tope contextTopes >>= \case
      False -> return Nothing
      True -> do
        origs <- asks varOrigs
        term <- localTope tope (whnfT (appT retType f (Pure super)))
        label <- typeOf term >>= \case
          UniverseTopeT{} -> return ""
          _ -> do
            case term of
              AppT _ (Pure z) arg
                | Just (Just "_") <- lookup z origs -> return ""
                | null (nub (freeVars (untyped arg)) \\ [super]) -> ppTermInContext (Pure z)
              _ -> ppTermInContext term
        color <- checkEntails tope contextTopes' >>= \case
          True -> do
            case term of
              Pure{} -> return "purple"
              AppT _ (Pure z) arg
                | Just (Just "_") <- lookup z origs -> return mainColor
                | null (nub (freeVars (untyped arg)) \\ [super]) -> return "purple"
              _ -> return mainColor
          False -> return "gray"
        return $ Just (shapeId, RenderObjectData
          { renderObjectDataLabel = label
          , renderObjectDataFullLabel = label
          , renderObjectDataColor = color
          })

renderForSubShapeSVG
  :: Eq var
  => String
  -> Int
  -> [var]
  -> var
  -> TermT var
  -> TermT var
  -> TermT var
  -> TypeCheck var String
renderForSubShapeSVG mainColor dim sub super retType f x = do
  objects <- renderObjectsInSubShapeFor mainColor dim sub super retType f x
  let objects' = map mk objects
  return $ renderCube defaultCamera (if dim > 2 then (pi/7) else 0) $ \obj ->
    lookup obj objects'
  where
    mk (shapeId, renderData) = (intercalate "-" (map fill shapeId), renderData)
    fill xs = xs <> replicate (3 - length xs) '1'

renderForSVG :: Eq var => String -> Int -> TermT var -> TermT var -> TypeCheck var String
renderForSVG mainColor dim t term = do
  objects <- renderObjectsFor mainColor dim t term
  let objects' = map mk objects
  return $ renderCube defaultCamera (if dim > 2 then (pi/7) else 0) $ \obj ->
    lookup obj objects'
  where
    mk (shapeId, renderData) = (intercalate "-" (map fill shapeId), renderData)
    fill xs = xs <> replicate (3 - length xs) '1'

renderTermSVGFor
  :: Eq var
  => String -- ^ Main color.
  -> Int    -- ^ Accumulated dimensions so far (from 0 to 3).
  -> (Maybe (TermT var, TermT var), [var])  -- ^ Accumulated point term (and its time).
  -> TermT var  -- ^ Term to render.
  -> TypeCheck var (Maybe String)
renderTermSVGFor mainColor accDim (mp, xs) t = do
  t' <- whnfT t
  ty <- typeOf t'
  case t of -- check unevaluated term
    AppT _info f x -> typeOf f >>= \case
      TypeFunT _ fOrig fArg mtopeArg ret | Just dim <- dimOf fArg, dim <= maxDim -> do
        enterScope fOrig fArg $ do
          maybe id localTope mtopeArg $ do
            Just <$> renderForSubShapeSVG mainColor dim (map S xs) Z ret (S <$> f) (S <$> x)  -- FIXME: breaks for 2 * (2 * 2), but works for 2 * 2 * 2 = (2 * 2) * 2
      _ -> traverse (\(p', _) -> renderForSVG mainColor accDim p' t') mp
    TypeFunT{} | null xs -> enterScope (Just "_") t' $ do
      renderTermSVGFor "blue" 0 (Nothing, []) (Pure Z)  -- use blue for types

    _ -> case t' of -- check evaluated term
      AppT _info f x -> typeOf f >>= \case
        TypeFunT _ fOrig fArg mtopeArg ret | Just dim <- dimOf fArg, dim <= maxDim -> do
          enterScope fOrig fArg $ do
            maybe id localTope mtopeArg $ do
              Just <$> renderForSubShapeSVG mainColor dim (map S xs) Z ret (S <$> f) (S <$> x)  -- FIXME: breaks for 2 * (2 * 2), but works for 2 * 2 * 2 = (2 * 2) * 2
        _ -> traverse (\(p', _) -> renderForSVG mainColor accDim p' t') mp
      TypeFunT{} | null xs -> enterScope (Just "_") t' $ do
        renderTermSVGFor "blue" 0 (Nothing, []) (Pure Z)  -- use blue for types

      _ -> case ty of -- check type of the term
        TypeFunT _ orig arg mtope ret
          | Just dim <- dimOf arg, accDim + dim <= maxDim -> enterScope orig arg $ do
              maybe id localTope mtope $
                renderTermSVGFor mainColor (accDim + dim)
                  (join' (both (fmap S) <$> mp) (S <$> arg) (Pure Z), Z : map S xs) $
                    case t' of
                      LambdaT _ _orig _marg body -> body
                      _                          -> appT ret (S <$> t') (Pure Z)
          | null xs -> enterScope orig arg $ do
              maybe id localTope mtope $
                renderTermSVGFor mainColor accDim
                  (both (fmap S) <$> mp, map S xs) $
                    case t' of
                      LambdaT _ _orig _marg body -> body
                      _                          -> appT ret (S <$> t') (Pure Z)
        _ -> traverse (\(p', _) -> renderForSVG mainColor accDim p' t') mp
  where
    maxDim = 3

    both f (x, y) = (f x, f y)

    join' Nothing Cube2T{} x = Just (x, cube2T)
    join' (Just (p, pt)) Cube2T{} x = Just (p', pt')
      where
        pt' = cubeProductT pt cube2T
        p' = pairT pt' p x
    join' p (CubeProductT _ l r) x =
      join' (join' p l (firstT l x)) r (secondT r x)
    join' _ _ _ = Nothing -- FIXME: error?

    dimOf = \case
      Cube2T{}           -> Just 1
      CubeProductT _ l r -> (+) <$> dimOf l <*> dimOf r
      _                  -> Nothing

renderTermSVG :: Eq var => TermT var -> TypeCheck var (Maybe String)
renderTermSVG = renderTermSVGFor "red" 0 (Nothing, [])  -- use red for terms by default

renderTermSVG' :: Eq var => TermT var -> TypeCheck var (Maybe String)
renderTermSVG' t = whnfT t >>= \t' -> typeOf t >>= \case
  TypeFunT _ orig arg mtope ret -> enterScope orig arg $ do
    maybe id localTope mtope $ case t' of
      LambdaT _ _orig _marg (AppT _info f x) ->
        typeOf f >>= \case
          TypeFunT _ fOrig fArg mtope2 _ret | Just dim <- dimOf fArg -> do
            enterScope fOrig fArg $ do
              maybe id localTope mtope2 $ do
                Just <$> renderForSubShapeSVG "red" dim [S Z] Z (S <$> ret) (S <$> f) (S <$> x)
          _ -> defaultRenderTermSVG t' arg ret
      _ -> defaultRenderTermSVG t' arg ret
  _t' -> return Nothing
  where
    dimOf = \case
      Cube2T{}           -> Just 1
      CubeProductT _ l r -> (+) <$> dimOf l <*> dimOf r -- WARNING: breaks for 2 * (2 * 2)
      _                  -> Nothing

    defaultRenderTermSVG t' arg ret =
      case dimOf arg of
        Just dim | dim <= 3 ->
          Just <$> renderForSVG "red" dim (Pure Z) (appT ret (S <$> t') (Pure Z))
        _ -> renderTermSVG' (appT ret (S <$> t') (Pure Z))


type Point2D a = (a, a)
type Point3D a = (a, a, a)
type Edge3D a = (Point3D a, Point3D a)
type Face3D a = (Point3D a, Point3D a, Point3D a)
type Volume3D a = (Point3D a, Point3D a, Point3D a, Point3D a)

data CubeCoords2D a b = CubeCoords2D
  { vertices :: [(Point3D a, Point2D b)]
  , edges    :: [(Edge3D a, (Point2D b, Point2D b))]
  , faces    :: [(Face3D a, (Point2D b, Point2D b, Point2D b))]
  , volumes  :: [(Volume3D a, (Point2D b, Point2D b, Point2D b, Point2D b))]
  }

data Matrix3D a = Matrix3D
  a a a
  a a a
  a a a

data Matrix4D a = Matrix4D
  a a a a
  a a a a
  a a a a
  a a a a

data Vector3D a = Vector3D a a a

data Vector4D a = Vector4D a a a a

rotateX :: Floating a => a -> Matrix3D a
rotateX theta = Matrix3D
  1 0 0
  0 (cos theta) (- sin theta)
  0 (sin theta) (cos theta)

rotateY :: Floating a => a -> Matrix3D a
rotateY theta = Matrix3D
  (cos theta) 0 (sin theta)
  0 1 0
  (- sin theta) 0 (cos theta)

rotateZ :: Floating a => a -> Matrix3D a
rotateZ theta = Matrix3D
  (cos theta) (- sin theta) 0
  (sin theta) (cos theta) 0
  0 0 1

data Camera a = Camera
  { cameraPos         :: Point3D a
  , cameraFoV         :: a
  , cameraAspectRatio :: a
  , cameraAngleY      :: a
  , cameraAngleX      :: a
  }

viewRotateX :: Floating a => Camera a -> Matrix4D a
viewRotateX Camera{..} = matrix3Dto4D (rotateX cameraAngleX)

viewRotateY :: Floating a => Camera a -> Matrix4D a
viewRotateY Camera{..} = matrix3Dto4D (rotateY cameraAngleY)

viewTranslate :: Num a => Camera a -> Matrix4D a
viewTranslate Camera{..} = Matrix4D
  1 0 0 0
  0 1 0 0
  0 0 1 0
  (-x) (-y) (-z) 1
  where
    (x, y, z) = cameraPos

project2D :: Floating a => Camera a -> Matrix4D a
project2D Camera{..} = Matrix4D
  (2 * n / (r - l)) 0 ((r + l) / (r - l)) 0
  0 (2 * n / (t - b)) ((t + b) / (t - b)) 0
  0 0 (- (f + n) / (f - n)) (- 2 * f * n / (f - n))
  0 0 (-1) 0
  where
    n = 1
    f = 2
    r = n * tan (cameraFoV / 2)
    l = -r
    t = r * cameraAspectRatio
    b = -t


matrixVectorMult4D :: Num a => Matrix4D a -> Vector4D a -> Vector4D a
matrixVectorMult4D
  (Matrix4D
    a1 a2 a3 a4
    b1 b2 b3 b4
    c1 c2 c3 c4
    d1 d2 d3 d4)
  (Vector4D a b c d)
    = Vector4D a' b' c' d'
  where
    a' = sum (zipWith (*) [a1, b1, c1, d1] [a, b, c, d])
    b' = sum (zipWith (*) [a2, b2, c2, d2] [a, b, c, d])
    c' = sum (zipWith (*) [a3, b3, c3, d3] [a, b, c, d])
    d' = sum (zipWith (*) [a4, b4, c4, d4] [a, b, c, d])

matrix3Dto4D :: Num a => Matrix3D a -> Matrix4D a
matrix3Dto4D
  (Matrix3D
    a1 b1 c1
    a2 b2 c2
    a3 b3 c3) = Matrix4D
      a1 b1 c1 0
      a2 b2 c2 0
      a3 b3 c3 0
      0 0 0 1

fromAffine :: Fractional a => Vector4D a -> (Point2D a, a)
fromAffine (Vector4D a b c d) = ((x, y), zIndex)
  where
    x = a / d
    y = b / d
    zIndex = c / d

point3Dto2D :: Floating a => Camera a -> a -> Point3D a -> (Point2D a, a)
point3Dto2D camera rotY (x, y, z) = fromAffine $
  foldr matrixVectorMult4D (Vector4D x y z 1) $ reverse
    [ matrix3Dto4D (rotateY rotY)
    , viewTranslate camera
    , viewRotateY camera
    , viewRotateX camera
    , project2D camera
    ]

data RenderObjectData = RenderObjectData
  { renderObjectDataLabel     :: String
  , renderObjectDataFullLabel :: String
  , renderObjectDataColor     :: String
  }

renderCube
  :: (Floating a, Show a)
  => Camera a
  -> a
  -> (String -> Maybe RenderObjectData)
  -> String
renderCube camera rotY renderDataOf' = unlines $ filter (not . null)
  [ "<svg class=\"rzk-render\" viewBox=\"-175 -200 350 375\" width=\"150\" height=\"150\">"
  , intercalate "\n"
      [ "  <path d=\"M " <> show x1 <> " " <> show y1
                <> " L " <> show x2 <> " " <> show y2
                <> " L " <> show x3 <> " " <> show y3
                <> " Z\" style=\"fill: " <> renderObjectDataColor <> "; opacity: 0.2\"><title>" <> renderObjectDataFullLabel <> "</title></path>" <> "\n" <>
        "  <text x=\"" <> show x <> "\" y=\"" <> show y <> "\" fill=\"" <> renderObjectDataColor <> "\">" <> renderObjectDataLabel <> "</text>"
      | (faceId, (((x1, y1), (x2, y2), (x3, y3)), _)) <- faces
      , Just RenderObjectData{..} <- [renderDataOf faceId]
      , let x = (x1 + x2 + x3) / 3
      , let y = (y1 + y2 + y3) / 3 ]
  , intercalate "\n"
      [ "  <polyline points=\"" <> show x1 <> "," <> show y1 <> " " <> show x2 <> "," <> show y2
        <> "\" stroke=\"" <> renderObjectDataColor <> "\" stroke-width=\"3\" marker-end=\"url(#arrow)\"><title>" <> renderObjectDataFullLabel <> "</title></polyline>" <> "\n" <>
        "  <text x=\"" <> show x <> "\" y=\"" <> show y <> "\" fill=\"" <> renderObjectDataColor <> "\" stroke=\"white\" stroke-width=\"10\" stroke-opacity=\".8\" paint-order=\"stroke\">" <> renderObjectDataLabel <> "</text>"
      | (edge, (((x1, y1), (x2, y2)), _)) <- edges
      , Just RenderObjectData{..} <- [renderDataOf edge]
      , let x = (x1 + x2) / 2
      , let y = (y1 + y2) / 2 ]
  , intercalate "\n"
      [ "  <text x=\"" <> show x <> "\" y=\"" <> show y <> "\" fill=\"" <> renderObjectDataColor <> "\">" <> renderObjectDataLabel <> "</text>"
      | (v, ((x, y), _)) <- vertices
      , Just RenderObjectData{..} <- [renderDataOf v]]
  , "</svg>" ]
  where
    renderDataOf shapeId =
      case renderDataOf' shapeId of
        Nothing -> Nothing
        Just RenderObjectData{..} -> Just RenderObjectData
          -- FIXME: move constants to configurable parameters
          { renderObjectDataLabel = hideWhenLargerThan shapeId 5 renderObjectDataLabel
          , renderObjectDataFullLabel = limitLength 30 renderObjectDataFullLabel
          , .. }

    hideWhenLargerThan shapeId n s
      | null s || length s > n = if '-' `elem` shapeId then "" else "•"
      | otherwise = s

    vertices =
      [ (show x <> show y <> show z, ((500 * x'', 500 * y''), zIndex))
      | x <- [0,1]
      , y <- [0,1]
      , z <- [0,1]
      , let f c = 2 * fromInteger c - 1
      , let x' = f x
      , let y' = f (1-y)
      , let z' = f z
      , let ((x'', y''), zIndex) = point3Dto2D camera rotY (x', y', z') ]

    radius = 20

    mkEdge r (x1, y1) (x2, y2) = ((x1 + dx, y1 + dy), ((x2 - dx), (y2 - dy)))
      where
        d = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
        dx = r * (x2 - x1) / d
        dy = r * (y2 - y1) / d

    scaleAround (cx, cy) s (x, y) = (cx + s * (x - cx), cy + s * (y - cy))

    mkFace (x1, y1) (x2, y2) (x3, y3) = (p1, p2, p3)
      where
        cx = (x1 + x2 + x3) / 3
        cy = (y1 + y2 + y3) / 3
        p1 = scaleAround (cx, cy) 0.85 (x1, y1)
        p2 = scaleAround (cx, cy) 0.85 (x2, y2)
        p3 = scaleAround (cx, cy) 0.85 (x3, y3)

    edges =
      [ (intercalate "-" [fromName, toName], (mkEdge radius from to, 0))
      | (fromName, (from, _)) : vs <- tails vertices
      , (toName, (to, _)) <- vs
      , and (zipWith (<=) fromName toName)
      ]

    faces =
      [ (intercalate "-" [name1, name2, name3], (mkFace v1 v2 v3, 0))
      | (name1, (v1, _)) : vs <- tails vertices
      , (name2, (v2, _)) : vs' <- tails vs
      , and (zipWith (<=) name1 name2)
      , (name3, (v3, _)) <- vs'
      , and (zipWith (<=) name2 name3)
      ]


defaultCamera :: Floating a => Camera a
defaultCamera = Camera
  { cameraPos = (0, 7, 10)
  , cameraAngleY = pi
  , cameraAngleX = pi/5
  , cameraFoV = pi/15
  , cameraAspectRatio = 1
  }
