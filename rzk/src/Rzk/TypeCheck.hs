{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-orphans #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Rzk.TypeCheck where

import           Control.Applicative      ((<|>))
import           Control.Monad            (forM, forM_, join, unless, when)
import           Control.Monad.Except
import           Control.Monad.Reader
-- An explicit list: a bare 'Control.Monad.Writer' import also re-exports
-- 'Data.Monoid' (incl. 'First'/'Last') on some mtl versions, which clashes with
-- the 'First'/'Last' term patterns from 'Language.Rzk.Free.Syntax'.
import           Control.Monad.Writer     (WriterT, censor, runWriterT, tell)
import           Data.Bifoldable          (bifoldr)
import           Data.Bifunctor           (first)
import           Data.List                (intercalate, intersect, nub, partition,
                                           tails, (\\))
import           Data.Maybe               (catMaybes, fromMaybe, isNothing,
                                           mapMaybe)
import           Data.String              (IsString (..))
import           Data.Tuple               (swap)

import           Free.Scoped
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
defaultTypeCheck = fmap fst . defaultTypeCheckWithHoles' emptyContext

-- | Like 'defaultTypeCheck', but runs in lenient hole mode ('allowHoles') and
-- also returns the holes recorded during checking (with their goal and context).
defaultTypeCheckWithHoles
  :: TypeCheck var a
  -> Either (TypeErrorInScopedContext var) (a, [HoleInfo])
defaultTypeCheckWithHoles = defaultTypeCheckWithHoles' (allowHoles emptyContext)

defaultTypeCheckWithHoles'
  :: Context var
  -> TypeCheck var a
  -> Either (TypeErrorInScopedContext var) (a, [HoleInfo])
defaultTypeCheckWithHoles' ctx tc =
  runExcept (runWriterT (runReaderT tc ctx))

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

-- | Typecheck modules in lenient hole mode, returning the elaborated
-- declarations, any type errors, and the holes recorded (each with its goal and
-- local context). This is the structured goal/context query consumed by the
-- LSP and the game. Strict callers (the default CLI path, CI) keep using
-- 'typecheckModulesWithLocation', where holes are 'TypeErrorUnsolvedHole'.
typecheckModulesWithHoles
  :: [(FilePath, Rzk.Module)]
  -> Either (TypeErrorInScopedContext VarIdent)
            ([(FilePath, [Decl'])], [TypeErrorInScopedContext VarIdent], [HoleInfo])
typecheckModulesWithHoles modules =
  flatten <$> defaultTypeCheckWithHoles (typecheckModulesWithLocation' modules)
  where
    flatten ((decls, errs), holes) = (decls, errs, holes)

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
          ty' <- typecheck (toTerm' (addParamDecls paramDecls ty)) universeT >>= whnfT
          term' <- typecheck (toTerm' (addParams params term)) ty' >>= whnfT
          loc <- asks location
          let decl = Decl (varIdentAt path name) ty' (Just term') False (varIdentAt path <$> vars) loc
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
          ty' <- typecheck (toTerm' (addParamDecls paramDecls ty)) universeT >>= whnfT
          loc <- asks location
          let decl = Decl (varIdentAt path name) ty' Nothing False (varIdentAt path <$> vars) loc
          fmap (first (decl :)) $
            localDeclPrepared decl $
              go (i + 1) moreCommands

    go  i (command@(Rzk.CommandCheck _loc term ty) : moreCommands) =
      traceTypeCheck Normal ("[ " <> show i <> " out of " <> show totalCommands <> " ]"
          <> " Checking " <> Rzk.printTree term <> " : " <> Rzk.printTree ty ) $ do
        withCommand command $ do
          ty' <- typecheck (toTerm' ty) universeT >>= whnfT
          _term' <- typecheck (toTerm' term) ty'
          go (i + 1) moreCommands

    go  i (Rzk.CommandCompute loc term : moreCommands) =
      go i (Rzk.CommandComputeWHNF loc term : moreCommands)

    go  i (command@(Rzk.CommandComputeNF _loc term) : moreCommands) =
      traceTypeCheck Normal ("[ " <> show i <> " out of " <> show totalCommands <> " ]"
          <> " Computing NF for " <> Rzk.printTree term) $ do
        withCommand command $ do
          term' <- infer (toTerm' term) >>= nfT
          traceTypeCheck Normal ("  " <> show (untyped term')) $ do
            go (i + 1) moreCommands

    go  i (command@(Rzk.CommandComputeWHNF _loc term) : moreCommands) =
      traceTypeCheck Normal ("[ " <> show i <> " out of " <> show totalCommands <> " ]"
          <> " Computing WHNF for " <> Rzk.printTree term) $ do
        withCommand command $ do
          term' <- infer (toTerm' term) >>= whnfT
          traceTypeCheck Normal ("  " <> show (untyped term')) $ do
            go (i + 1) moreCommands

    go  i (command@(Rzk.CommandAssume _loc names ty) : moreCommands) =
      traceTypeCheck Normal ("[ " <> show i <> " out of " <> show totalCommands <> " ]"
          <> " Checking #assume " <> intercalate " " [ Rzk.printTree name | name <- names ] ) $ do
        withCommand command $ do
          ty' <- typecheck (toTerm' ty) universeT
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
setOption optionName = const $ const $
  issueTypeError $ TypeErrorOther ("unknown option " <> show optionName)

unsetOption :: String -> TypeCheck var a -> TypeCheck var a
unsetOption "verbosity" = localVerbosity (verbosity emptyContext)
unsetOption "render" = localRenderBackend (renderBackend emptyContext)
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
paramToParamDecl (Rzk.ParamPatternModalType loc pats mc ty) = pure
  [ Rzk.ParamTermModalType loc (patternToTerm pat) mc ty | pat <- pats ]
paramToParamDecl (Rzk.ParamPatternModalShape loc pats mc cube tope) = pure
  [ Rzk.ParamTermModalShape loc (patternToTerm pat) mc cube tope | pat <- pats ]

addParamDecls :: [Rzk.ParamDecl] -> Rzk.Term -> Rzk.Term
addParamDecls [] = id
addParamDecls (paramDecl : paramDecls)
  = Rzk.TypeFun Nothing paramDecl . addParamDecls paramDecls

addParams :: [Rzk.Param] -> Rzk.Term -> Rzk.Term
addParams []     = id
addParams params = Rzk.Lambda Nothing params

data TypeError var
  = TypeErrorOther String
  | TypeErrorUnify (TermT var) (TermT var) (TermT var)
  | TypeErrorUnifyTerms (TermT var) (TermT var)
  | TypeErrorNotPair (TermT var) (TermT var)
  | TypeErrorNotModal (Term var) TModality (TermT var)
  | TypeErrorModalityMismatch TModality TModality (Term var)
  | TypeErrorUnaccessibleVar var TModality TModality
  | TypeErrorNotTypeInModal (TermT var)
  | TypeErrorNotFunction (TermT var) (TermT var)
  | TypeErrorUnexpectedLambda (Term var) (TermT var)
  | TypeErrorUnexpectedPair (Term var) (TermT var)
  | TypeErrorUnexpectedRefl (Term var) (TermT var)
  | TypeErrorCannotInferBareLambda (Term var)
  | TypeErrorCannotInferBareRefl (Term var)
  | TypeErrorCannotInferHole (Term var)
  | TypeErrorUnsolvedHole (Maybe VarIdent) (TermT var)
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

ppModality :: TModality -> String
ppModality = \case
  Flat  -> "♭"
  Sharp -> "♯"
  Op    -> "ᵒᵖ"
  Id    -> "_id"

-- | Render a type error, folding pattern-binder projections (e.g. @π₁ x@ back to
-- the user's @t@) using the supplied map (see 'contextBinders').
ppTypeError' :: [(VarIdent, [([Proj], VarIdent)])] -> TypeError' -> String
ppTypeError' pm = \case
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
        _ -> ""
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
    [ "undefined variable: " <> show (Pure var :: Term') ]
  TypeErrorTopeNotSatisfied topes tope -> block TopDown
    [ "local context is not included in (does not entail) the tope"
    , "  " <> ppU (untyped tope)
    , "in local context (normalised)"
    , intercalate "\n" (map ("  " <>) (map ppTyped topes))] -- FIXME: remove
  TypeErrorTopeContextDisjoint tope topes -> block TopDown
    [ "the tope"
    , "  " <> ppU (untyped tope)
    , "is disjoint from the local tope context (their conjunction is the empty tope ⊥),"
    , "so this restriction face or recOR branch is vacuous everywhere"
    , "in local context (normalised)"
    , intercalate "\n" (map ("  " <>) (map ppTyped topes))]
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
    , "  " <> Rzk.printTree (getVarIdent name) <> " : " <> ppU (untyped type_)
    ]

  TypeErrorUnusedUsedVariables vars name -> block TopDown
    [ "unused variables"
    , "  " <> intercalate " " (map (Rzk.printTree . getVarIdent) vars)
    , "declared as used in definition of"
    , "  " <> Rzk.printTree (getVarIdent name)
    ]

  TypeErrorImplicitAssumption (a, aType) name -> block TopDown
    [ "implicit assumption"
    , "  " <> Rzk.printTree (getVarIdent a) <> " : " <> ppU (untyped aType)
    , "used in definition of"
    , "  " <> Rzk.printTree (getVarIdent name)
    ]
  where
    -- render an (untyped) term, folding pattern-binder projections
    ppU :: Term' -> String
    ppU = show . foldBinderProjections pm
    -- render a type-annotated term, folding pattern-binder projections
    ppTyped :: TermT' -> String
    ppTyped = show . foldBinderProjectionsT pm


-- | The pattern binders in scope, freshened and keyed by their (current) name,
-- together with the projection-folding map derived from them. Both share the
-- same freshened component names, so a term's projections and the binder shown
-- in the context agree.
contextBinders
  :: Context VarIdent
  -> ([(VarIdent, Binder)], [(VarIdent, [([Proj], VarIdent)])])
contextBinders ctx = (fbs, binderProjMap id fbs)
  where
    mapping = [ (v, v) | (v, _) <- varTypes ctx ]
    fbs     = freshBinders id mapping (varBinders ctx)

ppTypeErrorInContext :: OutputDirection -> TypeErrorInContext VarIdent -> String
ppTypeErrorInContext dir TypeErrorInContext{..} = block dir
  [ ppTypeError' pm typeErrorError
  , ""
  , ppContext' dir typeErrorContext
  ]
  where
    (_, pm) = contextBinders typeErrorContext

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
  | ActionContextEntailsUnion [TermT var] [TermT var]
  | ActionWHNF (TermT var)
  | ActionNF (TermT var)
  | ActionCheckCoherence (TermT var, TermT var) (TermT var, TermT var)
  | ActionCloseSection (Maybe Rzk.SectionName)
  | ActionCheckLetValue (Maybe VarIdent)
  deriving (Functor, Foldable)

type Action' = Action VarIdent

-- | Freshen the compound (pattern) binders in scope so their component names
-- avoid the display names already in use and one another. Returns each relevant
-- variable paired with its freshened binder. Variables bound to a single name
-- are omitted: they need no projection folding and are displayed normally.
freshBinders
  :: Eq var
  => (var -> VarIdent)
  -> [(var, VarIdent)]
  -> [(var, Binder)]
  -> [(var, Binder)]
freshBinders name mapping binders = go (map (name . fst) mapping) compound
  where
    compound = [ (v, b) | (v, b) <- binders, binderIsCompound b, v `elem` map fst mapping ]
    go _    []             = []
    go used ((v, b) : rest) =
      let b' = freshenBinderLeaves used b
      in (v, b') : go (binderLeaves b' ++ used) rest

-- | The projection-folding map for rendering: each pattern-binder variable's
-- display name mapped to the projection paths of its component names (e.g.
-- @π₁@ ↦ @t@, @π₂@ ↦ @s@). Ordinary projections (of non-pattern variables) are
-- left untouched.
binderProjMap :: (var -> VarIdent) -> [(var, Binder)] -> [(VarIdent, [([Proj], VarIdent)])]
binderProjMap name fbs = [ (name v, binderPaths b) | (v, b) <- fbs ]

ppTermInContext :: Eq var => TermT var -> TypeCheck var String
ppTermInContext term =  do
  vars <- freeVarsT_ term
  let mapping = zip vars defaultVarIdents
      toRzkVarIdent origs var = fromMaybe "_" $
        join (lookup var origs) <|> lookup var mapping
  origs <- asks varOrigs
  binders <- asks varBinders
  let name = toRzkVarIdent origs
      fbs  = freshBinders name mapping binders
  return (show (foldBinderProjections (binderProjMap name fbs)
                  (untyped (name <$> term))))

-- | Classify a (WHNF) type as a cube, so cube variables (e.g. @t : 2@) are
-- shown separately from ordinary term variables in a hole's context.
isCubeType :: TermT var -> Bool
isCubeType = \case
  CubeUnitT{}     -> True
  Cube2T{}        -> True
  CubeIT{}        -> True
  CubeProductT{}  -> True
  UniverseCubeT{} -> True
  _               -> False

-- | Is this term a hole? Holes only exist in lenient mode (see 'allowHoles');
-- they are opaque placeholders standing for a term of the expected type.
isHoleT :: TermT var -> Bool
isHoleT HoleT{} = True
isHoleT _       = False

-- | Does this term contain a hole anywhere (including nested, e.g. @f ?@)?
-- Used to keep unification lenient around incomplete terms: a term with an
-- unfilled hole cannot be meaningfully compared, so a unification that would
-- otherwise fail is deferred. Evaluated only on the failure path.
containsHole :: TermT var -> Bool
containsHole = \case
  HoleT{}             -> True
  Pure{}              -> False
  Free (AnnF _ termf) -> bifoldr ((||) . containsHole) ((||) . containsHole) False termf

-- | All ways to eliminate a hypothesis into a value usable at a goal. Given a
-- @target@ type and a hypothesis /term/ (e.g. @Pure v@ for a context variable),
-- return every elimination spine over that term whose type fits the target (or
-- a subtype of it). Arguments introduced by application are left as holes for
-- the caller to fill later. A value that already fits is returned as-is; a
-- function is applied to holes; a Σ-type (or anything that unfolds to one, e.g.
-- @is-contr@) is projected, possibly repeatedly — so e.g.
-- @first (first (is-segal-A ? ? ? ? ?))@ is discovered.
--
-- The search is driven uniformly by the eliminators a (weak head normal) type
-- admits (see 'eliminatorsOf'), so adding a new eliminator extends it without
-- touching the search. Depth is bounded by 'maxEliminationDepth'.
allEliminationsInto
  :: Eq var => TermT var -> TermT var -> TypeCheck var [TermT var]
allEliminationsInto target = go maxEliminationDepth
  where
    go depth term = do
      ty   <- typeOf term
      fits <- fitsInto term ty target
      deeper <-
        if depth <= 0
          then pure []
          else do
            elims <- eliminatorsOf ty
            concat <$> mapM (\wrap -> go (depth - 1) (wrap term)) elims
      pure ([term | fits] <> deeper)

-- | How deep an elimination spine 'allEliminationsInto' will build. A temporary
-- fixed bound: seven is enough for the spines seen so far (fully applying a
-- five-argument hypothesis and projecting twice), and a larger bound mostly adds
-- self-referential spines (a built result applied again). It should be made
-- configurable, and likely raised, once there is more evidence of what real
-- goals need. The chain only branches at Σ-types, which are shallow, so the
-- search stays small.
maxEliminationDepth :: Int
maxEliminationDepth = 7

-- | Whether a term of the given (whnf) type may stand where a value of the
-- @target@ type is expected: the two types unify under 'structuralHoleUnify',
-- so a hole acts as a wildcard leaf but a structural mismatch around it is still
-- a mismatch (an under-applied function does not match an extension-type goal,
-- but a partial application that genuinely fits an ordinary-function goal does).
--
-- Outer type restrictions are stripped from both sides first: an extension-type
-- boundary is satisfied by later refinement, not by the choice of spine, and
-- matching against the restricted goal would reject the very spine that
-- introduces the holes meant to satisfy it (e.g. @f ?@ at a boundary goal).
--
-- Holes or constraints recorded while probing are discarded, so this is a pure
-- yes/no query.
fitsInto :: Eq var => TermT var -> TermT var -> TermT var -> TypeCheck var Bool
fitsInto term ty target = do
  ty'     <- stripTypeRestrictions <$> whnfT ty
  target' <- stripTypeRestrictions <$> whnfT target
  censor (const []) $ local structuralHoleUnify
    ((unify (Just term) target' ty' >> pure True) `catchError` \_ -> pure False)

-- | The eliminators a value of the given (weak head normal) type admits, each
-- as a function wrapping the eliminated term. A Π-type is eliminated by
-- application to a fresh hole; a Σ-type by either projection. Anything else
-- admits no simple eliminator.
eliminatorsOf :: TermT var -> TypeCheck var [TermT var -> TermT var]
eliminatorsOf ty =
  case stripTypeRestrictions ty of
    TypeFunT _ty _orig param _mtope ret ->
      pure [ \term -> let h = mkHole param in appT (substituteT h ret) term h ]
    TypeSigmaT _ty _orig a b ->
      pure [ \term -> firstT a term
           , \term -> secondT (substituteT (firstT a term) b) term ]
    -- A cube point pair (e.g. a pattern-bound @(t , s) : 2 × 2@) projects to its
    -- coordinates; rzk renders those projections back as the pattern names.
    CubeProductT _ty a b ->
      pure [ \term -> firstT a term
           , \term -> secondT b term ]
    _ -> pure []
  where
    mkHole t = HoleT TypeInfo{ infoType = t, infoWHNF = Nothing, infoNF = Nothing } Nothing

-- | The binder for a λ introduced over a domain type. A binder the type already
-- gives as a pattern is kept as-is — it carries the user's own names (e.g.
-- @(t , s)@). Otherwise an /explicit/ (pre-whnf) Σ-type or product domain is
-- destructured into a fresh pair pattern, recursively for products, so that a
-- nameless @2 × 2 × 2@ parameter is introduced as @((t1 , t2) , t3)@ rather than
-- a single opaque variable. Any other domain keeps its single binder.
--
-- Leaves are named by what they range over: a cube-product component is a point,
-- named @t@/@s@-style as @tN@; a Σ component is a term, named @xN@. The names are
-- display-only (the body is a hole that does not mention them) and carry a
-- shared running index, so every leaf in the pattern is distinct.
destructuringBinder :: Binder -> TermT var -> Binder
destructuringBinder orig param = case orig of
  BinderPair{} -> orig                 -- already a pattern: keep the user's names
  _ -> case param of
    CubeProductT{} -> fst (go 1 param)
    TypeSigmaT{}   -> fst (go 1 param)
    _              -> orig             -- not a product/Σ: leave the binder alone
  where
    -- a product/Σ becomes a pair; we recurse into a product's components (plain
    -- types) but not under a Σ's binder (a scope). A leaf is named by its
    -- enclosing constructor: @tN@ under a cube product, @xN@ under a Σ.
    go n = \case
      CubeProductT _ a b ->
        let (l, n')  = child "t" n  a
            (r, n'') = child "t" n' b
        in (BinderPair l r, n'')
      TypeSigmaT _ _ a _b ->
        let (l, n') = child "x" n a
        in (BinderPair l (BinderVar (Just (leaf "x" n'))), n' + 1)
      _ -> (BinderVar (Just (leaf "t" n)), n + 1)  -- unreached: go is called on products only
    child pfx n = \case
      c@CubeProductT{} -> go n c
      c@TypeSigmaT{}   -> go n c
      _                -> (BinderVar (Just (leaf pfx n)), n + 1)
    leaf pfx n = fromString (pfx <> show n :: String)

-- | All ways to introduce a value /of/ a goal type by its head constructor,
-- leaving the constituents as holes. Given a @target@ type, return its
-- introduction forms:
--
--   * a Π-type is introduced by a λ-abstraction over a hole body (@\\ x -> ?@);
--     the binder is taken from the type, so a pattern domain (e.g. a @Δ²@ point
--     @(t , s)@) is introduced as @\\ (t , s) -> ?@;
--   * a Σ-type or a cube product by a pair of holes (@(? , ?)@);
--   * an identity type by @refl@, but only when its two endpoints already agree
--     (otherwise @refl@ would not typecheck);
--   * the unit type by @unit@;
--   * the tope universe by each tope constructor — @TOP@, @BOT@, @? ≡ ?@,
--     @? ≤ ?@, @? ∧ ?@, @? ∨ ?@ — so a shape (a hole of type @TOPE@) can be
--     built up by tapping.
--
-- Any other type admits no simple introduction. Unlike 'allEliminationsInto'
-- this does not search: a type has at most one introduction form (the tope
-- universe is the one exception), read off its (weak head normal) head
-- constructor. Outer type restrictions are stripped first, so an extension type
-- is introduced by the form of its underlying type (its boundary is met by later
-- refinement of the holes, not by the choice of constructor).
allIntroductionsOf :: Eq var => TermT var -> TypeCheck var [TermT var]
allIntroductionsOf target = do
  target' <- stripTypeRestrictions <$> whnfT target
  case target' of
    TypeFunT _ty orig param _mtope ret ->
      pure [ lambdaT target' (destructuringBinder orig param) Nothing (mkHole ret) ]
    TypeSigmaT _ty _orig a b ->
      let h = mkHole a in pure [ pairT target' h (mkHole (substituteT h b)) ]
    CubeProductT _ty a b ->
      pure [ pairT target' (mkHole a) (mkHole b) ]
    TypeIdT _ty a _tA b -> do
      agree <- endpointsAgree a b
      pure [ reflT target' Nothing | agree ]
    TypeUnitT{} -> pure [ unitT ]
    -- the tope universe: every tope constructor builds a tope, so all are
    -- introductions of a shape goal. Point arguments (of ≡, ≤) and tope
    -- arguments (of ∧, ∨) are left as holes.
    UniverseTopeT{} ->
      let point = mkHole (mkHole cubeT)  -- a point of an as-yet-unknown cube
          tope  = mkHole target'         -- a tope (its type is the tope universe)
       in pure [ topeTopT, topeBottomT
               , topeEQT  point point, topeLEQT point point
               , topeAndT tope  tope,  topeOrT  tope  tope ]
    _ -> pure []
  where
    mkHole t = HoleT TypeInfo{ infoType = t, infoWHNF = Nothing, infoNF = Nothing } Nothing

-- | Whether the two endpoints of an identity type are definitionally equal, so
-- that @refl@ inhabits it. Like 'fitsInto', any holes or constraints recorded
-- while probing are discarded, leaving a pure yes\/no query.
endpointsAgree :: Eq var => TermT var -> TermT var -> TypeCheck var Bool
endpointsAgree a b =
  censor (const [])
    ((unify Nothing a b >> pure True) `catchError` \_ -> pure False)

-- | Whether the local tope context is contradictory (entails ⊥). Reads the
-- precomputed flag when present, falling back to an entailment check. Used to
-- decide whether @recBOT@ (ex falso) is available.
contextEntailsBottom :: Eq var => TypeCheck var Bool
contextEntailsBottom = asks localTopesEntailBottom >>= \case
  Just b  -> return b
  Nothing -> asks localTopesNF >>= (`entailM` topeBottomT)

-- | Ex falso: in a contradictory tope context @recBOT@ inhabits any type, so it
-- is a candidate for every goal there (and only there — elsewhere it would not
-- typecheck). Independent of the goal and of the local hypotheses.
recBottomCandidates :: Eq var => TypeCheck var [TermT var]
recBottomCandidates = do
  vacuous <- contextEntailsBottom
  pure [ recBottomT | vacuous ]

-- | Whether the local tope context is covered by the union of the given topes —
-- the coverage obligation of @recOR@ (see 'contextEntailsUnion'), but as a pure
-- yes\/no query rather than a check that issues an error.
coverageHolds :: Eq var => [TermT var] -> TypeCheck var Bool
coverageHolds topes = do
  contextTopes <- asks localTopesNF
  topesNF      <- mapM nfTope topes
  contextTopes `entailM` foldr topeOrT topeBottomT topesNF

-- | Tope case-split moves: ways to build a value of the goal by @recOR@,
-- splitting the proof over a cover of the local tope context. Three sources,
-- offered together (the UI ranks and filters):
--
--   * each disjunction @ψ ∨ φ@ already in the context becomes
--     @recOR(ψ ↦ ?, φ ↦ ?)@ — its cover is immediate;
--   * when the goal is an extension type, its restriction faces are a cover
--     candidate @recOR(ψ₁ ↦ ?, …)@, offered only when they actually cover the
--     context (so the move typechecks);
--   * a generic two-way split @recOR(? ↦ ?, ? ↦ ?)@ with the guards left as
--     holes, for an unusual split the player fills in by hand.
--
-- All three are offered only in a setting where a split makes sense — a cube
-- variable is in scope, the context has a non-trivial tope, or the goal is a
-- restricted type — so an ordinary (tope-free) goal is left alone.
recOrCandidates :: Eq var => TermT var -> TypeCheck var [TermT var]
recOrCandidates goal = do
  goalW   <- whnfT goal
  topes   <- asks (filter (/= topeTopT) . availableTopes)
  locals  <- asks (filter (not . varIsTopLevel . snd) . varInfos)
  hasCube <- or <$> mapM (fmap isCubeType . whnfT . varType . snd) locals
  let stripped     = stripTypeRestrictions goalW
      mkRecOr gs   = recOrT stripped [ (g, mkHole stripped) | g <- gs ]
      fromContext  = [ mkRecOr [l, r] | TopeOrT _ l r <- topes ]
      faces        = case goalW of
        TypeRestrictedT _ _ rs -> map fst rs
        _                      -> []
      isRestricted = case goalW of TypeRestrictedT{} -> True; _ -> False
      inShape      = hasCube || not (null topes) || isRestricted
      generic      = [ recOrT stripped [ (mkHole topeT, mkHole stripped)
                                       , (mkHole topeT, mkHole stripped) ]
                     | inShape ]
  fromFaces <- if length faces >= 2
    then do covered <- coverageHolds faces
            pure [ mkRecOr faces | covered ]
    else pure []
  pure (fromContext <> fromFaces <> generic)
  where
    mkHole t = HoleT TypeInfo{ infoType = t, infoWHNF = Nothing, infoNF = Nothing } Nothing

-- | Record the goal and local context at a hole (lenient mode only). The goal,
-- the local hypotheses, and the tope assumptions are all rendered to
-- user-facing 'VarIdent' names here — reusing the same resolution as
-- 'ppTermInContext' — so the resulting 'HoleInfo' is independent of the De
-- Bruijn scope. The global environment is deliberately excluded (only
-- @varIsTopLevel == False@ locals are kept), and locals are split into cube
-- variables and ordinary term variables.
recordHole :: Eq var => Maybe VarIdent -> TermT var -> TypeCheck var ()
recordHole mname goalTy = recordHoleShape mname goalTy Nothing

-- | Record a hole. When the hole is the argument of a shape-restricted function
-- its goal is a /shape/: the cube @goalTy@ together with a membership tope. The
-- tope is a scope over the shape's bound variable (the third argument carries
-- the declared binder name, if any, and the tope); it is rendered under that
-- binder so the goal reads @(binder : goalTy | tope)@.
recordHoleShape
  :: Eq var
  => Maybe VarIdent
  -> TermT var
  -> Maybe (Binder, TermT (Inc var))
  -> TypeCheck var ()
recordHoleShape mname goalTy mshape = do
  goal'     <- whnfT goalTy
  locals    <- asks (filter (not . varIsTopLevel . snd) . varInfos)
  cubeFlags <- mapM (fmap isCubeType . whnfT . varType . snd) locals
  topes     <- asks (filter (/= topeTopT) . availableTopes)
  origs     <- asks varOrigs
  binders   <- asks varBinders
  loc       <- asks location
  -- for each local hypothesis, the elimination spines that land in the goal
  -- (arguments left as holes). Probing must not leak holes into the recorded
  -- output, hence 'censor'.
  candidates <- censor (const []) $ do
    elims  <- concat <$> mapM (\(v, _) -> allEliminationsInto goalTy (Pure v)) locals
    -- context-driven moves (independent of the goal's head and the hypotheses):
    -- ex falso in a contradictory context, and tope case-splits.
    recbot <- recBottomCandidates
    recor  <- recOrCandidates goalTy
    pure (elims <> recbot <> recor)
  -- the introduction forms for the goal itself (constituents left as holes).
  introductions <- censor (const []) (allIntroductionsOf goalTy)
  let shapeTope     = snd <$> mshape
      shapeTopeVars = maybe [] (\t -> [ v | S v <- foldr (:) [] t ]) shapeTope
  varsList  <- concat <$> mapM freeVarsT_ (goal' : map (varType . snd) locals ++ topes)
  let mapping  = zip (nub (varsList ++ shapeTopeVars ++ map fst locals)) defaultVarIdents
      name v   = fromMaybe "_" (join (lookup v origs) <|> lookup v mapping)
      fbs      = freshBinders name mapping binders
      render t = foldBinderProjections (binderProjMap name fbs) (untyped (name <$> t))
      -- a pattern binder is shown as its pattern, e.g. (t , s); others by name
      entryName v = maybe (name v) binderDisplayName (lookup v fbs)
      entries  = [ HoleEntry (entryName v) (render (varType info)) | (v, info) <- locals ]
      flagged  = zip cubeFlags entries
      -- binder name for the shape: the declared name if any, else a default
      shapeBinder   = fromMaybe (fromString "t") (binderName =<< (fst <$> mshape))
      nameInc Z     = shapeBinder
      nameInc (S v) = name v
      goalShape = (\t -> (shapeBinder, untyped (nameInc <$> t))) <$> shapeTope
  lift $ tell
    [ HoleInfo
        { holeName      = mname
        , holeGoal      = render goal'
        , holeGoalShape = goalShape
        , holeTermVars  = [ e | (False, e) <- flagged ]
        , holeCubeVars  = [ e | (True,  e) <- flagged ]
        , holeTopes     = map render topes
        , holeCandidates = map render candidates
        , holeIntroductions = map render introductions
        , holeLocation  = loc
        } ]

-- | Check a hole that appears as the argument of a shape-restricted function,
-- whose domain is the cube @cube@ restricted by @tope@ (a scope over the
-- domain's bound variable). Mirrors the hole case of 'typecheck', but records
-- the shape as the hole's goal so the diagnostic shows @(binder : cube | tope)@.
checkHoleAgainstShape
  :: Eq var
  => Maybe VarIdent -> Binder -> TermT var -> TermT (Inc var)
  -> TypeCheck var (TermT var)
checkHoleAgainstShape mname orig cube tope = do
  reject <- asks holesAreErrors
  if reject
    then issueTypeError (TypeErrorUnsolvedHole mname cube)
    else do
      recordHoleShape mname cube (Just (orig, tope))
      return (HoleT TypeInfo{ infoType = cube, infoWHNF = Nothing, infoNF = Nothing } mname)

ppSomeAction :: Eq var => [(var, Maybe VarIdent)] -> Int -> Action var -> String
ppSomeAction origs n action = ppAction [] n (toRzkVarIdent <$> action)
  where
    vars = nub (foldMap pure action)
    mapping = zip vars defaultVarIdents
    toRzkVarIdent var = fromMaybe "_" $
      join (lookup var origs) <|> lookup var mapping

ppAction :: [(VarIdent, [([Proj], VarIdent)])] -> Int -> Action' -> String
ppAction pm n = unlines . map (replicate (2 * n) ' ' <>) . \case
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

  ActionContextEntailedBy ctxTopes term ->
    [ "checking if local context"
    , intercalate "\n" (map (("  " <>) . ppU . untyped) ctxTopes)
    , "includes (is entailed by) restriction tope"
    , "  " <> ppU (untyped term) ]

  ActionContextEntails ctxTopes term ->
    [ "checking if local context"
    , intercalate "\n" (map (("  " <>) . ppU . untyped) ctxTopes)
    , "is included in (entails) the tope"
    , "  " <> ppU (untyped term) ]

  ActionContextEntailsUnion ctxTopes terms ->
    [ "checking if local context"
    , intercalate "\n" (map (("  " <>) . ppU . untyped) ctxTopes)
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
    ppU :: Term' -> String
    ppU = show . foldBinderProjections pm
    ppTyped :: TermT' -> String
    ppTyped = show . foldBinderProjectionsT pm


traceAction' :: Int -> Action' -> a -> a
traceAction' n action = trace ("[debug]\n" <> ppAction [] n action)

unsafeTraceAction' :: Int -> Action var -> a -> a
unsafeTraceAction' n = traceAction' n . unsafeCoerce

data LocationInfo = LocationInfo
  { locationFilePath :: Maybe FilePath
  , locationLine     :: Maybe Int
  } deriving (Eq, Show)

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

addModalityToScope :: TModality -> ScopeInfo var -> ScopeInfo var
addModalityToScope md ScopeInfo{..} = ScopeInfo
  { scopeVars = map (\(var, VarInfo{..}) -> (var, VarInfo{ modAccum = comp modAccum md, ..})) scopeVars, .. }

data VarInfo var = VarInfo
  { varType                :: TermT var
  , varValue               :: Maybe (TermT var)
  , varModality            :: TModality
  , modAccum               :: TModality
  , varOrig                :: Binder
  , varIsAssumption        :: Bool -- FIXME: perhaps, introduce something like decl kind?
  , varIsTopLevel          :: Bool
  , varDeclaredAssumptions :: [var]
  , varLocation            :: Maybe LocationInfo
  } deriving (Functor, Foldable)


class ModeTheory m where
    iden :: m
    comp :: m -> m -> m
    coe :: m -> m -> Bool

instance ModeTheory TModality where
  iden = Id

  comp Flat Flat   = Flat
  comp Flat Sharp  = Flat
  comp Flat Op     = Flat
  comp Op Flat     = Flat
  comp Sharp Sharp = Sharp
  comp Sharp Flat  = Sharp
  comp Sharp Op    = Sharp
  comp Op Sharp    = Sharp
  comp Op Op       = Id
  comp Id m        = m
  comp m Id        = m

  coe Flat Id    = True
  coe Id Sharp   = True
  coe Flat Sharp = True
  coe a b        = a == b

data ModalTope var = ModalTope
  { tModAccum :: TModality
  , tModVar   :: TModality
  , tTope     :: TermT var
  } deriving (Functor, Foldable, Eq)

data Context var = Context
  { localScopes            :: [ScopeInfo var]
  , localTopes             :: [ModalTope var]
  , localTopesNF           :: [ModalTope var]
  , localTopesNFUnion      :: [[ModalTope var]]
  , localTopesEntailBottom :: Maybe Bool
  , actionStack            :: [Action var]
  , currentCommand         :: Maybe Rzk.Command
  , location               :: Maybe LocationInfo
  , verbosity              :: Verbosity
  , covariance             :: Covariance
  , renderBackend          :: Maybe RenderBackend
  , holesAreErrors         :: Bool
    -- ^ When 'True' (the default), an unfilled hole is reported as a
    -- 'TypeErrorUnsolvedHole'; finished work (and CI) must have no holes. The
    -- lenient mode ('allowHoles') instead records each hole's goal and context.
  , deferHoleMismatches    :: Bool
    -- ^ How holes behave during unification, giving three modes overall. With
    -- 'holesAreErrors' a hole is rejected outright (strict). Otherwise a hole
    -- always unifies as a leaf; this flag then chooses what happens when the
    -- /surrounding/ structure disagrees: 'True' (the default) defers — any term
    -- containing a hole is accepted, for an in-progress sketch — while 'False'
    -- keeps such a mismatch an error, so only a hole standing in a matching
    -- structure is accepted ('structuralHoleUnify').
  } deriving (Functor, Foldable)

addVarInCurrentScope :: var -> VarInfo var -> Context var -> Context var
addVarInCurrentScope var info Context{..} = Context
  { localScopes =
      case localScopes of
        []             -> [ScopeInfo Nothing [(var, info)]]
        scope : scopes -> addVarToScope var info scope : scopes
  , .. }

applyModalityToScopes :: TModality -> [ScopeInfo var] -> [ScopeInfo var]
applyModalityToScopes md scopes = map (addModalityToScope md) scopes

applyModalityToTopes :: TModality -> [ModalTope var] -> [ModalTope var]
applyModalityToTopes md topes = map (\ModalTope{..} -> ModalTope{tModAccum = comp tModAccum md, ..}) topes

applyModality :: TModality -> Context var -> Context var
applyModality md Context{..} = Context
  { localScopes = applyModalityToScopes md localScopes
  , localTopes = applyModalityToTopes md localTopes
  , localTopesNF = applyModalityToTopes md localTopesNF
  , localTopesNFUnion = map (applyModalityToTopes md) localTopesNFUnion
  , .. }

emptyTopeContext :: [ModalTope var]
emptyTopeContext =
  [ ModalTope Id Id    topeTopT
  , ModalTope Id Flat  topeTopT
  , ModalTope Id Op    topeTopT
  , ModalTope Id Sharp topeTopT
  ]

emptyContext :: Context var
emptyContext = Context
  { localScopes = [ScopeInfo Nothing []]
  , localTopes = emptyTopeContext
  , localTopesNF = emptyTopeContext
  , localTopesNFUnion = [emptyTopeContext]
  , localTopesEntailBottom = Just False
  , actionStack = []
  , currentCommand = Nothing
  , location = Nothing
  , verbosity = Normal
  , covariance = Covariant
  , renderBackend = Nothing
  , holesAreErrors = True
  , deferHoleMismatches = True
  }

-- | Switch to lenient hole mode: record each hole's goal and context instead
-- of reporting it as an error. Used by the structured goal/context query and
-- the @--allow-holes@ CLI mode; the default (strict) mode rejects holes.
allowHoles :: Context var -> Context var
allowHoles ctx = ctx { holesAreErrors = False }

-- | Within the given action, a hole unifies only as a leaf in an otherwise
-- matching structure: a structural mismatch around a hole stays an error rather
-- than being deferred (see 'deferHoleMismatches'). Used to ask whether a term
-- /could/ have a given type, as opposed to tolerating an in-progress sketch.
structuralHoleUnify :: Context var -> Context var
structuralHoleUnify ctx = ctx { deferHoleMismatches = False }

askCurrentScope :: TypeCheck var (ScopeInfo var)
askCurrentScope = asks localScopes >>= \case
  []              -> panicImpossible "no current scope available"
  scope : _scopes -> pure scope

isAccessible :: ModalTope var -> Bool
isAccessible mt = coe (tModVar mt) (tModAccum mt)

filterAccessible :: [ModalTope var] -> [ModalTope var]
filterAccessible = filter isAccessible

filterInaccessible :: [ModalTope var] -> [ModalTope var]
filterInaccessible = filter (not . isAccessible)

partitionAccessible :: [ModalTope var] -> ([ModalTope var], [ModalTope var])
partitionAccessible = partition isAccessible

accessibleTopes :: [ModalTope var] -> [TermT var]
accessibleTopes = map tTope . filterAccessible

plainTope :: TermT var -> ModalTope var
plainTope = ModalTope Id Id

availableTopes :: Context var -> [TermT var]
availableTopes ctx = map tTope $ filterAccessible (localTopes ctx)

availableTopesNF :: Context var -> [TermT var]
availableTopesNF ctx = map tTope $ filterAccessible (localTopesNF ctx)

varInfos :: Context var -> [(var, VarInfo var)]
varInfos Context{..} = concatMap scopeVars localScopes

varTypes :: Context var -> [(var, TermT var)]
varTypes = map (fmap varType) . varInfos

varValues :: Context var -> [(var, Maybe (TermT var))]
varValues = map (fmap varValue) . varInfos

varOrigs :: Context var -> [(var, Maybe VarIdent)]
varOrigs = map (fmap (binderName . varOrig)) . varInfos

-- | The full binder (pattern) of each in-scope variable, used to restore
-- pattern-binder component names (e.g. @t@\/@s@ for @\\ (t , s) -> …@) when
-- rendering goals, holes and contexts.
varBinders :: Context var -> [(var, Binder)]
varBinders = map (fmap varOrig) . varInfos

varModalities :: Context var -> [(var, TModality)]
varModalities = map (fmap varModality) . varInfos

varLocks :: Context var -> [(var, TModality)]
varLocks = map (fmap modAccum) . varInfos

varTopLevels :: Context var -> [(var, Bool)]
varTopLevels = map (fmap varIsTopLevel) . varInfos

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
      , varModality = varModality
      , modAccum = modAccum
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
      , varModality = Id
      , modAccum = Id
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
      , declLocation = updatePosition (binderName varOrig >>= fmap fst . Rzk.hasPosition . fromVarIdent) <$> loc -- FIXME
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
  , case filter (/= topeTopT) (availableTopes ctx) of
      [] -> "Local tope context is unrestricted (⊤)."
      localTopes' -> namedBlock TopDown "Local tope context:"
        [ "  " <> ppU (untyped tope)
        | tope <- localTopes' ]
  , ""
  , block dir
    [ "when " <> ppAction pm 0 action
    | action <- actionStack ]
  , namedBlock TopDown "Definitions in context:"
    [ block dir
      [ dispName x <> " : " <> ppU (untyped ty)
      | (x, ty) <- reverse (varTypes ctx) ] ]
  ]
  where
    (fbs, pm) = contextBinders ctx
    ppU = show . foldBinderProjections pm
    -- a pattern binder is shown as its pattern, e.g. (t , s); others by name
    dispName x = maybe (show (Pure x :: Term')) (show . binderDisplayName) (lookup x fbs)

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
  ty' <- whnfT ty
  term' <- traverse whnfT term
  localDeclPrepared (Decl x ty' term' isAssumption vars loc) tc

localDeclPrepared :: Decl VarIdent -> TypeCheck VarIdent a -> TypeCheck VarIdent a
localDeclPrepared (Decl x ty term isAssumption vars loc) tc = do
  checkTopLevelDuplicate x
  local update tc
  where
    update = addVarInCurrentScope x VarInfo
      { varType = ty
      , varValue = term
      , varOrig = BinderVar (Just x)
      , varModality  = Id
      , modAccum = Id
      , varIsAssumption = isAssumption
      , varIsTopLevel = True
      , varDeclaredAssumptions = vars
      , varLocation = loc
      }

-- | A binding shown in a hole's local context: the display name and its type,
-- already rendered with user-facing 'VarIdent' names (see 'HoleInfo').
data HoleEntry = HoleEntry
  { holeEntryName :: VarIdent
  , holeEntryType :: Term'
  } deriving (Eq, Show)

-- | The structured goal and context at a hole, recorded in lenient mode (see
-- 'allowHoles'). Everything is rendered to user-facing 'VarIdent' names at
-- record time, so 'HoleInfo' is independent of the De Bruijn scope it came
-- from. Local hypotheses are split into ordinary term variables and cube
-- variables (the cube/tope layer is specific to Rzk); the global environment is
-- deliberately excluded — it belongs in a searchable inventory, not the goal
-- panel.
data HoleInfo = HoleInfo
  { holeName      :: Maybe VarIdent  -- ^ the @?name@, if the hole was named
  , holeGoal      :: Term'           -- ^ expected type (the goal), kept symbolic
  , holeGoalShape :: Maybe (VarIdent, Term')
    -- ^ when the goal is a /shape/ (the hole is the argument of a
    -- shape-restricted function), the shape's bound variable and its tope: the
    -- goal then reads @(binder : holeGoal | tope)@. 'Nothing' for an ordinary
    -- goal. (Extension-type goals need no special handling — they are already a
    -- restricted type in 'holeGoal'.)
  , holeTermVars  :: [HoleEntry]     -- ^ local hypotheses whose type is not a cube
  , holeCubeVars  :: [HoleEntry]     -- ^ local cube variables (type is a cube)
  , holeTopes     :: [Term']         -- ^ local tope assumptions (excluding ⊤)
  , holeCandidates :: [Term']
    -- ^ elimination spines over the local hypotheses whose type fits the goal,
    -- with applied arguments left as holes (see 'allEliminationsInto'). Already
    -- rendered, like the other fields.
  , holeIntroductions :: [Term']
    -- ^ introduction forms for the goal type, built from its head constructor
    -- with the constituents left as holes (see 'allIntroductionsOf'). Already
    -- rendered, like the other fields.
  , holeLocation  :: Maybe LocationInfo
  } deriving (Eq, Show)

type TypeCheck var =
  ReaderT (Context var)
    (WriterT [HoleInfo] (Except (TypeErrorInScopedContext var)))

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

entailM :: Eq var => [ModalTope var] -> TermT var -> TypeCheck var Bool
entailM modalTopes goal = do
  -- genTopes <- generateTopesForPointsM (allTopePoints goal)
  discreteAxioms <- generateTopesForModalCubeVarsM
  let topes'    = nubTermT (modalTopes <> discreteAxioms)
      topes''   = simplifyLHSwithDisjunctions topes'
  topes'''  <- mapM (fmap (saturateTopes (allTopePoints goal) . saturateBottom) . saturateInv) topes''
  prettyTopes <- mapM ppTermInContext (map tTope (saturateTopes (allTopePoints goal) (simplifyLHS topes')))
  prettyTope <- ppTermInContext goal
  traceTypeCheck Debug
    ("entail " <> intercalate ", " prettyTopes <> " |- " <> prettyTope) $
      and <$> mapM (`solveRHSM` goal) topes'''


generateTopesForModalCubeVarsM :: Eq var => TypeCheck var [ModalTope var]
generateTopesForModalCubeVarsM = do
  infos <- asks varInfos
  fmap (nubTermT . concat) $ forM infos $ \(var, info) -> do
    whnfT (varType info) >>= \case
      TypeModalT _ Flat inner -> do
        whnfT inner >>= \case
          Cube2T{} -> do
            let pt = modExtractT cube2T Id Flat (Pure var)
            return [plainTope (topeOrT (topeEQT pt cube2_0T) (topeEQT pt cube2_1T))]
          CubeIT{} -> do
            let pt = modExtractT cubeIT Id Flat (Pure var)
            return [plainTope (topeOrT (topeEQT pt cubeI_0T) (topeEQT pt cubeI_1T))]
          _ -> return []
      _ -> return []

entailTraceM :: Eq var => [ModalTope var] -> TermT var -> TypeCheck var Bool
entailTraceM modalTopes goal = do
  topes' <- mapM ppTermInContext (accessibleTopes modalTopes)
  goal' <- ppTermInContext goal
  result <- trace ("entail " <> intercalate ", " topes' <> " |- " <> goal') $
        modalTopes `entailM` goal
  return $ trace ("  " <> show result) result

nubTermT :: Eq a => [a] -> [a]
nubTermT []     = []
nubTermT (t:ts) = t : nubTermT (filter (/= t) ts)

saturateTopes :: Eq var => [TermT var] -> [ModalTope var] -> [ModalTope var]
saturateTopes _points topes =
  let (accessible, inaccessible) = partitionAccessible topes
      saturated = saturateWith
        (\mt ts -> mt `elem` ts)
        (\new old -> map plainTope $ generateTopes (map tTope new) (map tTope old))
        accessible
  in saturated <> inaccessible

saturateInv :: Eq var => [ModalTope var] -> TypeCheck var [ModalTope var]
saturateInv modalTopes = do
    -- FIXME: this is a workaround; ideally we should regenerate all topes
    -- on EVERY modality change in any layer, but that would produce too many;
    -- for now we also invert topes that were accessible before the modality shift
    let accessible = filterAccessible modalTopes
        accessibleById = filter (\mt -> coe (tModVar mt) Id) modalTopes
    invResults <- forM (nubTermT (accessible <> accessibleById)) $ \mt -> do
      nf <- nfTope $ modExtractT topeT Id Op (topeInvT (tTope mt))
      return $ ModalTope (tModAccum mt) Op nf
    let accessibleUnderOp = filter (\mt -> coe (tModVar mt) (comp (tModAccum mt) Op)) modalTopes
    uninvResults <- forM accessibleUnderOp $ \(ModalTope acc var' phi) -> do
      nf <- nfTope $ topeUninvT (modAppT topeT Op phi)
      return $ ModalTope (comp acc Op) var' nf
    let newTopes = nubTermT (invResults <> uninvResults)
        fresh = filter (`notElem` modalTopes) newTopes
    return (modalTopes <> fresh)

-- | Ex falso for BOT, lifted across modalities.
--
-- A contradiction in the topes that are genuinely available at the identity
-- modality entails BOT, and BOT entails @_μ BOT@ for every modality @μ@ by the
-- absurd rule (this holds for BOT specifically; a general tope @φ@ does NOT give
-- @_μ φ@, which would need the missing unit @id ⇒ μ@). Re-asserting @_μ BOT@ at
-- each lock @μ@ where an available tope was hidden lets the contradiction survive
-- the lock: e.g. @_b BOT@ is accessible under a @_b@ lock (@coe Flat Flat@), so
-- @mod _b recBOT@ in a vacuous context is accepted.
--
-- A tope counts as available at the identity modality when its variable modality
-- coerces into @Id@: a @_b@-modal tope qualifies via the counit (@coe Flat Id@),
-- but a @_#@-modal one does not (@coe Sharp Id@ is False) — which is exactly why
-- @_# BOT@ does not leak to plain BOT (see ill-modal-sharp-bot-not-bot).
saturateBottom :: Eq var => [ModalTope var] -> [ModalTope var]
saturateBottom modalTopes
  | null droppedAccums = modalTopes   -- nothing hidden by a lock; ordinary saturation suffices
  | botDerivable       = modalTopes <> fresh
  | otherwise          = modalTopes
  where
    idAccessible  = filter (\mt -> coe (tModVar mt) Id) modalTopes
    droppedAccums = nub [ tModAccum mt | mt <- idAccessible, not (isAccessible mt) ]
    saturatedId   = saturateWith (\t ts -> t `elem` ts) generateTopes (map tTope idAccessible)
    botDerivable  = topeBottomT `elem` saturatedId
    fresh = [ mt
            | acc <- droppedAccums
            , let mt = ModalTope acc acc topeBottomT
            , mt `notElem` modalTopes ]

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
  | topeEQT cubeI_0T cubeI_1T `elem` newTopes = [topeBottomT]
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
      , [ topeEQT x y | TopeLEQT _ty x y@CubeI_0T{} <- newTopes ]
        -- FIXME: consequence of LEM for LEQ and antisymmetry for LEQ
      , [ topeEQT x y | TopeLEQT _ty x@CubeI_1T{} y <- newTopes ]

        -- subtyping 2 <: II: endpoints and order of 2 lift to II
      , [ topeEQT x cubeI_0T | TopeEQT _ty x Cube2_0T{} <- newTopes ]
      , [ topeEQT cubeI_0T x | TopeEQT _ty Cube2_0T{} x <- newTopes ]
      , [ topeEQT x cubeI_1T | TopeEQT _ty x Cube2_1T{} <- newTopes ]
      , [ topeEQT cubeI_1T x | TopeEQT _ty Cube2_1T{} x <- newTopes ]
      , [ topeLEQT x cubeI_0T | TopeLEQT _ty x Cube2_0T{} <- newTopes ]
      , [ topeLEQT cubeI_0T x | TopeLEQT _ty Cube2_0T{} x <- newTopes ]
      , [ topeLEQT x cubeI_1T | TopeLEQT _ty x Cube2_1T{} <- newTopes ]
      , [ topeLEQT cubeI_1T x | TopeLEQT _ty Cube2_1T{} x <- newTopes ]
      ]

generateTopesForPointsM :: Eq var => [TermT var] -> TypeCheck var [TermT var]
generateTopesForPointsM points = do
  let pairs = nub $ concat
        [ [ (x, y)
          | x : points' <- tails (filter (`notElem` [cube2_0T, cube2_1T, cubeI_0T, cubeI_1T]) points)
          , y <- points'
          , x /= y ]
        ]
  stars <- forM points $ \x -> do
    xType <- typeOf x
    return $ if (xType == cubeUnitT)
      then [topeEQT x cubeUnitStarT]
      else []
  topes <- forM pairs $ \(x, y) -> do
    xType <- typeOf x
    yType <- typeOf y
    return $ if (xType == cube2T) && (yType == cube2T)
      then [topeOrT (topeLEQT x y) (topeLEQT y x)]
      else []
  return (concat (topes ++ stars))

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
    | CubeUnitT{} <- infoType -> [p]
  _ -> []

-- | Simplify the context, including disjunctions. See also 'simplifyLHS'.
simplifyLHSwithDisjunctions :: Eq var => [ModalTope var] -> [[ModalTope var]]
simplifyLHSwithDisjunctions topes = map nubTermT $
  case topes of
    [] -> [[]]
    (ModalTope _ _ TopeTopT{}) : topes' -> simplifyLHSwithDisjunctions topes'
    (ModalTope mAcc mVar TopeBottomT{}) : _  -> [[ModalTope mAcc mVar topeBottomT]]
    (ModalTope mAcc mVar (TopeAndT _ l r)) : topes' -> simplifyLHSwithDisjunctions ((ModalTope mAcc mVar l) : (ModalTope mAcc mVar r) : topes')

    -- NOTE: it is inefficient to expand disjunctions immediately
    (ModalTope mAcc mVar (TopeOrT  _ l r)) : topes' -> simplifyLHSwithDisjunctions ((ModalTope mAcc mVar l) : topes') <> simplifyLHSwithDisjunctions ((ModalTope mAcc mVar r) : topes')

    (ModalTope mAcc mVar (TopeEQT  _ (PairT _ x y) (PairT _ x' y'))) : topes' ->
      simplifyLHSwithDisjunctions (ModalTope mAcc mVar (topeEQT x x') : ModalTope mAcc mVar (topeEQT y y') : topes')
    (ModalTope mAcc mVar (TypeModalT _ md inTope)) : topes' ->
      simplifyLHSwithDisjunctions ((ModalTope mAcc (comp mVar md) inTope) : topes')
    t : topes' -> map (t :) (simplifyLHSwithDisjunctions topes')

-- | Simplify the context, except disjunctions. See also 'simplifyLHSwithDisjunctions'.
simplifyLHS :: Eq var => [ModalTope var] -> [ModalTope var]
simplifyLHS topes = nubTermT $
  case topes of
    [] -> []
    (ModalTope _ _ TopeTopT{}) : topes' -> simplifyLHS topes'
    (ModalTope _ _ TopeBottomT{}) : _  -> [plainTope topeBottomT]
    (ModalTope mAcc mVar (TopeAndT _ l r)) : topes' -> simplifyLHS (ModalTope mAcc mVar l : ModalTope mAcc mVar r : topes')

    -- NOTE: it is inefficient to expand disjunctions immediately
    -- (ModalTope mAcc mVar (TopeOrT _ l r)) : topes' -> simplifyLHS (ModalTope mAcc mVar l : topes') <> simplifyLHS (ModalTope mAcc mVar r : topes')

    (ModalTope mAcc mVar (TopeEQT _ (PairT _ x y) (PairT _ x' y'))) : topes' ->
      simplifyLHS (ModalTope mAcc mVar (topeEQT x x') : ModalTope mAcc mVar (topeEQT y y') : topes')
    (ModalTope mAcc mVar (TypeModalT _ md inTope)) : topes' ->
      simplifyLHS (ModalTope mAcc (comp mVar md) inTope : topes')
    t : topes' -> t : simplifyLHS topes'

solveRHSM :: Eq var => [ModalTope var] -> TermT var -> TypeCheck var Bool
solveRHSM modalTopes goal =
  let topes = accessibleTopes modalTopes
  in case goal of
    _ | topeBottomT `elem` topes -> return True
    TopeTopT{}     -> return True
    TypeModalT _ty md inTope -> do
      let shifted = applyModalityToTopes md modalTopes
          resaturated = saturateTopes [] shifted
      resaturatedInv <- saturateInv resaturated
      solveRHSM resaturatedInv inTope
    TopeEQT  _ty (PairT _ty1 x y) (PairT _ty2 x' y') ->
      solveRHSM modalTopes $ topeAndT
        (topeEQT x x')
        (topeEQT y y')
    TopeEQT  _ty (PairT TypeInfo{ infoType = CubeProductT _ cubeI cubeJ } x y) r ->
      solveRHSM modalTopes $ topeAndT
        (topeEQT x (firstT cubeI r))
        (topeEQT y (secondT cubeJ r))
    TopeEQT  _ty l (PairT TypeInfo{ infoType = CubeProductT _ cubeI cubeJ } x y) ->
      solveRHSM modalTopes $ topeAndT
        (topeEQT (firstT cubeI l) x)
        (topeEQT (secondT cubeJ l) y)
    TopeEQT  _ty l r
      | or
          [ l == r
          , goal `elem` topes
          , topeEQT r l `elem` topes
          ] -> return True
    TopeEQT  _ty l r -> do
      lType <- typeOf l
      rType <- typeOf r
      return $ case (lType, rType) of
        (CubeUnitT{}, CubeUnitT{}) -> True
        _                          -> False
    TopeLEQT _ty l r
      | l == r -> return True
      | solveRHS topes (topeEQT l r) -> return True
      | solveRHS topes (topeEQT l cube2_0T) -> return True
      | solveRHS topes (topeEQT r cube2_1T) -> return True
    TopeAndT _ l r -> (&&)
      <$> solveRHSM modalTopes l
      <*> solveRHSM modalTopes r
    _ | goal `elem` topes -> return True
    TopeInvT{} -> do
      goal' <- nfTope goal
      case goal' of
        TopeInvT{} -> return False
        _          -> solveRHSM modalTopes goal'
    TopeUninvT{} -> do
      goal' <- nfTope goal
      case goal' of
        TopeUninvT{} -> return False
        _            -> solveRHSM modalTopes goal'
    TopeOrT  _ l r -> do
      l' <- solveRHSM modalTopes l
      r' <- solveRHSM modalTopes r
      if (l' || r')
        then return True
        else do
          lems <- generateTopesForPointsM (allTopePoints goal)
          let lems' = [ lem | lem@(TopeOrT _ t1 t2) <- lems, all (`notElem` topes) [t1, t2] ]
              (accessible, hidden) = partitionAccessible modalTopes
              withTope t = hidden ++ saturateTopes [] (plainTope t : accessible)

          case lems' of
            TopeOrT _ t1 t2 : _ -> do
              l'' <- solveRHSM (withTope t1) goal
              r'' <- solveRHSM (withTope t2) goal
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
  ctxTopes <- asks availableTopes
  performing (ActionContextEntails ctxTopes tope) $ do
    topes' <- asks localTopesNF
    tope' <- nfTope tope
    topes' `entailM` tope'

checkTopeEntails :: Eq var => TermT var -> TypeCheck var Bool
checkTopeEntails tope = do
  ctxTopes <- asks availableTopes
  performing (ActionContextEntailedBy ctxTopes tope) $ do
    contextTopes <- asks availableTopesNF
    restrictionTope <- nfTope tope
    let contextTopesRHS = foldr topeAndT topeTopT contextTopes
    [plainTope restrictionTope] `entailM` contextTopesRHS

checkEntails :: Eq var => TermT var -> TermT var -> TypeCheck var Bool
checkEntails l r = do  -- FIXME: add action
  l' <- nfTope l
  r' <- nfTope r
  [plainTope l'] `entailM` r'

contextEntails :: Eq var => TermT var -> TypeCheck var ()
contextEntails tope = do
  ctxTopes <- asks availableTopes
  performing (ActionContextEntails ctxTopes tope) $ do
    topeIsEntailed <- checkTope tope
    topes' <- asks availableTopesNF
    -- When a hole is used in a cube/tope position (e.g. as the argument of a
    -- shape-restricted function), the tope being checked mentions the hole and
    -- cannot be decided. Treat it as satisfied (defer) rather than failing.
    unless (topeIsEntailed || containsHole tope) $
      issueTypeError $ TypeErrorTopeNotSatisfied topes' tope

topesEquiv :: Eq var => TermT var -> TermT var -> TypeCheck var Bool
topesEquiv expected actual = performing (ActionUnifyTerms expected actual) $ do
  expected' <- nfT expected
  actual' <- nfT actual
  (&&)
    <$> [plainTope expected'] `entailM` actual'
    <*> [plainTope actual'] `entailM` expected'

-- | Check that the local tope context is included in (entails) the union of
-- the given topes. This is the COVERAGE obligation of @recOR@: every point of the
-- context must be covered by some branch guard.
--
-- Note that only coverage is required, not equivalence: branch guards may overhang
-- the context (e.g. when splitting with an already-defined shape), so we do not
-- require @OR(guards) |- context@.
contextEntailsUnion :: Eq var => [TermT var] -> TypeCheck var ()
contextEntailsUnion topes = do
  ctxTopes <- asks availableTopes
  performing (ActionContextEntailsUnion ctxTopes topes) $ do
    contextTopes <- asks localTopesNF
    topesNF <- mapM nfTope topes
    let unionRHS = foldr topeOrT topeBottomT topesNF
    contextTopes `entailM` unionRHS >>= \case
      -- a guard mentioning an (unfilled) hole can't be decided; defer coverage
      False | not (any containsHole topesNF) ->
        issueTypeError $ TypeErrorTopeNotSatisfied (accessibleTopes contextTopes) unionRHS
      _ -> return ()

-- | Diagnose a recOR branch guard or restriction face against the local tope
-- context. There are three cases, by how the tope relates to the context:
--
--   * DISJOINT — the tope and a consistent context have empty overlap (their
--     conjunction is ⊥). The face/branch is then vacuous everywhere, so this is a
--     hard error.
--   * OVERHANG — the tope is not entailed by the context but still overlaps it.
--     This is allowed and often intentional (e.g. splitting or restricting with an
--     already-defined shape, whose faces live on the whole cube rather than being
--     relativised to the context), so we only emit a non-fatal hint. The hint is
--     gated at 'Normal' verbosity, hence silent under 'Silent' (e.g. in tests).
--   * CONTAINED — the tope entails the context: nothing to report.
checkTopeAgainstContext :: Eq var => String -> TermT var -> TypeCheck var ()
checkTopeAgainstContext what tope = do
  -- a contradictory context is handled elsewhere (recBOT)
  ctxEntailsBottom <- contextEntailsBottom
  unless ctxEntailsBottom $ do
    contextTopes <- asks localTopesNF
    let ctxTopes = filter (/= topeTopT) (accessibleTopes contextTopes)
    disjoint <- (plainTope tope : contextTopes) `entailM` topeBottomT
    -- a face/guard mentioning an (unfilled) hole can't be decided; defer
    if disjoint && not (containsHole tope)
      then issueTypeError (TypeErrorTopeContextDisjoint tope ctxTopes)
      else do
        entailed <- checkTopeEntails tope     -- tope |- AND(accessible context)
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

enterScopeContext :: Binder -> TModality -> TermT var -> Maybe (TermT var) -> Context var -> Context (Inc var)
enterScopeContext orig md ty val context =
  addVarInCurrentScope Z VarInfo
    { varType   = S <$> ty
    , varValue  = fmap (S <$>) val
    , varOrig   = orig
    , varModality = md
    , modAccum = Id
    , varIsAssumption = False
    , varIsTopLevel = False
    , varDeclaredAssumptions = []
    , varLocation = location context
    }
    (S <$> context)

enterScope :: Binder -> TermT var -> TypeCheck (Inc var) b -> TypeCheck var b
enterScope orig ty action = do
  newContext <- asks (enterScopeContext orig Id ty Nothing)
  closeScope orig (runReaderT action newContext)

enterScopeWithBind :: Binder -> TModality -> TermT var -> TermT var -> TypeCheck (Inc var) b -> TypeCheck var b
enterScopeWithBind orig md ty val action = do
  newContext <- asks (enterScopeContext orig md ty (Just val))
  closeScope orig (runReaderT action newContext)

-- | Run a sub-scope computation and lift it back to the enclosing scope: close
-- the error channel one binder with 'ScopedTypeError' (as before), and re-emit
-- the holes it recorded. 'HoleInfo' is already rendered to 'VarIdent' names, so
-- no De Bruijn re-indexing is needed — only a plain re-'tell'. On a thrown
-- error the sub-scope's holes are dropped, which is intended: holes only matter
-- on the success path (lenient mode), and strict mode wants the error anyway.
closeScope
  :: Binder
  -> WriterT [HoleInfo] (Except (TypeErrorInScopedContext (Inc var))) b
  -> TypeCheck var b
closeScope orig inner = do
  (b, holes) <- lift . lift . withExceptT (ScopedTypeError (binderName orig)) $ runWriterT inner
  lift (tell holes)
  return b

enterModality :: TModality -> TypeCheck var b -> TypeCheck var b
enterModality Id action = action
enterModality md action = do
  newContext <- asks (applyModality md)
  let newContext' = newContext { localTopesEntailBottom = Nothing }
  lift $ runReaderT action newContext'

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
-- | Subtyping on interval
etaMatch _mterm CubeIT{} Cube2T{} = pure (cubeIT, cubeIT)
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
  CubeIT{} -> pure tt
  CubeI_0T{} -> pure tt
  CubeI_1T{} -> pure tt
  CubeFlipT{} -> nfTope tt
  CubeUnflipT{} -> nfTope tt

  -- tope layer (except vars, pairs of points, and applications)
  TopeTopT{} -> pure tt
  TopeBottomT{} -> pure tt
  TopeAndT{} -> nfTope tt
  TopeOrT{} -> nfTope tt
  TopeEQT{} -> nfTope tt
  TopeLEQT{} -> nfTope tt
  TopeInvT{} -> nfTope tt
  TopeUninvT{} -> nfTope tt

  -- type layer terms that should not be evaluated further
  LambdaT{} -> pure tt
  PairT{} -> pure tt
  ReflT{} -> pure tt
  TypeFunT{} -> pure tt
  TypeSigmaT{} -> pure tt
  TypeIdT{} -> pure tt
  TypeModalT{} -> pure tt
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
        tryRestriction typeOf_tt >>= \case
            Just tt' -> whnfT tt'
            Nothing -> case tt of
              -- a hole is opaque: it never reduces, it is already a normal form
              HoleT{} -> pure tt
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
                          tryRestriction ret' >>= \case -- FIXME: too many unnecessary checks?
                            Nothing  -> pure (AppT ty { infoType = ret' } f' x)
                            Just tt' -> whnfT tt'
                    _ -> pure (AppT ty f' x)

              LetT _ty _orig _mparam val body ->
                whnfT (substituteT val body)
              LetModT _ty _orig app inn _mparam val body -> do
                val' <- (enterModality app $ whnfT val) >>= \case
                  ModAppT _ md t | md == inn -> enterModality md $ whnfT t
                  b' -> do
                    bty <- typeOf b' >>= \case
                      TypeModalT _ _ t -> pure t
                      _ -> panicImpossible "not modal in letmod"
                    pure (modExtractT bty app inn b')
                whnfT (substituteT val' body)
              FirstT ty t ->
                whnfT t >>= \case
                  PairT _ l _r -> whnfT l
                  t'           -> pure (FirstT ty t')

              SecondT ty t ->
                whnfT t >>= \case
                  PairT _ _l r -> whnfT r
                  t'           -> pure (SecondT ty t')
              ModAppT ty md b -> do
                (enterModality md $ whnfT b) >>= \case
                  ModExtractT _ app inn t | inn == md -> enterModality (comp md app) $ whnfT t
                  b' -> pure $ ModAppT ty md b'
              ModExtractT ty app inn b -> do
                (enterModality app $ whnfT b) >>= \case
                  ModAppT _ md t | inn == md -> enterModality inn $ whnfT t
                  b' -> pure (ModExtractT ty app inn b')
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
  HoleT{} -> pure tt
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
  CubeIT{} -> pure tt
  CubeI_0T{} -> pure tt
  CubeI_1T{} -> pure tt

  -- type layer constants
  TypeUnitT{} -> pure tt
  UnitT{} -> pure tt

  -- cube layer with computation
  CubeProductT _ty l r -> cubeProductT <$> nfTope l <*> nfTope r

  CubeFlipT ty t ->
    nfTope t >>= \case
      CubeUnflipT _ t' -> pure t'
      Cube2_0T{}       -> pure (modAppT (typeModalT cubeT Op cube2T) Op cube2_1T)
      Cube2_1T{}       -> pure (modAppT (typeModalT cubeT Op cube2T) Op cube2_0T)
      CubeI_0T{}       -> pure (modAppT (typeModalT cubeT Op cubeIT) Op cubeI_1T)
      CubeI_1T{}       -> pure (modAppT (typeModalT cubeT Op cubeIT) Op cubeI_0T)
      t'               -> pure (CubeFlipT ty t')

  CubeUnflipT ty t -> 
    nfTope t >>= \case
      CubeFlipT _ t'          -> pure t'
      ModAppT _ Op Cube2_0T{} -> pure cube2_1T
      ModAppT _ Op Cube2_1T{} -> pure cube2_0T
      ModAppT _ Op CubeI_0T{} -> pure cubeI_1T
      ModAppT _ Op CubeI_1T{} -> pure cubeI_0T
      t'                      -> pure (CubeUnflipT ty t')

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

  TopeInvT ty t ->
    -- Match And/Or on the *unnormalized* input: nfTope of a shape-restricted
    -- App produces a TopeAnd via shape-side-condition propagation, and
    -- distributing inv over that synthetic conjunction loops forever because
    -- the recursive topeInvT renormalizes the same App back into a TopeAnd.
    case t of
      TopeTopT _ -> pure $ modAppT topeT Op topeTopT
      TopeBottomT _ -> pure $ modAppT topeT Op topeBottomT
      TopeLEQT _ x y -> do
        xTy <- typeOf x
        yTy <- typeOf y
        nfTope $
          modAppT (typeModalT universeT Op topeT) Op
            (topeLEQT
              (modExtractT topeT Id Op (cubeFlipT xTy y))
              (modExtractT topeT Id Op (cubeFlipT yTy x)))
      TopeEQT _ x y -> do
        xTy <- typeOf x
        yTy <- typeOf y
        nfTope $
          modAppT (typeModalT universeT Op topeT) Op
            (topeEQT
              (modExtractT topeT Id Op (cubeFlipT xTy y))
              (modExtractT topeT Id Op (cubeFlipT yTy x)))
      TopeAndT _ phi psi -> nfTope $
        modAppT (typeModalT universeT Op topeT) Op
          (topeAndT
            (modExtractT topeT Id Op (topeInvT phi))
            (modExtractT topeT Id Op (topeInvT psi)))
      TopeOrT _ phi psi -> nfTope $
        modAppT (typeModalT universeT Op topeT) Op
          (topeOrT
            (modExtractT topeT Id Op (topeInvT phi))
            (modExtractT topeT Id Op (topeInvT psi)))
      _ ->
        nfTope t >>= \case
          TopeTopT _ -> pure topeTopT
          TopeBottomT _ -> pure topeBottomT
          TopeUninvT _ phi -> pure phi
          TopeLEQT _ x y -> do
            xTy <- typeOf x
            yTy <- typeOf y
            nfTope $
              modAppT (typeModalT universeT Op topeT) Op
                (topeLEQT
                  (modExtractT topeT Id Op (cubeFlipT xTy y))
                  (modExtractT topeT Id Op (cubeFlipT yTy x)))
          TopeEQT _ x y -> do
            xTy <- typeOf x
            yTy <- typeOf y
            nfTope $
              modAppT (typeModalT universeT Op topeT) Op
                (topeEQT
                  (modExtractT topeT Id Op (cubeFlipT xTy y))
                  (modExtractT topeT Id Op (cubeFlipT yTy x)))
          t' -> pure (TopeInvT ty t')

  TopeUninvT ty t ->
    case t of
      ModAppT _ Op inner -> case inner of
        TopeTopT _ -> pure topeTopT
        TopeBottomT _ -> pure topeBottomT 
        TopeAndT _ phi psi ->
          nfTope $
              (topeAndT
                (topeUninvT phi)
                (topeUninvT psi))
        TopeOrT _ phi psi ->
          nfTope $
              (topeOrT
                (topeUninvT phi)
                (topeUninvT psi))
        _ ->
          nfTope t >>= \case
            TopeTopT _ -> pure topeTopT
            TopeBottomT _ -> pure topeBottomT
            TopeInvT _ phi -> pure phi
            ModAppT _ Op inner'' -> case inner'' of
              TopeLEQT _ x y -> do
                xTy <- typeOf x
                yTy <- typeOf y
                nfTope $
                  (topeLEQT
                    (cubeUnflipT xTy (modAppT (typeModalT cubeT Op xTy) Op y))
                    (cubeUnflipT yTy (modAppT (typeModalT cubeT Op yTy) Op x)))
              TopeEQT _ x y -> do
                xTy <- typeOf x
                yTy <- typeOf y
                nfTope $
                  (topeEQT
                    (cubeUnflipT xTy (modAppT (typeModalT cubeT Op xTy) Op y))
                    (cubeUnflipT yTy (modAppT (typeModalT cubeT Op yTy) Op x)))
              inner' ->
                pure $
                  TopeUninvT ty
                    (modAppT (typeModalT universeT Op topeT) Op inner')
            t' ->
                pure (TopeUninvT ty t')
      _ ->
        nfTope t >>= \case
          TopeInvT _ phi -> pure phi
          t'@(ModAppT _ Op _) -> nfTope (TopeUninvT ty t')
          t' -> pure (TopeUninvT ty t')

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
  ModAppT ty md b ->
    (enterModality md $ nfTope b) >>= \case
      ModExtractT _ _ inn t | inn == md -> pure t
      b' -> pure $ ModAppT ty md b'
  ModExtractT ty app inn b ->
    (enterModality app $ nfTope b) >>= \case
      ModAppT _ md t | inn == md -> pure t
      b' -> pure $ ModExtractT ty app inn b'
  LetModT _ty _orig app inn _mparam val body -> do
    val' <- (enterModality app $ nfTope val) >>= \case
      ModAppT _ md t | md == inn -> return t
      b' -> do
        ty <- typeOf b' >>= \case
          TypeModalT _ _ t -> pure t
          _ -> panicImpossible "not modal in letmod"
        pure (modExtractT ty app inn b')
    nfTope (substituteT val' body)

  TypeModalT ty md inner -> TypeModalT ty md <$> (enterModality md $ nfTope inner)
  LetT _ty _orig _mparam val body -> nfTope (substituteT val body)
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
  CubeIT{} -> pure tt
  CubeI_0T{} -> pure tt
  CubeI_1T{} -> pure tt

  -- cube layer with computation
  CubeProductT{} -> nfTope tt
  CubeFlipT{} -> nfTope tt
  CubeUnflipT{} -> nfTope tt

  -- tope layer constants
  TopeTopT{} -> pure tt
  TopeBottomT{} -> pure tt

  -- tope layer with computation
  TopeAndT{} -> nfTope tt
  TopeOrT{} -> nfTope tt
  TopeEQT{} -> nfTope tt
  TopeLEQT{} -> nfTope tt
  TopeInvT{} -> nfTope tt
  TopeUninvT{} -> nfTope tt

  -- type layer constants
  ReflT ty _x -> pure (ReflT ty Nothing)
  RecBottomT{} -> pure tt
  TypeUnitT{} -> pure tt
  UnitT{} -> pure tt

  -- type ascriptions are ignored, since we already have a typechecked term
  TypeAscT _ty term _ty' -> nfT term

  -- now we are in the type layer
  _ -> do
    typeOf tt >>= tryRestriction >>= \case
        Just tt' -> nfT tt'
        Nothing -> case tt of
          -- a hole is opaque: it never reduces, it is already a normal form
          HoleT{} -> pure tt
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
          LetT _ty _orig _mparam val body ->
            nfT (substituteT val body)
          LetModT _ty _orig app inn _mparam val body -> do
            val' <- (enterModality app $ whnfT val) >>= \case
              ModAppT _ md t | md == inn -> enterModality md $ nfT t
              b' -> do
                bty <- typeOf b' >>= \case
                  TypeModalT _ _ t -> pure t
                  _ -> panicImpossible "not modal in letmod"
                pure (modExtractT bty app inn b')
            nfT (substituteT val' body)
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
          TypeModalT ty md b -> do
            b' <- enterModality md $ nfT b
            pure (TypeModalT ty md b')
          ModAppT ty md b -> do
            (enterModality md $ whnfT b) >>= \case
              ModExtractT _ app inn t | inn == md -> enterModality (comp inn app) $ nfT t
              b' -> ModAppT ty md <$> (enterModality md $ nfT b')
          ModExtractT ty app inn b -> do
            (enterModality app $ whnfT b) >>= \case
              ModAppT _ md t | inn == md -> enterModality (comp inn app) $ nfT t
              b' -> ModExtractT ty app inn <$> (enterModality app $ nfT b') 
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

modalityOfVar :: Eq var => var -> TypeCheck var (TModality)
modalityOfVar x = asks (lookup x . varModalities) >>= \case
  Nothing -> issueTypeError $ TypeErrorUndefined x
  Just m -> return m

locksOfVar :: Eq var => var -> TypeCheck var (TModality)
locksOfVar x = asks (lookup x . varLocks) >>= \case
  Nothing -> issueTypeError $ TypeErrorUndefined x
  Just m -> return m

isTopLevelVar :: Eq var => var -> TypeCheck var Bool
isTopLevelVar x = asks (lookup x . varTopLevels) >>= \case
  Nothing -> issueTypeError $ TypeErrorUndefined x
  Just b -> return b

typeOfUncomputed :: Eq var => TermT var -> TypeCheck var (TermT var)
typeOfUncomputed = \case
  Pure x                     -> typeOfVar x
  Free (AnnF TypeInfo{..} _) -> pure infoType

typeOf :: Eq var => TermT var -> TypeCheck var (TermT var)
typeOf t = typeOfUncomputed t >>= whnfT

unifyTopes :: Eq var => TermT var -> TermT var -> TypeCheck var ()
unifyTopes l r = do
  equiv <- (&&)
    <$> [plainTope l] `entailM` r
    <*> [plainTope r] `entailM` l
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
  unify Nothing f g
  setVariance Invariant $ unify Nothing x y
unifyViaDecompose _ _ = issueTypeError (TypeErrorOther "cannot decompose")

unifyInCurrentContext :: Eq var => Maybe (TermT var) -> TermT var -> TermT var -> TypeCheck var ()
unifyInCurrentContext mterm expected actual = performing action $ do
  inBottom <- contextEntailsBottom
  unless inBottom $
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
        -- A hole (lenient mode) stands for a term of the expected type, so it
        -- unifies with anything; accept it instead of falling through to the
        -- dispatch below (which would panic on an unexpected term).
        Just (expected', actual') | isHoleT expected' || isHoleT actual' -> return ()
        Just (expected', actual') ->
          unless (expected' == actual') $ do  -- NOTE: this gives a small, but noticeable speedup
            case actual' of
              RecBottomT{} -> return ()
              RecOrT _ty rs' ->
                case expected' of
                  RecOrT _ty rs -> sequence_ $
                    checkCoherence <$> rs <*> rs'
                  _ -> do
                    forM_ rs' $ \(tope, term) ->
                      localTope tope $
                        unifyTerms expected' term
              _ -> typeOf expected' >>= typeOf >>= \case
                UniverseCubeT{} -> contextEntails (topeEQT expected' actual')
                _ -> do
                  -- A hole stands for a term of the expected type, so a
                  -- unification that would otherwise fail is deferred when either
                  -- side still contains an (unfilled) hole — including one nested
                  -- in a larger term, e.g. @f ?@ checked against an extension-type
                  -- boundary. 'structuralHoleUnify' turns this off, keeping a
                  -- structural mismatch around a hole an error. Lazy: only runs on
                  -- the failure path.
                  defer <- asks deferHoleMismatches
                  let def = unless (expected' == actual') err
                      holePresent = defer && (containsHole expected' || containsHole actual')
                      err
                        | holePresent = return ()
                        | otherwise =
                            case mterm of
                              Nothing   -> issueTypeError (TypeErrorUnifyTerms expected' actual')
                              Just term -> issueTypeError (TypeErrorUnify term expected' actual')
                      errS
                        | holePresent = return ()
                        | otherwise = do
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
                    CubeIT{} -> def
                    CubeI_0T{} -> def
                    CubeI_1T{} -> def
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
                              unifyTerms term actual'

                    TypeFunT _ty _orig cube mtope ret ->
                      case actual' of
                        TypeFunT _ty' orig' cube' mtope' ret' -> do
                          switchVariance $  -- unifying in the negative position!
                            unifyTerms cube cube' -- FIXME: unifyCubes
                          enterScope orig' cube' $ do
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
                                actualEntailsExpected <- [plainTope actualTopeNF] `entailM` expectedTopeNF
                                unless (actualEntailsExpected || containsHole expectedTopeNF || containsHole actualTopeNF) $
                                  issueTypeError (TypeErrorTopeNotSatisfied [actualTopeNF] expectedTopeNF)
                              _ -> do
                                -- this is the case for Π-types and extension types
                                --
                                -- Ξ | Φ | Γ   ⊢   {t : I | φ} → A t   <:   {s : J | ψ} → B s
                                -- when
                                -- Ξ | Φ, ψ ⊢ φ
                                expectedTopeNF <- fromMaybe topeTopT <$> traverse nfT mtope
                                actualTopeNF   <- fromMaybe topeTopT <$> traverse nfT mtope'
                                localTope expectedTopeNF $
                                  contextEntails actualTopeNF
                            case mterm of
                              Nothing -> unifyTerms ret ret'
                              Just term -> unifyTypes (appT ret' (S <$> term) (Pure Z)) ret ret'
                        _ -> err

                    TypeSigmaT _ty _orig a b ->
                      case actual' of
                        TypeSigmaT _ty' orig' a' b' -> do
                          unify Nothing a a'
                          enterScope orig' a' $ unify Nothing b b'
                        _ -> err

                    TypeIdT _ty x _tA y ->
                      case actual' of
                        TypeIdT _ty' x' _tA' y' -> do
                          -- unify Nothing tA tA' -- TODO: do we need this check?
                          unify Nothing x x'
                          unify Nothing y y'
                        _ -> err

                    AppT _ty f x ->
                      case actual' of
                        AppT _ty' f' x' -> do
                          unify Nothing f f'
                          setVariance Invariant $
                            unify Nothing x x'
                        _ -> err

                    LambdaT ty _orig _mparam body ->
                      case stripTypeRestrictions (infoType ty) of
                        TypeFunT _ty _origF param mtope _ret ->
                          case actual' of
                            LambdaT ty' orig' _mparam' body' -> do
                              case stripTypeRestrictions (infoType ty') of
                                TypeFunT _ty' _origF' param' mtope' _ret' -> do
                                  unify Nothing param param' -- we (should) have already checked this in types!
                                  enterScope orig' param $ do
                                    case (mtope, mtope') of
                                      (Just tope, Just tope') -> do
                                        unify Nothing tope tope' -- we (should) have already checked this in types!
                                        localTope tope $ unify Nothing body body'
                                      (Nothing, Nothing) -> do
                                        unify Nothing body body'
                                      _ -> errS
                                _ -> err
                            _ -> err
                        _ -> err

                    LetT{} -> panicImpossible "let at the root of WHNF"
                    LetModT{} -> panicImpossible "let-mod at the root of WHNF"

                    ReflT ty _x | TypeIdT _ty x _tA y <- infoType ty ->
                      case actual' of
                        ReflT ty' _x' | TypeIdT _ty' x' _tA' y' <- infoType ty' -> do
                          -- unify Nothing tA tA' -- TODO: do we need this check?
                          unify Nothing x x'
                          unify Nothing y y'
                        _ -> err
                    ReflT{} -> panicImpossible "refl with a non-identity type!"

                    IdJT _ty a b c d e f ->
                      case actual' of
                        IdJT _ty' a' b' c' d' e' f' -> do
                          unify Nothing a a'
                          unify Nothing b b'
                          unify Nothing c c'
                          unify Nothing d d'
                          unify Nothing e e'
                          unify Nothing f f'
                        _ -> err

                    TypeAscT{} -> panicImpossible "type ascription at the root of WHNF"

                    TypeRestrictedT _ty ty rs ->
                      case actual' of
                        TypeRestrictedT _ty' ty' rs' -> do
                          unify mterm ty ty'
                          sequence_
                            [ localTope tope $ do
                                -- FIXME: can do less entails checks?
                                contextEntails (foldr topeOrT topeBottomT (map fst rs')) -- expected is less specified than actual
                                forM_ rs' $ \(tope', term') -> do
                                  localTope tope' $
                                    unify Nothing term term'
                            | (tope, term) <- rs
                            ]
                        _ -> err    -- FIXME: need better unification for restrictions
                    TypeModalT _ty m ty ->
                      case actual' of
                        TypeModalT _ty' m' ty' -> do
                          when (m' /= m) $ err
                          enterModality m $ unify Nothing ty ty'
                        _ -> err
                    ModAppT _ty m ty ->
                      case actual' of
                        ModAppT _ty' m' ty' -> do
                          when (m' /= m) $ err
                          enterModality m $ unify Nothing ty ty'
                        _ -> err
                    ModExtractT _ty app inn te ->
                      case actual' of
                        ModExtractT _ty' app' inn' te' -> do
                          when (app' /= app) $ err
                          when (inn' /= inn) $ err
                          enterModality app $ unify Nothing te te'
                        _ -> err
                    -- defensive: a hole nested anywhere also defers here rather
                    -- than panicking on an otherwise unexpected shape
                    _ | holePresent -> return ()
                    _ -> panicImpossible "unexpected term in UNIFY"


  where
    action = case mterm of
               Nothing   -> ActionUnifyTerms expected actual
               Just term -> ActionUnify term expected actual

unifyTypes :: Eq var => TermT var -> TermT var -> TermT var -> TypeCheck var ()
unifyTypes = unify . Just

unifyTerms :: Eq var => TermT var -> TermT var -> TypeCheck var ()
unifyTerms = unify Nothing

localTope :: Eq var => TermT var -> TypeCheck var a -> TypeCheck var a
localTope tope tc = do
  Context{..} <- ask
  tope' <- nfTope tope
  let modalTope' = plainTope tope'
  -- A small optimisation to help unify terms faster
  let refine = case tope' of
        TopeEQT _ x y | x == y -> const tc          -- no new information added!
        _ | modalTope' `elem` localTopesNF -> const tc     -- no new information added!
          | otherwise -> id
  refine $ do
    entailsBottom <- (modalTope' : localTopesNF) `entailM` topeBottomT
    local (f modalTope' entailsBottom) tc
  where
    f tope' entailsBottom Context{..} = Context
      { localTopes = plainTope tope : localTopes
      , localTopesNF = tope' : localTopesNF
      , localTopesNFUnion = map nubTermT
          [ new <> old
          | new <- simplifyLHSwithDisjunctions [tope']
          , old <- localTopesNFUnion ]
      , localTopesEntailBottom = Just entailsBottom
      , .. }

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

cubeIT :: TermT var
cubeIT = CubeIT TypeInfo
  { infoType = cubeT
  , infoNF = Just cubeIT
  , infoWHNF = Just cubeIT }

cubeI_0T :: TermT var
cubeI_0T = CubeI_0T TypeInfo
  { infoType = cubeIT
  , infoNF = Just cubeI_0T
  , infoWHNF = Just cubeI_0T }

cubeI_1T :: TermT var
cubeI_1T = CubeI_1T TypeInfo
  { infoType = cubeIT
  , infoNF = Just cubeI_1T
  , infoWHNF = Just cubeI_1T }

cubeFlipT :: TermT var -> TermT var -> TermT var
cubeFlipT cubeTy t = CubeFlipT info t
  where
    info = TypeInfo
      { infoType = typeModalT cubeT Op cubeTy
      , infoNF = Nothing
      , infoWHNF = Nothing
      }

cubeUnflipT :: TermT var -> TermT var -> TermT var
cubeUnflipT cubeTy t = CubeUnflipT info t
  where
    info = TypeInfo
      { infoType = cubeTy
      , infoNF = Nothing
      , infoWHNF = Nothing
      }

topeInvT :: TermT var -> TermT var
topeInvT t = TopeInvT info t
  where
    info = TypeInfo
      { infoType = typeModalT universeT Op topeT
      , infoNF = Nothing
      , infoWHNF = Nothing
      }

topeUninvT :: TermT var -> TermT var
topeUninvT t = TopeUninvT info t
  where
    info = TypeInfo
      { infoType = topeT
      , infoNF = Nothing
      , infoWHNF = Nothing
      }

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
  -> Binder
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


letT :: TermT var -> Binder -> Maybe (TermT var) -> TermT var -> Scope TermT var -> TermT var
letT ty orig mparam val body = t
  where
    t = LetT info orig mparam val body
    info = TypeInfo
      { infoType = ty
      , infoNF = Nothing
      , infoWHNF = Nothing
      }

letModT :: TermT var -> Binder -> TModality -> TModality -> Maybe (TermT var) -> TermT var -> Scope TermT var -> TermT var
letModT ty orig app inn mparam val body = t
  where
    t = LetModT info orig app inn mparam val body
    info = TypeInfo
      { infoType = ty
      , infoNF = Nothing
      , infoWHNF = Nothing
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
  :: Binder
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
  :: Binder
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

typeModalT :: TermT var -> TModality -> TermT var -> TermT var
typeModalT ty md te = t
  where
    t = TypeModalT info md te
    info = TypeInfo
      { infoType = ty
      , infoNF = Nothing
      , infoWHNF = Nothing
      }

modAppT :: TermT var -> TModality -> TermT var -> TermT var
modAppT ty md term = t
  where
    t = ModAppT info md term
    info = TypeInfo
      { infoType = ty
      , infoNF = Nothing
      , infoWHNF = Nothing
      }

modExtractT :: TermT var -> TModality -> TModality -> TermT var -> TermT var
modExtractT ty app inn term = t
  where
    t = ModExtractT info app inn term
    info = TypeInfo
      { infoType = ty
      , infoNF = Nothing
      , infoWHNF = Nothing
      }

typecheck :: Eq var => Term var -> TermT var -> TypeCheck var (TermT var)
typecheck term ty = performing (ActionTypeCheck term ty) $ case term of
  -- A hole is checked against a known type (this is checking position): in
  -- strict mode it is reported as an unsolved hole; in lenient mode its goal
  -- and context are recorded and it is treated as inhabiting the expected type.
  Hole mname -> do
    reject <- asks holesAreErrors
    if reject
      then issueTypeError (TypeErrorUnsolvedHole mname ty)
      else do
        recordHole mname ty
        return (HoleT TypeInfo{ infoType = ty, infoWHNF = Nothing, infoNF = Nothing } mname)

  _ -> whnfT ty >>= \case

    RecBottomT{} -> do
      -- Even under an absurd tope context (where the expected type collapses to
      -- recBOT), the term must still be well-formed in its own right, so that
      -- ill-typed bodies are not silently admitted under a false hypothesis. We
      -- synthesise its type, discard the result, and keep the recBOT elaboration.
      _ <- infer term
      return recBottomT

    TypeRestrictedT _ty ty' rs -> do
      term' <- typecheck term ty'
      -- NOTE: restriction faces need not be contained in the local tope context.
      -- Each face is checked only on its overlap with the context below, so an
      -- overhanging face is harmless (we only hint); a face disjoint from the context
      -- is vacuous, however, and is reported as an error by checkTopeAgainstContext.
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
                      mapM_ checkNameShadowing (binderLeaves orig)
                      enterScope orig cube $ do
                        let tope' = appT topeT (S <$> paramType) (Pure Z)  -- eta expand ty'
                        return (cube, Just tope')
                    _kind -> return (paramType, Nothing)
                unifyTerms param' paramType
                mapM_ checkNameShadowing (binderLeaves orig)
                enterScope orig param' $ do
                  mapM_ (unifyTerms (fromMaybe topeTopT mtope')) mtope
              Just (param, mtope) -> do
                param'' <- typecheck param =<< typeOf param'
                unifyTerms param' param''
                mapM_ checkNameShadowing (binderLeaves orig)
                enterScope orig param' $ do
                  mtope'' <- typecheck (fromMaybe TopeTop mtope) topeT
                  unifyTerms (fromMaybe topeTopT mtope') mtope''

            mapM_ checkNameShadowing (binderLeaves orig)
            enterScope orig param' $ do
              maybe id localTope mtope' $ do
                body' <- typecheck body ret
                return (lambdaT ty' orig (Just (param', mtope')) body')

          _ -> issueTypeError $ TypeErrorUnexpectedLambda term ty
      Let orig annot val body -> do
        val' <- performing (ActionCheckLetValue (binderName orig)) $ case annot of
          Nothing -> infer val
          Just bindType -> do
            bindType' <- typecheck bindType universeT
            typecheck val bindType'
        bindTy <- typeOf val'
        body' <- enterScopeWithBind orig Id bindTy val' $ do
          typecheck body (S <$> ty')
        return (letT ty' orig (Just bindTy) val' body')
      LetMod orig app inn annot val body  -> do
        val' <- performing (ActionCheckLetValue (binderName orig)) $ case annot of
          Nothing -> enterModality app $ infer val
          Just bindType -> do
            bindType' <- infer bindType
            bindUniv <- typeOf bindType'
            enterModality app $ typecheck val (typeModalT bindUniv inn bindType')
        bindTy <- typeOf val' >>= \case
          o@(TypeModalT _ty md t) ->
            if md == inn then
              return t
            else
              issueTypeError $ TypeErrorNotModal (untyped o) inn val'
          o -> issueTypeError $ TypeErrorNotModal (untyped o) inn val'
        bindVal <- whnfT val' >>= \case
          ModAppT _ty _m t -> pure t
          o -> pure (modExtractT bindTy app inn o)
        body' <- enterScopeWithBind orig (comp app inn) bindTy bindVal $ do
          typecheck body (S <$> ty')
        return (letModT ty' orig app inn (Just bindTy) val' body')
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
              unifyTerms x' y >> unifyTerms y x'
              unifyTerms x' z >> unifyTerms z x'
            when (isNothing mx) $
              unifyTerms y z >> unifyTerms z y
            return (reflT ty' (Just (y, Just tA)))
          _ -> issueTypeError $ TypeErrorUnexpectedRefl term ty
      ModExtract{} -> panicImpossible "extract is an internal term and cannot be typechecked"
      ModApp md body -> case ty' of
        TypeModalT _ty md' tpe -> do
            when (md /= md') $ issueTypeError $
              TypeErrorModalityMismatch md' md term
            body' <- enterModality md $ typecheck body tpe
            return $ modAppT ty' md body'
        _ -> issueTypeError $ TypeErrorNotModal term md ty'

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
  Hole _mname -> issueTypeError (TypeErrorCannotInferHole tt)
  Pure x -> do
    topLevel <- isTopLevelVar x
    unless topLevel $ do
      varMod <- modalityOfVar x
      locks <- locksOfVar x
      when (not (coe varMod locks)) $ issueTypeError $ TypeErrorUnaccessibleVar x varMod locks
    pure (Pure x)

  Universe     -> pure universeT
  UniverseCube -> pure cubeT
  UniverseTope -> pure topeT

  CubeUnit      -> pure cubeUnitT
  CubeUnitStar  -> pure cubeUnitStarT

  Cube2 -> pure cube2T
  Cube2_0 -> pure cube2_0T
  Cube2_1 -> pure cube2_1T

  CubeI -> pure cubeIT
  CubeI_0 -> pure cubeI_0T
  CubeI_1 -> pure cubeI_1T
  CubeProduct l r -> do
    l' <- typecheck l cubeT
    r' <- typecheck r cubeT
    return (cubeProductT l' r')

  CubeFlip t -> do
    t' <- infer t
    typeOf t' >>= \case
      CubeIT{} -> pure $ cubeFlipT cubeIT t'
      Cube2T{} -> pure $ cubeFlipT cube2T t'
      ty -> do
        tyStr <- ppTermInContext ty
        issueTypeError $ TypeErrorOther $
          "flip expects an interval cube (2 or 𝕀); got " <> tyStr
  CubeUnflip t -> do
    t' <- infer t
    typeOf t' >>= \case
      CubeIT{} -> pure $ cubeUnflipT cubeIT t'
      Cube2T{} -> pure $ cubeUnflipT cube2T t'
      ty -> do
        tyStr <- ppTermInContext ty
        issueTypeError $ TypeErrorOther $
          "unflip expects an interval cube (2 or 𝕀); got " <> tyStr
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
        return (pairT (typeSigmaT (BinderVar Nothing) lt (S <$> rt)) l' r')

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
    l' <- inferAs cubeT l
    r' <- inferAs cubeT r
    lTy <- typeOf l'
    rTy <- typeOf r'
    case (lTy, rTy) of
      (Cube2T{}, Cube2T{}) -> return (topeLEQT l' r')
      (CubeIT{}, CubeIT{}) -> return (topeLEQT l' r')
      (CubeIT{}, Cube2T{}) -> do
        r'' <- typecheck r cubeIT
        return (topeLEQT l' r'')
      (Cube2T{}, CubeIT{}) -> do
        l'' <- typecheck l cubeIT
        return (topeLEQT l'' r')
      _ -> do
        lStr <- ppTermInContext lTy
        rStr <- ppTermInContext rTy
        issueTypeError $ TypeErrorOther $
          "the (t ≤ s) tope expects points in interval cubes (2 or 𝕀); got "
            <> lStr <> " and " <> rStr

  TopeAnd l r -> do
    l' <- typecheck l topeT
    r' <- typecheck r topeT
    return (topeAndT l' r')

  TopeOr l r -> do
    l' <- typecheck l topeT
    r' <- typecheck r topeT
    return (topeOrT l' r')

  TopeInv t -> do
    t' <- typecheck t topeT
    return (topeInvT t')

  TopeUninv t -> do
    t' <- typecheck t (typeModalT universeT Op topeT )
    return (topeUninvT t')

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
      -- NOTE: branch guards need not be contained in the context. recOR requires
      -- only coverage (context |- OR(guards)), enforced by contextEntailsUnion below;
      -- a guard may overhang the context (e.g. when splitting with a named shape).
      -- checkTopeAgainstContext warns on overhang and errors only if the guard is
      -- disjoint from the context (a vacuous branch).
      checkTopeAgainstContext "recOR branch guard" tope'
      localTope tope' $ do
        term' <- inferAs universeT term
        ty <- typeOf term'
        return (tope', (term', ty))
    let rs' = map (fmap fst) ttts
        ts  = map (fmap snd) ttts
    sequence_ [ checkCoherence l r | l:rs'' <- tails rs', r <- rs'' ]
    contextEntailsUnion (map fst ttts)
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
            mapM_ checkNameShadowing (binderLeaves orig)
            b' <- enterScope orig a' $ typecheck b universeT
            return (typeFunT orig a' Nothing b')
      -- an argument can be a cube
      UniverseCubeT{} -> do
        mapM_ checkNameShadowing (binderLeaves orig)
        b' <- enterScope orig a' $ typecheck b universeT
        return (typeFunT orig a' Nothing b')
      -- an argument can be a shape
      TypeFunT _ty _orig cube mtope UniverseTopeT{} -> do
        mapM_ checkNameShadowing (binderLeaves orig)
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
    mapM_ checkNameShadowing (binderLeaves orig)
    enterScope orig cube' $ do
      tope' <- typecheck tope topeT
      localTope tope' $ do
        ret' <- typecheck ret universeT
        return (typeFunT orig cube' (Just tope') ret')

  TypeSigma orig a b -> do
    a' <- typecheck a universeT
    mapM_ checkNameShadowing (binderLeaves orig)
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
      TypeFunT _ty orig a mtope b -> do
        -- A hole argument to a shape-restricted function carries the shape as
        -- its goal: record (binder : a | tope) rather than just the cube a.
        x' <- case (x, mtope) of
          (Hole mname, Just tope) -> checkHoleAgainstShape mname orig a tope
          _                       -> typecheck x a
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
        mapM_ checkNameShadowing (binderLeaves orig)
        enterScope orig cube $ do
          let tope' = appT topeT (S <$> ty') (Pure Z)  -- eta expand ty'
          return (Just tope')
      kind -> issueTypeError $ TypeErrorInvalidArgumentType ty kind
    mapM_ checkNameShadowing (binderLeaves orig)
    enterScope orig ty' $ do
      maybe id localTope mtope $ do
        body' <- infer body
        ret <- typeOf body'
        return (lambdaT (typeFunT orig ty' mtope ret) orig (Just (ty', mtope)) body')
  Lambda orig (Just (cube, Just tope)) body -> do
    cube' <- typecheck cube cubeT
    mapM_ checkNameShadowing (binderLeaves orig)
    enterScope orig cube' $ do
      tope' <- infer tope
      body' <- localTope tope' $ infer body
      ret <- typeOf body'
      return (lambdaT (typeFunT orig cube' (Just tope') ret) orig (Just (cube', Just tope')) body')
  Let orig annot val body -> do
    val' <- performing (ActionCheckLetValue (binderName orig)) $ case annot of
      Nothing -> infer val
      Just ty -> do
        bindTy <- typecheck ty universeT
        typecheck val bindTy
    bindTy <- typeOf val'
    enterScopeWithBind orig Id bindTy val' $ do
      body' <- infer body
      ret <- typeOf body'
      return (letT (substituteT val' ret) orig (Just bindTy) val' body')
  LetMod orig app inn annot val body -> do
    val' <- performing (ActionCheckLetValue (binderName orig)) $ case annot of
      Nothing -> enterModality app $ infer val
      Just bindType -> do
        bindType' <- infer bindType
        bindUniv <- typeOf bindType'
        enterModality app $ typecheck val (typeModalT bindUniv inn bindType')
    bindTy <- typeOf val' >>= \case
      o@(TypeModalT _ty md t) ->
        if md == inn then
          return t
        else
          issueTypeError $ TypeErrorNotModal (untyped o) inn val'
      o -> issueTypeError $ TypeErrorNotModal (untyped o) inn val'
    bindVal <- whnfT val' >>= \case
      ModAppT _ty _m t -> pure t
      o -> pure (modExtractT bindTy app inn o)
    enterScopeWithBind orig (comp app inn) bindTy bindVal $ do
      body' <- infer body
      ret <- typeOf body'
      return (letModT (substituteT val' ret) orig app inn (Just bindTy) val' body')
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
          typeFunT (BinderVar Nothing) tA' Nothing $
            typeFunT (BinderVar Nothing) (typeIdT (S <$> a') (Just (S <$> tA')) (Pure Z)) Nothing $
              universeT
    tC' <- typecheck tC typeOf_C
    let typeOf_d =
          appT universeT
            (appT (typeFunT (BinderVar Nothing) (typeIdT a' (Just tA') a') Nothing universeT)
              tC' a')
            (reflT (typeIdT a' (Just tA') a') Nothing)
    d' <- typecheck d typeOf_d
    x' <- typecheck x tA'
    p' <- typecheck p (typeIdT a' (Just tA') x')
    let ret =
          appT universeT
            (appT (typeFunT (BinderVar Nothing) (typeIdT a' (Just tA') x') Nothing universeT)
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
  TypeModal md ty -> do
    ty' <- enterModality md $ infer ty
    universeTy <- typeOf ty'
    _ <- case universeTy of
      UniverseT {}     -> pure universeTy
      UniverseCubeT {} -> pure universeTy
      UniverseTopeT {} -> pure universeTy
      _                -> issueTypeError $ TypeErrorNotTypeInModal universeTy
    return (typeModalT universeTy md ty')
  ModApp md term -> do
    term' <- enterModality md $ infer term
    ty <- typeOf term'
    tyUniv <- typeOf ty
    return $ modAppT (typeModalT tyUniv md ty) md term'
  ModExtract _ _ _ -> error "untypable $extract$"

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
        . map (map tTope)
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
    TypeFunT{} | null xs -> enterScope (BinderVar (Just "_")) t' $ do
      renderTermSVGFor "blue" 0 (Nothing, []) (Pure Z)  -- use blue for types

    _ -> case t' of -- check evaluated term
      AppT _info f x -> typeOf f >>= \case
        TypeFunT _ fOrig fArg mtopeArg ret | Just dim <- dimOf fArg, dim <= maxDim -> do
          enterScope fOrig fArg $ do
            maybe id localTope mtopeArg $ do
              Just <$> renderForSubShapeSVG mainColor dim (map S xs) Z ret (S <$> f) (S <$> x)  -- FIXME: breaks for 2 * (2 * 2), but works for 2 * 2 * 2 = (2 * 2) * 2
        _ -> traverse (\(p', _) -> renderForSVG mainColor accDim p' t') mp
      TypeFunT{} | null xs -> enterScope (BinderVar (Just "_")) t' $ do
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
