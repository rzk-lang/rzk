{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Declarations, sections, commands, and the public entry points.
--
-- A top-level entry is a name bound in the outermost scope, so the scope /grows/
-- as a module is checked: each @#define@, @#postulate@ and @#assume@ enters a
-- binder and the rest of the module is checked under it. The driver is therefore
-- written in continuation-passing style, and the result is packaged with the scope
-- it was produced in ('Checked').
--
-- That also gives sections their shape. A @#assume@d assumption is an ordinary
-- binder, and closing the section abstracts it out of the definitions that used it
-- (@makeAssumptionExplicit@), rewriting the later definitions to apply them to it.
module Rzk.TypeCheck.Decl where

import           Control.Monad             (forM, unless, when)
import           Control.Monad.Except      (catchError, runExcept)
import           Control.Monad.Reader      (ask, asks, local, runReaderT)
import           Control.Monad.Writer      (runWriterT)
import           Data.List                 (intercalate)
import qualified Data.Map                  as Map
import           Debug.Trace               (trace)

import           Control.Monad.Foil        (DExt, Distinct, NameBinder)
import qualified Control.Monad.Foil        as Foil
import           Control.Monad.Free.Foil   (AST (Var), ScopedAST (..))

import           Language.Rzk.Foil.Convert (Env, toTerm)
import           Language.Rzk.Foil.Syntax
import           Language.Rzk.Foil.Names  (Binder (..), TModality (..),
                                            VarIdent, patternToTerm, varIdentAt)
import qualified Language.Rzk.Syntax       as Rzk
import           Rzk.TypeCheck.Context
import           Rzk.TypeCheck.Display
import           Rzk.TypeCheck.Error
import           Rzk.TypeCheck.Eval
import           Rzk.TypeCheck.Judgements
import           Rzk.TypeCheck.Monad
import           Rzk.TypeCheck.Render

-- * Declarations

-- FIXME: merge with VarInfo
data Decl n = Decl
  { declName         :: VarIdent
    -- ^ the surface name, as the user wrote it (with its source position)
  , declNameOf       :: Foil.Name n
    -- ^ the name it is bound to in the top-level scope
  , declType         :: TermT n
  , declValue        :: Maybe (TermT n)
  , declIsAssumption :: Bool
  , declUsedVars     :: [Foil.Name n]
  , declLocation     :: Maybe LocationInfo
  }

-- | A declaration sinks along a scope extension, by coercion (see the note in
-- "Rzk.TypeCheck.Context").
sinkDecl :: DExt n l => Decl n -> Decl l
sinkDecl decl = decl
  { declNameOf = Foil.sink (declNameOf decl)
  , declType = Foil.sink (declType decl)
  , declValue = Foil.sink <$> declValue decl
  , declUsedVars = Foil.sink <$> declUsedVars decl
  }

-- | What a run of the checker produced: the top-level scope, the declarations in
-- it, and the errors found.
--
-- The scope is existential, and the declarations live in it. Anything that wants
-- to /resume/ from a checked prefix (the LSP's incremental path) keeps the whole
-- package and carries on from that context; anything that only displays them reads
-- them through the context's naming.
data Checked where
  Checked
    :: Distinct n
    => Context n
    -> [(FilePath, [Decl n])]
    -> [TypeErrorInScopedContext]
    -> Checked

-- * Entering a top-level entry

-- | Bind a top-level entry and run the rest of the module under it.
withTopLevel
  :: Distinct n
  => VarIdent            -- ^ the surface name
  -> TermT n             -- ^ its type
  -> Maybe (TermT n)     -- ^ its value, for a definition
  -> Bool                -- ^ is it an assumption (a @#assume@)?
  -> [Foil.Name n]       -- ^ the variables it declared it uses
  -> (forall l. (DExt n l, Distinct l) => NameBinder n l -> Decl l -> TypeCheck l r)
  -> TypeCheck n r
withTopLevel name ty mval isAssumption usedVars k = do
  checkTopLevelDuplicate name
  ctx <- ask
  Foil.withFresh (ctxScope ctx) $ \binder -> do
    let info = VarInfo
          { varType = ty
          , varValue = mval
          , varModality = Id
          , varModAccum = Id
          , varOrig = BinderVar (Just name)
          , varIsAssumption = isAssumption
          , varIsTopLevel = True
          , varDeclaredAssumptions = usedVars
          , varLocation = ctxLocation ctx
          }
        ctx' = recordInSection (Foil.nameOf binder) (enterBinder binder info [] ctx)
        decl = Decl
          { declName = name
          , declNameOf = Foil.nameOf binder
          , declType = Foil.sink ty
          , declValue = Foil.sink <$> mval
          , declIsAssumption = isAssumption
          , declUsedVars = Foil.sink <$> usedVars
          , declLocation = ctxLocation ctx
          }
    inContext ctx' (k binder decl)

-- | Record a new entry in the innermost open section.
recordInSection :: Foil.Name n -> Context n -> Context n
recordInSection name ctx = ctx
  { ctxSections = case ctxSections ctx of
      []       -> [SectionInfo Nothing [name]]
      s : rest -> s { sectionEntries = name : sectionEntries s } : rest
  }

-- * Sections

startSection :: Maybe Rzk.SectionName -> TypeCheck n a -> TypeCheck n a
startSection name = local $ \ctx -> ctx
  { ctxSections = SectionInfo name [] : ctxSections ctx }

-- | Close a section: abstract each of its assumptions out of the definitions that
-- used it, report the ones that went unused, and take the assumptions back out of
-- scope.
--
-- The definitions stay in scope, with their entries rewritten to take the
-- assumption as an explicit parameter. The assumptions' names stay in the /scope
-- index/ — a scope only ever grows — but they are removed from the surface-name map
-- and from the list of what is in scope, so they can no longer be referred to,
-- shown, or shadowed.
endSection
  :: Distinct n
  => [TypeErrorInScopedContext]
  -> TypeCheck n ([Decl n], [TypeErrorInScopedContext], Context n)
endSection errs = do
  ctx <- ask
  let entries = case ctxSections ctx of
        []    -> []
        s : _ -> sectionEntries s          -- newest first
      infos = [ (name, lookupVarInfo name ctx) | name <- entries ]

  -- In lenient (hole-checking) mode an as-yet-unfilled hole may still come to use a
  -- declared variable, so we tolerate the unused-variable diagnostics wherever such
  -- a hole is present anywhere in the section. This covers both an unused section
  -- assumption and an unused 'uses' variable, and crucially a hole-free definition
  -- whose body refers to an in-progress (hole-bearing) one: its 'uses' reads as
  -- unused only because the referenced definition is incomplete. Strict mode (the
  -- default, and CI) keeps reporting both.
  lenient <- not <$> asks ctxHolesAreErrors
  let sectionHasHole = any (maybe False containsHole . varValue . snd) infos
      tolerateUnused = lenient && sectionHasHole

  (kept, errs') <- collectSectionDecls tolerateUnused errs [] infos

  loc <- asks ctxLocation
  let decls = map (toDecl loc) kept
      assumptions = [ name | (name, info) <- infos, varIsAssumption info ]
      isAssumption v = any (sameName v) assumptions
      -- the definitions of the section are now abstracted over its assumptions
      ctx' = foldr (uncurry insertVarInfo) ctx
        { ctxSections = closeInnermost (ctxSections ctx)
        , ctxBound = filter (not . isAssumption) (ctxBound ctx)
        , ctxNamed = Map.filter (not . isAssumption) (ctxNamed ctx)
        } kept

      -- The section's definitions outlive it, so the enclosing section adopts
      -- them: closing /that/ one must see them too, and abstract them over its own
      -- assumptions in turn.
      closeInnermost sections = case drop 1 sections of
        []       -> []
        s : rest -> s { sectionEntries = reverse (map fst kept) <> sectionEntries s } : rest

  -- only issue unused-variable errors if there were none before in the section
  unusedErrors <- fmap concat $ forM decls $ \decl -> do
    let unusedUsedVars = [ v | v <- declUsedVars decl, isAssumption v ]
    if null errs && not (null unusedUsedVars) && not tolerateUnused
      then local (\c -> c { ctxLocation = declLocation decl }) $ do
        err <- typeErrorHere (TypeErrorUnusedUsedVariables unusedUsedVars (declNameOf decl))
        return [err]
      else return []

  pure (decls, errs' <> unusedErrors, ctx')
  where
    sameName a b = Foil.nameId a == Foil.nameId b

    toDecl loc (name, info) = Decl
      { declName = case varOrig info of
          BinderVar (Just x) -> x
          _                  -> "_"
      , declNameOf = name
      , declType = varType info
      , declValue = varValue info
      , declIsAssumption = varIsAssumption info
      , declUsedVars = varDeclaredAssumptions info
      , declLocation = loc
      }

-- | An error, captured in the current context (rather than thrown).
typeErrorHere :: Distinct n => TypeError n -> TypeCheck n TypeErrorInScopedContext
typeErrorHere err = do
  ctx <- ask
  pure (TypeErrorInScopedContext ctx err)

-- | Turn the section's entries into declarations, abstracting each assumption out
-- of the definitions that follow it.
--
-- The entries come newest first, so the definitions accumulated in @recent@ are
-- exactly those that could have used the assumption currently being processed.
collectSectionDecls
  :: Distinct n
  => Bool                                -- ^ tolerate unused variables
  -> [TypeErrorInScopedContext]
  -> [(Foil.Name n, VarInfo n)]          -- ^ the definitions seen so far (oldest last)
  -> [(Foil.Name n, VarInfo n)]          -- ^ the entries still to process (newest first)
  -> TypeCheck n ([(Foil.Name n, VarInfo n)], [TypeErrorInScopedContext])
collectSectionDecls _tolerate errs recent [] = pure (recent, errs)
collectSectionDecls tolerate errs recent (entry@(name, info) : rest)
  | varIsAssumption info = do
      (used, recent') <- makeAssumptionExplicit entry recent
      unusedErr <-
        if null errs && not used && not tolerate
          then local (\c -> c { ctxLocation = varLocation info }) $
            pure <$> typeErrorHere (TypeErrorUnusedVariable name (varType info))
          else return []
      collectSectionDecls tolerate (errs <> unusedErr) recent' rest
  | otherwise =
      collectSectionDecls tolerate errs (entry : recent) rest

-- | Abstract one assumption out of the definitions that come after it.
--
-- A definition that mentions the assumption gains it as an explicit parameter, and
-- every later definition that mentions /that/ definition is rewritten to apply it
-- to the assumption. A definition that mentions it without declaring it in its
-- @uses@ clause is an implicit assumption, and an error.
makeAssumptionExplicit
  :: Distinct n
  => (Foil.Name n, VarInfo n)
  -> [(Foil.Name n, VarInfo n)]
  -> TypeCheck n (Bool, [(Foil.Name n, VarInfo n)])
makeAssumptionExplicit _ [] = pure (False {- UNUSED -}, [])
makeAssumptionExplicit assumption@(a, aInfo) ((x, xInfo) : xs) = do
  scope <- asks ctxScope
  let usesA t = any (sameName a) (freeVarsOfTermT t)
      inType = usesA (varType xInfo)
      inBody = any usesA (varValue xInfo)
      hasAssumption = inType || inBody
      declared = any (sameName a) (varDeclaredAssumptions xInfo)
  if hasAssumption
    then do
      unless declared $
        issueTypeError $ TypeErrorImplicitAssumption (a, varType aInfo) x
      let xInfo' = abstractOver scope a aInfo xInfo
          xs' = map (fmap (applyToAssumption scope a (x, xInfo'))) xs
      (_used, xs'') <- makeAssumptionExplicit assumption xs'
      return (True {- USED -}, (x, xInfo') : xs'')
    else do
      (used, xs'') <- makeAssumptionExplicit assumption xs
      return (used, (x, xInfo) : xs'')
  where
    sameName p q = Foil.nameId p == Foil.nameId q

-- | Give an entry the assumption as an explicit parameter.
abstractOver
  :: Distinct n
  => Foil.Scope n -> Foil.Name n -> VarInfo n -> VarInfo n -> VarInfo n
abstractOver scope a aInfo info = info
  { varType = newType
  , varValue = fmap abstractValue (varValue info)
  , varModality = Id
  , varModAccum = Id
  , varDeclaredAssumptions =
      filter (\v -> Foil.nameId v /= Foil.nameId a) (varDeclaredAssumptions info)
  }
  where
    orig = varOrig aInfo
    newType =
      abstractName scope a (varType info) $ \binder body ->
        typeFunT orig Id (varType aInfo) Nothing (ScopedAST binder body)
    abstractValue value =
      abstractName scope a value $ \binder body ->
        lambdaT newType orig Nothing (ScopedAST binder body)

-- | Rewrite every use of a definition into an application of it to the assumption
-- it has just been abstracted over.
applyToAssumption
  :: Distinct n
  => Foil.Scope n -> Foil.Name n -> (Foil.Name n, VarInfo n) -> VarInfo n -> VarInfo n
applyToAssumption scope a (defName, defInfo) info
  | varIsAssumption info = info
  | otherwise = info
      { varType = rewrite (varType info)
      , varValue = rewrite <$> varValue info
      }
  where
    applied = appT (varType defInfo) (Var defName) (Var a)
    rewrite = substituteName scope defName applied

-- * Commands

countCommands :: Integral a => [Rzk.Command] -> a
countCommands = fromIntegral . length

setOption :: Distinct n => String -> String -> TypeCheck n a -> TypeCheck n a
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
-- Render the shape only, hiding the proof term (drop the <title> everywhere and
-- blank interior labels), so a worked term can be shown as the cell it builds
-- without giving the term away.
setOption "render-hide-term" = \case
  "yes" -> localHideTerm True
  "no"  -> localHideTerm False
  _ -> const $
    issueTypeError $ TypeErrorOther "unknown value for \"render-hide-term\" (use \"yes\" or \"no\")"
setOption optionName = const $ const $
  issueTypeError $ TypeErrorOther ("unknown option " <> show optionName)

unsetOption :: Distinct n => String -> TypeCheck n a -> TypeCheck n a
unsetOption "verbosity" = localVerbosity (ctxVerbosity emptyContext)
unsetOption "render" = localRenderBackend (ctxRenderBackend emptyContext)
unsetOption "render-hide-term" = localHideTerm (ctxRenderHideTerm emptyContext)
unsetOption optionName = const $
  issueTypeError $ TypeErrorOther ("unknown option " <> show optionName)

paramToParamDecl :: Distinct n => Rzk.Param -> TypeCheck n [Rzk.ParamDecl]
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

withCommand :: Rzk.Command -> TypeCheck n a -> TypeCheck n a
withCommand command = local $ \ctx -> ctx { ctxCurrentCommand = Just command }

-- | Elaborate a surface term in the current top-level scope: a free identifier
-- resolves to the top-level entry it names.
elaborate :: forall n. Distinct n => Rzk.Term -> TypeCheck n (Term n)
elaborate term = do
  ctx <- ask
  let env :: Env n
      env name = case lookupNamed name ctx of
        Just v  -> Var v
        Nothing -> panicImpossible ("undefined variable: " <> show name)
  pure (toTerm (ctxScope ctx) env term)

-- | Is a surface identifier defined at the top level?
checkDefined :: Distinct n => VarIdent -> TypeCheck n (Foil.Name n)
checkDefined name = asks (lookupNamed name) >>= \case
  Just v  -> pure v
  Nothing -> issueTypeError (TypeErrorUndefined name)

splitSectionCommands
  :: Distinct n
  => Rzk.SectionName -> [Rzk.Command] -> TypeCheck n ([Rzk.Command], [Rzk.Command])
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

-- * The module driver

-- | Check a module's commands, extending the scope with each definition.
--
-- The continuation runs in the /final/ scope, with the declarations the module
-- produced (sunk into it) and the errors found.
checkCommands
  :: forall n r. Distinct n
  => Maybe FilePath -> Integer -> Integer -> [Rzk.Command]
  -> (forall l. (DExt n l, Distinct l)
        => [Decl l] -> [TypeErrorInScopedContext] -> TypeCheck l r)
  -> TypeCheck n r
checkCommands path i total commands k = case commands of
  [] -> k [] []

  command@(Rzk.CommandUnsetOption _loc optionName) : more ->
    announce ("Unsetting option " <> optionName) $
      withCommand command $
        unsetOption optionName $
          checkCommands path (i + 1) total more k

  command@(Rzk.CommandSetOption _loc optionName optionValue) : more ->
    announce ("Setting option " <> optionName <> " = " <> optionValue) $
      withCommand command $
        setOption optionName optionValue $
          checkCommands path (i + 1) total more k

  command@(Rzk.CommandDefine _loc name (Rzk.DeclUsedVars _ vars) params ty term) : more ->
    announce (" Checking #define " <> Rzk.printTree name) $
      withCommand command $ do
        used <- mapM (checkDefined . varIdentAt path) vars
        paramDecls <- concat <$> mapM paramToParamDecl params
        -- Store the elaborated type and term unreduced, but memoise their WHNF on
        -- the top node. Reducing in place would discard or expose a variable
        -- occurrence, so the section unused/implicit-assumption checks (run over the
        -- stored type and value) would disagree with the term the user wrote;
        -- keeping the WHNF cached preserves the original one-shot reduction.
        tyTerm <- elaborate (addParamDecls paramDecls ty)
        ty' <- memoizeWHNF =<< typecheck tyTerm universeT
        valTerm <- elaborate (addParams params term)
        term' <- memoizeWHNF =<< typecheck valTerm ty'
        withTopLevel (varIdentAt path name) ty' (Just term') False used $ \binder decl -> do
          backend <- asks ctxRenderBackend
          termSVG <- case backend of
            Just RenderSVG   -> renderTermSVG (Var (Foil.nameOf binder))
            Just RenderLaTeX -> issueTypeError $
              TypeErrorOther "\"latex\" rendering is not yet supported"
            Nothing          -> pure Nothing
          maybe id trace termSVG $
            checkCommands path (i + 1) total more $ \decls errs ->
              k (sinkDecl decl : decls) errs

  command@(Rzk.CommandPostulate _loc name (Rzk.DeclUsedVars _ vars) params ty) : more ->
    announce (" Checking #postulate " <> Rzk.printTree name) $
      withCommand command $ do
        used <- mapM (checkDefined . varIdentAt path) vars
        paramDecls <- concat <$> mapM paramToParamDecl params
        tyTerm <- elaborate (addParamDecls paramDecls ty)
        ty' <- memoizeWHNF =<< typecheck tyTerm universeT
        withTopLevel (varIdentAt path name) ty' Nothing False used $ \_binder decl ->
          checkCommands path (i + 1) total more $ \decls errs ->
            k (sinkDecl decl : decls) errs

  command@(Rzk.CommandAssume _loc names ty) : more ->
    announce (" Checking #assume "
        <> intercalate " " [ Rzk.printTree name | name <- names ]) $
      withCommand command $ do
        tyTerm <- elaborate ty
        ty' <- typecheck tyTerm universeT
        assume (map (varIdentAt path) names) ty' $ \assumed ->
          checkCommands path (i + 1) total more $ \decls errs ->
            k (map sinkDecl assumed <> decls) errs

  command@(Rzk.CommandCheck _loc term ty) : more ->
    announce (" Checking " <> Rzk.printTree term <> " : " <> Rzk.printTree ty) $
      withCommand command $ do
        tyTerm <- elaborate ty
        ty' <- typecheck tyTerm universeT >>= whnfT
        termTerm <- elaborate term
        _term' <- typecheck termTerm ty'
        checkCommands path (i + 1) total more k

  Rzk.CommandCompute loc term : more ->
    checkCommands path i total (Rzk.CommandComputeWHNF loc term : more) k

  command@(Rzk.CommandComputeNF _loc term) : more ->
    announce (" Computing NF for " <> Rzk.printTree term) $
      withCommand command $ do
        term' <- elaborate term >>= infer >>= nfT
        shown <- ppInContext term'
        traceTypeCheck Normal ("  " <> shown) $
          checkCommands path (i + 1) total more k

  command@(Rzk.CommandComputeWHNF _loc term) : more ->
    announce (" Computing WHNF for " <> Rzk.printTree term) $
      withCommand command $ do
        term' <- elaborate term >>= infer >>= whnfT
        shown <- ppInContext term'
        traceTypeCheck Normal ("  " <> shown) $
          checkCommands path (i + 1) total more k

  command@(Rzk.CommandSection _loc name) : more ->
    withCommand command $ do
      (sectionCommands, more') <- splitSectionCommands name more
      withSection (Just name) i sectionCommands path total $ \sectionDecls sectionErrs ->
        if null sectionErrs
          then checkCommands path (i + countCommands sectionCommands) total more' $
            \decls errs -> k (map sinkDecl sectionDecls <> decls) errs
          else k sectionDecls sectionErrs

  command@(Rzk.CommandSectionEnd _loc endName) : _more ->
    withCommand command $
      issueTypeError $ TypeErrorOther $
        "unexpected #end " <> Rzk.printTree endName <> ", no section was declared!"
  where
    announce :: String -> TypeCheck n a -> TypeCheck n a
    announce what =
      traceTypeCheck Normal
        ("[ " <> show i <> " out of " <> show total <> " ]" <> what)

-- | Assume a list of names of the same type, each a top-level entry.
assume
  :: Distinct n
  => [VarIdent] -> TermT n
  -> (forall l. (DExt n l, Distinct l) => [Decl l] -> TypeCheck l r)
  -> TypeCheck n r
assume [] _ty k = k []
assume (name : names) ty k =
  withTopLevel name ty Nothing True [] $ \_binder decl ->
    assume names (Foil.sink ty) $ \decls ->
      k (sinkDecl decl : decls)

-- | Check the commands of a section, then close it.
withSection
  :: forall n r. Distinct n
  => Maybe Rzk.SectionName -> Integer -> [Rzk.Command] -> Maybe FilePath -> Integer
  -> (forall l. (DExt n l, Distinct l)
        => [Decl l] -> [TypeErrorInScopedContext] -> TypeCheck l r)
  -> TypeCheck n r
withSection name i sectionCommands path total k =
  startSection name $
    checkCommands path i total sectionCommands $ \_decls errs ->
      performing (ActionCloseSection name) $ do
        result <- (Right <$> endSection errs) `catchError` (return . Left)
        case result of
          Left err -> k [] (errs <> [err])
          Right (decls', errs', ctx') -> inContext ctx' (k decls' errs')

-- | Check one module.
checkModule
  :: forall n r. Distinct n
  => Maybe FilePath -> Rzk.Module
  -> (forall l. (DExt n l, Distinct l)
        => [Decl l] -> [TypeErrorInScopedContext] -> TypeCheck l r)
  -> TypeCheck n r
checkModule path (Rzk.Module _moduleLoc _lang commands) k =
  -- FIXME: use the module name? or an anonymous section?
  withSection Nothing 1 commands path (countCommands commands) k

checkModuleWithLocation
  :: Distinct n
  => (FilePath, Rzk.Module)
  -> (forall l. (DExt n l, Distinct l)
        => [Decl l] -> [TypeErrorInScopedContext] -> TypeCheck l r)
  -> TypeCheck n r
checkModuleWithLocation (path, module_) k =
  traceTypeCheck Normal ("Checking module from " <> path) $
    withLocation (LocationInfo { locationFilePath = Just path, locationLine = Nothing }) $
      checkModule (Just path) module_ k

-- | Check a list of modules, one after another, in a scope that grows as it goes.
--
-- Checking stops at the first module with an error, as it did before.
checkModules
  :: forall n r. Distinct n
  => [(FilePath, Rzk.Module)]
  -> (forall l. (DExt n l, Distinct l)
        => [(FilePath, [Decl l])] -> [TypeErrorInScopedContext] -> TypeCheck l r)
  -> TypeCheck n r
checkModules [] k = k [] []
checkModules (m@(path, _) : ms) k =
  checkModuleWithLocation m $ \decls errs ->
    case errs of
      _:_ -> k [(path, decls)] errs
      _ -> checkModules ms $ \rest errors ->
        k ((path, map sinkDecl decls) : rest) errors

-- * The public entry points

-- | Check the modules, and package the result with the scope it was checked in.
checkedModules :: [(FilePath, Rzk.Module)] -> Context Foil.VoidS -> Either TypeErrorInScopedContext (Checked, [HoleInfo])
checkedModules modules ctx =
  runExcept $ runWriterT $ flip runReaderT ctx $
    checkModules modules $ \decls errs -> do
      ctx' <- ask
      pure (Checked ctx' decls errs)

-- | Check the modules strictly: an unfilled hole is an error, and the first error
-- stops the run.
typecheckModules
  :: [(FilePath, Rzk.Module)] -> Either TypeErrorInScopedContext Checked
typecheckModules modules = do
  (checked@(Checked _ _ errs), _holes) <- checkedModules modules emptyContext
  case errs of
    err : _ -> Left err
    []      -> Right checked

-- | Check the modules in lenient hole mode, returning the holes recorded (each
-- with its goal and local context). This is the structured goal/context query the
-- LSP and the game consume.
typecheckModulesWithHoles
  :: [(FilePath, Rzk.Module)]
  -> Either TypeErrorInScopedContext (Checked, [HoleInfo])
typecheckModulesWithHoles = typecheckModulesWithHolesAndLemmas []

-- | Like 'typecheckModulesWithHoles', but additionally offers the given named
-- top-level definitions as hole candidates (each applied to holes when its type
-- fits the goal). The game passes a level's allow-list of relevant lemmas so they
-- surface as moves; an empty list reproduces 'typecheckModulesWithHoles'.
typecheckModulesWithHolesAndLemmas
  :: [VarIdent]
  -> [(FilePath, Rzk.Module)]
  -> Either TypeErrorInScopedContext (Checked, [HoleInfo])
typecheckModulesWithHolesAndLemmas lemmas modules =
  checkedModules modules (withHintLemmas lemmas (allowHoles emptyContext))

-- * What a consumer sees

-- | A declaration, rendered: no scope index, and nothing to re-elaborate.
--
-- This is what the LSP shows — a name, a type, a location — and it is all it needs.
-- The elaborated terms stay inside the 'Checked' package, which is what a /resume/
-- needs.
data DeclView = DeclView
  { declViewName         :: VarIdent
  , declViewType         :: Rendered
  , declViewIsAssumption :: Bool
  , declViewLocation     :: Maybe LocationInfo
  } deriving (Eq, Show)

-- | The declarations of a checked run, rendered, grouped by the file they came
-- from.
declViews :: Checked -> [(FilePath, [DeclView])]
declViews (Checked ctx decls _errs) = map (fmap (map view)) decls
  where
    naming = namingOfContext ctx
    view decl = DeclView
      { declViewName = declName decl
      , declViewType = renderTerm naming (untyped (declType decl))
      , declViewIsAssumption = declIsAssumption decl
      , declViewLocation = declLocation decl
      }

-- | Continue checking from a prefix that has already been checked.
--
-- This is the incremental path: the cached context /is/ the elaborated prefix, so
-- nothing is replayed and nothing is re-elaborated.
recheckFrom
  :: Checked
  -> [(FilePath, Rzk.Module)]
  -> Either TypeErrorInScopedContext (Checked, [HoleInfo])
recheckFrom (Checked ctx decls _errs) modules =
  runExcept $ runWriterT $ flip runReaderT ctx $
    checkModules modules $ \newDecls errs -> do
      ctx' <- ask
      pure (Checked ctx' (map (fmap (map sinkDecl)) decls <> newDecls) errs)

-- | The errors of a checked run.
checkedErrors :: Checked -> [TypeErrorInScopedContext]
checkedErrors (Checked _ _ errs) = errs

-- | Nothing checked yet: the empty context, and no declarations.
emptyChecked :: Checked
emptyChecked = Checked emptyContext [] []
