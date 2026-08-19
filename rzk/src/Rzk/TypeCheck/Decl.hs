-- The scope-extension evidence on 'sinkDeclGroups' (a coercion) is its
-- soundness contract, not an argument it can consume, so GHC calls it
-- redundant. It stays.
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-redundant-constraints #-}
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

import           Control.Monad             (forM, forM_, unless, when)
import           Control.Monad.Except      (catchError)
import           Data.Data                 (Data, cast, gmapQ)
import           Control.Monad.Reader      (ask, asks, local)
import           Data.List                 (intercalate)
import qualified Data.Map                  as Map
import qualified Data.Text                 as T
import           Debug.Trace               (trace)
import           Unsafe.Coerce             (unsafeCoerce)

import           Control.Monad.Foil        (DExt, Distinct, NameBinder)
import qualified Control.Monad.Foil        as Foil
import           Control.Monad.Free.Foil   (AST (Var), ScopedAST (..))

import           Language.Rzk.Foil.Convert (Env, toTerm)
import           Language.Rzk.Foil.Syntax
import           Language.Rzk.Foil.Names   (Binder (..), TModality (..),
                                            VarIdent, binderName, markUnresolved,
                                            patternToTerm, varIdent, varIdentAt)
import qualified Language.Rzk.Syntax       as Rzk
import           Rzk.TypeCheck.Context
import           Rzk.TypeCheck.Decl.Data
import           Rzk.TypeCheck.Display
import           Rzk.TypeCheck.Error
import           Rzk.TypeCheck.Eval
import           Rzk.TypeCheck.Judgements
import           Rzk.TypeCheck.MetaPrefix
import           Rzk.TypeCheck.Monad
import           Rzk.TypeCheck.Render
import           Rzk.TypeCheck.Unify       (unifyTerms)

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

-- | A declaration sinks along a scope extension by coercion, like the
-- context does (see the note in "Rzk.TypeCheck.Context"); the proof
-- obligation is discharged field by field.
instance Foil.Sinkable Decl where
  sinkabilityProof rename decl = decl
    { declNameOf = rename (declNameOf decl)
    , declType = Foil.sinkabilityProof rename (declType decl)
    , declValue = Foil.sinkabilityProof rename <$> declValue decl
    , declUsedVars = rename <$> declUsedVars decl
    }

sinkDecl :: DExt n l => Decl n -> Decl l
sinkDecl = Foil.sink

-- | Sinking a whole list is a coercion too, with no per-element rebuild.
sinkDecls :: DExt n l => [Decl n] -> [Decl l]
sinkDecls = Foil.sinkContainer

-- | The per-file groups also sink by coercion. 'Foil.sinkContainer' cannot
-- see through the pair (its element must be the sunk type itself), but the
-- sinkability argument is the same: only the declarations mention the scope.
sinkDeclGroups :: DExt n l => [(FilePath, [Decl n])] -> [(FilePath, [Decl l])]
sinkDeclGroups = unsafeCoerce

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
    -> [CheckWarning]
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
  -> Maybe (DataRole n)  -- ^ its role for a @#data@ declaration, if any
  -> (forall l. (DExt n l, Distinct l) => NameBinder n l -> Decl l -> TypeCheck l r)
  -> TypeCheck n r
withTopLevel name ty mval isAssumption usedVars mrole k = do
  checkTopLevelDuplicate name
  metaPrefix <- metaPrefixOf ty
  recordMetaPrefixUses name ty mval
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
          , varDataRole = mrole
          , varMetaPrefix = metaPrefix
          }
        ctx' = recordInSection (Foil.nameOf binder) (enterBinder binder info [] ctx)
        decl = Decl
          { declName = name
          , declNameOf = Foil.nameOf binder
          , declType = Foil.sink ty
          , declValue = Foil.sinkContainer mval
          , declIsAssumption = isAssumption
          , declUsedVars = Foil.sinkContainer usedVars
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

  (kept0, errs') <- collectSectionDecls tolerateUnused errs [] infos

  -- Abstracting over the section's assumptions rewrote the entries' types,
  -- which can change their meta-parameter prefix (an assumption such as
  -- funext becomes a leading meta parameter), so recompute it.
  kept <- forM kept0 $ \(name, info) -> do
    metaPrefix <- metaPrefixOf (varType info)
    pure (name, info { varMetaPrefix = metaPrefix })

  loc <- asks ctxLocation
  let decls = map (toDecl loc) kept
      assumptions = [ name | (name, info) <- infos, varIsAssumption info ]
      isAssumption v = any (sameName v) assumptions
      -- the names the section's assumptions were written with, which leave scope
      assumedNames =
        [ x | (_, info) <- infos, varIsAssumption info
            , Just x <- [binderName (varOrig info)] ]

      -- the definitions of the section are now abstracted over its assumptions
      ctx' = foldr (uncurry insertVarInfo) ctx
        { ctxSections = closeInnermost (ctxSections ctx)
        , ctxBound = filter (not . isAssumption) (ctxBound ctx)
        , ctxNamed = Map.filter (not . isAssumption) (ctxNamed ctx)
        , ctxShadow = foldr Map.delete (ctxShadow ctx) assumedNames
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

    -- The entry records where it was declared; the section-close location is
    -- only the fallback (previously every declaration of a section carried
    -- the #end-time location, i.e. the last command's line).
    toDecl loc (name, info) = Decl
      { declName = case varOrig info of
          BinderVar (Just x) -> x
          _                  -> "_"
      , declNameOf = name
      , declType = varType info
      , declValue = varValue info
      , declIsAssumption = varIsAssumption info
      , declUsedVars = varDeclaredAssumptions info
      , declLocation = case varLocation info of
          Just declaredAt -> Just declaredAt
          Nothing         -> loc
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
      (use, recent') <- makeAssumptionExplicit entry recent
      unusedErr <- case use of
        AssumptionUsed -> return []
        AssumptionUnused
          | null errs && not tolerate ->
              local (\c -> c { ctxLocation = varLocation info }) $
                pure <$> typeErrorHere (TypeErrorUnusedVariable name (varType info))
          | otherwise -> return []
      collectSectionDecls tolerate (errs <> unusedErr) recent' rest
  | otherwise =
      collectSectionDecls tolerate errs (entry : recent) rest

-- | Abstract one assumption out of the definitions that come after it.
--
-- A definition that mentions the assumption gains it as an explicit parameter, and
-- every later definition that mentions /that/ definition is rewritten to apply it
-- to the assumption. A definition that mentions it without declaring it in its
-- @uses@ clause is an implicit assumption, and an error.
-- | Whether an assumption was taken up by (abstracted into) any definition
-- that followed it in its section.
data AssumptionUse = AssumptionUsed | AssumptionUnused

makeAssumptionExplicit
  :: forall n. Distinct n
  => (Foil.Name n, VarInfo n)
  -> [(Foil.Name n, VarInfo n)]
  -> TypeCheck n (AssumptionUse, [(Foil.Name n, VarInfo n)])
makeAssumptionExplicit (a, aInfo) entries = do
    -- A #data family closes over a section assumption uniformly: its type
    -- former is abstracted whenever any entry of the family uses the
    -- assumption, even though the former's own type (params → U) cannot
    -- mention it. Abstracting the former rewrites every later use of it to
    -- an application, so the constructors and eliminators (which all
    -- mention the former) follow through the ordinary deep-use path; not
    -- forcing the former would leave one unparameterised type inhabited by
    -- constructors of every instantiation.
    forced <- fmap concat $ forM entries $ \(_x, xInfo) ->
      case varDataRole xInfo of
        Nothing   -> pure []
        Just role -> do
          inTy <- freeVarsDeep (varType xInfo)
          pure [ Foil.nameId (dataRoleDataType role) | a `elemName` inTy ]
    go forced entries
  where
    go _ [] = pure (AssumptionUnused, [])
    go forced ((x, xInfo) : xs) = do
      scope <- asks ctxScope
      -- Two notions of use, and the difference between them is what 'implicit' means.
      -- The deep one follows the types of the variables the entry mentions, so it sees
      -- a dependency the entry never names; the shallow one is a syntactic occurrence.
      deepVars <- do
        inTy <- freeVarsDeep (varType xInfo)
        inVal <- concat <$> traverse freeVarsDeep (varValue xInfo)
        pure (inTy <> inVal)
      -- The syntactic check reads the declaration as the user wrote it, from the
      -- context — not the entry, which the assumptions abstracted before this one have
      -- already rewritten (and which therefore mentions them).
      written <- asks (lookupVarInfo x)
      let forcedFormer = Foil.nameId x `elem` forced
          hasAssumption = forcedFormer || a `elemName` deepVars
          inTypeSyntactically = a `elemName` freeVarsOfTermT (varType written)
          inBodySyntactically = any (elemName a . freeVarsOfTermT) (varValue written)
          declared = a `elemName` varDeclaredAssumptions xInfo
          -- used, but never written down: neither in the type, nor in the body, nor in
          -- the 'uses' clause. A forced type former is exempt: the use lives in its
          -- constructors, which have their own declarations.
          implicit = and
            [ hasAssumption
            , not (inTypeSyntactically || inBodySyntactically)
            , not declared
            , not forcedFormer
            ]
      if hasAssumption
        then do
          when implicit $
            issueTypeError $ TypeErrorImplicitAssumption (a, varType aInfo) x
          let xInfo' = abstractOver scope a aInfo xInfo
              xs' = map (fmap (applyToAssumption scope a (x, xInfo'))) xs
          (_use, xs'') <- go forced xs'
          return (AssumptionUsed, (x, xInfo') : xs'')
        else do
          (use, xs'') <- go forced xs
          return (use, (x, xInfo) : xs'')

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
  -- All products of a #data depend on the datatype (deeply, through their
  -- types), so a section assumption is abstracted over all of them
  -- uniformly, and the spine layouts the ι-rule relies on shift together.
  , varDataRole = bumpDataRoleParams <$> varDataRole info
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
-- The overhang hint costs a solver entailment per restriction face and recOR
-- guard, so it is off by default and opted into per module (or scope).
setOption "warn-overhang" = \case
  "yes" -> localWarnOverhang True
  "no"  -> localWarnOverhang False
  _ -> const $
    issueTypeError $ TypeErrorOther "unknown value for \"warn-overhang\" (use \"yes\" or \"no\")"
-- The sensitivity of the meta-parameter layer check (see
-- "Rzk.TypeCheck.MetaPrefix"); strict by default.
setOption "warn-meta-prefix" = \case
  "off"        -> localMetaPrefixSensitivity MetaPrefixOff
  "structural" -> localMetaPrefixSensitivity MetaPrefixStructural
  "strict"     -> localMetaPrefixSensitivity MetaPrefixStrict
  _ -> const $
    issueTypeError $ TypeErrorOther "unknown value for \"warn-meta-prefix\" (use \"off\", \"structural\", or \"strict\")"
setOption optionName = const $ const $
  issueTypeError $ TypeErrorOther ("unknown option " <> show optionName)

unsetOption :: Distinct n => String -> TypeCheck n a -> TypeCheck n a
unsetOption "verbosity" = localVerbosity (ctxVerbosity emptyContext)
unsetOption "render" = localRenderBackend (ctxRenderBackend emptyContext)
unsetOption "render-hide-term" = localHideTerm (ctxRenderHideTerm emptyContext)
unsetOption "warn-overhang" = localWarnOverhang (ctxWarnOverhang emptyContext)
unsetOption "warn-meta-prefix" =
  localMetaPrefixSensitivity (ctxMetaPrefixSensitivity emptyContext)
unsetOption optionName = const $
  issueTypeError $ TypeErrorOther ("unknown option " <> show optionName)

paramToParamDecl :: Distinct n => Rzk.Param -> TypeCheck n [Rzk.ParamDecl]
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

addParams :: [Rzk.Param] -> Rzk.Term -> Rzk.Term
addParams []     = id
addParams params = Rzk.Lambda Nothing params

-- * @#data@ declarations
--
-- A @#data@ elaborates to ordinary top-level entries: the type former, one
-- entry per constructor, and the generated eliminators @ind-D@ and @rec-D@.
-- All of them are opaque (no value); computation is the ι-rule in
-- "Rzk.TypeCheck.Eval", driven by the 'DataRole's recorded here. The types
-- of the generated entries are built as /surface/ terms and pushed through
-- the ordinary 'elaborate' and 'typecheck', so nothing here constructs core
-- terms by hand.
--
-- The declaration grammar already covers the later stages; this checker
-- rejects what it does not support (cube/shape and modal constructor
-- fields, function-typed recursive fields).

-- | The index telescope of the sort. The sort must be @U@ (no indices) or a
-- Π-telescope of plain types ending in @U@.
dataSortIndices :: Distinct n => Rzk.DataSort -> TypeCheck n [SortIndex]
dataSortIndices = \case
  Rzk.NoDataSort _        -> pure []
  Rzk.SomeDataSort _ sort -> go sort
  where
    go = \case
      Rzk.Universe{} -> pure []
      Rzk.TypeFun _ param ret       -> (:) <$> indexOf param <*> go ret
      Rzk.ASCII_TypeFun _ param ret -> (:) <$> indexOf param <*> go ret
      sort -> issueTypeError $ TypeErrorOther $
        "the sort of a #data must be U, or an index telescope ending in U, got "
          <> Rzk.printTree sort
    indexOf = \case
      Rzk.ParamType _ ty -> pure (SortIndex Nothing ty)
      Rzk.ParamTermType _ (Rzk.Var _ v) ty -> pure (SortIndex (Just v) ty)
      Rzk.ParamTermType _ pat _ -> issueTypeError $ TypeErrorOther $
        "an index binder of a #data sort must be a plain variable, got "
          <> Rzk.printTree pat
      p -> issueTypeError $ TypeErrorOther $
        "an index of a #data sort must be a plain type, got " <> Rzk.printTree p

dataBodyParts :: Rzk.DataBody -> ([Rzk.Constructor], [Rzk.DataElim])
dataBodyParts = \case
  Rzk.NoDataBody _              -> ([], [])
  Rzk.SomeDataBody _ cons elims -> (cons, elims)

-- | The parameters of a @#data@ must be plain typed variables @(x : A)@:
-- the constructors and eliminators apply the type former to them.
dataParamVars :: Distinct n => [Rzk.Param] -> TypeCheck n [Rzk.VarIdent]
dataParamVars = fmap concat . mapM paramVarsOf
  where
    paramVarsOf = \case
      Rzk.ParamPatternType _ pats _ty -> forM pats $ \case
        Rzk.PatternVar _ v -> pure v
        pat -> issueTypeError $ TypeErrorOther $
          "a parameter of a #data must be a plain variable, got "
            <> Rzk.printTree pat
      p -> issueTypeError $ TypeErrorOther $
        "a parameter of a #data must be a typed variable (x : A), got "
          <> Rzk.printTree p

-- | A constructor field: a typed parameter, with cube/shape fields rejected
-- (over a directed interval they would declare directed cells, out of scope
-- for M3) and modal fields deferred (crisp induction).
dataFieldToParamDecl :: Distinct n => Rzk.VarIdent -> Rzk.Param -> TypeCheck n [Rzk.ParamDecl]
dataFieldToParamDecl cname = \case
  p@Rzk.ParamPatternType{} -> paramToParamDecl p
  Rzk.ParamPattern _ pat -> issueTypeError $ TypeErrorOther $
    "untyped field " <> Rzk.printTree pat <> " in constructor " <> Rzk.printTree cname
  Rzk.ParamPatternShape{} -> issueTypeError $ TypeErrorOther $
    "constructor " <> Rzk.printTree cname
      <> " takes a cube or shape argument; over the directed interval this would declare a directed cell, which is not supported"
  Rzk.ParamPatternModalType{} -> modalFieldError cname
  Rzk.ParamPatternModalShape{} -> modalFieldError cname

modalFieldError :: Distinct n => Rzk.VarIdent -> TypeCheck n a
modalFieldError cname = issueTypeError $ TypeErrorOther $
  "modal fields are not supported yet in constructor " <> Rzk.printTree cname

dataConSurface
  :: Distinct n
  => Rzk.VarIdent -> [Rzk.VarIdent] -> [Rzk.ParamDecl] -> Int -> Rzk.Constructor
  -> TypeCheck n DataConSurface
dataConSurface dataName paramVars paramDecls indexArity (Rzk.Constructor _loc cname cparams ctype) = do
  fieldDecls <- concat <$> mapM (dataFieldToParamDecl cname) cparams
  let defaultRet = surfaceApps (surfaceVar dataName) (map surfaceVar paramVars)
      recIndicesOf = dataAppliedIndices dataName paramVars indexArity
  (ret, retIndices, sort) <- case ctype of
    Rzk.NoConstructorType _
      | indexArity == 0 -> pure (defaultRet, [], DataConPoint)
      | otherwise -> issueTypeError $ TypeErrorOther $
          "constructor " <> Rzk.printTree cname
            <> " must spell out its return type: the family has "
            <> show indexArity <> " index(es)"
    Rzk.SomeConstructorType _ ret
      | Just ixs <- recIndicesOf ret -> pure (ret, ixs, DataConPoint)
    Rzk.SomeConstructorType _ ret@(Rzk.TypeId _ l carrier r)
      | Just _ <- recIndicesOf carrier ->
          if indexArity == 0
            then pure (ret, [], DataConPath l r)
            else issueTypeError $ TypeErrorOther $
              "path constructor " <> Rzk.printTree cname
                <> " in an indexed family is not supported"
      | isIdentity carrier -> higherPathError
    Rzk.SomeConstructorType _ (Rzk.TypeIdSimple _ _ _) ->
      issueTypeError $ TypeErrorOther $
        "the return type of path constructor " <> Rzk.printTree cname
          <> " must spell out the carrier of the identification, like l =_{"
          <> Rzk.printTree defaultRet <> "} r"
    Rzk.SomeConstructorType _ _ -> issueTypeError $ TypeErrorOther $
      "the return type of constructor " <> Rzk.printTree cname
        <> " must be the declared type applied to its parameters and "
        <> show indexArity <> " index(es), like "
        <> Rzk.printTree defaultRet <> " …"
  let fieldTypes = [ ty | Rzk.ParamTermType _ _ ty <- fieldDecls ]
      recursive =
        [ (j, ixs) | (j, ty) <- zip [0 ..] fieldTypes, Just ixs <- [recIndicesOf ty] ]
      isRec ty = maybe False (const True) (recIndicesOf ty)
      nonRecTypes = [ ty | ty <- fieldTypes, not (isRec ty) ]
      nonRecDecls = [ d | d@(Rzk.ParamTermType _ _ ty) <- fieldDecls, not (isRec ty) ]
  forM_ fieldTypes $ \case
    fieldTy@(Rzk.TypeId _ _ carrier _)
      | matchesDataApplied dataName paramVars indexArity carrier ->
          issueTypeError $ TypeErrorOther $
            "constructor " <> Rzk.printTree cname
              <> " has a field of an identity type over the declared type ("
              <> Rzk.printTree fieldTy
              <> "); higher paths are not supported yet"
    _ -> pure ()
  pure DataConSurface
    { dataConName = cname
    , dataConFields = fieldDecls
    , dataConFieldPats = concatMap fieldPats cparams
    , dataConRecursive = recursive
    , dataConRetIndices = retIndices
    , dataConType = addParamDecls (paramDecls <> fieldDecls) ret
    , dataConProbe = addParamDecls nonRecDecls (Rzk.TypeUnit Nothing)
    , dataConNonRec = nonRecTypes
    , dataConLocalNames = identTokenOf cname
        : map identTokenOf (concatMap fieldVars cparams)
    , dataConSort = sort
    }
  where
    fieldVars = \case
      Rzk.ParamPatternType _ pats _ -> concatMap surfacePatternVars pats
      _                             -> []
    fieldPats = \case
      Rzk.ParamPatternType _ pats _ -> map patternToTerm pats
      _                             -> []
    isIdentity = \case
      Rzk.TypeId{}       -> True
      Rzk.TypeIdSimple{} -> True
      _                  -> False
    higherPathError = issueTypeError $ TypeErrorOther $
      "constructor " <> Rzk.printTree cname
        <> " declares a path between paths; higher path constructors are not supported yet"

-- | The assumptions an entry depends on, possibly only through the types of
-- what it mentions. The generated entries of a @#data@ declare these
-- dependencies, so that closing a section abstracts the assumption over all
-- of them uniformly instead of reporting an implicit assumption.
assumptionDepsOf :: TermT n -> TypeCheck n [Foil.Name n]
assumptionDepsOf ty = do
  ctx <- ask
  deep <- freeVarsDeep ty
  pure [ v | v <- deep, varIsAssumption (lookupVarInfo v ctx) ]

-- | Several distinct fresh identifiers with a shared base.
freshIdents
  :: Int -> [Rzk.VarIdentToken] -> T.Text -> TypeCheck n [Rzk.VarIdent]
freshIdents n avoid base = go n avoid []
  where
    go 0 _ acc = pure (reverse acc)
    go k avoidNow acc = do
      ident <- freshIdent Nothing avoidNow base
      go (k - 1) (identTokenOf ident : avoidNow) (ident : acc)

-- | An identifier that is neither bound at the top level nor among the given
-- local binder tokens; primes are appended until one is free. Used for the
-- motive and scrutinee binders of the generated eliminator types, which
-- close over the user's field types.
freshIdent
  :: Maybe FilePath -> [Rzk.VarIdentToken] -> T.Text -> TypeCheck n Rzk.VarIdent
freshIdent path avoid base = do
  ctx <- ask
  let candidates =
        [ Rzk.VarIdent Nothing (Rzk.VarIdentToken (base <> T.replicate p "'"))
        | p <- [0 ..] ]
      isFree cand = identTokenOf cand `notElem` avoid
        && case lookupNamed (varIdentAt path cand) ctx of
             Nothing -> True
             Just _  -> False
  case filter isFree candidates of
    cand : _ -> pure cand
    []       -> panicImpossible "no fresh identifier candidate"

-- | Bind the products of one @#data@ declaration: the type former, the
-- constructors (checked in a scope where the type former exists), and the
-- generated eliminators @ind-D@ and @rec-D@.
withDataDecls
  :: forall n r. Distinct n
  => Maybe FilePath
  -> [Foil.Name n]        -- ^ the declared used variables
  -> Rzk.VarIdent         -- ^ the datatype name (surface)
  -> [Rzk.VarIdent]       -- ^ the parameter variables
  -> [Rzk.ParamDecl]      -- ^ the parameter telescope
  -> [SortIndex]          -- ^ the index telescope of the sort
  -> [DataConSurface]     -- ^ the constructors, preprocessed
  -> [Rzk.DataElim]       -- ^ the re-ascription clauses
  -> (forall l. (DExt n l, Distinct l) => [Decl l] -> TypeCheck l r)
  -> TypeCheck n r
withDataDecls path used name paramVars paramDecls sortIndices consData elims k = do
  -- The type former's type spells the sort as written: params → indices → U.
  let sortTerm = foldr wrapIndex (Rzk.Universe Nothing) sortIndices
      wrapIndex (SortIndex mv ty) body = case mv of
        Just v  -> surfacePi v ty body
        Nothing -> surfaceArrow ty body
  dTyTerm <- elaborate (addParamDecls paramDecls sortTerm)
  dTy <- memoizeWHNF =<< typecheck dTyTerm universeT
  dDeps <- assumptionDepsOf dTy
  withTopLevel (varIdentAt path name) dTy Nothing False
    (nubNames (used <> dDeps)) Nothing $ \dBinder dDecl ->
      bindCons (Foil.nameOf dBinder) (Foil.sinkContainer used) 0 consData [sinkDecl dDecl] $
        \dName usedL declsAcc -> bindElims dName usedL declsAcc
  where
    numParams  = length paramDecls
    numMethods = length consData
    indexArity = length sortIndices

    bindCons
      :: forall m. DExt n m
      => Foil.Name m -> [Foil.Name m] -> Int -> [DataConSurface] -> [Decl m]
      -> (forall l. (DExt n l, Distinct l)
            => Foil.Name l -> [Foil.Name l] -> [Decl l] -> TypeCheck l r)
      -> TypeCheck m r
    bindCons dName usedHere _index [] acc kk = kk dName usedHere acc
    bindCons dName usedHere index (con : rest) acc kk = do
      -- Directly recursive fields (type = the declared type applied to its
      -- parameters) are recognised syntactically and excluded from the
      -- probe; any other occurrence of the type in a field is an error.
      let dOccursIn t = any ((== Foil.nameId dName) . Foil.nameId) (freeVarsOfTerm t)
      probeT <- elaborate (dataConProbe con)
      when (dOccursIn probeT) $ do
        -- Locate the offending field for a precise message: a positive but
        -- function-typed recursive field is meaningful and merely
        -- unsupported; anything else violates strict positivity.
        offending <- forM (dataConNonRec con) $ \ty -> do
          t <- elaborate ty
          pure (ty, dOccursIn t)
        funRec <- case [ ty | (ty, True) <- offending ] of
          ty : _ -> do
            let (domains, codomain) = surfacePiSpine ty
            domainsClean <- mapM (fmap (not . dOccursIn) . elaborate) domains
            pure (matchesDataApplied name paramVars indexArity codomain && and domainsClean)
          [] -> pure False
        issueTypeError $ TypeErrorOther $ if funRec
          then "constructor " <> Rzk.printTree (dataConName con)
            <> " has a function-typed recursive field; only directly recursive fields are supported for now"
          else "constructor " <> Rzk.printTree (dataConName con)
            <> " is not strictly positive: the declared type may occur in a field only as the type of the whole field"
      when (containsUniverse probeT) $ do
        loc <- asks ctxLocation
        recordCheckWarning $ LargeInductiveTypeWarning
          (varIdentAt path name)
          (varIdentAt path (dataConName con))
          loc
      conTyTerm <- elaborate (dataConType con)
      conTy <- memoizeWHNF =<< typecheck conTyTerm universeT
      conDeps <- assumptionDepsOf conTy
      let conSort = case dataConSort con of
            DataConPoint  -> PointCon
            DataConPath{} -> PathCon
          role = DataRole dName numParams
            (DataConKind conSort index (length (dataConFields con))
              (map fst (dataConRecursive con)))
      withTopLevel (varIdentAt path (dataConName con)) conTy Nothing False
        (nubNames (usedHere <> conDeps)) (Just role) $ \_conBinder conDecl ->
          bindCons (Foil.sink dName) (Foil.sinkContainer usedHere) (index + 1) rest
            (sinkDecls acc <> [conDecl]) kk

    bindElims
      :: forall m. DExt n m
      => Foil.Name m -> [Foil.Name m] -> [Decl m] -> TypeCheck m r
    bindElims dName usedHere declsAcc = do
      let avoid = identTokenOf name
            : map identTokenOf paramVars
            <> [ identTokenOf v | SortIndex (Just v) _ <- sortIndices ]
            <> concatMap dataConLocalNames consData
      -- Anonymous indices of the sort get fresh names; named ones keep the
      -- user's spelling (later index types may depend on them).
      freshIx <- freshIdents (length [ () | SortIndex Nothing _ <- sortIndices ]) avoid "i"
      let indexVars = go sortIndices freshIx
            where
              go [] _ = []
              go (SortIndex (Just v) _ : rest) fresh = v : go rest fresh
              go (SortIndex Nothing _ : rest) (f : fresh) = f : go rest fresh
              go (SortIndex Nothing _ : _) [] =
                error "impossible: not enough fresh index names"
          indexDecls =
            [ Rzk.ParamTermType Nothing (surfaceVar v) (sortIndexType si)
            | (v, si) <- zip indexVars sortIndices ]
          avoid' = map identTokenOf indexVars <> avoid
      motiveV <- freshIdent path avoid' "C"
      scrutV <- freshIdent path (identTokenOf motiveV : avoid') "x"
      -- Distinct induction-hypothesis binders, one per recursive field of
      -- the widest constructor; separate methods reuse the same names.
      let maxRec = maximum (0 : map (length . dataConRecursive) consData)
      ihNames <- freshIdents maxRec
        (identTokenOf motiveV : identTokenOf scrutV : avoid') "ih"
      -- A path-method type refers to the point methods by name, so a
      -- declaration with path constructors binds its methods (@m-<con>@);
      -- a point-only declaration keeps the anonymous arrows it always had.
      let hasPaths = or [ True | DataConPath{} <- map dataConSort consData ]
          avoidM = map identTokenOf (motiveV : scrutV : ihNames) <> avoid'
      methodVars <-
        if not hasPaths then pure Nothing else Just <$>
          let go _ [] = pure []
              go av (con : rest) = do
                let Rzk.VarIdentToken t = identTokenOf (dataConName con)
                m <- freshIdent path av ("m-" <> t)
                (m :) <$> go (identTokenOf m : av) rest
           in go avoidM consData
      -- Binders of the inlined transport/apd spellings (rzk has no
      -- primitive transport, so the generated types spell it through idJ).
      let avoidM' = maybe [] (map identTokenOf) methodVars <> avoidM
      endpointV <- freshIdent path avoidM' "y"
      pathV <- freshIdent path (identTokenOf endpointV : avoidM') "q"
      -- the inlined transport gets its own binder: it ends up nested inside
      -- a λ that binds 'endpointV' (the apd motive), and reusing the name
      -- there would shadow it
      transportV <- freshIdent path
        (identTokenOf endpointV : identTokenOf pathV : avoidM') "y"
      -- The images of a path constructor's endpoints under the section
      -- being defined; also the endpoint well-formedness check (an endpoint
      -- must be built from constructors and fields, so that its image is
      -- syntactically computable — the standard HIT schema restriction).
      let conMethods = case methodVars of
            Just ms -> zip (map (identTokenOf . dataConName) consData)
                           (zip ms consData)
            Nothing -> []
          dataTok = identTokenOf name
          conToks = map (identTokenOf . dataConName) consData
          -- the induction-hypothesis binder for each recursive field of a
          -- constructor, by the field's token (a recursive field is a plain
          -- variable: no pattern form inhabits the datatype)
          ihByFieldTok c =
            [ (identTokenOf v, ih)
            | (ih, j) <- zip ihNames (recPositionsOf c)
            , Rzk.Var _ v <-
                [nthByConstruction "constructor field patterns" (dataConFieldPats c) j] ]
          endpointErr c t = issueTypeError $ TypeErrorOther $
            "in path constructor " <> Rzk.printTree (dataConName c)
              <> ": an endpoint must be built from the declaration's"
              <> " constructors and the constructor's fields, but "
              <> Rzk.printTree t <> " is not"
          -- a subterm at a non-recursive argument position must not touch
          -- the datatype at all: the section being defined does not act on it
          ensureDFree c t =
            let bad = dataTok : conToks <> map fst (ihByFieldTok c)
             in unless (null [ () | tk <- surfaceVarTokens t, tk `elem` bad ])
                  (endpointErr c t)
          imageOf c t = case surfaceAppSpine t of
            (Rzk.Var _ v, args)
              | Just (m, target) <- lookup (identTokenOf v) conMethods -> do
                  let nP = length paramVars
                      arity = length (dataConFields target)
                      (pArgs, fArgs) = splitAt nP args
                      varTokOf = \case
                        Rzk.Var _ u -> Just (identTokenOf u)
                        _           -> Nothing
                  unless (length args == nP + arity
                      && map varTokOf pArgs == map (Just . identTokenOf) paramVars) $
                    endpointErr c t
                  imgArgs <- forM (zip [0 :: Int ..] fArgs) $ \(j, a) ->
                    if j `elem` recPositionsOf target
                      then do { ia <- imageOf c a; pure [a, ia] }
                      else do { ensureDFree c a; pure [a] }
                  pure (surfaceApps (surfaceVar m) (concat imgArgs))
              | null args
              , Just ih <- lookup (identTokenOf v) (ihByFieldTok c) ->
                  pure (surfaceVar ih)
            _ -> endpointErr c t
      pathData <- forM consData $ \con -> case dataConSort con of
        DataConPoint    -> pure Nothing
        DataConPath l r -> do
          iL <- imageOf con l
          iR <- imageOf con r
          pure (Just ((l, r), (iL, iR)))
      -- The surface types of the eliminators and the path computation rules
      -- (see "Rzk.TypeCheck.Decl.Data"), pushed through the ordinary
      -- elaborator below just like any user-written type.
      let spec = ElimSpec
            { esName = name, esParamVars = paramVars, esParamDecls = paramDecls
            , esIndexVars = indexVars, esIndexDecls = indexDecls, esMotiveV = motiveV
            , esScrutV = scrutV, esIhNames = ihNames, esMethodVars = methodVars
            , esEndpointV = endpointV, esPathV = pathV, esTransportV = transportV
            , esConsData = consData, esPathData = pathData }
          et = elimTerms spec
          indName = prefixedIdent "ind-" name
          recName = prefixedIdent "rec-" name
      (mindTy, mrecTy, computeReasc) <- splitClauses indName recName (map fst (computeRules et))
      indTyT <- elaborate (indTypeTerm et)
      indCanonical <- memoizeWHNF =<< typecheck indTyT universeT
      indTy' <- reascribe indName indCanonical mindTy
      indDeps <- assumptionDepsOf indTy'
      withTopLevel (varIdentAt path indName) indTy' Nothing False
        (nubNames (usedHere <> indDeps))
        (Just (DataRole dName numParams (DataElimKind numMethods indexArity ElimInd))) $ \_ indDecl -> do
          recTyT <- elaborate (recTypeTerm et)
          recCanonical <- memoizeWHNF =<< typecheck recTyT universeT
          recTy' <- reascribe recName recCanonical mrecTy
          recDeps <- assumptionDepsOf recTy'
          let usedAtInd = Foil.sinkContainer usedHere
          withTopLevel (varIdentAt path recName) recTy' Nothing False
            (nubNames (usedAtInd <> recDeps))
            (Just (DataRole (Foil.sink dName) numParams (DataElimKind numMethods indexArity ElimRec))) $ \_ recDecl ->
              bindComputes
                (Foil.sinkContainer usedAtInd)
                computeReasc
                (computeRules et)
                (sinkDecls (sinkDecls declsAcc <> [indDecl]) <> [recDecl])

    -- | Bind the generated @compute-@ lemmas (one @ind@/@rec@ pair per
    -- path constructor), re-ascribing the ones a @compute with@ clause
    -- names, then hand the accumulated declarations to the continuation of
    -- 'withDataDecls'.
    bindComputes
      :: forall m. DExt n m
      => [Foil.Name m] -> [(Rzk.VarIdentToken, Rzk.Term)]
      -> [(Rzk.VarIdent, Rzk.Term)] -> [Decl m]
      -> TypeCheck m r
    bindComputes _used _reasc [] acc = k acc
    bindComputes usedHere reasc ((cname, cty) : rest) acc = do
      tyT <- elaborate cty
      canonical <- memoizeWHNF =<< typecheck tyT universeT
      ty' <- reascribe cname canonical (lookup (identTokenOf cname) reasc)
      deps <- assumptionDepsOf ty'
      withTopLevel (varIdentAt path cname) ty' Nothing False
        (nubNames (usedHere <> deps)) Nothing $ \_ decl ->
          bindComputes (Foil.sinkContainer usedHere) reasc rest
            (sinkDecls acc <> [decl])

    -- | Split the re-ascription clauses among the generated entries: the
    -- two eliminators (@eliminate with@) and the computation rules of the
    -- path constructors (@compute with@). A clause must name an entry of
    -- the matching kind, at most once each.
    splitClauses
      :: Distinct m
      => Rzk.VarIdent -> Rzk.VarIdent -> [Rzk.VarIdent]
      -> TypeCheck m
           (Maybe Rzk.Term, Maybe Rzk.Term, [(Rzk.VarIdentToken, Rzk.Term)])
    splitClauses indName recName computeNames = go (Nothing, Nothing, []) elims
      where
        go acc [] = pure acc
        go (mind, mrec, mcomp) (Rzk.DataElim _loc elimName ty : rest)
          | sameIdent elimName indName =
              case mind of
                Nothing -> go (Just ty, mrec, mcomp) rest
                Just _  -> duplicate elimName
          | sameIdent elimName recName =
              case mrec of
                Nothing -> go (mind, Just ty, mcomp) rest
                Just _  -> duplicate elimName
          | otherwise = issueTypeError $ TypeErrorOther $
              "eliminate with clause for " <> Rzk.printTree elimName
                <> ", which is not an eliminator of " <> Rzk.printTree name
                <> " (the eliminators are " <> Rzk.printTree indName
                <> " and " <> Rzk.printTree recName <> ")"
        go (mind, mrec, mcomp) (Rzk.DataCompute _loc ruleName ty : rest)
          | any (sameIdent ruleName) computeNames =
              if identTokenOf ruleName `elem` map fst mcomp
                then duplicate ruleName
                else go (mind, mrec, mcomp <> [(identTokenOf ruleName, ty)]) rest
          | otherwise = issueTypeError $ TypeErrorOther $
              "compute with clause for " <> Rzk.printTree ruleName
                <> ", which is not a computation rule of " <> Rzk.printTree name
                <> case computeNames of
                     [] -> " (the declaration has no path constructors, so no computation rules are generated)"
                     _  -> " (the computation rules are "
                             <> intercalate ", " (map Rzk.printTree computeNames)
                             <> ")"
        duplicate n = issueTypeError $ TypeErrorOther $
          "duplicate re-ascription clause for " <> Rzk.printTree n
        sameIdent a b = identTokenOf a == identTokenOf b

    -- | Check a re-ascribed generated type (an eliminator's or a
    -- computation rule's) against the canonical one and pick the type to
    -- store. The user's spelling must be definitionally equal to the
    -- canonical type (checked with the type former and constructors in
    -- scope); definitionally equal types are interchangeable, so storing
    -- the user's spelling only changes how the entry's type is displayed,
    -- not what it accepts or computes.
    reascribe
      :: Distinct m
      => Rzk.VarIdent -> TermT m -> Maybe Rzk.Term -> TypeCheck m (TermT m)
    reascribe _entryName canonical Nothing = pure canonical
    reascribe entryName canonical (Just ty) = do
      tyT <- elaborate ty
      ty' <- memoizeWHNF =<< typecheck tyT universeT
      unifyTerms canonical ty' `catchError` \_ ->
        issueTypeError $
          TypeErrorReascribedTypeMismatch (varIdentAt path entryName) canonical ty'
      pure ty'

-- | Run a command, recording which one it is and where.
--
-- An error raised anywhere in the command (or in the rest of the module, which is
-- checked inside it) is /collected/ rather than thrown: the declarations made
-- before it stand, the error is reported, and the rest of the module is skipped.
-- The strict entry points turn the first collected error back into a thrown one.
withCommand
  :: Distinct n
  => Rzk.Command
  -> ([Decl n] -> [TypeErrorInScopedContext] -> TypeCheck n r)
  -> TypeCheck n r
  -> TypeCheck n r
withCommand command k action =
  local atCommand (checked `catchError` \err -> k [] [err])
  where
    checked = case duplicateBinders command of
      (dup, previous) : _ -> issueTypeError (TypeErrorRepeatedBinder dup previous)
      []                  -> action
    atCommand ctx = ctx
      { ctxCurrentCommand = Just command
      , ctxLocation = updatePosition (Rzk.hasPosition command) <$> ctxLocation ctx
      }
    -- Where the command starts. A judgement made inside it narrows this to the
    -- sub-term it is about (see @narrowLocation@); this is what an error with
    -- no sub-term of its own falls back to.
    updatePosition pos loc =
      loc { locationLine = fst <$> pos, locationColumn = snd <$> pos }

-- | The binder leaves repeated within one binder /group/ of a surface
-- command: the parameter list of a λ, of a declaration, or of a constructor
-- (a single pattern's leaves are part of their group). Each hit pairs the
-- repeated occurrence with the earlier ones it clashes with.
--
-- Shadowing an outer binder stays a warning ('checkNameShadowing'); a
-- duplicate inside one group can only be a mistake, and the silent
-- freshening it used to get showed names the user never wrote (issue #321).
duplicateBinders :: Data a => a -> [(VarIdent, [VarIdent])]
duplicateBinders x = concat
  [ maybe [] termGroup (cast x)
  , maybe [] commandGroup (cast x)
  , maybe [] constructorGroup (cast x)
  , concat (gmapQ duplicateBinders x)
  ]
  where
    termGroup :: Rzk.Term -> [(VarIdent, [VarIdent])]
    termGroup = \case
      Rzk.Lambda _ params _       -> groupDuplicates (concatMap paramLeaves params)
      Rzk.ASCII_Lambda _ params _ -> groupDuplicates (concatMap paramLeaves params)
      _                           -> []

    commandGroup :: Rzk.Command -> [(VarIdent, [VarIdent])]
    commandGroup = \case
      Rzk.CommandDefine _ _ _ params _ _  -> groupDuplicates (concatMap paramLeaves params)
      Rzk.CommandPostulate _ _ _ params _ -> groupDuplicates (concatMap paramLeaves params)
      Rzk.CommandData _ _ _ params _ _    -> groupDuplicates (concatMap paramLeaves params)
      _                                   -> []

    constructorGroup :: Rzk.Constructor -> [(VarIdent, [VarIdent])]
    constructorGroup (Rzk.Constructor _ _ params _) =
      groupDuplicates (concatMap paramLeaves params)

    paramLeaves :: Rzk.Param -> [Rzk.VarIdent]
    paramLeaves = \case
      Rzk.ParamPattern _ pat                  -> patternLeaves pat
      Rzk.ParamPatternType _ pats _           -> concatMap patternLeaves pats
      Rzk.ParamPatternShape _ pats _ _        -> concatMap patternLeaves pats
      Rzk.ParamPatternModalType _ pats _ _    -> concatMap patternLeaves pats
      Rzk.ParamPatternModalShape _ pats _ _ _ -> concatMap patternLeaves pats

    patternLeaves :: Rzk.Pattern -> [Rzk.VarIdent]
    patternLeaves = \case
      Rzk.PatternUnit _ -> []
      Rzk.PatternVar _ v@(Rzk.VarIdent _ (Rzk.VarIdentToken t)) -> [ v | t /= "_" ]
      Rzk.PatternPair _ l r -> patternLeaves l <> patternLeaves r
      Rzk.PatternTuple _ a b cs -> concatMap patternLeaves (a : b : cs)

    -- positions differ between occurrences, so compare by spelling
    groupDuplicates = go []
      where
        go _ [] = []
        go seen (v : vs) =
          case [ e | e <- seen, sameSpelling e v ] of
            []      -> go (v : seen) vs
            earlier -> (varIdent v, map varIdent (reverse earlier)) : go (v : seen) vs
    sameSpelling (Rzk.VarIdent _ a) (Rzk.VarIdent _ b) = a == b

-- | Elaborate a surface term in the current top-level scope: a free identifier
-- resolves to the top-level entry it names.
-- | Run a check with the location narrowed to where a surface term was written.
--
-- Descending through a judgement already narrows to the sub-term it is about
-- (@narrowLocation@ in "Rzk.TypeCheck.Monad"), but only a /node/ carries a
-- position: a variable is a leaf of the core syntax, with nowhere to put one.
-- So a check that starts from a surface term says where that term starts, and a
-- definition whose body is a bare variable is reported at the body rather than
-- at the declaration above it.
atSurface :: Rzk.HasPosition a => a -> TypeCheck n b -> TypeCheck n b
atSurface x = local $ \ctx ->
  ctx { ctxLocation = narrow <$> ctxLocation ctx }
  where
    narrow loc = case Rzk.hasPosition x of
      Nothing          -> loc
      Just (line, col) ->
        loc { locationLine = Just line, locationColumn = Just col }

elaborate :: forall n. Distinct n => Rzk.Term -> TypeCheck n (Term n)
elaborate term = do
  ctx <- ask
  let env :: Env n
      env name = case lookupNamed name ctx of
        Just v  -> Var v
        Nothing -> Hole (Just (markUnresolved name))
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
      withCommand command k $
        unsetOption optionName $
          checkCommands path (i + 1) total more k

  command@(Rzk.CommandSetOption _loc optionName optionValue) : more ->
    announce ("Setting option " <> optionName <> " = " <> optionValue) $
      withCommand command k $
        setOption optionName optionValue $
          checkCommands path (i + 1) total more k

  command@(Rzk.CommandDefine _loc name (Rzk.DeclUsedVars _ vars) params ty term) : more ->
    announce (" Checking #define " <> Rzk.printTree name) $
      withCommand command k $ do
        used <- mapM (checkDefined . varIdentAt path) vars
        paramDecls <- concat <$> mapM paramToParamDecl params
        -- Store the elaborated type and term unreduced, but memoise their WHNF on
        -- the top node. Reducing in place would discard or expose a variable
        -- occurrence, so the section unused/implicit-assumption checks (run over the
        -- stored type and value) would disagree with the term the user wrote;
        -- keeping the WHNF cached preserves the original one-shot reduction.
        tyTerm <- elaborate (addParamDecls paramDecls ty)
        ty' <- atSurface ty $ memoizeWHNF =<< typecheck tyTerm universeT
        valTerm <- elaborate (addParams params term)
        term' <- atSurface term $ memoizeWHNF =<< typecheck valTerm ty'
        withTopLevel (varIdentAt path name) ty' (Just term') False used Nothing $ \binder decl -> do
          backend <- asks ctxRenderBackend
          termSVG <- case backend of
            Just RenderSVG   -> renderTermSVG (Var (Foil.nameOf binder))
            Just RenderLaTeX -> issueTypeError $
              TypeErrorOther "\"latex\" rendering is not yet supported"
            Nothing          -> pure Nothing
          maybe id trace termSVG $
            checkCommands path (i + 1) total more $ \decls errs ->
              k (sinkDecl decl : decls) errs

  command@(Rzk.CommandData _loc name (Rzk.DeclUsedVars _ vars) params sort body) : more ->
    announce (" Checking #data " <> Rzk.printTree name) $
      withCommand command k $ do
        used <- mapM (checkDefined . varIdentAt path) vars
        sortIndices <- dataSortIndices sort
        let (cons, elims) = dataBodyParts body
        paramVars <- dataParamVars params
        paramDecls <- concat <$> mapM paramToParamDecl params
        consData <- mapM
          (dataConSurface name paramVars paramDecls (length sortIndices)) cons
        withDataDecls path used name paramVars paramDecls sortIndices consData elims $ \decls ->
          checkCommands path (i + 1) total more $ \moreDecls errs ->
            k (sinkDecls decls <> moreDecls) errs

  command@(Rzk.CommandPostulate _loc name (Rzk.DeclUsedVars _ vars) params ty) : more ->
    announce (" Checking #postulate " <> Rzk.printTree name) $
      withCommand command k $ do
        used <- mapM (checkDefined . varIdentAt path) vars
        paramDecls <- concat <$> mapM paramToParamDecl params
        tyTerm <- elaborate (addParamDecls paramDecls ty)
        ty' <- atSurface ty $ memoizeWHNF =<< typecheck tyTerm universeT
        withTopLevel (varIdentAt path name) ty' Nothing False used Nothing $ \_binder decl ->
          checkCommands path (i + 1) total more $ \decls errs ->
            k (sinkDecl decl : decls) errs

  command@(Rzk.CommandAssume _loc names ty) : more ->
    announce (" Checking #assume "
        <> intercalate " " [ Rzk.printTree name | name <- names ]) $
      withCommand command k $ do
        tyTerm <- elaborate ty
        ty' <- atSurface ty $ typecheck tyTerm universeT
        assume (map (varIdentAt path) names) ty' $ \assumed ->
          checkCommands path (i + 1) total more $ \decls errs ->
            k (sinkDecls assumed <> decls) errs

  command@(Rzk.CommandCheck _loc term ty) : more ->
    announce (" Checking " <> Rzk.printTree term <> " : " <> Rzk.printTree ty) $
      withCommand command k $ do
        tyTerm <- elaborate ty
        ty' <- atSurface ty $ typecheck tyTerm universeT >>= whnfT
        termTerm <- elaborate term
        _term' <- atSurface term $ typecheck termTerm ty'
        checkCommands path (i + 1) total more k

  Rzk.CommandCompute loc term : more ->
    checkCommands path i total (Rzk.CommandComputeWHNF loc term : more) k

  command@(Rzk.CommandComputeNF _loc term) : more ->
    announce (" Computing NF for " <> Rzk.printTree term) $
      withCommand command k $ do
        term' <- elaborate term >>= infer >>= nfT
        shown <- ppInContext term'
        traceTypeCheck Normal ("  " <> shown) $
          checkCommands path (i + 1) total more k

  command@(Rzk.CommandComputeWHNF _loc term) : more ->
    announce (" Computing WHNF for " <> Rzk.printTree term) $
      withCommand command k $ do
        term' <- elaborate term >>= infer >>= whnfT
        shown <- ppInContext term'
        traceTypeCheck Normal ("  " <> shown) $
          checkCommands path (i + 1) total more k

  command@(Rzk.CommandSection _loc name) : more ->
    withCommand command k $ do
      (sectionCommands, more') <- splitSectionCommands name more
      withSection (Just name) i sectionCommands path total $ \sectionDecls sectionErrs ->
        if null sectionErrs
          then checkCommands path (i + countCommands sectionCommands) total more' $
            \decls errs -> k (sinkDecls sectionDecls <> decls) errs
          else k sectionDecls sectionErrs

  command@(Rzk.CommandSectionEnd _loc endName) : _more ->
    withCommand command k $
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
  withTopLevel name ty Nothing True [] Nothing $ \_binder decl ->
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
    withLocation (LocationInfo
      { locationFilePath = Just path
      , locationLine = Nothing
      , locationColumn = Nothing }) $
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
        k ((path, sinkDecls decls) : rest) errors

-- * The public entry points

-- | Check the modules, and package the result with the scope it was checked in.
-- The warnings are recorded during the run and are folded into the 'Checked'
-- package here.
checkedModules :: [(FilePath, Rzk.Module)] -> Context Foil.VoidS -> Either TypeErrorInScopedContext (Checked, [HoleInfo])
checkedModules modules ctx =
  package $ runTypeCheckWith ctx $
    checkModules modules $ \decls errs -> do
      ctx' <- ask
      pure (Checked ctx' decls errs [])
  where
    package (Left err, _) = Left err
    package (Right (Checked ctx' decls errs _), (holes, warnings)) =
      Right (Checked ctx' decls errs warnings, holes)

-- | Check the modules strictly: an unfilled hole is an error, and the first error
-- stops the run.
typecheckModules
  :: [(FilePath, Rzk.Module)] -> Either TypeErrorInScopedContext Checked
typecheckModules modules = do
  (checked@(Checked _ _ errs _), _holes) <- checkedModules modules emptyContext
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
-- fits the goal, and never below its meta prefix — an unsaturated schema is not
-- a suggestion, see "Rzk.TypeCheck.MetaPrefix"). The game passes a level's
-- allow-list of relevant lemmas so they surface as moves; an empty list
-- reproduces 'typecheckModulesWithHoles'.
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
  , declViewKind         :: DeclKind
  } deriving (Eq, Show)

-- | What kind of declaration a 'DeclView' renders: a plain definition, a
-- postulate, or one of the products of a @#data@ (the symbol providers group
-- constructors under their type and keep the generated eliminators out of
-- the outline; the semantic tokens highlight postulates distinctly).
data DeclKind
  = DeclKindDefine
  | DeclKindPostulate          -- ^ a @#postulate@ or @#assume@: declared, but not proven
  | DeclKindData
  | DeclKindDataCon VarIdent   -- ^ a constructor of the named type
  | DeclKindDataElim VarIdent  -- ^ a generated eliminator of the named type
  deriving (Eq, Show)

-- | The declarations of a checked run, rendered, grouped by the file they came
-- from.
declViews :: Checked -> [(FilePath, [DeclView])]
declViews (Checked ctx decls _errs _warnings) = map (fmap (map view)) decls
  where
    naming = namingOfContext ctx
    -- The type formers carry no role themselves; they are the names the
    -- constructor and eliminator roles point at.
    formerIds =
      [ Foil.nameId (dataRoleDataType role)
      | d <- concatMap snd decls
      , Just role <- [varDataRole (lookupVarInfo (declNameOf d) ctx)] ]
    parentNameOf p = case binderName (varOrig (lookupVarInfo p ctx)) of
      Just x  -> x
      Nothing -> "_"
    kindOf d = case varDataRole (lookupVarInfo (declNameOf d) ctx) of
      Just (DataRole parent _ DataConKind{})  -> DeclKindDataCon (parentNameOf parent)
      Just (DataRole parent _ DataElimKind{}) -> DeclKindDataElim (parentNameOf parent)
      Nothing
        | Foil.nameId (declNameOf d) `elem` formerIds -> DeclKindData
        -- A declaration with no value is an axiom, whether written as a
        -- @#postulate@ or an @#assume@ (in practice both are used for
        -- axioms such as function extensionality).
        | Nothing <- declValue d -> DeclKindPostulate
        | otherwise -> DeclKindDefine
    view decl = DeclView
      { declViewName = declName decl
      , declViewType = renderTerm naming (untyped (declType decl))
      , declViewIsAssumption = declIsAssumption decl
      , declViewLocation = declLocation decl
      , declViewKind = kindOf decl
      }

-- | Continue checking from a prefix that has already been checked.
--
-- This is the incremental path: the cached context /is/ the elaborated prefix, so
-- nothing is replayed and nothing is re-elaborated.
recheckFrom
  :: Checked
  -> [(FilePath, Rzk.Module)]
  -> Either TypeErrorInScopedContext (Checked, [HoleInfo])
recheckFrom (Checked ctx decls _errs _warnings) modules =
  package $ runTypeCheckWith ctx $
    checkModules modules $ \newDecls errs -> do
      ctx' <- ask
      pure (Checked ctx' (sinkDeclGroups decls <> newDecls) errs [])
  where
    -- Only this run's warnings: like the errors, the prefix's warnings were
    -- already reported when the prefix was checked.
    package (Left err, _) = Left err
    package (Right (Checked ctx' decls' errs _), (holes, warnings)) =
      Right (Checked ctx' decls' errs warnings, holes)

-- | The errors of a checked run.
checkedErrors :: Checked -> [TypeErrorInScopedContext]
checkedErrors (Checked _ _ errs _) = errs

-- | The warnings of a checked run.
checkedWarnings :: Checked -> [CheckWarning]
checkedWarnings (Checked _ _ _ warnings) = warnings

-- | Nothing checked yet: the empty context, and no declarations.
emptyChecked :: Checked
emptyChecked = Checked emptyContext [] [] []

-- | Nothing checked yet, in lenient hole mode: what an editor resumes from.
--
-- A hole is work in progress there, to be reported with its goal and context
-- rather than as an error (see 'allowHoles'). 'recheckFrom' continues in the
-- context it is given, so the mode has to be set on the empty one it starts
-- with: resuming from 'emptyChecked' made every hole a @TypeErrorUnsolvedHole@
-- and stopped the file at the first one.
emptyCheckedWithHoles :: Checked
emptyCheckedWithHoles = Checked (allowHoles emptyContext) [] [] []
