{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE PatternSynonyms     #-}
-- | Declarations, commands and the module driver.
--
-- A top-level entry is a name bound in the outermost scope, so the scope
-- grows as a module is checked: each @#define@ and @#postulate@ enters a
-- binder and the rest of the module is checked under it. The driver is
-- written in continuation-passing style, and the result is packaged with
-- the scope it was produced in.
--
-- Sections, @#assume@ and @#data@ are not covered yet: such a command is
-- reported and skipped.
module Rzk.Check.Decl
  ( Decl (..)
  , checkModules
  , elaborate
  ) where

import           Control.Monad.Foil       (DExt, Distinct)
import qualified Control.Monad.Foil       as Foil
import           Control.Monad.Free.Foil  (AST (..))
import qualified FreeFoil.NbE             as NbE
import           Debug.Trace              (trace)

import           Language.Rzk.Foil.Names  (RzkPosition (..), TModality (..),
                                           VarIdent, markUnresolved,
                                           patternToTerm, varIdentAt)
import qualified Language.Rzk.Syntax      as Rzk
import           Rzk.Check.Context
import           Rzk.Check.Convert
import           Rzk.Check.Display
import           Rzk.Check.Error
import           Rzk.Check.Monad
import           Rzk.Check.Rules
import           Rzk.Check.Syntax
import           Rzk.Check.Value

-- | A top-level declaration, as checked.
data Decl n = Decl
  { declName     :: VarIdent
  , declNameOf   :: Foil.Name n
  , declType     :: Value n
  , declValue    :: Maybe (Value n)
  , declLocation :: Maybe LocationInfo
  }

instance Foil.Sinkable Decl where
  sinkabilityProof rename decl = decl
    { declNameOf = rename (declNameOf decl)
    , declType = Foil.sinkabilityProof rename (declType decl)
    , declValue = Foil.sinkabilityProof rename <$> declValue decl
    }

sinkDecls :: DExt n l => [Decl n] -> [Decl l]
sinkDecls = Foil.sink1

-- | A surface term in the current scope: a free identifier resolves to the
-- entry it names, and an unknown one is marked for the checker to report
-- where it is used.
elaborate :: (Checking m, Distinct n) => Rzk.Term -> m n (Raw n)
elaborate term = do
  ctx <- askContext
  let env name = case lookupNamed name ctx of
        Just v  -> Var v
        Nothing -> rawNode (RzkPosition Nothing Nothing) (HoleF (Just (markUnresolved name)))
  pure (toRaw (ctxScope ctx) env term)

-- | Bind a top-level entry and run the rest of the module under it.
withTopLevel
  :: (Checking m, Distinct n)
  => VarIdent -> Value n -> Maybe (Core n) -> EntryKind
  -> (forall l. (DExt n l, Distinct l) => Decl l -> m l r)
  -> m n r
withTopLevel name ty mval kind k = do
  checkTopLevelDuplicate name
  ctx <- askContext
  let scope = ctxScope ctx
  Foil.withFresh scope $ \binder -> do
    let pat = singlePattern binder
        scope' = Foil.extendScopePattern pat scope
        ctx0 = sinkContextUnchecked ctx
        value = fmap (evalCore scope ctx) mval
        entry = Entry
          { entryType = Foil.sink ty
          , entryValue = Foil.sink <$> value
          , entryModality = Id
          , entryLocks = Id
          , entryKind = kind
          , entryName = Just name
          , entryLocation = ctxLocation ctx
          , entryUses = []
          , entryFlags = flagsOfType scope' ctx0 (Foil.sink ty)
          }
        ctx' = enterPattern pat [entry] ctx
        decl = Decl
          { declName = name
          , declNameOf = Foil.nameOf binder
          , declType = Foil.sink ty
          , declValue = Foil.sink <$> value
          , declLocation = ctxLocation ctx
          }
    inContext ctx' (k decl)

checkTopLevelDuplicate :: (Checking m, Distinct n) => VarIdent -> m n ()
checkTopLevelDuplicate name = do
  ctx <- askContext
  case lookupNamed name ctx of
    Nothing -> pure ()
    Just v -> case entryName (lookupEntry v ctx) of
      Just previous -> issueTypeError (TypeErrorDuplicateTopLevel [previous] name)
      Nothing       -> issueTypeError (TypeErrorDuplicateTopLevel [] name)

-- * Commands

-- | The surface parameters of a declaration as binders of its type.
paramToParamDecl :: Rzk.Param -> Either String [Rzk.ParamDecl]
paramToParamDecl = \case
  Rzk.ParamPatternShape loc pats cube tope ->
    Right [ Rzk.ParamTermShape loc (patternToTerm pat) cube tope | pat <- pats ]
  Rzk.ParamPatternType loc pats ty ->
    Right [ Rzk.ParamTermType loc (patternToTerm pat) ty | pat <- pats ]
  Rzk.ParamPattern{} -> Left "untyped pattern in parameters"
  Rzk.ParamPatternModalType loc pats mc ty ->
    Right [ Rzk.ParamTermModalType loc (patternToTerm pat) mc ty | pat <- pats ]
  Rzk.ParamPatternModalShape loc pats mc cube tope ->
    Right [ Rzk.ParamTermModalShape loc (patternToTerm pat) mc cube tope | pat <- pats ]

addParamDecls :: [Rzk.ParamDecl] -> Rzk.Term -> Rzk.Term
addParamDecls [] = id
addParamDecls (paramDecl : paramDecls) = Rzk.TypeFun Nothing paramDecl . addParamDecls paramDecls

addParams :: [Rzk.Param] -> Rzk.Term -> Rzk.Term
addParams []     = id
addParams params = Rzk.Lambda Nothing params

setOption :: (Checking m, Distinct n) => String -> String -> m n a -> m n a
setOption name value = case (name, value) of
  ("verbosity", "debug")  -> withOptions (\o -> o { optVerbosity = Debug })
  ("verbosity", "normal") -> withOptions (\o -> o { optVerbosity = Normal })
  ("verbosity", "silent") -> withOptions (\o -> o { optVerbosity = Silent })
  ("verbosity", _) -> const $ issueTypeError $ TypeErrorOther
    "unknown verbosity level (use \"debug\", \"normal\", or \"silent\")"
  ("warn-overhang", "yes") -> withOptions (\o -> o { optWarnOverhang = True })
  ("warn-overhang", "no")  -> withOptions (\o -> o { optWarnOverhang = False })
  ("warn-overhang", _) -> const $ issueTypeError $ TypeErrorOther
    "unknown value for \"warn-overhang\" (use \"yes\" or \"no\")"
  ("warn-tope-family-domain", "yes") -> withOptions (\o -> o { optWarnTopeFamilyDomain = True })
  ("warn-tope-family-domain", "no")  -> withOptions (\o -> o { optWarnTopeFamilyDomain = False })
  ("warn-tope-family-domain", _) -> const $ issueTypeError $ TypeErrorOther
    "unknown value for \"warn-tope-family-domain\" (use \"yes\" or \"no\")"
  -- options of checks the new checker does not run yet are accepted
  ("render", _) -> id
  ("render-hide-term", _) -> id
  ("rstt-safe", _) -> id
  ("warn-meta-prefix", _) -> id
  ("warn-free-standing-restriction", _) -> id
  ("warn-shape-dependency", _) -> id
  ("warn-meta-binder", _) -> id
  _ -> const $ issueTypeError $ TypeErrorOther ("unknown option " <> show name)

unsetOption :: (Checking m, Distinct n) => String -> m n a -> m n a
unsetOption name = case name of
  "verbosity" -> withOptions (\o -> o { optVerbosity = optVerbosity defaultOptions })
  "warn-overhang" -> withOptions (\o -> o { optWarnOverhang = optWarnOverhang defaultOptions })
  "warn-tope-family-domain" ->
    withOptions (\o -> o { optWarnTopeFamilyDomain = optWarnTopeFamilyDomain defaultOptions })
  "render" -> id
  "render-hide-term" -> id
  "rstt-safe" -> id
  "warn-meta-prefix" -> id
  "warn-free-standing-restriction" -> id
  "warn-shape-dependency" -> id
  "warn-meta-binder" -> id
  _ -> const $ issueTypeError $ TypeErrorOther ("unknown option " <> show name)

-- | Run a check with the location narrowed to where a surface term was
-- written.
atSurface :: (Checking m, Rzk.HasPosition a) => a -> m n b -> m n b
atSurface x = localContext $ \ctx -> ctx { ctxLocation = narrow <$> ctxLocation ctx }
  where
    narrow loc = case Rzk.hasPosition x of
      Nothing          -> loc
      Just (line, col) -> loc { locationLine = Just line, locationColumn = Just col }

-- * The module driver

-- | Check a module's commands, extending the scope with each definition.
-- The continuation runs in the final scope with the declarations and the
-- errors found.
checkCommands
  :: forall m n r. (Checking m, Distinct n)
  => Maybe FilePath -> Integer -> Integer -> [Rzk.Command]
  -> (forall l. (DExt n l, Distinct l) => [Decl l] -> [FailureOf m] -> m l r)
  -> m n r
checkCommands path i total commands k = case commands of
  [] -> k [] []

  Rzk.CommandUnsetOption _loc optionName : more ->
    announce ("Unsetting option " <> optionName) $
      unsetOption optionName $ checkCommands path (i + 1) total more k

  Rzk.CommandSetOption _loc optionName optionValue : more ->
    announce ("Setting option " <> optionName <> " = " <> optionValue) $
      setOption optionName optionValue $ checkCommands path (i + 1) total more k

  Rzk.CommandDefine _loc name (Rzk.DeclUsedVars _ _vars) params ty term : more ->
    announce (" Checking #define " <> Rzk.printTree name) $ do
      typeResult <- attempt $ do
        paramDecls <- either (issueTypeError . TypeErrorOther) (pure . concat) (mapM paramToParamDecl params)
        tyTerm <- elaborate (addParamDecls paramDecls ty)
        ty' <- atSurface ty $ check tyTerm universe
        evalM ty'
      case typeResult of
        Left err -> skipping err path i total more k
        Right tyV -> do
          -- the body is checked on its own, so that a definition whose
          -- type is fine but whose proof is not still enters the scope
          result <- attempt $ do
            valTerm <- elaborate (addParams params term)
            val' <- atSurface term $ check valTerm tyV
            case force tyV of
              VNodeUniverse -> evalM val' >>= ensureTypeAtU "a definition at type U"
              _ -> pure ()
            pure val'
          let (mvalue, bodyErrors) = case result of
                Right val' -> (Just val', [])
                Left err   -> (Nothing, [err])
          withTopLevel (varIdentAt path name) tyV mvalue EntryTopLevel $ \decl ->
            checkCommands path (i + 1) total more $ \decls errs ->
              k (Foil.sink decl : decls) (bodyErrors <> errs)

  Rzk.CommandPostulate _loc name (Rzk.DeclUsedVars _ _vars) params ty : more ->
    announce (" Checking #postulate " <> Rzk.printTree name) $ do
      typeResult <- attempt $ do
        paramDecls <- either (issueTypeError . TypeErrorOther) (pure . concat) (mapM paramToParamDecl params)
        tyTerm <- elaborate (addParamDecls paramDecls ty)
        ty' <- atSurface ty $ check tyTerm universe
        evalM ty'
      case typeResult of
        Left err -> skipping err path i total more k
        Right tyV ->
          withTopLevel (varIdentAt path name) tyV Nothing EntryTopLevel $ \decl ->
            checkCommands path (i + 1) total more $ \decls errs ->
              k (Foil.sink decl : decls) errs

  Rzk.CommandCheck _loc term ty : more ->
    announce (" Checking " <> Rzk.printTree term <> " : " <> Rzk.printTree ty) $ do
      result <- attempt $ do
        tyTerm <- elaborate ty
        ty' <- atSurface ty $ check tyTerm universe
        tyV <- evalM ty'
        termTerm <- elaborate term
        _ <- atSurface term $ check termTerm tyV
        pure ()
      case result of
        Left err -> skipping err path i total more k
        Right () -> checkCommands path (i + 1) total more k

  Rzk.CommandCompute loc term : more ->
    checkCommands path i total (Rzk.CommandComputeWHNF loc term : more) k

  Rzk.CommandComputeNF _loc term : more ->
    announce (" Computing NF for " <> Rzk.printTree term) $ do
      result <- attempt $ do
        (term', _) <- atSurface term $ elaborate term >>= infer
        ctx <- askContext
        pure (ppCore (namingOfContext ctx) (nfCore (ctxScope ctx) ctx term'))
      case result of
        Left err -> skipping err path i total more k
        Right shown -> announce ("  " <> shown) $ checkCommands path (i + 1) total more k

  Rzk.CommandComputeWHNF _loc term : more ->
    announce (" Computing WHNF for " <> Rzk.printTree term) $ do
      result <- attempt $ do
        (term', _) <- atSurface term $ elaborate term >>= infer
        ctx <- askContext
        pure (ppCore (namingOfContext ctx) (quoteWhnfCore (ctxScope ctx) ctx term'))
      case result of
        Left err -> skipping err path i total more k
        Right shown -> announce ("  " <> shown) $ checkCommands path (i + 1) total more k

  Rzk.CommandData{} : more -> unsupported "#data" more
  Rzk.CommandAssume{} : more -> unsupported "#assume" more
  Rzk.CommandSection{} : more -> unsupported "#section" more
  Rzk.CommandSectionEnd{} : more -> unsupported "#end" more
  where
    announce :: String -> m l a -> m l a
    announce what = traceNormal ("[ " <> show i <> " out of " <> show total <> " ]" <> what)

    unsupported :: String -> [Rzk.Command] -> m n r
    unsupported what more = do
      result <- attempt $ issueTypeError (TypeErrorUnsupported what)
      case result of
        Left err -> skipping err path i total more k
        Right () -> checkCommands path (i + 1) total more k

-- | Report a command's error and go on to the next one.
skipping
  :: forall m n r. (Checking m, Distinct n)
  => FailureOf m -> Maybe FilePath -> Integer -> Integer -> [Rzk.Command]
  -> (forall l. (DExt n l, Distinct l) => [Decl l] -> [FailureOf m] -> m l r)
  -> m n r
skipping err path i total more k =
  checkCommands path (i + 1) total more $ \decls errs -> k decls (err : errs)

-- | A trace at normal verbosity.
traceNormal :: Checking m => String -> m n a -> m n a
traceNormal msg action = do
  verbosity <- asksContext (optVerbosity . ctxOptions)
  if verbosity <= Normal then trace msg action else action

-- | Check one module.
checkModule
  :: forall m n r. (Checking m, Distinct n)
  => Maybe FilePath -> Rzk.Module
  -> (forall l. (DExt n l, Distinct l) => [Decl l] -> [FailureOf m] -> m l r)
  -> m n r
checkModule path (Rzk.Module _loc _lang commands) k =
  checkCommands path 1 (fromIntegral (length commands)) commands k

-- | Check a list of modules, one after another, in a scope that grows as
-- it goes. Checking stops at the first module with an error.
checkModules
  :: forall m n r. (Checking m, Distinct n)
  => [(FilePath, Rzk.Module)]
  -> (forall l. (DExt n l, Distinct l) => [(FilePath, [Decl l])] -> [FailureOf m] -> m l r)
  -> m n r
checkModules [] k = k [] []
checkModules ((path, m) : ms) k =
  traceNormal ("Checking module from " <> path) $
    withLocation (LocationInfo (Just path) Nothing Nothing) $
      checkModule (Just path) m $ \decls errs ->
        case errs of
          _ : _ -> k [(path, decls)] errs
          []    -> checkModules ms $ \rest errors ->
            k ((path, sinkDecls decls) : rest) errors

-- | The weak head normal form of a term, read back.
quoteWhnfCore :: Distinct n => Foil.Scope n -> Context d n -> Core n -> Core n
quoteWhnfCore scope ctx = quoteFolded scope ctx . evalCore scope ctx

pattern VNodeUniverse :: Whnf n
pattern VNodeUniverse <- NbE.VNode UniverseF
