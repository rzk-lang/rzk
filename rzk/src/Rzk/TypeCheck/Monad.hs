{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | The checker's monad.
--
-- @TypeCheck n@ is the old @TypeCheck var@ with the scope index in place of the
-- variable type. The error channel, though, is /not/ indexed: an error carries
-- the context it was raised in (see "Rzk.TypeCheck.Error"). That is what makes
-- 'inContext' a one-liner — running a judgement in an inner scope is just running
-- it under a different reader, with nothing to re-index on the way out. The old
-- @closeScope@ had to wrap the error one binder deeper and re-emit the holes.
module Rzk.TypeCheck.Monad where

import           Control.Monad            (unless)
import           Control.Monad.Except     (ExceptT,
                                           MonadError (catchError, throwError),
                                           runExceptT)
import           Control.Monad.Reader     (ReaderT (..), ask, asks, local)
import           Control.Monad.Trans      (lift)
import           Control.Monad.Trans.State.Strict (State, get, modify', put,
                                           runState)
import           Debug.Trace              (trace)

import           Control.Monad.Foil       (Distinct)
import qualified Control.Monad.Foil       as Foil

import           Language.Rzk.Foil.Names (VarIdent)
import           Language.Rzk.Foil.Syntax (positionOfTerm)
import           Rzk.TypeCheck.Context
import           Rzk.TypeCheck.Display
import           Rzk.TypeCheck.Error

-- | A binding shown in a hole's local context: the display name and its type,
-- already rendered.
data HoleEntry = HoleEntry
  { holeEntryName :: VarIdent
  , holeEntryType :: Rendered
  } deriving (Eq, Show)

-- | The structured goal and context at a hole, recorded in lenient mode (see
-- 'allowHoles'). Everything is rendered to user-facing names at record time, so
-- 'HoleInfo' is independent of the scope it came from. Local hypotheses are split
-- into ordinary term variables and cube variables (the cube/tope layer is
-- specific to Rzk); the global environment is deliberately excluded — it belongs
-- in a searchable inventory, not the goal panel.
data HoleInfo = HoleInfo
  { holeName          :: Maybe VarIdent -- ^ the @?name@, if the hole was named
  , holeGoal          :: Rendered       -- ^ expected type (the goal), kept symbolic
  , holeGoalShape     :: Maybe (VarIdent, Rendered)
    -- ^ when the goal is a /shape/ (the hole is the argument of a
    -- shape-restricted function), the shape's bound variable and its tope: the
    -- goal then reads @(binder : holeGoal | tope)@. 'Nothing' for an ordinary
    -- goal. (Extension-type goals need no special handling — they are already a
    -- restricted type in 'holeGoal'.)
  , holeTermVars      :: [HoleEntry]    -- ^ local hypotheses whose type is not a cube
  , holeCubeVars      :: [HoleEntry]    -- ^ local cube variables (type is a cube)
  , holeTopes         :: [Rendered]     -- ^ local tope assumptions (excluding ⊤)
  , holeCandidates    :: [Rendered]
    -- ^ elimination spines over the local hypotheses whose type fits the goal,
    -- with applied arguments left as holes. Already rendered, like the rest.
  , holeIntroductions :: [Rendered]
    -- ^ introduction forms for the goal type, built from its head constructor
    -- with the constituents left as holes. Already rendered, like the rest.
  , holeDiagram       :: Maybe String
    -- ^ an SVG of the goal cell, when the goal is a renderable shape (an arrow,
    -- triangle, or square up to dimension 3).
  , holeLocation      :: Maybe LocationInfo
  } deriving (Eq, Show)

-- | A non-fatal finding of the checker, recorded on the writer channel
-- beside the holes and carried out of a run in @Checked@. Structured, so
-- the CLI, the LSP, and (later) safe mode each decide how to present or
-- escalate it.
data CheckWarning
  = LargeInductiveTypeWarning
      VarIdent              -- ^ the data type
      VarIdent              -- ^ the constructor whose field stores a universe
      (Maybe LocationInfo)
  | MetaPrefixWarning
      VarIdent              -- ^ the declaration whose type or body contains the use
      VarIdent              -- ^ the declaration used with too few meta-prefix arguments
      Int                   -- ^ the arguments supplied
      Int                   -- ^ the length of the meta prefix
      MetaPrefixRule
      (Maybe LocationInfo)
  deriving (Eq, Show)

-- | Where a warning points, for per-file attribution.
warningLocation :: CheckWarning -> Maybe LocationInfo
warningLocation (LargeInductiveTypeWarning _ _ loc)  = loc
warningLocation (MetaPrefixWarning _ _ _ _ _ loc)    = loc

-- | Which candidate rule of the meta-parameter layer check flags a
-- 'MetaPrefixWarning' (see "Rzk.TypeCheck.MetaPrefix"): the structural
-- rule, or only its stricter variant. Both are emitted so the two
-- candidate defaults can be measured on a corpus side by side.
data MetaPrefixRule
  = MetaPrefixBoth
  | MetaPrefixStrictOnly
  deriving (Eq, Show)

-- | What a run records besides its result: the holes it found and the non-fatal
-- findings it made. Both accumulate in reverse and are turned around by
-- 'checkLog' when the run ends.
data CheckLog = CheckLog
  { logHolesRev    :: [HoleInfo]
  , logWarningsRev :: [CheckWarning]
  }

emptyCheckLog :: CheckLog
emptyCheckLog = CheckLog [] []

-- | What a run recorded, in the order it was recorded.
checkLog :: CheckLog -> ([HoleInfo], [CheckWarning])
checkLog (CheckLog holes warnings) = (reverse holes, reverse warnings)

-- | The record of a run is kept in the /state/, beneath the error channel,
-- rather than on a writer channel above it.
--
-- The two differ exactly where a caught error is concerned: a writer discards
-- what the failing action wrote, and the state keeps it. That is what the
-- checker wants. A command that fails still reports the holes the user wrote in
-- it, and checking goes on to the next command with those holes in hand (see
-- @withCommand@ in "Rzk.TypeCheck.Decl"). A probe that wants the older
-- behaviour asks for it, with 'suppressing'.
type TypeCheck n =
  ReaderT (Context n)
    (ExceptT TypeErrorInScopedContext (State CheckLog))

-- | Run a judgement in a given context, keeping what it recorded.
runTypeCheckWith
  :: Context n -> TypeCheck n a
  -> (Either TypeErrorInScopedContext a, ([HoleInfo], [CheckWarning]))
runTypeCheckWith ctx tc =
  case runState (runExceptT (runReaderT tc ctx)) emptyCheckLog of
    (result, logged) -> (result, checkLog logged)

-- | Run a judgement in the empty context, discarding the holes it records.
runTypeCheck :: TypeCheck Foil.VoidS a -> Either TypeErrorInScopedContext a
runTypeCheck = runTypeCheckIn emptyContext

-- | Run a judgement in a given context, discarding the holes it records.
runTypeCheckIn :: Context n -> TypeCheck n a -> Either TypeErrorInScopedContext a
runTypeCheckIn ctx tc = fst (runTypeCheckWith ctx tc)

-- | Run a judgement in another scope's context.
--
-- The error channel and the hole channel are shared and carry no scope index, so
-- there is nothing to translate: this is 'runReaderT' with the inner scope's
-- context, lifted back. Holes recorded inside land in the same state, and an
-- error thrown inside already carries its own context.
inContext :: Context l -> TypeCheck l a -> TypeCheck n a
inContext ctx = lift . flip runReaderT ctx

-- * Errors

-- | Raise a type error, capturing the context it happened in.
issueTypeError :: Distinct n => TypeError n -> TypeCheck n a
issueTypeError err = do
  ctx <- ask
  throwError (TypeErrorInScopedContext ctx err)

issueWarning :: String -> TypeCheck n ()
issueWarning message = trace ("Warning: " <> message) (return ())

-- * Tracing

trace' :: Verbosity -> Verbosity -> String -> a -> a
trace' Silent _ _ = id
trace' Normal Debug _ = id
trace' _ _ msg = trace msg

traceTypeCheck :: Verbosity -> String -> TypeCheck n a -> TypeCheck n a
traceTypeCheck verbosity msg action = do
  configuredVerbosity <- asks ctxVerbosity
  trace' configuredVerbosity verbosity msg action

localVerbosity :: Verbosity -> TypeCheck n a -> TypeCheck n a
localVerbosity verbosity = local $ \ctx -> ctx { ctxVerbosity = verbosity }

localRenderBackend :: Maybe RenderBackend -> TypeCheck n a -> TypeCheck n a
localRenderBackend backend = local $ \ctx -> ctx { ctxRenderBackend = backend }

localHideTerm :: Bool -> TypeCheck n a -> TypeCheck n a
localHideTerm hide = local $ \ctx -> ctx { ctxRenderHideTerm = hide }

localWarnOverhang :: Bool -> TypeCheck n a -> TypeCheck n a
localWarnOverhang warn = local $ \ctx -> ctx { ctxWarnOverhang = warn }

localMetaPrefixSensitivity :: MetaPrefixSensitivity -> TypeCheck n a -> TypeCheck n a
localMetaPrefixSensitivity sensitivity =
  local $ \ctx -> ctx { ctxMetaPrefixSensitivity = sensitivity }

-- | Render the enclosed action with the proof term hidden.
hidingTerm :: TypeCheck n a -> TypeCheck n a
hidingTerm = localHideTerm True

-- * Variance

switchVariance :: TypeCheck n a -> TypeCheck n a
switchVariance = local $ \ctx -> ctx { ctxCovariance = switch (ctxCovariance ctx) }
  where
    switch Covariant     = Contravariant
    switch Contravariant = Covariant
    switch Invariant     = Invariant

setVariance :: Covariance -> TypeCheck n a -> TypeCheck n a
setVariance variance = local $ \ctx -> ctx { ctxCovariance = variance }

-- * The judgement stack

-- | The depth of nested judgements at which type checking gives up. Well-typed
-- input stays far below it; the cap catches a non-terminating search.
-- FIXME: expose as a parameter (@--max-depth@ and @rzk.yaml@).
maxActionStackDepth :: Int
maxActionStackDepth = 1000

performing :: Distinct n => Action n -> TypeCheck n a -> TypeCheck n a
performing action tc = do
  ctx@Context{..} <- ask
  unless (ctxActionStackDepth < maxActionStackDepth) $
    issueTypeError $ TypeErrorOther "maximum depth reached"
  let ctx' = ctx
        { ctxActionStack = action : ctxActionStack
        , ctxActionStackDepth = ctxActionStackDepth + 1
        , ctxLocation = narrowLocation action ctxLocation
        }
  -- The trace message is built only when it is actually printed: at normal
  -- verbosity rendering the action's terms on every judgement would cost a
  -- thunk per judgement.
  if ctxVerbosity <= Debug
    then trace (ppAction (namingOfContext ctx) ctxActionStackDepth action) $
           local (const ctx') tc
    else local (const ctx') tc

-- | Point the location at the sub-term an action is about.
--
-- The checker descends through 'performing', so the location narrows as it
-- goes and an error is reported where the sub-term that caused it was written,
-- rather than at the declaration it is in (issue #81). A judgement about a term
-- the checker built itself carries no position, and leaves the location as it
-- found it: that is the innermost enclosing term the user did write.
narrowLocation :: Action n -> Maybe LocationInfo -> Maybe LocationInfo
narrowLocation action loc = case termOf action of
  Just term | Just pos <- positionOfTerm term -> atPosition pos <$> loc
  _                                           -> loc
  where
    termOf (ActionTypeCheck term _) = Just term
    termOf (ActionInfer term)       = Just term
    termOf _                        = Nothing

-- * What a run records

modifyLog :: (CheckLog -> CheckLog) -> TypeCheck n ()
modifyLog f = lift (lift (modify' f))

-- | Run a probe for its answer alone, discarding whatever it records.
--
-- A hole's inventory is built by trying candidate moves and seeing which fit,
-- and each trial checks terms of its own; their holes and warnings are not the
-- user's and must not reach the report. This is what the writer channel's
-- @censor@ did before the record moved into the state: the state survives an
-- error, so it is put back on that path too.
suppressing :: TypeCheck n a -> TypeCheck n a
suppressing action = do
  saved <- lift (lift get)
  let restore = lift (lift (put saved))
  result <- action `catchError` \err -> restore >> throwError err
  restore
  return result

-- * Holes

recordHoleInfo :: HoleInfo -> TypeCheck n ()
recordHoleInfo info =
  modifyLog $ \l -> l { logHolesRev = info : logHolesRev l }

-- * Warnings

recordCheckWarning :: CheckWarning -> TypeCheck n ()
recordCheckWarning warning =
  modifyLog $ \l -> l { logWarningsRev = warning : logWarningsRev l }

-- * Locations

withLocation :: LocationInfo -> TypeCheck n a -> TypeCheck n a
withLocation loc = local $ \ctx -> ctx { ctxLocation = Just loc }
