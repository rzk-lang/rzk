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
import           Control.Monad.Except     (Except, MonadError (throwError),
                                           runExcept)
import           Control.Monad.Reader     (ReaderT (..), ask, asks, local)
import           Control.Monad.Trans      (lift)
import           Control.Monad.Writer     (WriterT, runWriterT, tell)
import           Debug.Trace              (trace)

import           Control.Monad.Foil       (Distinct)
import qualified Control.Monad.Foil       as Foil

import           Language.Rzk.Foil.Names (VarIdent)
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

type TypeCheck n =
  ReaderT (Context n)
    (WriterT [HoleInfo] (Except TypeErrorInScopedContext))

-- | Run a judgement in the empty context, discarding the holes it records.
runTypeCheck :: TypeCheck Foil.VoidS a -> Either TypeErrorInScopedContext a
runTypeCheck tc = fst <$> runExcept (runWriterT (runReaderT tc emptyContext))

-- | Run a judgement in a given context, discarding the holes it records.
runTypeCheckIn :: Context n -> TypeCheck n a -> Either TypeErrorInScopedContext a
runTypeCheckIn ctx tc = fst <$> runExcept (runWriterT (runReaderT tc ctx))

-- | Run a judgement in another scope's context.
--
-- The error channel and the hole channel are shared and carry no scope index, so
-- there is nothing to translate: this is 'runReaderT' with the inner scope's
-- context, lifted back. Holes recorded inside are told into the same writer, and
-- an error thrown inside already carries its own context.
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
        }
  -- The trace message is built only when it is actually printed: at normal
  -- verbosity rendering the action's terms on every judgement would cost a
  -- thunk per judgement.
  if ctxVerbosity <= Debug
    then trace (ppAction (namingOfContext ctx) ctxActionStackDepth action) $
           local (const ctx') tc
    else local (const ctx') tc

-- * Holes

recordHoleInfo :: HoleInfo -> TypeCheck n ()
recordHoleInfo info = lift (tell [info])

-- * Locations

withLocation :: LocationInfo -> TypeCheck n a -> TypeCheck n a
withLocation loc = local $ \ctx -> ctx { ctxLocation = Just loc }
