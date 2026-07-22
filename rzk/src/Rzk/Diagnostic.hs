{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Structured diagnostics for rzk: type errors and holes as data (severity,
-- a stable code, a source location, and a message) rather than a single
-- pre-formatted string. The core library produces these; the LSP maps them to
-- LSP diagnostics, and the CLI can emit them as JSON (@rzk typecheck --json@).
--
-- Locations are line-level: rzk currently retains only file + line at the point
-- an error is produced (the column is discarded, and core terms keep no
-- per-node position), so diagnostics point at the enclosing command's line.
module Rzk.Diagnostic where

import           Data.Aeson           (ToJSON (..), Value (String), object,
                                       (.=))

import           Rzk.TypeCheck

-- | Diagnostic severity, mirroring the usual LSP levels.
data Severity
  = SeverityError
  | SeverityWarning
  | SeverityInformation
  | SeverityHint
  deriving (Eq, Show)

-- | A structured diagnostic. Independent of any editor protocol: the LSP maps
-- it to its own @Diagnostic@, and the CLI serialises it as JSON.
--
-- == JSON wire format (@rzk typecheck --json@)
--
-- This is a stable format that downstream tools (e.g. richer LSP hovers) pin
-- to. Each diagnostic is an object:
--
-- > { "severity": "error" | "warning" | "information" | "hint"
-- > , "code":     <string>           -- "hole", or a TypeError tag (e.g. "TypeErrorUnify")
-- > , "location": { "file": <string|null>, "line": <int|null> } | null
-- > , "message":  <string>           -- human-facing prose (the CLI/LSP display text)
-- > , "hole":     <HoleData> | null  -- present only for hole diagnostics (see 'HoleData')
-- > }
--
-- The @message@ field is kept for back-compat (the LSP and the CLI human mode
-- use it). Structured consumers should read the @hole@ object instead of
-- parsing @message@. For non-hole diagnostics @hole@ is @null@.
data Diagnostic = Diagnostic
  { diagnosticSeverity :: Severity
  , diagnosticCode     :: String            -- ^ stable category, e.g. @\"TypeErrorUnify\"@ or @\"hole\"@
  , diagnosticLocation :: Maybe LocationInfo -- ^ file + line (line-level granularity)
  , diagnosticMessage  :: String
  , diagnosticHole     :: Maybe HoleData     -- ^ structured hole payload ('Nothing' for type errors)
  } deriving (Eq, Show)

-- | The structured payload of a hole diagnostic: the goal and local context as
-- already-rendered display strings (the same strings the LSP/CLI show, i.e.
-- @show@ = @printTree@ on the underlying terms). Consumers render hole panels
-- from these fields directly, without parsing the human-facing @message@.
--
-- == JSON wire format
--
-- > { "name":     <string|null>                 -- the ?name, if the hole was named
-- > , "goal":     <string>                       -- the expected type (the goal)
-- > , "shape":    { "binder": <string>, "tope": <string> } | null
-- > , "termVars": [ { "name": <string>, "type": <string> }, ... ]
-- > , "cubeVars": [ { "name": <string>, "type": <string> }, ... ]
-- > , "topes":    [ <string>, ... ]              -- local tope assumptions (excludes ⊤)
-- > }
--
-- Notes for consumers:
--
-- * @shape@ is non-null only for a shape-restricted /argument/ goal, where the
--   goal reads @(binder : goal | tope)@. For an /extension/ type the boundary is
--   already part of @goal@ (a restricted type @A [ … ↦ … ]@), and @shape@ is
--   @null@ — read the boundary out of @goal@.
-- * @cubeVars@ names can be patterns like @\"(t, s)\"@ (pair-pattern binders);
--   they are display strings, not single identifiers.
data HoleData = HoleData
  { holeDataName     :: Maybe String           -- ^ the @?name@, if named
  , holeDataGoal     :: String                 -- ^ the goal (expected type)
  , holeDataShape    :: Maybe (String, String) -- ^ (binder, tope) for a shape-argument goal
  , holeDataTermVars :: [(String, String)]     -- ^ local hypotheses: (name, type)
  , holeDataCubeVars :: [(String, String)]     -- ^ local cube variables: (name, type)
  , holeDataTopes    :: [String]               -- ^ local tope assumptions (excluding ⊤)
  } deriving (Eq, Show)

instance ToJSON Severity where
  toJSON = String . \case
    SeverityError       -> "error"
    SeverityWarning     -> "warning"
    SeverityInformation -> "information"
    SeverityHint        -> "hint"

-- | Encode a location as JSON. A plain helper rather than a @ToJSON@ instance,
-- to avoid an orphan instance ('LocationInfo' is defined in "Rzk.TypeCheck").
locationToJSON :: LocationInfo -> Value
locationToJSON (LocationInfo path line) = object
  [ "file" .= path
  , "line" .= line
  ]

instance ToJSON Diagnostic where
  toJSON Diagnostic{..} = object
    [ "severity" .= diagnosticSeverity
    , "code"     .= diagnosticCode
    , "location" .= fmap locationToJSON diagnosticLocation
    , "message"  .= diagnosticMessage
    , "hole"     .= diagnosticHole
    ]

instance ToJSON HoleData where
  toJSON HoleData{..} = object
    [ "name"     .= holeDataName
    , "goal"     .= holeDataGoal
    , "shape"    .= fmap shapeToJSON holeDataShape
    , "termVars" .= map entryToJSON holeDataTermVars
    , "cubeVars" .= map entryToJSON holeDataCubeVars
    , "topes"    .= holeDataTopes
    ]
    where
      shapeToJSON (binder, tope) = object [ "binder" .= binder, "tope" .= tope ]
      entryToJSON (name, ty)     = object [ "name" .= name, "type" .= ty ]

-- | A stable tag for a type error, used as its diagnostic code. Independent of
-- the variable type, so it survives the scoped-error unfolding.
typeErrorTag :: TypeError n -> String
typeErrorTag = \case
  TypeErrorOther{}                 -> "TypeErrorOther"
  TypeErrorUnify{}                 -> "TypeErrorUnify"
  TypeErrorUnifyTerms{}            -> "TypeErrorUnifyTerms"
  TypeErrorNotPair{}               -> "TypeErrorNotPair"
  TypeErrorNotModal{}              -> "TypeErrorNotModal"
  TypeErrorModalityMismatch{}      -> "TypeErrorModalityMismatch"
  TypeErrorUnaccessibleVar{}       -> "TypeErrorUnaccessibleVar"
  TypeErrorNotTypeInModal{}        -> "TypeErrorNotTypeInModal"
  TypeErrorNotFunction{}           -> "TypeErrorNotFunction"
  TypeErrorUnexpectedLambda{}      -> "TypeErrorUnexpectedLambda"
  TypeErrorUnexpectedPair{}        -> "TypeErrorUnexpectedPair"
  TypeErrorUnexpectedRefl{}        -> "TypeErrorUnexpectedRefl"
  TypeErrorCannotInferBareLambda{} -> "TypeErrorCannotInferBareLambda"
  TypeErrorCannotInferBareRefl{}   -> "TypeErrorCannotInferBareRefl"
  TypeErrorCannotInferHole{}       -> "TypeErrorCannotInferHole"
  TypeErrorUnsolvedHole{}          -> "TypeErrorUnsolvedHole"
  TypeErrorUndefined{}             -> "TypeErrorUndefined"
  TypeErrorTopeNotSatisfied{}      -> "TypeErrorTopeNotSatisfied"
  TypeErrorTopeContextDisjoint{}   -> "TypeErrorTopeContextDisjoint"
  TypeErrorTopesNotEquivalent{}    -> "TypeErrorTopesNotEquivalent"
  TypeErrorInvalidArgumentType{}   -> "TypeErrorInvalidArgumentType"
  TypeErrorDuplicateTopLevel{}     -> "TypeErrorDuplicateTopLevel"
  TypeErrorUnusedVariable{}        -> "TypeErrorUnusedVariable"
  TypeErrorUnusedUsedVariables{}   -> "TypeErrorUnusedUsedVariables"
  TypeErrorImplicitAssumption{}    -> "TypeErrorImplicitAssumption"
  TypeErrorNotIntervalCube{}       -> "TypeErrorNotIntervalCube"
  TypeErrorRepeatedBinder{}        -> "TypeErrorRepeatedBinder"
  TypeErrorMatchScrutineeNotData{} -> "TypeErrorMatchScrutineeNotData"
  TypeErrorMatchCannotInfer{}      -> "TypeErrorMatchCannotInfer"
  TypeErrorMatchMissingBranch{}    -> "TypeErrorMatchMissingBranch"
  TypeErrorMatchDuplicateBranch{}  -> "TypeErrorMatchDuplicateBranch"
  TypeErrorMatchUnknownBranch{}    -> "TypeErrorMatchUnknownBranch"
  TypeErrorMatchBranchArity{}      -> "TypeErrorMatchBranchArity"
  TypeErrorEliminatorTypeMismatch{} -> "TypeErrorEliminatorTypeMismatch"

-- | The tag of a type error.
--
-- An error carries the context it was raised in, so there are no binder layers to
-- peel: the old representation nested the error one Inc deeper at every binder.
typeErrorTagInScopedContext :: TypeErrorInScopedContext -> String
typeErrorTagInScopedContext (TypeErrorInScopedContext _ctx err) = typeErrorTag err

-- | The source location of a type error (the enclosing command's line).
locationOfTypeError :: TypeErrorInScopedContext -> Maybe LocationInfo
locationOfTypeError (TypeErrorInScopedContext ctx _err) = ctxLocation ctx

-- | A structured diagnostic for a type error. The message is the usual
-- formatted error text; severity is always 'SeverityError'.
diagnoseTypeError :: OutputDirection -> TypeErrorInScopedContext -> Diagnostic
diagnoseTypeError dir err = Diagnostic
  { diagnosticSeverity = SeverityError
  , diagnosticCode     = typeErrorTagInScopedContext err
  , diagnosticLocation = locationOfTypeError err
  , diagnosticMessage  = ppTypeErrorInScopedContext dir err
  , diagnosticHole     = Nothing
  }

-- | A checker warning as a diagnostic: warning severity, with the warning's
-- constructor name as the stable code (like 'diagnoseTypeError' does for
-- errors).
diagnoseCheckWarning :: CheckWarning -> Diagnostic
diagnoseCheckWarning (LargeInductiveTypeWarning dataName conName loc) = Diagnostic
  { diagnosticSeverity = SeverityWarning
  , diagnosticCode     = "LargeInductiveTypeWarning"
  , diagnosticLocation = loc
  , diagnosticMessage  =
      "large inductive type: a field of constructor " <> show conName
        <> " stores a universe; predicatively " <> show dataName
        <> " lives above U"
  , diagnosticHole     = Nothing
  }
diagnoseCheckWarning (MetaPrefixWarning defName usedName supplied required rule loc) = Diagnostic
  { diagnosticSeverity = SeverityWarning
  , diagnosticCode     = case rule of
      MetaPrefixBoth       -> "MetaPrefixWarning"
      MetaPrefixStrictOnly -> "MetaPrefixWarningStrictOnly"
  , diagnosticLocation = loc
  , diagnosticMessage  =
      "meta-parameter layer: " <> show usedName <> " is used with "
        <> show supplied <> " of its " <> show required
        <> " meta-prefix arguments"
        <> (case rule of
              MetaPrefixBoth       -> " at an object-level position"
              MetaPrefixStrictOnly -> " outside another declaration's meta prefix")
        <> " (in " <> show defName <> ")"
  , diagnosticHole     = Nothing
  }

-- | A checker warning as a human-readable line (the CLI).
ppCheckWarning :: CheckWarning -> String
ppCheckWarning = ("Warning: " <>) . diagnosticMessage . diagnoseCheckWarning

-- | The stable tag of a warning (its diagnostic code), used to match
-- @warnings@ in fixtures; sharing the definition keeps the two from
-- drifting apart.
checkWarningTag :: CheckWarning -> String
checkWarningTag = diagnosticCode . diagnoseCheckWarning

-- | A structured diagnostic for a hole, carrying the hole's goal and local
-- context. A hole is an unfilled obligation, so it is a 'SeverityWarning' —
-- mirroring Agda's yellow \"unsolved\" highlight, and visible in the editor's
-- problems panel (unlike 'SeverityHint', which editors render almost
-- invisibly). Finished work still rejects holes outright: the strict default
-- of @rzk typecheck@ reports them as errors (cf. Agda's @--safe@).
diagnoseHole :: HoleInfo -> Diagnostic
diagnoseHole hole = Diagnostic
  { diagnosticSeverity = SeverityWarning
  , diagnosticCode     = "hole"
  , diagnosticLocation = holeLocation hole
  , diagnosticMessage  = ppHoleInfo hole
  , diagnosticHole     = Just (holeData hole)
  }

-- | The structured payload of a hole: its goal and local context rendered to
-- display strings (the same rendering 'ppHoleInfo' uses, but kept as separate
-- fields rather than concatenated into prose). See 'HoleData'.
holeData :: HoleInfo -> HoleData
holeData HoleInfo{..} = HoleData
  { holeDataName     = fmap show holeName
  , holeDataGoal     = show holeGoal
  , holeDataShape    = fmap (\(s, tope) -> (show s, show tope)) holeGoalShape
  , holeDataTermVars = map entry holeTermVars
  , holeDataCubeVars = map entry holeCubeVars
  , holeDataTopes    = map show holeTopes
  }
  where
    entry e = (show (holeEntryName e), show (holeEntryType e))

-- | Render a hole's goal and local context (the structured query) for display,
-- separating term variables, cube variables, and tope assumptions.
ppHoleInfo :: HoleInfo -> String
ppHoleInfo HoleInfo{..} = unlines $
  [ "Hole" <> maybe "" (\name -> " ?" <> show name) holeName
      <> maybe "" (\loc -> " at " <> ppLocationInfo loc) holeLocation
  , "  goal:"
  , "    " <> goalStr
  ]
  <> section "context" holeTermVars
  <> section "cube variables" holeCubeVars
  <> (if null holeTopes
        then []
        else "  tope context:" : [ "    " <> show t | t <- holeTopes ])
  where
    -- a shape goal reads (binder : cube | tope); otherwise just the type
    goalStr = case holeGoalShape of
      Nothing        -> show holeGoal
      Just (s, tope) -> "(" <> show s <> " : " <> show holeGoal <> " | " <> show tope <> ")"
    section title entries
      | null entries = []
      | otherwise = ("  " <> title <> ":")
          : [ "    " <> show (holeEntryName e) <> " : " <> show (holeEntryType e)
            | e <- entries ]

ppLocationInfo :: LocationInfo -> String
ppLocationInfo (LocationInfo mpath mline) =
  maybe "<input>" id mpath <> maybe "" ((":" <>) . show) mline
