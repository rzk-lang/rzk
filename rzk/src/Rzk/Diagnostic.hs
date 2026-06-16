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

import           Language.Rzk.Free.Syntax (VarIdent)
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
data Diagnostic = Diagnostic
  { diagnosticSeverity :: Severity
  , diagnosticCode     :: String            -- ^ stable category, e.g. @\"TypeErrorUnify\"@ or @\"hole\"@
  , diagnosticLocation :: Maybe LocationInfo -- ^ file + line (line-level granularity)
  , diagnosticMessage  :: String
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
    ]

-- | A stable tag for a type error, used as its diagnostic code. Independent of
-- the variable type, so it survives the scoped-error unfolding.
typeErrorTag :: TypeError var -> String
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

-- | The tag of a scoped type error (peels the binder layers; the tag does not
-- depend on the variable type).
typeErrorTagInScopedContext :: TypeErrorInScopedContext var -> String
typeErrorTagInScopedContext = \case
  PlainTypeError e    -> typeErrorTag (typeErrorError e)
  ScopedTypeError _ e -> typeErrorTagInScopedContext e

-- | The source location of a scoped type error (the enclosing command's line).
locationOfTypeError :: TypeErrorInScopedContext var -> Maybe LocationInfo
locationOfTypeError = \case
  PlainTypeError e    -> location (typeErrorContext e)
  ScopedTypeError _ e -> locationOfTypeError e

-- | A structured diagnostic for a type error. The message is the usual
-- formatted error text; severity is always 'SeverityError'.
diagnoseTypeError :: OutputDirection -> TypeErrorInScopedContext VarIdent -> Diagnostic
diagnoseTypeError dir err = Diagnostic
  { diagnosticSeverity = SeverityError
  , diagnosticCode     = typeErrorTagInScopedContext err
  , diagnosticLocation = locationOfTypeError err
  , diagnosticMessage  = ppTypeErrorInScopedContext' dir err
  }

-- | A structured diagnostic for a hole, carrying the hole's goal and local
-- context. Uses 'SeverityInformation' rather than 'SeverityHint': editors
-- (VS Code in particular) render hints almost invisibly — no entry in the
-- problems panel and only a faint decoration — whereas the whole point of a
-- hole diagnostic is to show its goal where the user can see it.
diagnoseHole :: HoleInfo -> Diagnostic
diagnoseHole hole = Diagnostic
  { diagnosticSeverity = SeverityInformation
  , diagnosticCode     = "hole"
  , diagnosticLocation = holeLocation hole
  , diagnosticMessage  = ppHoleInfo hole
  }

-- | Render a hole's goal and local context (the structured query) for display,
-- separating term variables, cube variables, and tope assumptions.
ppHoleInfo :: HoleInfo -> String
ppHoleInfo HoleInfo{..} = unlines $
  [ "Hole" <> maybe "" (\name -> " ?" <> show name) holeName
      <> maybe "" (\loc -> " at " <> ppLocationInfo loc) holeLocation
  , "  goal:"
  , "    " <> show holeGoal
  ]
  <> section "context" holeTermVars
  <> section "cube variables" holeCubeVars
  <> (if null holeTopes
        then []
        else "  tope context:" : [ "    " <> show t | t <- holeTopes ])
  where
    section title entries
      | null entries = []
      | otherwise = ("  " <> title <> ":")
          : [ "    " <> show (holeEntryName e) <> " : " <> show (holeEntryType e)
            | e <- entries ]

ppLocationInfo :: LocationInfo -> String
ppLocationInfo (LocationInfo mpath mline) =
  maybe "<input>" id mpath <> maybe "" ((":" <>) . show) mline
