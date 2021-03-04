{-# LANGUAGE LambdaCase #-}
module Rzk.Free.TypeCheck.TypeError where

import           Bound
import           Data.Text            (Text)

import           Rzk.Free.Bound.Name
import           Rzk.Free.Pretty      ()
import           Rzk.Free.Syntax.Term

data TypeError b a
  = TypeErrorInfinite a (Term b a)
  | TypeErrorUnexpected (Term b a) (Term b a) (Term b a) (Term b a) (Term b a)
  | TypeErrorUndefinedVariable a
  | TypeErrorCannotInferLambda (Term b a)
  | TypeErrorCannotInferPair (Term b a)
  | TypeErrorNotAFunction (Term b a) (Term b a) (Term b a)
  | TypeErrorNotAPair (Term b a) (Term b a) (Term b a)
  | TypeErrorExpectedFunctionType (Term b a) (Term b a)
  | TypeErrorInvalidTypeFamily
  | TypeErrorTopeContextNotSatisfied (Term b a) (Term b a) [Term b a]
  | TypeErrorOther Text
  | TypeErrorScope (TypeError b (Var (Name b ()) a))
  -- deriving (Show)


-- instance (Pretty b, Pretty a, IsString b) => Pretty (TypeError b a) where
--   pretty = ppTypeError
--
-- ppTypeError :: (Pretty a, Pretty b) => TypeError b a -> Doc ann
-- ppTypeError = \case
--   TypeErrorInfinite x t -> Text.intercalate "\n"
--     [ "Can't construct infinite type " <> pretty x <> " ~ " <> ppTerm t ]
--   TypeErrorUnexpected term inferredFull expectedFull inferred expected -> Text.intercalate "\n"
--     [ "Expected type"
--     , "  " <> ppTerm expected
--     , "but inferred"
--     , "  " <> ppTerm inferred
--     , "when trying to unify expected type"
--     , "  " <> ppTerm expectedFull
--     , "with inferred type"
--     , "  " <> ppTerm inferredFull
--     , "for the term"
--     , "  " <> ppTerm term
--     ]
--   TypeErrorUndefinedVariable x -> Text.intercalate "\n"
--     [ "Undefined variable:"
--     , "    " <> pretty x
--     ]
--   TypeErrorOther msg -> "Error occurred in the typechecker: " <> msg
--   TypeErrorCannotInferLambda t -> Text.intercalate "\n"
--     [ "Error while attempting to infer the type for a lambda abstraction"
--     , "  " <> ppTerm t
--     ]
--   TypeErrorCannotInferPair t -> Text.intercalate "\n"
--     [ "Error while attempting to infer the type for a dependent tuple"
--     , "  " <> ppTerm t
--     ]
--   TypeErrorNotAFunction f t e -> Text.intercalate "\n"
--     [ "Expected a function type but got"
--     , "  " <> ppTerm t
--     , "for the term"
--     , "  " <> ppTerm f
--     , "in expression"
--     , "  " <> ppTerm (App f e)
--     ]
--   TypeErrorNotAPair f t e -> Text.intercalate "\n"
--     [ "Expected a dependent pair (sum) type or a cube product but got"
--     , "  " <> ppTerm t
--     , "for the term"
--     , "  " <> ppTerm f
--     , "in expression"
--     , "  " <> ppTerm e
--     ]
--   TypeErrorExpectedFunctionType term expected -> Text.intercalate "\n"
--     [ "Expected type is not a function type"
--     , "  " <> ppTerm expected
--     , "but the term is a lambda abstraction"
--     , "   " <> ppTerm term
--     ]
--   TypeErrorInvalidTypeFamily -> "Expected a type family, but got something else" -- FIXME
--   TypeErrorTopeContextNotSatisfied term phi topes -> Text.intercalate "\n"
--     [ "Cannot satisfy the tope constraint:"
--     , "  " <> ppTerm phi
--     , "in local tope context"
--     , Text.intercalate "\n" (map (("  " <>) . ppTerm) topes)
--     , "when typechecking term"
--     , "  " <> ppTerm term
--     ]
--
--
