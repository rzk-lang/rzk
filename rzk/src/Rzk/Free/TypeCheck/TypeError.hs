{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Rzk.Free.TypeCheck.TypeError where

import           Bound
import           Data.String
import           Data.Text.Prettyprint.Doc

import           Rzk.Free.Bound.Name
import           Rzk.Free.Pretty
import           Rzk.Free.Syntax.Term

data TypeError b a
  = TypeErrorUnexpected (TypedTerm b a) (TypedTerm b a)
  | TypeErrorCannotInferLambda (Term b a)
  | TypeErrorNotAFunction (TypedTerm b a)
  | TypeErrorScope (TypeError b (Var (Name b ()) a))
  deriving (Functor)

instance (IsString a, Pretty a, Pretty b) => Show (TypeError b a) where
  show = show . pretty

instance (IsString a, Pretty a, Pretty b) => Pretty (TypeError b a) where
  pretty = ppTypeError defaultFreshVars

ppTypeErrorScope
  :: (IsString a, Pretty a, Pretty b)
  => [a] -> TypeError b (Var (Name b ()) a) -> Doc ann
ppTypeErrorScope [] _err = error "not enough fresh variables"
ppTypeErrorScope (z:zs) err = ppTypeError zs (prettyVar <$> err)
  where
    prettyVar (B _) = z
    prettyVar (F x) = x

ppTypeError
  :: (IsString a, Pretty a, Pretty b)
  => [a] -> TypeError b a -> Doc ann
ppTypeError vars = \case
  TypeErrorScope err -> ppTypeErrorScope vars err

  TypeErrorUnexpected actual expected -> vsep
    [ "expected"
    , indent 2 (ppTerm vars (untyped expected))
    , "but found"
    , indent 2 (ppTerm vars (untyped actual))
    ]

  TypeErrorCannotInferLambda term -> vsep
    [ "cannot infer the type of lambda abstraction"
    , indent 2 (ppTerm vars term)
    ]

  TypeErrorNotAFunction _ -> undefined

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
