-- | Facts about elaborated terms, from which the RSTT checks are certified
-- clean without being run.
--
-- The standalone checks ("Rzk.TypeCheck.Fragment.RSTT",
-- "Rzk.TypeCheck.Schematic", "Rzk.TypeCheck.MetaPrefix") walk a declaration
-- and ask the evaluator about the terms they meet. The facts answer the same
-- questions from the structure of the terms and from the facts of the
-- top-level definitions they mention. A definition's facts are computed once,
-- with its parameters symbolic (its 'Table'); a fact that depends on the
-- argument for a parameter records a need, which a use settles against the
-- argument.
--
-- The facts of a node are computed from those of its children, lazily, so
-- only what a check asks for is computed: they are synthesised attributes in
-- the sense of D. E. Knuth, Semantics of context-free languages, Mathematical
-- Systems Theory 2(2), 1968. The tables play the part of procedure summaries
-- in M. Sharir and A. Pnueli, Two approaches to interprocedural data flow
-- analysis, in S. S. Muchnick and N. D. Jones (eds.), Program Flow Analysis:
-- Theory and Applications, Prentice-Hall, 1981.
--
-- A certificate is one-sided: 'Clean' means that the standalone check would
-- report nothing and would not fail, and anything else means that it runs.
-- So the facts never reproduce a warning; they recognise when there is none.
-- Each certificate follows the decisions of its standalone check, which is
-- the specification.
--
-- The standalone checks ask the evaluator ("Rzk.TypeCheck.Eval": @whnfT@,
-- @fragmentHead@, @isMetaType@) and report an incomplete check when a query
-- fails. The facts are computed without the evaluator and cannot see such a
-- failure: 'Clean' assumes that the queries succeed on well-typed terms.
-- Running out of fuel is the exception that is modelled, by the limits in
-- "Rzk.TypeCheck.Fragment.Facts.Expose".
module Rzk.TypeCheck.Fragment.Facts (
  -- * Building facts
  VarFacts,
  topLevelFacts,
  DeclFacts,
  declarationFactsIn,
  -- * Verdicts
  FragmentUse (..),
  PrefixPosition (..),
  Verdict (..),
  isClean,
  verdictReason,
  -- * The fragment check
  fragmentVerdict,
  -- * The schematic check
  SVerdict (..),
  SchematicCertificate (..),
  declarationCertificate,
  entryCertificate,
  schematicDepthLimit,
  -- * The meta-parameter layer check
  metaPrefixVerdict,
) where

import qualified Data.IntMap                             as IntMap
import           Data.Maybe                              (isJust)

import           Control.Monad.Foil                      (Distinct)
import qualified Control.Monad.Foil                      as Foil

import           Language.Rzk.Foil.Names                 (binderName)
import           Language.Rzk.Foil.Syntax                (TermT)
import           Rzk.TypeCheck.Context                   (VarInfo (..))
import           Rzk.TypeCheck.Fragment.Facts.Build
import           Rzk.TypeCheck.Fragment.Facts.Expose
import           Rzk.TypeCheck.Fragment.Facts.Fragment
import           Rzk.TypeCheck.Fragment.Facts.MetaPrefix
import           Rzk.TypeCheck.Fragment.Facts.Schematic
import           Rzk.TypeCheck.Fragment.Facts.Types

-- | The equations of the memo fields, from the modules of the checks.
attrs :: Attrs
attrs = Attrs
  { attrDerived = derivedOf
  , attrVerdicts = verdictsOf
  , attrSchem = schemOf
  , attrScopedVerdicts = scopedVerdictsOf
  , attrScopedSchem = scopedSchemOf
  }

-- | The environment of a term with no variables bound yet.
outerEnv :: Foil.Scope n -> IntMap.IntMap VarFacts -> Int -> Env n
outerEnv scope outer owner = Env
  { envScope = scope
  , envBound = IntMap.empty
  , envOuter = outer
  , envOwner = owner
  , envAttrs = attrs
  }

-- | The facts of a top-level entry, with its table if it has a value. The
-- entries it mentions are looked up in the map given, by name identifier.
topLevelFacts
  :: Distinct n => IntMap.IntMap VarFacts -> Foil.Scope n -> Int -> VarInfo n -> VarFacts
topLevelFacts outer scope i info = v
  where
    -- The entry's own terms and its table have owners of their own.
    env = outerEnv scope outer (2 * i + 1)
    tableEnv = outerEnv scope outer (2 * i + 2)
    name = maybe "_" show (binderName (varOrig info))
    v = VarFacts
      { vfKey = i
      , vfOrig = varOrig info
      , vfType = facts env (varType info)
      , vfSchemType = vfType v
      , vfValue = facts env <$> varValue info
      , vfTop = Just TopLevel
          { tlTable = buildTable tableEnv i name (vfType v) <$> varValue info
          , tlDataRole = isJust (varDataRole info)
          , tlMetaPrefix = varMetaPrefix info
          , tlObjectFamily = objectFamilyOf (vfType v) (vfValue v)
          }
      , vfReach = reachOf i (vfType v) (vfValue v)
      }

-- | The facts of a declaration under check, with the given owner for the
-- variables bound in it (see 'envOwner').
declarationFactsIn
  :: Distinct n
  => Foil.Scope n -> IntMap.IntMap VarFacts -> Int -> TermT n -> Maybe (TermT n) -> DeclFacts
declarationFactsIn scope outer owner ty mval = DeclFacts
  { declType = facts env ty
  , declValue = facts env <$> mval
  }
  where
    env = outerEnv scope outer owner

-- | The schematic certificate of a declaration under check.
declarationCertificate :: DeclFacts -> SchematicCertificate
declarationCertificate decl = schematicCertificate (declType decl) (declValue decl)

-- | The schematic certificate of a top-level entry.
entryCertificate :: VarFacts -> SchematicCertificate
entryCertificate v = schematicCertificate (vfType v) (vfValue v)
