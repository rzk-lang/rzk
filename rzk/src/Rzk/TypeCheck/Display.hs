{-# LANGUAGE OverloadedStrings #-}

-- | Showing a term to the user.
--
-- A term of the core names its variables by 'Foil.Name' (an @Int@), so anything
-- user-facing — an error, a trace of a judgement, a hole's goal — has to say what
-- each name is /called/. That is a 'Naming': a display name and a display binder
-- per name in scope, plus the supply of fresh names for the binders the printer
-- meets on the way down.
--
-- This replaces the old @var -> VarIdent@ threading (@name@, @nameInc@,
-- @BinderNames@, @ppTermInContext@ — four copies of the same idea), and with it
-- the unwinding loop that rebuilt those names one binder at a time.
module Rzk.TypeCheck.Display where

import           Control.Monad.Foil          (NameMap)
import qualified Control.Monad.Foil          as Foil
import           Control.Monad.Foil.Internal (NameMap (..))
import qualified Data.IntMap                 as IntMap
import           Data.List                   ((\\))

import           Language.Rzk.Foil.Print     (Display, fromTerm)
import           Language.Rzk.Foil.Syntax
import           Language.Rzk.Free.Syntax    (Binder (..), TypeInfo (..),
                                              VarIdent, binderIsCompound,
                                              binderLeaves, binderToPattern,
                                              defaultVarIdents,
                                              freshenBinderLeaves, fromVarIdent,
                                              refreshVar)
import qualified Language.Rzk.Syntax         as Rzk
import           Rzk.TypeCheck.Context

-- | What every name in scope is called, and what the printer may call the
-- binders it has yet to meet.
data Naming n = Naming
  { namingOf     :: NameMap n Display
  , namingUsed   :: [VarIdent]
    -- ^ the display names taken, so a bound binder is refreshed away from them
  , namingSupply :: [VarIdent]
    -- ^ the names left over, for anonymous binders
  }

-- | Read the naming off a context.
--
-- A named binder keeps its name, refreshed only if an outer name has already
-- taken it. An anonymous one draws from the supply. A pattern binder has its
-- component names freshened as a group, so that the pattern shown in the context
-- and the projections folded inside a term agree on them.
--
-- Entries are named oldest binding first (see 'ctxBound'), so an outer binder
-- keeps its name and an inner one is the one refreshed away from it.
namingOfContext :: Context n -> Naming n
namingOfContext ctx = Naming
  { namingOf = NameMap (IntMap.fromList entries)
  , namingUsed = used
  , namingSupply = defaultVarIdents \\ used
  }
  where
    (entries, used) = go [] defaultVarIdents (varsInScope ctx)

    -- Name the entries in binding order, each avoiding the names already taken.
    go taken _supply [] = ([], taken)
    go taken supply ((v, info) : rest) =
      case varOrig info of
        BinderVar (Just x) ->
          let x' = refreshVar taken x
           in name (x', BinderVar (Just x')) [x'] supply
        BinderVar Nothing ->
          case supply of
            x : supply' -> name (x, BinderVar (Just x)) [x] supply'
            []          -> panicImpossible "not enough fresh variables"
        binder ->
          -- A pattern binder: the variable itself needs a placeholder name (it is
          -- only shown when the whole point is used, and then it prints as the
          -- pattern), and its leaves are freshened together.
          case supply of
            x : supply' ->
              let binder' = freshenBinderLeaves taken binder
               in name (x, binder') (x : binderLeaves binder') supply'
            [] -> panicImpossible "not enough fresh variables"
      where
        name display claimed supply' =
          let (acc, taken') = go (claimed <> taken) supply' rest
           in ((Foil.nameId v, display) : acc, taken')

-- | A term already rendered for the user, kept as surface syntax rather than a
-- string so that a consumer may still inspect it.
--
-- Its 'Show' prints the surface syntax, which is what the old @Term'@ did, so a
-- rendered goal or candidate reads the same as it always has (@\\ (t, s) → ?@).
newtype Rendered = Rendered { getRendered :: Rzk.Term }

instance Show Rendered where
  show = Rzk.printTree . getRendered

-- | Two rendered terms are equal when they read the same. (The surface AST
-- carries source positions, which a rendered term should not be judged by.)
instance Eq Rendered where
  l == r = show l == show r

-- | A term as surface syntax, with the context's names.
renderTerm :: Naming n -> Term n -> Rendered
renderTerm naming =
  Rendered . fromTerm (namingUsed naming) (namingSupply naming) (namingOf naming)

-- | A term shown to the user.
ppTerm :: Naming n -> Term n -> String
ppTerm naming = show . renderTerm naming

-- | A typed term shown as @term : type@, as the old @ppFoldT@ did. A variable is
-- shown bare: its type is in the context, not on the node.
ppTermT :: Naming n -> TermT n -> String
ppTermT naming t =
  case typeInfoOf t of
    Nothing   -> ppTerm naming (untyped t)
    Just info -> ppTerm naming (untyped t) <> " : " <> ppTerm naming (untyped (infoType info))

-- | What a name is called, and the (freshened) binder it was introduced by.
displayOf :: Naming n -> Foil.Name n -> Display
displayOf naming name = Foil.lookupName name (namingOf naming)

-- | A variable as the user sees it: a pattern binder shows as its pattern
-- (@(t, s)@), anything else by its display name.
ppName :: Naming n -> Foil.Name n -> String
ppName naming name =
  case Foil.lookupName name (namingOf naming) of
    (_, binder) | binderIsCompound binder -> Rzk.printTree (binderToPattern binder)
    (x, _)                                -> Rzk.printTree (fromVarIdent x)

panicImpossible :: String -> a
panicImpossible msg = error $ unlines
  [ "PANIC! Impossible happened (" <> msg <> ")!"
  , "Please, report a bug at https://github.com/rzk-lang/rzk/issues"
  ]
