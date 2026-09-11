{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The RSTT fragment check: which restrictions may be assumed, and where
-- schematic variables may be bound.
--
-- One module per fragment: this one is for RSTT proper, the system the
-- conservativity result is about. The modal and inductive extensions of the
-- implementation are outside it and are not checked here (see
-- @rzk-paper/notes/rstt-safe-mode-design.md@).
--
-- Conservativity over RSTT is proved for the derivations that use
-- free-standing restrictions /positively/ only (§5 of the Rzk paper,
-- Kudasov, Sim, Ahrens, \"Rzk: a Proof Assistant for Synthetic
-- ∞-Categories\", <https://arxiv.org/abs/2607.12207 arXiv:2607.12207>). A
-- restriction is /ext-style/ when it is the codomain of a shape-Π, and a
-- type is ext-style when all of its restrictions are; a /tail type/ may
-- also carry restrictions along the spine of its codomains. A derivation
-- has /ext-style hypotheses/ when every binder binds at an ext-style type,
-- every context type is ext-style, and every concluded type is a tail type.
-- Intuitively, a free-standing restriction may be concluded but not
-- assumed. This module reports the two ways a declaration can leave the
-- fragment:
--
-- - a free-standing restriction is assumed, as the type of a binder, as an
--   eliminator motive, or inside a type passed as data
--   ('FreeStandingRestrictionWarning'); the last route matters because a
--   restricted type stored at @U@ is substituted into binder and motive
--   positions later;
-- - a variable is bound at a meta type (@U@, @CUBE@, @TOPE@, or a function
--   into one) by a λ inside a term rather than in the declaration's
--   parameter prefix ('MetaBinderWarning'), so the declaration does not
--   read as a family of object-theory statements, one per instantiation.
--   Two positions are still the parameter prefix: the branches of a case
--   split reached while peeling it, since a schema defined by recursion on a
--   schematic index has to split before binding its remaining parameters
--   (both end up in the meta prefix, so uses supply both); and an argument at
--   a schematic parameter, which is the plumbing the meta-prefix check
--   allows.
--
-- Positions are classified on the elaborated declaration. The type is
-- walked as a concluded type, except for an @#assume@, whose type enters
-- the context and must be ext-style. A type that elaboration /synthesised/
-- is concluded, not assumed, and is skipped or walked as a concluded type:
-- the type a @refl@ node stores for its element, and a @let@ annotation,
-- are @typeOf@ of a term (see the @Refl@ and @Let@ cases of 'typecheck'),
-- and a @let@ binder stands for its value. Otherwise a section of an
-- extension type applied at a point, whose type carries a restriction by
-- instantiation, would be reported wherever it is named. The walk is syntactic, as the
-- meta-prefix check is: a restriction that only appears after unfolding a
-- definition or reducing a redex is not seen. A restriction that overhangs
-- its shape tope is also not ext-style; that condition is reported
-- separately, by the overhang hint (@warn-overhang@), at every occurrence.
module Rzk.TypeCheck.Fragment.RSTT (
  recordFragmentUses,
) where

import           Control.Applicative      ((<|>))
import           Control.Monad            (forM_, unless, when)
import           Control.Monad.Except     (catchError)
import           Control.Monad.Reader     (asks)
import           Data.Bifoldable          (bifoldr)
import           Data.Maybe               (fromMaybe)

import           Control.Monad.Foil       (Distinct)
import           Control.Monad.Free.Foil  (AST (Node, Var))

import           Control.Monad.Free.Foil.Annotated (AnnSig (..))
import           Language.Rzk.Foil.Names  (Binder, TModality (..),
                                           TypeInfo (..), VarIdent, binderName)
import           Language.Rzk.Foil.Syntax
import           Rzk.TypeCheck.Context
import           Rzk.TypeCheck.Display
import           Rzk.TypeCheck.Eval
import           Rzk.TypeCheck.MetaPrefix (isMetaType)
import           Rzk.TypeCheck.Monad

-- | Where the walk is. A concluded type may carry restrictions along the
-- spine of its codomains; an assumed type may not carry any, and records
-- what assumes it; a term is walked for its binders and for the types it
-- passes as data.
data Pos
  = Tail
  | Assumed FragmentUse
  | Term Bool
    -- ^ 'True' while still peeling the value's leading λs, which bind the
    -- declaration's own parameters.

-- | Walk a declaration's elaborated type and value, reporting assumed
-- free-standing restrictions and schematic binders inside terms.
-- Advisory: never throws, and runs silently so that WHNF probes do not
-- trace.
recordFragmentUses
  :: forall n. Distinct n
  => VarIdent          -- ^ the declaration
  -> TermT n           -- ^ its type
  -> Maybe (TermT n)   -- ^ its value, for a definition
  -> Bool              -- ^ is it an assumption (an @#assume@)?
  -> TypeCheck n ()
recordFragmentUses defName ty mval isAssumption = do
  restrictions <- asks ctxWarnFreeStandingRestriction
  binders <- asks ctxWarnMetaBinder
  when (restrictions || binders) $
    localVerbosity Silent $ flip catchError (\_ -> pure ()) $ do
      go (if isAssumption then Assumed UseBinder else Tail) ty
      mapM_ (go (Term True)) mval
  where
    go :: forall l. Distinct l => Pos -> TermT l -> TypeCheck l ()
    go (Term inPrefix) t = goTerm inPrefix t
    go Tail t = goTail t
    go (Assumed use) t = goAssumed use t

    -- A concluded type: restrictions along the spine of codomains are the
    -- positive use that conservativity allows.
    goTail :: forall l. Distinct l => TermT l -> TypeCheck l ()
    goTail t = case t of
      TypeRestrictedT _ ty' rs -> do
        goTail ty'
        forM_ rs $ \(_tope, term) -> goTerm False term
      TypeFunT _ orig md param _mtope ret -> do
        goAssumed UseBinder param
        inScope orig md param ret goTail
      _ -> goAssumed UseConcluded t

    -- A type that must be ext-style: a restriction here is free-standing,
    -- except on the codomain of a shape-Π.
    goAssumed :: forall l. Distinct l => FragmentUse -> TermT l -> TypeCheck l ()
    goAssumed use t = case t of
      TypeRestrictedT _ ty' rs -> do
        reportFreeStanding use t
        goAssumed use ty'
        forM_ rs $ \(_tope, term) -> goTerm False term
      TypeFunT _ orig md param mtope ret -> do
        goAssumed UseBinder param
        shape <- isShapeBinder (maybe False (const True) mtope) param
        inScope orig md param ret $ \retIn ->
          if shape then goExtCodomain use retIn else goAssumed use retIn
      TypeSigmaT _ orig md a b -> do
        goAssumed UseBinder a
        inScope orig md a b (goAssumed use)
      -- The endpoints are terms, typed by the type argument. Def. 5.7 does
      -- not reach a restriction there: transporting a boundary equation
      -- along an identity is an @ap@, definitional on @refl@ only.
      TypeIdT _ a mtA b -> do
        goTerm False a
        mapM_ (goAssumed UseIdentity) mtA
        goTerm False b
      LambdaT info orig mparam body -> do
        md <- case mparam of
          Just (LambdaParam md param _mtope) -> do
            goAssumed UseBinder param
            pure md
          Nothing -> pure Id
        dom <- binderType info mparam
        inScope orig md dom body (goAssumed use)
      TypeModalT _ _ ty' -> goAssumed use ty'
      Var{} -> pure ()
      _ -> goTerm False t

    -- Directly under a shape-Π: one restriction is ext-style here.
    goExtCodomain
      :: forall l. Distinct l => FragmentUse -> TermT l -> TypeCheck l ()
    goExtCodomain use t = case t of
      TypeRestrictedT _ ty' rs -> do
        goAssumed use ty'
        forM_ rs $ \(_tope, term) -> goTerm False term
      _ -> goAssumed use t

    -- A term: its binders, and the types it passes as data. The body a
    -- declaration's parameters lead to is itself data when its type is meta,
    -- as in a definition at @U@ whose body is a type.
    goTerm :: forall l. Distinct l => Bool -> TermT l -> TypeCheck l ()
    goTerm True t | not (isLambda t) = goData t
    goTerm inPrefix t = case t of
      Var{} -> pure ()

      -- The element of a @refl@; its type annotation is the concluded type
      -- of that element, inserted by elaboration, not a type passed as data.
      ReflT _ mx -> forM_ mx $ \(x, _mxty) -> goTerm False x

      TypeAscT _ term' ty' -> do
        goTerm False term'
        goTail ty'

      LambdaT info orig mparam body -> do
        md <- case mparam of
          Just (LambdaParam md param _mtope) -> do
            goAssumed UseBinder param
            unless inPrefix $ reportMetaBinder orig param
            pure md
          Nothing -> pure Id
        dom <- binderType info mparam
        inScope orig md dom body (goTerm inPrefix)

      AppT{} -> do
        let (h, args) = collectSpine t []
        goTerm False h
        forM_ args $ \(fnode, arg) -> do
          -- Supplying a schema at a schematic parameter is the plumbing that
          -- the meta-prefix check allows, so its binders are not reported.
          plumbing <- domainIsMeta fnode
          if plumbing then goTerm True arg else goData arg

      -- The motive is assumed: the eliminator binds its variables at it.
      IdJT _ tA a tC d x p -> do
        goData tA
        goTerm False a
        goAssumed UseMotive tC
        goTerm False d
        goTerm False x
        goTerm False p

      -- A case split reached while still in the parameter prefix is a split
      -- on a schematic index: the declaration is a schema defined by
      -- recursion, and the branches bind its remaining parameters, which the
      -- recursion forces to be written after the split.
      MatchT _ scrut mmotive branches -> do
        goTerm False scrut
        mapM_ (goAssumed UseMotive) mmotive
        forM_ branches $ \(_con, branch) -> goTerm inPrefix branch

      PairT _ l r -> do
        goData l
        goData r

      LetT _ orig manno value body -> do
        mapM_ goTail manno
        goData value
        let dom = fromMaybe universeT (manno <|> valueType value)
        inScopeWith orig Id dom (Just value) body (goTerm False)

      LetModT _ orig _nu mu manno mmotive value body -> do
        mapM_ goTail manno
        mapM_ (goAssumed UseMotive) mmotive
        goData value
        inScope orig mu (fromMaybe universeT (manno <|> valueType value)) body
          (goTerm False)

      Node (AnnSig _ f) ->
        mapM_ (goTerm False) (bifoldr (\_ acc -> acc) (:) [] f)

    -- An argument or component. A /type/ passed here becomes data, and is
    -- substituted into binder and motive positions later. A proof of a
    -- statement quantified over a universe is not such a type, even though
    -- its type is meta-shaped in the sense of "Rzk.TypeCheck.MetaPrefix": it
    -- stays a term, so that its binders are checked.
    goData :: forall l. Distinct l => TermT l -> TypeCheck l ()
    goData t = do
      isType <- landsInUniverse t
      if isType then goAssumed UseData t else goTerm False t

    isLambda :: forall l. TermT l -> Bool
    isLambda LambdaT{} = True
    isLambda _         = False

    valueType :: forall l. TermT l -> Maybe (TermT l)
    valueType value = infoType <$> typeInfoOf value

    binderType
      :: forall l. Distinct l
      => TypeInfo (TermT l) -> Maybe (LambdaParam (ScopedTermT l) (TermT l))
      -> TypeCheck l (TermT l)
    binderType _info (Just (LambdaParam _ param _mtope)) = pure param
    binderType info Nothing = fromMaybe universeT <$> funDomain (infoType info)

    -- Does this Π bind a point of a shape? Either it carries a tope, or
    -- its domain is a cube, which is the shape with the tope ⊤.
    isShapeBinder :: forall l. Distinct l => Bool -> TermT l -> TypeCheck l Bool
    isShapeBinder True _ = pure True
    isShapeBinder False param = flip catchError (\_ -> pure False) $
      typeOfUncomputed param >>= whnfT >>= \case
        UniverseCubeT{} -> pure True
        _               -> pure False

    -- Is the Π-domain of this function node schematic?
    domainIsMeta :: forall l. Distinct l => TermT l -> TypeCheck l Bool
    domainIsMeta f = flip catchError (\_ -> pure False) $ do
      tf <- typeOfUncomputed f
      whnfT (stripTypeRestrictions tf) >>= \case
        TypeFunT _ _ _ dom _ _ -> isMetaType dom
        _                      -> pure False

    -- Is this term a type, or a family of types? That is, does its type land
    -- in a universe once its Π prefix is stripped.
    landsInUniverse :: forall l. Distinct l => TermT l -> TypeCheck l Bool
    landsInUniverse t = flip catchError (\_ -> pure False) $
      typeOfUncomputed t >>= go
      where
        go :: forall k. Distinct k => TermT k -> TypeCheck k Bool
        go ty = whnfT (stripTypeRestrictions ty) >>= \case
          UniverseT{}     -> pure True
          UniverseCubeT{} -> pure True
          UniverseTopeT{} -> pure True
          TypeFunT _ orig md param _mtope ret -> inScope orig md param ret go
          _               -> pure False

    funDomain :: forall l. Distinct l => TermT l -> TypeCheck l (Maybe (TermT l))
    funDomain tf = flip catchError (\_ -> pure Nothing) $
      whnfT (stripTypeRestrictions tf) >>= \case
        TypeFunT _ _ _ dom _ _ -> pure (Just dom)
        _                      -> pure Nothing

    -- A λ below the leading λs of the value binds a schematic variable where
    -- the parameter prefix should.
    reportMetaBinder :: forall l. Distinct l => Binder -> TermT l -> TypeCheck l ()
    reportMetaBinder orig param = do
      enabled <- asks ctxWarnMetaBinder
      when enabled $ do
        meta <- flip catchError (\_ -> pure False) (isMetaType param)
        when meta $ do
          naming <- asks namingOfContext
          loc <- asks ctxLocation
          recordCheckWarning $ MetaBinderWarning
            defName
            (fromMaybe "_" (binderName orig))
            (ppTerm naming (untyped param))
            loc

    reportFreeStanding :: forall l. FragmentUse -> TermT l -> TypeCheck l ()
    reportFreeStanding use t = do
      enabled <- asks ctxWarnFreeStandingRestriction
      when enabled $ do
        naming <- asks namingOfContext
        loc <- asks ctxLocation
        recordCheckWarning $
          FreeStandingRestrictionWarning defName (ppTerm naming (untyped t)) use loc

collectSpine :: TermT n -> [(TermT n, TermT n)] -> (TermT n, [(TermT n, TermT n)])
collectSpine (AppT _ f x) acc = collectSpine f ((f, x) : acc)
collectSpine h acc            = (h, acc)
