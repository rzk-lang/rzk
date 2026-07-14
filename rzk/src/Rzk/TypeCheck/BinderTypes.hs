{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The elaborated types of a declaration's local binders, for LSP hover.
--
-- Every node of a typed term carries its type, so even a bare λ's binder is typed
-- — by the domain of the λ's own Π-type.
--
-- The old version threaded a @BinderNames@ environment down through the term, to
-- say what each variable it met was called. There is no need: entering a binder
-- puts it in the context, and the context already knows what everything in it is
-- called (see "Rzk.TypeCheck.Display").
module Rzk.TypeCheck.BinderTypes where

import           Control.Monad.Except     (catchError)
import           Control.Monad.Reader     (asks)
import           Data.Bifoldable          (bifoldr)
import           Data.Maybe               (fromMaybe)

import           Control.Monad.Foil       (Distinct)
import           Control.Monad.Free.Foil  (AST (Node, Var))

import           Language.Rzk.Foil.Syntax
import           Language.Rzk.Foil.Names (Binder (..), TModality (..),
                                           TypeInfo (..), VarIdent)
import           Rzk.TypeCheck.Context
import           Rzk.TypeCheck.Decl
import           Rzk.TypeCheck.Display
import           Rzk.TypeCheck.Eval
import           Rzk.TypeCheck.Monad

-- | A binder's displayed type: a plain type, or a cube together with a tope, for a
-- shaped binder like @(t : I | φ t)@.
data BinderTypeView
  = TypeView Rendered
  | ShapeView Rendered Rendered
  deriving (Eq, Show)

-- | Render a term with the names of the context it is in.
renderHere :: TermT n -> TypeCheck n Rendered
renderHere t = do
  naming <- asks namingOfContext
  pure (renderTerm naming (untyped t))

-- | The memoised weak head normal form of a typed term, if present.
memoWHNF :: TermT n -> TermT n
memoWHNF t = fromMaybe t (typeInfoOf t >>= infoWHNF)

-- | The variables a binder introduces, with their rendered types.
--
-- A pair binder splits its type along Σ-types and cube products; when the shape is
-- not syntactic (the type is a defined name applied to arguments, as in
-- @((η , (ϵ , (α , β))) : has-quasi-diagrammatic-adj A B f u)@), the type is put in
-- weak head normal form first, which needs the top-level definitions in scope. The
-- dependent part is rendered under the earlier component's display name, giving
-- @q : B p@.
binderTypeEntries
  :: Distinct n => Binder -> TermT n -> TypeCheck n [(VarIdent, BinderTypeView)]
binderTypeEntries binder ty = case binder of
  BinderUnit         -> pure []
  BinderVar Nothing  -> pure []
  BinderVar (Just x) -> do
    rendered <- renderHere ty
    pure [(x, TypeView rendered)]
  BinderPair l r     -> splitViewM ty >>= \case
    Just (TypeSigmaT _ _ md a bscope) -> do
      ls <- binderTypeEntries l a
      rs <- inScope l md a bscope $ \bBody -> binderTypeEntries r bBody
      pure (ls ++ rs)
    Just (CubeProductT _ a b) ->
      (++) <$> binderTypeEntries l a <*> binderTypeEntries r b
    _ -> pure []   -- unrecognised shape; the surface annotation is the fallback

-- | View a type as a Σ-type or a cube product: syntactically, or through the
-- memoised WHNF if possible, computing the WHNF otherwise. Never throws.
splitViewM :: Distinct n => TermT n -> TypeCheck n (Maybe (TermT n))
splitViewM ty = case splitView (memoWHNF ty) of
  Just t  -> pure (Just t)
  Nothing -> (splitView <$> whnfT ty) `catchError` \_ -> pure Nothing
  where
    splitView t = case stripTypeRestrictions t of
      t'@TypeSigmaT{}   -> Just t'
      t'@CubeProductT{} -> Just t'
      _                 -> Nothing

-- | The elaborated types of the local binders of a typed term, keyed by the
-- binder's original identifier (whose position points at its defining occurrence).
binderTypesOfTerm
  :: Distinct n => TermT n -> TypeCheck n [(VarIdent, BinderTypeView)]
binderTypesOfTerm = go
  where
    go :: Distinct n => TermT n -> TypeCheck n [(VarIdent, BinderTypeView)]
    go t = case t of
      Var _ -> pure []

      LambdaT info binder mparam body -> do
        (paramType, paramTope) <- case mparam of
          Just (LambdaParam _ ty mtope) -> pure (Just ty, mtope)
          -- A bare λ: the domain (and shape) of its own Π-type.
          Nothing -> case funView (memoWHNF (infoType info)) of
            Just (p, mtope) -> pure (Just p, mtope)
            Nothing ->
              (maybe (Nothing, Nothing) (\(p, mtope) -> (Just p, mtope)) . funView
                 <$> whnfT (infoType info))
                `catchError` \_ -> pure (Nothing, Nothing)
        entries <- shapedBinderEntries binder paramType paramTope
        let md = maybe Id (\(LambdaParam m _ _) -> m) mparam
        annEntries <- case mparam of
          Nothing -> pure []
          Just (LambdaParam _ ty mtope) -> do
            tyEntries <- go ty
            topeEntries <- case mtope of
              Nothing   -> pure []
              Just tope -> inScope binder md ty tope go
            pure (tyEntries ++ topeEntries)
        bodyEntries <-
          inScope binder md (fromMaybe universeT paramType) body go
        pure (entries ++ annEntries ++ bodyEntries)

      TypeFunT _ binder md param mtope ret -> do
        entries      <- shapedBinderEntries binder (Just param) mtope
        paramEntries <- go param
        topeEntries  <- case mtope of
          Nothing   -> pure []
          Just tope -> inScope binder md param tope go
        retEntries   <- inScope binder md param ret go
        pure (entries ++ paramEntries ++ topeEntries ++ retEntries)

      TypeSigmaT _ binder md a bscope -> do
        entries  <- binderTypeEntries binder a
        aEntries <- go a
        bEntries <- inScope binder md a bscope go
        pure (entries ++ aEntries ++ bEntries)

      LetT _ binder manno value body -> do
        let valueType = case typeInfoOf value of
              Just valueInfo -> Just (infoType valueInfo)
              Nothing        -> manno
        entries      <- maybe (pure []) (binderTypeEntries binder) valueType
        annEntries   <- maybe (pure []) go manno
        valueEntries <- go value
        bodyEntries  <- inScopeWith binder Id (fromMaybe universeT valueType) (Just value) body go
        pure (entries ++ annEntries ++ valueEntries ++ bodyEntries)

      LetModT _ binder _nu mu manno value body -> do
        unwrapped <- case typeInfoOf value of
          Nothing -> pure Nothing
          Just valueInfo -> do
            let vt = infoType valueInfo
            case modalView (memoWHNF vt) of
              Just a  -> pure (Just a)
              Nothing -> (modalView <$> whnfT vt) `catchError` \_ -> pure Nothing
        entries      <- maybe (pure []) (binderTypeEntries binder) unwrapped
        annEntries   <- maybe (pure []) go manno
        valueEntries <- go value
        bodyEntries  <- inScope binder mu (fromMaybe universeT unwrapped) body go
        pure (entries ++ annEntries ++ valueEntries ++ bodyEntries)

      Node (AnnSig _ f) ->
        concat <$> mapM go (bifoldr (\_ acc -> acc) (:) [] f)

    funView t = case stripTypeRestrictions t of
      TypeFunT _ _ _ param mtope _ -> Just (param, mtope)
      _                            -> Nothing
    modalView t = case stripTypeRestrictions t of
      TypeModalT _ _ a -> Just a
      _                -> Nothing

    -- A shaped plain binder shows its cube together with the tope, as it is
    -- written: (t : I | φ t). A shaped pair binder splits along the cube, the tope
    -- constraining the components jointly (as in the surface tier).
    shapedBinderEntries
      :: Distinct n
      => Binder -> Maybe (TermT n) -> Maybe (ScopedTermT n)
      -> TypeCheck n [(VarIdent, BinderTypeView)]
    shapedBinderEntries binder mty mtope = case (binder, mty, mtope) of
      (BinderVar (Just x), Just cube, Just tope) -> do
        cubeR <- renderHere cube
        topeR <- inScope binder Id cube tope renderHere
        pure [(x, ShapeView cubeR topeR)]
      (_, Just ty, _) -> binderTypeEntries binder ty
      _ -> pure []

-- | All the local binder types of a declaration: Π and Σ binders from its type,
-- λ and let binders from its value.
declBinderTypes
  :: Distinct n => Decl n -> TypeCheck n [(VarIdent, BinderTypeView)]
declBinderTypes decl =
  (++) <$> binderTypesOfTerm (declType decl)
       <*> maybe (pure []) binderTypesOfTerm (declValue decl)

-- | The elaborated types of the local binders of one file's declarations, with the
-- rest of the run's declarations in scope so that 'whnfT' can unfold definitions
-- when splitting pair binders.
--
-- Pure at the interface: it runs the checker silently and returns no entries where
-- it fails.
binderTypesOfFile :: Checked -> FilePath -> [(VarIdent, BinderTypeView)]
binderTypesOfFile (Checked ctx decls _errs) path =
  case runTypeCheckIn ctx action of
    Left _        -> []
    Right entries -> entries
  where
    fileDecls = concat [ ds | (p, ds) <- decls, p == path ]
    action = localVerbosity Silent $
      concat <$> mapM declBinderTypes fileDecls
