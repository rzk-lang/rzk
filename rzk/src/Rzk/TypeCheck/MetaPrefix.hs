{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Diagnostics for unsaturated schematic parameters.
--
-- A declaration's /meta prefix/ ends at its last schematic parameter:
-- a universe, @CUBE@, @TOPE@, or a function quantifying over or into one.
--
-- * Structural checking requires the full prefix at object-level uses.
-- * Strict checking confines schema arguments to a top-level receiver's meta prefix.
--
-- Strict checking is the default. Neither check establishes universe
-- stratification; impredicative instantiation can escape detection.
module Rzk.TypeCheck.MetaPrefix (
  metaPrefixOf,
  isMetaType,
  recordMetaPrefixUses,
) where

import           Control.Monad            (forM_, when)
import           Control.Monad.Except     (catchError)
import           Control.Monad.Reader     (ask, asks)
import           Data.Bifoldable          (bifoldr)
import           Data.Maybe               (fromMaybe)

import           Control.Monad.Foil       (Distinct)
import qualified Control.Monad.Foil       as Foil
import           Control.Monad.Free.Foil  (AST (Node, Var))

import           Control.Monad.Free.Foil.Annotated (AnnSig (..))
import           Language.Rzk.Foil.Names  (TModality (..), TypeInfo (..),
                                           VarIdent, binderName)
import           Language.Rzk.Foil.Syntax
import           Rzk.TypeCheck.Context
import           Rzk.TypeCheck.Eval
import           Rzk.TypeCheck.Monad

-- * Classifying types

-- | Recognise universes and functions quantifying over or into universes.
-- Includes @CUBE@ and @TOPE@; returns 'False' if classification fails.
isMetaType :: Distinct n => TermT n -> TypeCheck n Bool
isMetaType t = flip catchError (\_ -> pure False) $ do
  t' <- headView t
  case t' of
    UniverseT{}     -> pure True
    UniverseCubeT{} -> pure True
    UniverseTopeT{} -> pure True
    TypeFunT _ orig md param _mtope ret -> do
      paramMeta <- isMetaType param
      if paramMeta
        then pure True
        else inScope orig md param ret isMetaType
    _               -> pure False

-- | Count parameters through the last schematic one.
-- Returns zero if there is none or classification fails.
metaPrefixOf :: Distinct n => TermT n -> TypeCheck n Int
metaPrefixOf ty = flip catchError (\_ -> pure 0) $ go 1 0 ty
  where
    go :: Distinct l => Int -> Int -> TermT l -> TypeCheck l Int
    go pos acc t = headView t >>= \case
      TypeFunT _ orig md param _mtope ret -> do
        meta <- isMetaType param
        let acc' = if meta then pos else acc
        inScope orig md param ret (go (pos + 1) acc')
      _ -> pure acc

-- Strip restrictions and unfold only when the head needs classification.
headView :: Distinct n => TermT n -> TypeCheck n (TermT n)
headView t = case stripTypeRestrictions t of
  t'@UniverseT{}     -> pure t'
  t'@UniverseCubeT{} -> pure t'
  t'@UniverseTopeT{} -> pure t'
  t'@TypeFunT{}      -> pure t'
  t'@TypeSigmaT{}    -> pure t'
  t'@TypeIdT{}       -> pure t'
  t'                 -> stripTypeRestrictions <$> whnfT t'

-- * The use-site walk

-- | How a position is classified under the two candidate rules.
data PosKind = MetaPos | ObjectPos
  deriving (Eq)

data Positions = Positions
  { posStructural :: PosKind
  , posStrict     :: PosKind
  }

rootPositions, typePositions, objectPositions :: Positions
rootPositions   = Positions MetaPos MetaPos
typePositions   = Positions MetaPos MetaPos
objectPositions = Positions ObjectPos ObjectPos

-- | Report uses that supply too few schematic arguments.
-- Violations are errors when @rstt-safe = "error"@.
recordMetaPrefixUses
  :: forall n. Distinct n
  => VarIdent -> TermT n -> Maybe (TermT n) -> TypeCheck n ()
recordMetaPrefixUses defName ty mval =
  asks effectiveMetaPrefixSensitivity >>= \case
    MetaPrefixOff -> pure ()
    _ -> localVerbosity Silent $ flip catchError ignoreAdvisoryError $ do
      go rootPositions ty
      mapM_ (go rootPositions) mval
  where
    go :: forall l. Distinct l => Positions -> TermT l -> TypeCheck l ()
    go pos t = case t of
      Var v -> checkHead pos v 0

      AppT{} -> do
        let (h, args) = collectSpine t []
        mk <- case h of
          Var v -> do
            ctx <- ask
            let info = lookupVarInfo v ctx
            checkHead pos v (length args)
            pure (if varIsTopLevel info then Just (varMetaPrefix info) else Nothing)
          _ -> do
            go objectPositions h
            pure Nothing
        forM_ (zip [1 :: Int ..] args) $ \(i, (fnode, arg)) -> do
          domMeta <- domainIsMeta fnode
          let strict = case mk of
                Just k | domMeta && i <= k -> MetaPos
                _                          -> ObjectPos
          go (Positions (if domMeta then MetaPos else ObjectPos) strict) arg

      LambdaT info orig mparam body -> do
        (mdom, md) <- case mparam of
          Just (LambdaParam m ty' _mtope) -> do
            go typePositions ty'
            pure (Just ty', m)
          -- A bare λ: the domain of its own Π-type.
          Nothing -> do
            dom <- funDomain (infoType info)
            pure (dom, Id)
        inScope orig md (fromMaybe universeT mdom) body (go pos)

      TypeFunT _ orig md param _mtope ret -> do
        go typePositions param
        inScope orig md param ret (go typePositions)

      TypeSigmaT _ orig md a bscope -> do
        go typePositions a
        inScope orig md a bscope (go typePositions)

      -- The endpoints are terms; storing an unsaturated schema in an
      -- identity type is an object-level use.
      TypeIdT _ a mtA b -> do
        go objectPositions a
        mapM_ (go typePositions) mtA
        go objectPositions b

      TypeRestrictedT _ ty' rs -> do
        go typePositions ty'
        forM_ rs $ \(_tope, term) -> go objectPositions term

      LetT _ orig manno value body -> do
        mapM_ (go typePositions) manno
        go objectPositions value
        let valueType = case typeInfoOf value of
              Just valueInfo -> Just (infoType valueInfo)
              Nothing        -> manno
        inScopeWith orig Id (fromMaybe universeT valueType) (Just value) body (go pos)

      LetModT _ orig _nu mu manno mmotive value body -> do
        mapM_ (go typePositions) manno
        mapM_ (go typePositions) mmotive
        go objectPositions value
        unwrapped <- case typeInfoOf value of
          Nothing        -> pure Nothing
          Just valueInfo -> modalDomain (infoType valueInfo)
        inScope orig mu (fromMaybe universeT unwrapped) body (go pos)

      Node (AnnSig _ f) ->
        mapM_ (go objectPositions) (bifoldr (\_ acc -> acc) (:) [] f)

    -- An unsaturated top-level head at an object position warns; at a
    -- position only the strict rule rejects, the warning is marked so.
    checkHead :: forall l. Distinct l => Positions -> Foil.Name l -> Int -> TypeCheck l ()
    checkHead pos v nargs = do
      ctx <- ask
      let info = lookupVarInfo v ctx
          k = varMetaPrefix info
      when (varIsTopLevel info && k > nargs) $ do
        sensitivity <- asks effectiveMetaPrefixSensitivity
        let mrule = case (posStructural pos, posStrict pos) of
              (ObjectPos, _) -> Just MetaPrefixBoth
              (MetaPos, ObjectPos)
                | MetaPrefixStrict <- sensitivity -> Just MetaPrefixStrictOnly
              _              -> Nothing
        forM_ mrule $ \rule -> do
          loc <- asks ctxLocation
          recordCheckWarning $ MetaPrefixWarning
            defName
            (fromMaybe "_" (binderName (varOrig info)))
            nargs
            k
            rule
            loc

    -- Is the Π-domain of this function node meta-shaped?
    domainIsMeta :: forall l. Distinct l => TermT l -> TypeCheck l Bool
    domainIsMeta f = flip catchError (\_ -> pure False) $ do
      tf <- typeOfUncomputed f
      headView tf >>= \case
        TypeFunT _ _ _ dom _ _ -> isMetaType dom
        _                      -> pure False

    funDomain :: forall l. Distinct l => TermT l -> TypeCheck l (Maybe (TermT l))
    funDomain tf = flip catchError (\_ -> pure Nothing) $
      headView tf >>= \case
        TypeFunT _ _ _ dom _ _ -> pure (Just dom)
        _                      -> pure Nothing

    modalDomain :: forall l. TermT l -> TypeCheck l (Maybe (TermT l))
    modalDomain tv = flip catchError (\_ -> pure Nothing) $
      pure $ case stripTypeRestrictions tv of
        TypeModalT _ _ a -> Just a
        _                -> Nothing

collectSpine :: TermT n -> [(TermT n, TermT n)] -> (TermT n, [(TermT n, TermT n)])
collectSpine (AppT _ f x) acc = collectSpine f ((f, x) : acc)
collectSpine h acc            = (h, acc)
