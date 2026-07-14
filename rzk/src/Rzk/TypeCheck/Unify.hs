{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unification, which in rzk is really subtyping: an extension type's faces, a
-- shape's tope and the variance of the position all take part.
module Rzk.TypeCheck.Unify where

import           Control.Monad            (forM_, unless, when)
import           Control.Monad.Except     (catchError, throwError)
import           Control.Monad.Reader     (asks)
import           Data.Maybe               (fromMaybe)
import           Data.Tuple               (swap)

import           Control.Monad.Foil       (DExt, Distinct, NameBinder)
import qualified Control.Monad.Foil       as Foil
import           Control.Monad.Free.Foil  (AST (Var))

import           Language.Rzk.Foil.Syntax
import           Language.Rzk.Free.Syntax (Binder, TModality (..),
                                           TypeInfo (..))
import           Rzk.TypeCheck.Context
import           Rzk.TypeCheck.Display (panicImpossible)
import           Rzk.TypeCheck.Error
import           Rzk.TypeCheck.Eval
import           Rzk.TypeCheck.Monad

-- | Open two scoped terms under /one/ binder, so that the two sides of a
-- comparison are compared as functions of the same variable.
inScope2
  :: Distinct n
  => Binder -> TModality -> TermT n
  -> ScopedTermT n -> ScopedTermT n
  -> (forall l. (DExt n l, Distinct l)
        => NameBinder n l -> TermT l -> TermT l -> TypeCheck l a)
  -> TypeCheck n a
inScope2 orig md ty s1 s2 k = do
  scope <- asks ctxScope
  withScopedT2 scope s1 s2 $ \binder body1 body2 ->
    underBinder binder orig md ty Nothing (k binder body1 body2)

-- | α-equivalence in the ambient scope.
alphaEq :: Distinct n => TermT n -> TermT n -> TypeCheck n Bool
alphaEq l r = do
  scope <- asks ctxScope
  pure (alphaEqT scope l r)

unifyTopes :: Distinct n => TermT n -> TermT n -> TypeCheck n ()
unifyTopes l r = do
  equiv <- (&&)
    <$> [plainTope l] `entailM` r
    <*> [plainTope r] `entailM` l
  unless equiv $
    issueTypeError (TypeErrorTopesNotEquivalent l r)

unify
  :: Distinct n
  => Maybe (TermT n) -> TermT n -> TermT n -> TypeCheck n ()
unify mterm expected actual = performUnification `catchError` \typeError -> do
  inAllSubContexts (throwError typeError) performUnification
  where
    performUnification = unifyInCurrentContext mterm expected actual

-- | The syntactic fast path.
--
-- α-equivalence, not the structural equality the old representation used: two
-- terms that differ only in a binder's name are the same term, and saying so here
-- saves the whole unification below.
unifyViaDecompose :: Distinct n => TermT n -> TermT n -> TypeCheck n ()
unifyViaDecompose expected actual = do
  same <- alphaEq expected actual
  if same
    then return ()
    else case (expected, actual) of
      (AppT _ f x, AppT _ g y) -> do
        unify Nothing f g
        setVariance Invariant $ unify Nothing x y
      _ -> issueTypeError (TypeErrorOther "cannot decompose")

unifyTypes :: Distinct n => TermT n -> TermT n -> TermT n -> TypeCheck n ()
unifyTypes = unify . Just

unifyTerms :: Distinct n => TermT n -> TermT n -> TypeCheck n ()
unifyTerms = unify Nothing

checkCoherence
  :: Distinct n
  => (TermT n, TermT n) -> (TermT n, TermT n) -> TypeCheck n ()
checkCoherence (ltope, lterm) (rtope, rterm) =
  performing (ActionCheckCoherence (ltope, lterm) (rtope, rterm)) $
    localTope (topeAndT ltope rtope) $ do
      ltype <- stripTypeRestrictions <$> typeOf lterm   -- FIXME: why strip?
      rtype <- stripTypeRestrictions <$> typeOf rterm   -- FIXME: why strip?
      -- FIXME: do we need to unify types here or is it included in unification of terms?
      unifyTerms ltype rtype
      unifyTerms lterm rterm

unifyInCurrentContext
  :: forall n. Distinct n
  => Maybe (TermT n) -> TermT n -> TermT n -> TypeCheck n ()
unifyInCurrentContext mterm expected actual = performing action $ do
  inBottom <- contextEntailsBottom
  unless inBottom $
    -- NOTE: the decomposition gives a small, but noticeable speedup
    unifyViaDecompose expected actual `catchError` \_ -> do
      expectedVal <- whnfT expected
      actualVal <- whnfT actual
      mea <- asks ctxCovariance >>= \case
        Covariant     -> Just <$> etaMatch mterm expectedVal actualVal
        Contravariant -> Just . swap <$> etaMatch mterm actualVal expectedVal
        Invariant     -> traceTypeCheck Debug "invariant" $ do
          -- FIXME: inefficient
          traceTypeCheck Debug "invariant->covariant" $
            setVariance Covariant     $ unifyInCurrentContext mterm expectedVal actualVal
          traceTypeCheck Debug "invariant->contravariant" $
            setVariance Contravariant $ unifyInCurrentContext mterm expectedVal actualVal
          return Nothing
      case mea of
        Nothing -> return ()
        -- A hole (in lenient mode) stands for a term of the expected type, so it
        -- unifies with anything; accept it rather than falling through to the
        -- dispatch below (which would panic on an unexpected term).
        Just (expected', actual') | isHoleT expected' || isHoleT actual' -> return ()
        Just (expected', actual') -> do
          same <- alphaEq expected' actual'
          unless same $ dispatch expected' actual'
  where
    action = case mterm of
               Nothing   -> ActionUnifyTerms expected actual
               Just term -> ActionUnify term expected actual

    dispatch :: TermT n -> TermT n -> TypeCheck n ()
    dispatch expected' actual' =
      case actual' of
        RecBottomT{} -> return ()
        RecOrT _ty rs' ->
          case expected' of
            RecOrT _ty rs -> sequence_ (checkCoherence <$> rs <*> rs')
            _ ->
              forM_ rs' $ \(tope, term) ->
                localTope tope $
                  unifyTerms expected' term
        _ -> typeOf expected' >>= typeOf >>= \case
          UniverseCubeT{} -> contextEntails (topeEQT expected' actual')
          _ -> unifyStructurally expected' actual'

    unifyStructurally :: TermT n -> TermT n -> TypeCheck n ()
    unifyStructurally expected' actual' = do
      -- A hole stands for a term of the expected type, so a unification that would
      -- otherwise fail is deferred when either side still contains an (unfilled)
      -- hole — including one nested in a larger term, e.g. @f ?@ checked against an
      -- extension-type boundary. The hole may also sit in the tope context rather
      -- than the terms: a hole standing for a whole shape point makes the enclosing
      -- 'recOR' split over hole-dependent faces, and a branch reduction can drop the
      -- hole from the terms while the assumed face (@π₁ ? ≤ π₂ ?@, say) still
      -- mentions it. Such a branch is only entered because the hole is unfilled, so
      -- a mismatch under it is deferred too. 'structuralHoleUnify' turns this off,
      -- keeping a structural mismatch around a hole an error.
      defer <- asks ctxDeferHoleMismatches
      topeContextHasHole <- asks (any (containsHole . tTope) . ctxTopes)
      let holePresent = defer &&
            (containsHole expected' || containsHole actual' || topeContextHasHole)

          err :: TypeCheck n ()
          err
            | holePresent = return ()
            | otherwise =
                case mterm of
                  Nothing   -> issueTypeError (TypeErrorUnifyTerms expected' actual')
                  Just term -> issueTypeError (TypeErrorUnify term expected' actual')

          -- The same error, raised from inside a binder: the terms are sunk into
          -- the inner scope, which is a coercion. (The old representation had to
          -- shift each of them with @S <$>@.)
          errIn :: DExt n l => TypeCheck l ()
          errIn
            | holePresent = return ()
            | otherwise =
                case mterm of
                  Nothing -> issueTypeError
                    (TypeErrorUnifyTerms (Foil.sink expected') (Foil.sink actual'))
                  Just term -> issueTypeError
                    (TypeErrorUnify (Foil.sink term) (Foil.sink expected') (Foil.sink actual'))

          def = do
            same <- alphaEq expected' actual'
            unless same err

      case expected' of
        Var{} -> def

        UniverseT{} -> def
        UniverseCubeT{} -> def
        UniverseTopeT{} -> def

        TypeUnitT{} -> def
        UnitT{} -> return ()  -- Unit always unifies!

        CubeUnitT{} -> def
        CubeUnitStarT{} -> def
        Cube2T{} -> def
        Cube2_0T{} -> def
        Cube2_1T{} -> def
        CubeIT{} -> def
        CubeI_0T{} -> def
        CubeI_1T{} -> def
        CubeProductT _ l r ->
          case actual' of
            CubeProductT _ l' r' -> do
              unifyTerms l l'
              unifyTerms r r'
            _ -> err

        PairT _ty l r ->
          case actual' of
            PairT _ty' l' r' -> do
              unifyTerms l l'
              unifyTerms r r'
            -- one part of eta-expansion for pairs
            -- FIXME: add symmetric version!
            _ -> err

        FirstT _ty t ->
          case actual' of
            FirstT _ty' t' -> unifyTerms t t'
            _              -> err

        SecondT _ty t ->
          case actual' of
            SecondT _ty' t' -> unifyTerms t t'
            _               -> err

        TopeTopT{}    -> unifyTopes expected' actual'
        TopeBottomT{} -> unifyTopes expected' actual'
        TopeEQT{}     -> unifyTopes expected' actual'
        TopeLEQT{}    -> unifyTopes expected' actual'
        TopeAndT{}    -> unifyTopes expected' actual'
        TopeOrT{}     -> unifyTopes expected' actual'
        TopeInvT{}    -> unifyTopes expected' actual'
        TopeUninvT{}  -> unifyTopes expected' actual'

        RecBottomT{} -> return () -- unifies with anything
        RecOrT _ty rs ->
          -- IMPORTANT: matching on actual' here would be redundant, but that is
          -- not obvious; take care when refactoring.
          forM_ rs $ \(tope, term) ->
            localTope tope $
              unifyTerms term actual'

        TypeFunT _ty _orig md cube mtope ret ->
          case actual' of
            TypeFunT _ty' orig' md' cube' mtope' ret' -> do
              when (md /= md') $
                issueTypeError (TypeErrorOther $ "modality mismatch in function type: expected " <> show md <> " but got " <> show md')
              switchVariance $  -- unifying in the negative position!
                unifyTerms cube cube' -- FIXME: unifyCubes
              inScope2 orig' md cube' ret ret' $ \binder retBody retBody' -> do
                -- The tope checks below are subtyping checks with a fixed direction
                -- relative to (subtype, supertype). Which side is the subtype
                -- depends on the ambient variance: under Covariant the actual type
                -- must be a subtype of the expected one; under Contravariant (inside
                -- a domain) the roles are reversed. Invariant is normally handled
                -- upstream by running both directions; it is handled here as well
                -- for safety.
                variance <- asks ctxCovariance
                scope <- asks ctxScope
                let openTope = fmap (openWith scope (Foil.nameOf binder))
                    mtopeIn = openTope mtope
                    mtopeIn' = openTope mtope'
                case retBody' of
                  UniverseTopeT{} -> do
                    -- This is the case for tope families (shapes).
                    --
                    -- (Λ → TOPE) <: (Δ → TOPE) since if φ : Λ → TOPE then φ ⊢ Δ.
                    -- We DO NOT take the tope context Φ into account!
                    expectedTopeNF <- fromMaybe topeTopT <$> traverse nfT mtopeIn
                    actualTopeNF   <- fromMaybe topeTopT <$> traverse nfT mtopeIn'
                    let subEntailsSuper subNF superNF = do
                          entails <- [plainTope subNF] `entailM` superNF
                          unless (entails || containsHole subNF || containsHole superNF) $
                            issueTypeError (TypeErrorTopeNotSatisfied [subNF] superNF)
                    case variance of
                      Covariant     -> subEntailsSuper actualTopeNF expectedTopeNF
                      Contravariant -> subEntailsSuper expectedTopeNF actualTopeNF
                      Invariant     -> do
                        subEntailsSuper actualTopeNF expectedTopeNF
                        subEntailsSuper expectedTopeNF actualTopeNF
                  _ -> do
                    -- this is the case for Π-types and extension types
                    --
                    -- Ξ | Φ | Γ ⊢ {t : I | φ} → A t <: {s : J | ψ} → B s
                    -- when Ξ | Φ, ψ ⊢ φ
                    expectedTopeNF <- fromMaybe topeTopT <$> traverse nfT mtopeIn
                    actualTopeNF   <- fromMaybe topeTopT <$> traverse nfT mtopeIn'
                    let superEntailsSub superNF subNF =
                          localTope superNF $ contextEntails subNF
                    case variance of
                      Covariant     -> superEntailsSub expectedTopeNF actualTopeNF
                      Contravariant -> superEntailsSub actualTopeNF expectedTopeNF
                      Invariant     -> do
                        superEntailsSub expectedTopeNF actualTopeNF
                        superEntailsSub actualTopeNF expectedTopeNF
                case mterm of
                  Nothing -> unifyTerms retBody retBody'
                  Just term ->
                    unifyTypes
                      (appT retBody' (Foil.sink term) (Var (Foil.nameOf binder)))
                      retBody retBody'
            _ -> err

        TypeSigmaT _ty _orig md a b ->
          case actual' of
            TypeSigmaT _ty' orig' md' a' b' -> do
              when (md /= md') $
                issueTypeError (TypeErrorOther $ "modality mismatch in sigma type: expected " <> show md <> " but got " <> show md')
              unify Nothing a a'
              inScope2 orig' md a' b b' $ \_binder bBody bBody' ->
                unify Nothing bBody bBody'
            _ -> err

        TypeIdT _ty x tA y ->
          case actual' of
            TypeIdT _ty' x' tA' y' -> do
              -- The underlying types must be compared: without this check the
              -- routine equates identity types over different types whenever the
              -- endpoints unify, accepting a free homotopy (a path in the type of
              -- functions) where an endpoint-fixing one (a path in a hom-type) is
              -- expected. Compared invariantly: subtyping between the underlying
              -- types must not leak into equality of identity types over them.
              mapM_ (\(t1, t2) -> setVariance Invariant (unify Nothing t1 t2))
                ((,) <$> tA <*> tA')
              unify Nothing x x'
              unify Nothing y y'
            _ -> err

        AppT _ty f x ->
          case actual' of
            AppT _ty' f' x' -> do
              unify Nothing f f'
              setVariance Invariant $
                unify Nothing x x'
            _ -> err

        LambdaT ty _orig _mparam body ->
          case stripTypeRestrictions (infoType ty) of
            TypeFunT _ty _origF md param mtope _ret ->
              case actual' of
                LambdaT ty' orig' _mparam' body' ->
                  case stripTypeRestrictions (infoType ty') of
                    TypeFunT _ty' _origF' md' param' mtope' _ret' -> do
                      when (md /= md') $
                        issueTypeError (TypeErrorOther $ "modality mismatch in lambda: expected " <> show md <> " but got " <> show md')
                      unify Nothing param param' -- we (should) have already checked this in types!
                      inScope2 orig' md param body body' $ \binder bodyIn bodyIn' -> do
                        scope <- asks ctxScope
                        let openTope = fmap (openWith scope (Foil.nameOf binder))
                        case (openTope mtope, openTope mtope') of
                          (Just tope, Just tope') -> do
                            unify Nothing tope tope' -- we (should) have already checked this in types!
                            localTope tope $ unify Nothing bodyIn bodyIn'
                          (Nothing, Nothing) ->
                            unify Nothing bodyIn bodyIn'
                          _ -> errIn
                    _ -> err
                _ -> err
            _ -> err

        LetT{} -> panicImpossible "let at the root of WHNF"
        LetModT _ orig app inn _ val body ->
          case actual' of
            LetModT _ _ app' inn' _ val' body'
              | app == app', inn == inn' -> do
                unify Nothing val val'
                bty <- typeOf val >>= \case
                  TypeModalT _ _ t -> pure t
                  _ -> panicImpossible "not modal in letmod"
                inScope2 orig (comp app inn) bty body body' $ \_binder bodyIn bodyIn' ->
                  unify Nothing bodyIn bodyIn'
            _ -> err

        ReflT ty _x | TypeIdT _ty x _tA y <- infoType ty ->
          case actual' of
            ReflT ty' _x' | TypeIdT _ty' x' _tA' y' <- infoType ty' -> do
              unify Nothing x x'
              unify Nothing y y'
            _ -> err
        ReflT{} -> panicImpossible "refl with a non-identity type!"

        IdJT _ty a b c d e f ->
          case actual' of
            IdJT _ty' a' b' c' d' e' f' -> do
              unify Nothing a a'
              unify Nothing b b'
              unify Nothing c c'
              unify Nothing d d'
              unify Nothing e e'
              unify Nothing f f'
            _ -> err

        TypeAscT{} -> panicImpossible "type ascription at the root of WHNF"

        TypeRestrictedT _ty ty rs ->
          case actual' of
            TypeRestrictedT _ty' ty' rs' -> do
              unify mterm ty ty'
              -- The faces of the supertype must be covered by the faces of the
              -- subtype (the subtype is at least as specified), with the boundary
              -- terms agreeing on overlaps. Which side is the subtype depends on the
              -- ambient variance.
              variance <- asks ctxCovariance
              let subCoversSuper subRs superRs = sequence_
                    [ localTope tope $ do
                        -- FIXME: can do less entails checks?
                        contextEntails (foldr topeOrT topeBottomT (map fst subRs))
                        forM_ subRs $ \(tope', term') ->
                          localTope tope' $
                            unify Nothing term term'
                    | (tope, term) <- superRs
                    ]
              case variance of
                Covariant     -> subCoversSuper rs' rs
                Contravariant -> subCoversSuper rs rs'
                Invariant     -> do
                  subCoversSuper rs' rs
                  subCoversSuper rs rs'
            _ -> err    -- FIXME: need better unification for restrictions

        TypeModalT _ty m ty ->
          case actual' of
            TypeModalT _ty' m' ty' -> do
              when (m' /= m) err
              enterModality m $ unify Nothing ty ty'
            _ -> err
        ModAppT _ty m ty ->
          case actual' of
            ModAppT _ty' m' ty' -> do
              when (m' /= m) err
              enterModality m $ unify Nothing ty ty'
            _ -> err
        ModExtractT _ty app inn te ->
          case actual' of
            ModExtractT _ty' app' inn' te' -> do
              when (app' /= app) err
              when (inn' /= inn) err
              enterModality app $ unify Nothing te te'
            _ -> err

        -- defensive: a hole nested anywhere also defers here rather than panicking
        -- on an otherwise unexpected shape
        _ | holePresent -> return ()
        _ -> panicImpossible "unexpected term in UNIFY"
