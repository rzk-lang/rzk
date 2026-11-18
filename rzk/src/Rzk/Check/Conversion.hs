{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ViewPatterns        #-}
-- | Subtyping and conversion, in the checker's monad.
--
-- The library's conversion decides the common case: two values compared
-- in the context's variance, with the actual term available through the
-- context when restricted-type faces need it. Where it answers no, the
-- checker explains the failure on the heads. There is no unification: a
-- mismatch is an error.
module Rzk.Check.Conversion
  ( subtype
  , subtypeValues
  , convertTerms
  , convertTypes
  , holesTolerated
  ) where

import           Control.Monad            (unless, when)
import           Control.Monad.Foil       (Distinct)
import qualified Control.Monad.Foil       as Foil
import           Control.Monad.Free.Foil  (AST (..))
import qualified FreeFoil.NbE             as NbE

import           Rzk.Check.Context
import           Rzk.Check.Error
import           Rzk.Check.Monad
import           Rzk.Check.Syntax
import           Rzk.Check.Topes
import           Rzk.Check.Value

-- | Check that the actual type of a term is a subtype of the expected type.
-- The arguments are the term, its expected type, and its actual inferred type.
subtype :: Distinct n => Core n -> Value n -> Value n -> Check n ()
subtype term expected actual = compareWith (Just term) Covariant expected actual

-- | Check that the second type (actual) is a subtype of the first (expected),
-- with no term at hand.
subtypeValues :: Distinct n => Value n -> Value n -> Check n ()
subtypeValues = compareWith Nothing Covariant

-- | Two types are convertible.
convertTypes :: Distinct n => Value n -> Value n -> Check n ()
convertTypes = compareWith Nothing Invariant

-- | Two terms are convertible.
convertTerms :: Distinct n => Value n -> Value n -> Check n ()
convertTerms = compareWith Nothing Invariant

-- | In a lenient hole mode, a term that still carries a hole is work in
-- progress, and a mismatch around it is tolerated.
holesTolerated :: Core n -> Check n Bool
holesTolerated term = asksContext $ \ctx ->
  optHoles (ctxOptions ctx) == HolesDeferred && containsHole term

-- | Compare expected with actual in the given variance. The optional core term
-- is the actual term whose type is being compared; it is needed when checking
-- that an unrestricted term satisfies the faces of a restricted expected type.
compareWith
  :: forall n. (Distinct n)
  => Maybe (Core n) -> Variance -> Value n -> Value n -> Check n ()
compareWith mterm variance expected actual =
  performing (ActionConvert expected actual) $ do
    ctx <- askContext
    scope <- askScope
    let topes = ctxTopes ctx
        ctx' = withSubject mterm (withVariance variance ctx)
        agrees c = conv scope (withVariance variance c) expected actual
    -- The value-level conversion retries in every alternative of a
    -- disjunctive tope context, so no second alternative loop is needed here.
    unless (entailsBottom topes || agrees ctx') $
      explain mterm variance expected actual

-- | Explain a failed value comparison with more specific diagnostics than
-- the library's Boolean answer. The logical restricted-type rules live in
-- value conversion, using the subject stored in the context. This is not
-- unification.
explain
  :: forall n. (Distinct n)
  => Maybe (Core n) -> Variance -> Value n -> Value n -> Check n ()
explain mterm variance expected actual = do
  ctx <- askContext
  scope <- askScope
  tolerated <- maybe (pure False) holesTolerated mterm
  if tolerated then pure () else case (force expected, force actual) of
    -- both restricted: the underlying types, then the faces of the
    -- supertype covered by those of the subtype, agreeing on overlaps
    (VTypeRestricted tyE rsE, VTypeRestricted tyA rsA) -> do
      compareWith mterm variance tyE tyA
      let (superRs, subRs) = case variance of
            Contravariant -> (rsA, rsE)
            _             -> (rsE, rsA)
      sequence_
        [ underTope superT $ do
            ctx' <- askContext
            let subTopes = map (topeOf scope ctx' . fst) subRs
            unless (entailsUnion (ctxTopes ctx') subTopes) $
              issueTypeError (TypeErrorTopeNotSatisfied (accessibleTopes (ctxTopes ctx')) (foldr topeOr TBot subTopes))
            sequence_
              [ underTope (topeOf scope ctx' subTope) $ convertTerms superTerm subTerm
              | (subTope, subTerm) <- subRs ]
        | (superTope, superTerm) <- superRs
        , let superT = topeOf scope ctx superTope ]
      when (variance == Invariant) $
        compareWith mterm Covariant actual expected
    -- two family types: the cubes, then the shapes, covariantly and on
    -- their own
    ((VTypeShape md cubeE mshapeE), (VTypeShape md' cubeA mshapeA)) | md == md' -> do
      compareWith Nothing (opposite variance) cubeE cubeA
      case (mshapeE, mshapeA) of
        (Nothing, Nothing) -> pure ()
        (Just sE, Just sA) -> underFreshVar scope ctx md cubeA Nothing $ \scope' ctx' name -> do
          let x = NbE.vvar name
              tE = topeOf scope' ctx' (appValue scope' ctx' (Foil.sink sE) x)
              tA = topeOf scope' ctx' (appValue scope' ctx' (Foil.sink sA) x)
              topes = ctxTopes ctx'
              holds = case variance of
                Covariant     -> topesEntail topes [tA] tE
                Contravariant -> topesEntail topes [tE] tA
                Invariant     -> equivalent topes tE tA
              (super, sub) = case variance of
                Contravariant -> (tE, tA)
                _             -> (tA, tE)
          unless holds $ inContext (ContextWithScope scope' ctx') $
            issueTypeError (TypeErrorTopeNotSatisfied [super] sub)
        _ -> mismatch
    -- a function is a subtype of another when its applications are
    (VPi (Pi envE md domE mshapeE retE), VPi (Pi envA md' domA mshapeA retA)) | md == md' -> do
      compareWith Nothing (opposite variance) domE domA
      underFreshVar scope ctx md domA Nothing $ \scope' ctx' name -> do
        let x = NbE.vvar name
            codE = codomainAt scope' ctx' (Foil.sink envE) retE x
            codA = codomainAt scope' ctx' (Foil.sink envA) retA x
            mterm' = fmap (\term -> App (Foil.sink term) (Var name)) mterm
        inContext (ContextWithScope scope' ctx') $ do
          -- the shapes: the expected one entails the actual one, as the
          -- domain is contravariant
          case (mshapeE, mshapeA) of
            (Nothing, Nothing) -> pure ()
            (Just sE, Just sA) -> do
              let tE = topeOf scope' ctx' (appValue scope' ctx' (Foil.sink sE) x)
                  tA = topeOf scope' ctx' (appValue scope' ctx' (Foil.sink sA) x)
                  topes = ctxTopes ctx'
                  holds = case variance of
                    Covariant     -> entails (assume tE topes) tA
                    Contravariant -> entails (assume tA topes) tE
                    Invariant     -> entails (assume tE topes) tA && entails (assume tA topes) tE
                  (super, sub) = case variance of
                    Contravariant -> (tA, tE)
                    _             -> (tE, tA)
              unless holds $
                issueTypeError (TypeErrorTopeNotSatisfied (accessibleTopes (assume super topes)) sub)
            _ -> inContext (ContextWithScope scope ctx) mismatch
          compareWith mterm' variance codE codA
    -- two lambdas: their bodies under a fresh variable of the actual one's
    -- domain, where both shapes hold, so that the mismatch is reported
    -- where it is
    (VLambda envE mpE bodyE, VLambda envA mpA bodyA) -> do
      let (md, dom) = case (mpA, mpE) of
            (Just p, _) -> (paramModality p, paramDomain p)
            (_, Just p) -> (paramModality p, paramDomain p)
            _           -> panicImpossible "an elaborated lambda without a parameter domain"
      underFreshVar scope ctx md dom (mpA >>= paramShape) $ \scope' ctx' name -> do
        let x = NbE.vvar name
            ctx'' = case mpE >>= paramShape of
              Nothing    -> ctx'
              Just shape -> assumeValue scope' ctx' (appValue scope' ctx' (Foil.sink shape) x)
        inContext (ContextWithScope scope' ctx'') $
          compareWith Nothing variance
            (evalScope scope' ctx'' (Foil.sink envE) bodyE x)
            (evalScope scope' ctx'' (Foil.sink envA) bodyA x)
    -- identity types: the carriers, then the endpoints
    ((VTypeId xE mtE yE), (VTypeId xA mtA yA)) -> do
      case (mtE, mtA) of
        (Just tE, Just tA) -> convertTypes tE tA
        _ -> pure ()
      convertTerms xE xA
      convertTerms yE yA
    _ -> mismatch
  where
    mismatch = case mterm of
      Just term -> issueTypeError (TypeErrorUnify term expected actual)
      Nothing   -> issueTypeError (TypeErrorUnifyTerms expected actual)


pattern VPi :: Pi n -> Whnf n
pattern VPi p <- (piOf . NbE.fromWhnf -> Just p)
