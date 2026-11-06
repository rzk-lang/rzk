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
-- in the context's variance. Where it answers no, the checker explains the
-- failure on the heads, with the term whose type is compared where a
-- restriction needs it: a term of an unrestricted type stands at a
-- restricted one when it agrees with each face, and a function does so
-- when its applications do. There is no unification: a mismatch is an
-- error.
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

-- | The actual type of a term is a subtype of the expected one.
subtype :: (Checking m, Distinct n) => Core n -> Value n -> Value n -> m n ()
subtype term expected actual = compareWith (Just term) Covariant expected actual

-- | The second type is a subtype of the first, with no term at hand.
subtypeValues :: (Checking m, Distinct n) => Value n -> Value n -> m n ()
subtypeValues = compareWith Nothing Covariant

-- | Two types are convertible.
convertTypes :: (Checking m, Distinct n) => Value n -> Value n -> m n ()
convertTypes = compareWith Nothing Invariant

-- | Two terms are convertible.
convertTerms :: (Checking m, Distinct n) => Value n -> Value n -> m n ()
convertTerms = compareWith Nothing Invariant

-- | In a lenient hole mode, a term that still carries a hole is work in
-- progress, and a mismatch around it is tolerated.
holesTolerated :: Checking m => Core n -> m n Bool
holesTolerated term = asksContext $ \ctx ->
  optHoles (ctxOptions ctx) == HolesDeferred && containsHole term

compareWith
  :: forall m n. (Checking m, Distinct n)
  => Maybe (Core n) -> Variance -> Value n -> Value n -> m n ()
compareWith mterm variance expected actual =
  performing (ActionConvert expected actual) $ do
    ctx <- askContext
    let topes = ctxTopes ctx
        scope = ctxScope ctx
        ctx' = withVariance variance ctx
        agrees c = conv scope (withVariance variance c) expected actual
    unless (entailsBottom topes || agrees ctx' || inAlternatives topes (agrees . withTopes ctx')) $
      explain mterm variance expected actual
  where
    withTopes c t = c { ctxTopes = t }

-- | Why two values do not compare, and the cases the library cannot
-- decide: a restriction met by the term, a function by its applications.
explain
  :: forall m n. (Checking m, Distinct n)
  => Maybe (Core n) -> Variance -> Value n -> Value n -> m n ()
explain mterm variance expected actual = do
  ctx <- askContext
  let scope = ctxScope ctx
  tolerated <- maybe (pure False) holesTolerated mterm
  if tolerated then pure () else case (force expected, force actual) of
    -- both restricted: the underlying types, then the faces of the
    -- supertype covered by those of the subtype, agreeing on overlaps
    (VNodeRestricted tyE rsE, VNodeRestricted tyA rsA) -> do
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
    -- the restricted type is the subtype of its underlying type
    (_, VNodeRestricted tyA _) | variance == Covariant ->
      compareWith mterm variance expected tyA
    (VNodeRestricted tyE _, _) | variance == Contravariant ->
      compareWith mterm variance tyE actual
    -- a term of an unrestricted type stands at a restricted one when it
    -- agrees with each face
    (VNodeRestricted tyE rsE, _) | Just term <- mterm, variance == Covariant -> do
      compareWith mterm variance tyE actual
      let termV = evalCore scope ctx term
      sequence_
        [ underTope (topeOf scope ctx tope) $ convertTerms face termV
        | (tope, face) <- rsE ]
    -- two family types: the cubes, then the shapes, covariantly and on
    -- their own
    (NbE.VNode (TypeShapeF md cubeE mshapeE), NbE.VNode (TypeShapeF md' cubeA mshapeA)) | md == md' -> do
      compareWith Nothing (opposite variance) cubeE cubeA
      case (mshapeE, mshapeA) of
        (Nothing, Nothing) -> pure ()
        (Just sE, Just sA) -> Foil.withFresh scope $ \b -> do
          let pat = singlePattern b
              scope' = Foil.extendScopePattern pat scope
              ctx' = underPattern scope' ctx md cubeA Nothing pat
              x = patternValue pat
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
          unless holds $ inContext ctx' $
            issueTypeError (TypeErrorTopeNotSatisfied [super] sub)
        _ -> mismatch
    -- a function is a subtype of another when its applications are
    (VPi (Pi envE md domE mshapeE retE), VPi (Pi envA md' domA mshapeA retA)) | md == md' -> do
      compareWith Nothing (opposite variance) domE domA
      Foil.withFresh scope $ \b -> do
        let pat = singlePattern b
            scope' = Foil.extendScopePattern pat scope
            ctx' = underPattern scope' ctx md domA Nothing pat
            x = patternValue pat
            codE = codomainAt scope' ctx' (Foil.sink envE) retE x
            codA = codomainAt scope' ctx' (Foil.sink envA) retA x
            mterm' = fmap (\term -> App (Foil.sink term) (Var (Foil.nameOf b))) mterm
        inContext ctx' $ do
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
            _ -> inContext ctx mismatch
          compareWith mterm' variance codE codA
    -- identity types: the carriers, then the endpoints
    (NbE.VNode (TypeIdF xE mtE yE), NbE.VNode (TypeIdF xA mtA yA)) -> do
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

pattern VNodeRestricted :: Value n -> [(Value n, Value n)] -> Whnf n
pattern VNodeRestricted ty rs <- NbE.VNode (TypeRestrictedF ty rs)

pattern VPi :: Pi n -> Whnf n
pattern VPi p <- (piOf . NbE.fromWhnf -> Just p)
