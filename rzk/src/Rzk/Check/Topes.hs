{-# LANGUAGE LambdaCase #-}
-- | The tope solver, on first-order topes.
--
-- A tope is read off a value into 'Tope', a first-order syntax with no
-- binders and no scope index, whose equality is structural and whose
-- ordering keys a set. The solver is the saturation solver of
-- "Rzk.TypeCheck.Eval" (Riehl and Shulman's tope logic, the lattice
-- operations on the intervals, and the cover of a disjunction by
-- alternatives), unchanged in its algorithm and pure: the context caches
-- its saturation lazily, and a query reads it.
--
-- The modal layer (topes assumed under a modality, inversion under @ᵒᵖ@)
-- keeps its representation here; its rules come with the modal fragment.
module Rzk.Check.Topes
  ( -- * Topes
    Cube (..)
  , Point (..)
  , Tope (..)
  , cubeOf
  , pointFirst
  , pointSecond
  , topeAnd
  , topeOr
  , ModalTope (..)
  , plainTope
  , isAccessible
    -- * The tope context
  , TopeContext
  , emptyTopeContext
  , contextTopes
  , accessibleTopes
  , assume
  , assumeDiscrete
  , applyModality
    -- * Queries
  , entails
  , representative
  , entailsBottom
  , entailsUnion
  , entailedBy
  , disjoint
  , equivalent
  , topesEntail
  , alternatives
  , inAlternatives
  ) where

import           Data.List               (minimumBy, nub, tails)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Ord                (comparing)

import qualified Data.Set                as Set

import           Language.Rzk.Foil.Names (VarIdent)
import           Rzk.Check.Modality

-- * Topes

-- | A cube, as far as the solver tells cubes apart.
data Cube
  = KUnit
  | K2
  | KI
  | KProd Cube Cube
  | KOther
    -- ^ a cube the solver has no rules for: a neutral one, or a modal one
  deriving (Eq, Ord, Show)

-- | A point of a cube. Evaluation has normalised it: a projection applies
-- only to a neutral, the lattice operations only to points that are not
-- endpoints, and a flip only to a neutral.
data Point
  = PVar !Int !Cube
    -- ^ a variable, with its cube
  | PStar
  | P2_0
  | P2_1
  | PI_0
  | PI_1
  | PPair Point Point
  | PFst Point
  | PSnd Point
  | PSup Point Point
  | PInf Point Point
  | PFlip Point
  | PUnflip Point
  | PModApp TModality Point
  | PModExtract TModality TModality Point
  | PApp !Cube Point Point
    -- ^ a neutral application, with its cube
  | PHole (Maybe VarIdent)
  | POpaque String
    -- ^ a point the solver cannot read (an elimination it has no rule for),
    -- keyed by a fingerprint of its term, so that equal values read back equal
  deriving (Eq, Ord, Show)

-- | A tope.
data Tope
  = TTop
  | TBot
  | TEq Point Point
  | TLeq Point Point
  | TAnd Tope Tope
  | TOr Tope Tope
  | TInv Tope
  | TUninv Tope
  | TModal TModality Tope
  | TModApp TModality Tope
  | TNeutral Point
    -- ^ a tope family applied, or a tope variable: a neutral point of the
    -- tope universe
  | THole (Maybe VarIdent)
  deriving (Eq, Ord, Show)

-- | The cube of a point.
cubeOf :: Point -> Cube
cubeOf = \case
  PVar _ c       -> c
  PStar          -> KUnit
  P2_0           -> K2
  P2_1           -> K2
  PI_0           -> KI
  PI_1           -> KI
  PPair l r      -> KProd (cubeOf l) (cubeOf r)
  PFst p         -> case cubeOf p of { KProd a _ -> a; _ -> KOther }
  PSnd p         -> case cubeOf p of { KProd _ b -> b; _ -> KOther }
  PSup l r       -> interval (cubeOf l) (cubeOf r)
  PInf l r       -> interval (cubeOf l) (cubeOf r)
  PFlip _        -> KOther
  PUnflip _      -> KOther
  PModApp{}      -> KOther
  PModExtract{}  -> KOther
  PApp c _ _     -> c
  PHole _        -> KOther
  POpaque _      -> KOther
  where
    interval K2 K2 = K2
    interval KI KI = KI
    interval KI K2 = KI
    interval K2 KI = KI
    interval _ _         = KOther

-- | The first component of a point of a product cube.
pointFirst :: Point -> Point
pointFirst (PPair a _) = a
pointFirst p           = PFst p

pointSecond :: Point -> Point
pointSecond (PPair _ b) = b
pointSecond p           = PSnd p

-- | Conjunction, with ⊥ absorbing and ⊤ dropped.
topeAnd :: Tope -> Tope -> Tope
topeAnd TBot _ = TBot
topeAnd _ TBot = TBot
topeAnd TTop r = r
topeAnd l TTop = l
topeAnd l r    = TAnd l r

-- | Disjunction, with ⊥ dropped.
topeOr :: Tope -> Tope -> Tope
topeOr TBot r = r
topeOr l TBot = l
topeOr l r    = TOr l r

isLatticePoint :: Point -> Bool
isLatticePoint PSup{} = True
isLatticePoint PInf{} = True
isLatticePoint _      = False

-- | A tope, together with the modalities under which it is available: the
-- lock accumulated since it was assumed, and the modality it was assumed
-- at.
data ModalTope = ModalTope
  { tModAccum :: TModality
  , tModVar   :: TModality
  , tTope     :: Tope
  } deriving (Eq, Ord, Show)

plainTope :: Tope -> ModalTope
plainTope = ModalTope Id Id

isAccessible :: ModalTope -> Bool
isAccessible mt = coe (tModVar mt) (tModAccum mt)

filterAccessible :: [ModalTope] -> [ModalTope]
filterAccessible = filter isAccessible

partitionAccessible :: [ModalTope] -> ([ModalTope], [ModalTope])
partitionAccessible topes = (filter isAccessible topes, filter (not . isAccessible) topes)

topesOf :: [ModalTope] -> [Tope]
topesOf = map tTope . filterAccessible

-- * The tope context

-- | The assumed topes and what the solver derives from them. The derived
-- parts are lazy: the alternatives of a disjunctive context, whether the
-- context is contradictory, and its saturation are computed by the first
-- query that needs them.
data TopeContext = TopeContext
  { tcTopes         :: [ModalTope]
    -- ^ the assumed topes, newest first
  , tcDiscrete      :: [ModalTope]
    -- ^ the discreteness axioms of the flat cube variables in scope
  , tcAlternatives  :: [[ModalTope]]
    -- ^ the context split at its disjunctions
  , tcEntailsBottom :: Bool
  , tcSaturated     :: [[ModalTope]]
    -- ^ each alternative saturated
  , tcRepresentatives :: Map Point Point
    -- ^ the representative of each point that the context makes equal to
    -- another one, and is not its own representative
  }

-- | The empty context: ⊤ holds under every modality.
emptyTopeContext :: TopeContext
emptyTopeContext = refresh TopeContext
  { tcTopes =
      [ ModalTope Id Id    TTop
      , ModalTope Id Flat  TTop
      , ModalTope Id Op    TTop
      , ModalTope Id Sharp TTop ]
  , tcDiscrete = []
  , tcAlternatives = []
  , tcEntailsBottom = False
  , tcSaturated = []
  , tcRepresentatives = Map.empty
  }

-- | Recompute the derived parts, lazily.
refresh :: TopeContext -> TopeContext
refresh tc = tc
  { tcAlternatives = alternativesOf (tcTopes tc)
  , tcEntailsBottom = entailsFrom tc (tcTopes tc) TBot
  , tcSaturated = saturated
  , tcRepresentatives = representativesOf saturated
  }
  where
    alternativesOf topes = map nub (simplifyLHSwithDisjunctions topes)
    saturated = saturateForEntailment (tcDiscrete tc) (tcTopes tc)

-- | The assumed topes, newest first, with their modalities.
contextTopes :: TopeContext -> [ModalTope]
contextTopes = tcTopes

-- | The assumed topes that are available at the current lock.
accessibleTopes :: TopeContext -> [Tope]
accessibleTopes = topesOf . tcTopes

-- | Assume a tope. A tope that says nothing new (a reflexive equation, or
-- one already assumed) leaves the context as it is, so that the derived
-- parts stay shared.
assume :: Tope -> TopeContext -> TopeContext
assume tope tc
  | noNewInformation = tc
  | otherwise = refresh tc { tcTopes = mt : tcTopes tc }
  where
    mt = plainTope tope
    noNewInformation = case tope of
      TTop           -> True
      TEq x y | x == y -> True
      _              -> mt `elem` tcTopes tc

-- | Add a discreteness axiom (a flat point of an interval is an endpoint).
assumeDiscrete :: Tope -> TopeContext -> TopeContext
assumeDiscrete tope tc = refresh tc { tcDiscrete = plainTope tope : tcDiscrete tc }

-- | Accumulate a lock over the assumed topes.
applyModality :: TModality -> TopeContext -> TopeContext
applyModality Id tc = tc
applyModality md tc = refresh tc
  { tcTopes = map lock (tcTopes tc)
  , tcDiscrete = map lock (tcDiscrete tc)
  }
  where
    lock mt = mt { tModAccum = comp (tModAccum mt) md }

-- * Queries

-- | Does the context entail the tope?
entails :: TopeContext -> Tope -> Bool
entails tc goal = all (`solveRHS` goal) (tcSaturated tc)

-- | The representative of a point's class: the points the context makes
-- equal to it, by the equations that hold in every alternative. An
-- endpoint represents its class if the class has one, else the variable
-- bound earliest (with its projections). 'Nothing' when the point is its
-- own representative.
representative :: TopeContext -> Point -> Maybe Point
representative tc p = Map.lookup p (tcRepresentatives tc)

-- | Is the context contradictory?
entailsBottom :: TopeContext -> Bool
entailsBottom = tcEntailsBottom

-- | Does the context entail the disjunction of the topes: is every point
-- of the context covered by one of them?
entailsUnion :: TopeContext -> [Tope] -> Bool
entailsUnion tc topes = entails tc (foldr topeOr TBot topes)

-- | Does the tope entail the conjunction of the context's accessible topes?
entailedBy :: TopeContext -> Tope -> Bool
entailedBy tc tope = entailsFrom tc [plainTope tope] (foldr topeAnd TTop (accessibleTopes tc))

-- | Is the tope disjoint from the context: does assuming it make the
-- context contradictory?
disjoint :: TopeContext -> Tope -> Bool
disjoint tc tope = entailsFrom tc (plainTope tope : tcTopes tc) TBot

-- | Are two topes equivalent on their own, under the context's
-- discreteness axioms?
equivalent :: TopeContext -> Tope -> Tope -> Bool
equivalent tc l r = l == r || (topesEntail tc [l] r && topesEntail tc [r] l)

-- | Do the topes alone (with the discreteness axioms) entail the goal?
topesEntail :: TopeContext -> [Tope] -> Tope -> Bool
topesEntail tc topes = entailsFrom tc (map plainTope topes)

-- | The alternatives of a disjunctive context, each as the context with
-- that alternative assumed instead; a context with one alternative has
-- none to split.
alternatives :: TopeContext -> [TopeContext]
alternatives tc = case tcAlternatives tc of
  [_] -> []
  alts -> [ refresh tc { tcTopes = alt } | alt <- alts ]

-- | Does a check hold in every alternative of the context? A context with
-- one alternative runs the check once, on itself.
inAlternatives :: TopeContext -> (TopeContext -> Bool) -> Bool
inAlternatives tc check = case alternatives tc of
  []   -> check tc
  alts -> all check alts

-- | Entailment from the given topes, with the context's discreteness
-- axioms.
entailsFrom :: TopeContext -> [ModalTope] -> Tope -> Bool
entailsFrom tc modalTopes goal =
  all (`solveRHS` goal) (saturateForEntailment (tcDiscrete tc) modalTopes)

-- * Representatives of points

-- | The representatives of the points of saturated alternatives. Only
-- variables, their projections and endpoints take part; a contradictory
-- alternative (which makes every point equal) and the topes under a
-- modality add nothing.
representativesOf :: [[ModalTope]] -> Map Point Point
representativesOf alts = case map equalities alts of
  [] -> Map.empty
  eqs : more ->
    let common = foldr (Map.intersectionWith Set.intersection) eqs more
    in Map.fromList
         [ (x, r)
         | (x, ys) <- Map.toList common
         , let r = minimumBy (comparing (\q -> (rank (cubeOf x) q, q))) (x : Set.toList ys)
         , r /= x ]
  where
    -- the points each point of an alternative is equal to
    equalities alt
      | TBot `elem` topes = Map.empty
      | otherwise = Map.fromListWith Set.union
          [ (x, Set.singleton y)
          | TEq x y <- topes, x /= y, refinable x, refinable y ]
      where topes = [ tTope mt | mt <- alt, isPlain mt ]
    isPlain mt = tModAccum mt == Id && tModVar mt == Id

    -- endpoints of the point's own cube first, then by the variable at
    -- the root (bound earliest first), then the shorter projection; ties
    -- (π₁ x against π₂ x) go by the order of points, so that the whole
    -- class agrees on one representative
    rank cube q = case q of
      _ | isEndpoint q, cubeOf q == cube -> (0 :: Int, 0, 0)
      _ | isEndpoint q -> (2, 0, 0)
      _ -> case root q of
        Just (i, depth) -> (1, i, depth)
        Nothing         -> (2, 0, 0)

    isEndpoint = \case
      PStar -> True
      P2_0  -> True
      P2_1  -> True
      PI_0  -> True
      PI_1  -> True
      _     -> False

    root = \case
      PVar i _ -> Just (i, 0 :: Int)
      PFst q   -> fmap (fmap (+ 1)) (root q)
      PSnd q   -> fmap (fmap (+ 1)) (root q)
      _        -> Nothing

    refinable q = isEndpoint q || maybe False (const True) (root q)

-- * Saturation

-- | The preprocessing before a search: dedup, split off the context's
-- disjunctions, and saturate each alternative.
saturateForEntailment :: [ModalTope] -> [ModalTope] -> [[ModalTope]]
saturateForEntailment discrete modalTopes =
  map (saturateTopes . saturateBottom . saturateInv) alts
  where
    alts = simplifyLHSwithDisjunctions (nub (modalTopes <> discrete))

saturateTopes :: [ModalTope] -> [ModalTope]
saturateTopes topes = saturated <> inaccessible
  where
    (accessible, inaccessible) = partitionAccessible topes
    saturated = saturateWith
      (\new old -> map plainTope (generateTopes (map tTope new) (map tTope old)))
      accessible

-- | Inversion under @ᵒᵖ@: the rules come with the modal fragment. Until
-- then a context whose topes all sit at the identity modality, which is
-- every context of the non-modal fragment, is left as it is.
saturateInv :: [ModalTope] -> [ModalTope]
saturateInv = id

-- | Ex falso for ⊥, lifted across modalities: a contradiction among the
-- topes available at the identity modality entails @_μ ⊥@ at every lock
-- where an available tope was hidden.
saturateBottom :: [ModalTope] -> [ModalTope]
saturateBottom modalTopes
  | null droppedAccums = modalTopes
  | botDerivable       = modalTopes <> fresh
  | otherwise          = modalTopes
  where
    idAccessible  = filter (\mt -> coe (tModVar mt) Id) modalTopes
    droppedAccums = nub [ tModAccum mt | mt <- idAccessible, not (isAccessible mt) ]
    saturatedId   = saturateWith generateTopes (map tTope idAccessible)
    botDerivable  = TBot `elem` saturatedId
    fresh = [ mt
            | acc <- droppedAccums
            , let mt = ModalTope acc acc TBot
            , mt `notElem` modalTopes ]

-- | Saturate a set under a step that derives new elements from the new
-- and the old ones, until nothing new comes.
saturateWith :: Ord a => ([a] -> [a] -> [a]) -> [a] -> [a]
saturateWith step zs = go (nubOrd zs) [] (Set.fromList zs)
  where
    go lastNew xs seen
      | null new  = lastNew
      | otherwise = lastNew <> go new xs' seen'
      where
        xs' = lastNew <> xs
        new = filter (`Set.notMember` seen) (nubOrd (step lastNew xs))
        seen' = foldr Set.insert seen new
    nubOrd = Set.toList . Set.fromList

generateTopes :: [Tope] -> [Tope] -> [Tope]
generateTopes newTopes oldTopes
  | TBot `elem` newTopes = []
  | TEq P2_0 P2_1 `elem` newTopes = [TBot]
  | TEq PI_0 PI_1 `elem` newTopes = [TBot]
  | length oldTopes > 100 = []    -- FIXME
  | otherwise = concat
      [  -- symmetry EQ
        [ TEq y x | TEq x y <- newTopes ]
        -- transitivity EQ (1)
      , [ TEq x z
        | TEq x y : newTopes' <- tails newTopes
        , TEq y' z <- newTopes' <> oldTopes
        , y == y' ]
        -- transitivity EQ (2)
      , [ TEq x z
        | TEq y z : newTopes' <- tails newTopes
        , TEq x y' <- newTopes' <> oldTopes
        , y == y' ]

        -- transitivity LEQ (1)
      , [ TLeq x z
        | TLeq x y : newTopes' <- tails newTopes
        , TLeq y' z <- newTopes' <> oldTopes
        , y == y' ]
        -- transitivity LEQ (2)
      , [ TLeq x z
        | TLeq y z : newTopes' <- tails newTopes
        , TLeq x y' <- newTopes' <> oldTopes
        , y == y' ]

        -- antisymmetry LEQ
      , [ TEq x y
        | TLeq x y : newTopes' <- tails newTopes
        , TLeq y' x' <- newTopes' <> oldTopes
        , y == y'
        , x == x' ]

        -- transitivity EQ-LEQ (1)
      , [ TLeq x z
        | TEq y z : newTopes' <- tails newTopes
        , TLeq x y' <- newTopes' <> oldTopes
        , y == y' ]
        -- transitivity EQ-LEQ (2)
      , [ TLeq x z
        | TEq x y : newTopes' <- tails newTopes
        , TLeq y' z <- newTopes' <> oldTopes
        , y == y' ]
        -- transitivity EQ-LEQ (3)
      , [ TLeq x z
        | TLeq y z : newTopes' <- tails newTopes
        , TEq x y' <- newTopes' <> oldTopes
        , y == y' ]
        -- transitivity EQ-LEQ (4)
      , [ TLeq x z
        | TLeq x y : newTopes' <- tails newTopes
        , TEq y' z <- newTopes' <> oldTopes
        , y == y' ]

        -- a point below 0 or above 1 is that endpoint
      , [ TEq x y | TLeq x y@P2_0 <- newTopes ]
      , [ TEq x y | TLeq x@P2_1 y <- newTopes ]
      , [ TEq x y | TLeq x y@PI_0 <- newTopes ]
      , [ TEq x y | TLeq x@PI_1 y <- newTopes ]

        -- subtyping 2 <: 𝕀: endpoints and order of 2 lift to 𝕀
      , [ TEq x PI_0 | TEq x P2_0 <- newTopes ]
      , [ TEq PI_0 x | TEq P2_0 x <- newTopes ]
      , [ TEq x PI_1 | TEq x P2_1 <- newTopes ]
      , [ TEq PI_1 x | TEq P2_1 x <- newTopes ]
      , [ TLeq x PI_0 | TLeq x P2_0 <- newTopes ]
      , [ TLeq PI_0 x | TLeq P2_0 x <- newTopes ]
      , [ TLeq x PI_1 | TLeq x P2_1 <- newTopes ]
      , [ TLeq PI_1 x | TLeq P2_1 x <- newTopes ]

      , [ TLeq a c | TLeq (PSup a _) c <- newTopes ]
      , [ TLeq b c | TLeq (PSup _ b) c <- newTopes ]
      , [ TLeq c a | TLeq c (PInf a _) <- newTopes ]
      , [ TLeq c b | TLeq c (PInf _ b) <- newTopes ]

        -- an equation between lattice points gives the two inequalities,
        -- which feed the sup and inf rules above
      , [ leq
        | TEq x y <- newTopes
        , isLatticePoint x || isLatticePoint y
        , leq <- [TLeq x y, TLeq y x] ]
      ]

-- | The lemmas about points that a disjunctive goal may case on: a point of
-- the unit cube is its point, and two points of @2@ are comparable.
generateTopesForPoints :: [Point] -> [Tope]
generateTopesForPoints points = concat (topes <> stars)
  where
    endpoints = [P2_0, P2_1, PI_0, PI_1]
    pairs = nub
      [ (x, y)
      | x : points' <- tails (filter (`notElem` endpoints) points)
      , y <- points'
      , x /= y ]
    stars = [ [TEq x PStar] | x <- points, cubeOf x == KUnit ]
    topes =
      [ [TOr (TLeq x y) (TLeq y x)]
      | (x, y) <- pairs
      , cubeOf x == K2, cubeOf y == K2 ]

allTopePoints :: Tope -> [Point]
allTopePoints = nub . foldMap subPoints . nub . topePoints

topePoints :: Tope -> [Point]
topePoints = \case
  TAnd l r -> topePoints l <> topePoints r
  TOr l r  -> topePoints l <> topePoints r
  TEq x y  -> [x, y]
  TLeq x y -> [x, y]
  _        -> []

subPoints :: Point -> [Point]
subPoints = \case
  p@(PPair x y) -> p : foldMap subPoints [x, y]
  p@PVar{}      -> [p]
  p -> case cubeOf p of
    KUnit -> [p]
    K2    -> [p]
    _        -> []

-- * Simplifying the left-hand side

-- | Simplify the context, splitting disjunctions into alternatives.
simplifyLHSwithDisjunctions :: [ModalTope] -> [[ModalTope]]
simplifyLHSwithDisjunctions topes = map nub $ case topes of
  [] -> [[]]
  ModalTope _ _ TTop : topes' -> simplifyLHSwithDisjunctions topes'
  ModalTope mAcc mVar TBot : _ -> [[ModalTope mAcc mVar TBot]]
  ModalTope mAcc mVar (TAnd l r) : topes' ->
    simplifyLHSwithDisjunctions (ModalTope mAcc mVar l : ModalTope mAcc mVar r : topes')
  ModalTope mAcc mVar (TOr l r) : topes' ->
    simplifyLHSwithDisjunctions (ModalTope mAcc mVar l : topes')
      <> simplifyLHSwithDisjunctions (ModalTope mAcc mVar r : topes')
  ModalTope mAcc mVar (TEq (PPair x y) (PPair x' y')) : topes' ->
    simplifyLHSwithDisjunctions
      (ModalTope mAcc mVar (TEq x x') : ModalTope mAcc mVar (TEq y y') : topes')
  ModalTope mAcc mVar (TModal md inTope) : topes' ->
    simplifyLHSwithDisjunctions (ModalTope mAcc (comp mVar md) inTope : topes')
  t : topes' -> map (t :) (simplifyLHSwithDisjunctions topes')

-- * Solving the right-hand side

-- | Search a saturated context for the goal.
solveRHS :: [ModalTope] -> Tope -> Bool
solveRHS modalTopes goal =
  let topes = topesOf modalTopes
  in case goal of
    _ | TBot `elem` topes -> True
    TTop -> True
    TModal md inTope ->
      let shifted = map (\mt -> mt { tModAccum = comp (tModAccum mt) md }) modalTopes
      in solveRHS (saturateInv (saturateTopes shifted)) inTope
    TEq (PPair x y) (PPair x' y') ->
      solveRHS modalTopes (TAnd (TEq x x') (TEq y y'))
    TEq l@(PPair x y) r | KProd _ _ <- cubeOf l ->
      solveRHS modalTopes (TAnd (TEq x (pointFirst r)) (TEq y (pointSecond r)))
    TEq l r@(PPair x y) | KProd _ _ <- cubeOf r ->
      solveRHS modalTopes (TAnd (TEq (pointFirst l) x) (TEq (pointSecond l) y))
    TEq P2_0 PI_0 -> True
    TEq PI_0 P2_0 -> True
    TEq P2_1 PI_1 -> True
    TEq PI_1 P2_1 -> True
    TEq l r
      | l == r -> True
      | goal `elem` topes -> True
      | TEq r l `elem` topes -> True
      | otherwise -> case (cubeOf l, cubeOf r) of
          (KUnit, KUnit) -> True
          -- antisymmetry proves an equation between lattice points
          _ | isLatticePoint l || isLatticePoint r ->
                solveRHS modalTopes (TAnd (TLeq l r) (TLeq r l))
          _ -> False
    TLeq l r
      | l == r -> True
      | goal `elem` topes -> True
      | solveTopes topes (TEq l r) -> True
      | solveTopes topes (TEq l P2_0) -> True
      | solveTopes topes (TEq l PI_0) -> True
      | solveTopes topes (TEq r P2_1) -> True
      | solveTopes topes (TEq r PI_1) -> True
    TLeq (PSup a b) r ->
      solveRHS modalTopes (TAnd (TLeq a r) (TLeq b r))
    TLeq l (PInf a b) ->
      solveRHS modalTopes (TAnd (TLeq l a) (TLeq l b))
    TLeq l (PSup a b) ->
      solveRHS modalTopes (TOr (TLeq l a) (TLeq l b))
    TLeq (PInf a b) r ->
      solveRHS modalTopes (TOr (TLeq a r) (TLeq b r))
    TAnd l r -> solveRHS modalTopes l && solveRHS modalTopes r
    _ | goal `elem` topes -> True
    TInv{} -> False
    TUninv{} -> False
    TOr l r
      | solveRHS modalTopes l || solveRHS modalTopes r -> True
      | otherwise ->
          -- case on a lemma about the goal's points whose sides are not
          -- yet in the context
          let lems = [ lem | lem@(TOr t1 t2) <- generateTopesForPoints (allTopePoints goal)
                           , all (`notElem` topes) [t1, t2] ]
              (accessible, hidden) = partitionAccessible modalTopes
              withTope t = hidden ++ saturateTopes (plainTope t : accessible)
          in case lems of
               TOr t1 t2 : _ -> solveRHS (withTope t1) goal && solveRHS (withTope t2) goal
               _ -> False
    _ -> False

-- | The syntactic part of the search, on the accessible topes alone.
solveTopes :: [Tope] -> Tope -> Bool
solveTopes topes tope = case tope of
  _ | TBot `elem` topes -> True
  TTop -> True
  TEq (PPair x y) (PPair x' y')
    | solveTopes topes (TEq x x') && solveTopes topes (TEq y y') -> True
  TEq l@(PPair x y) r
    | KProd _ _ <- cubeOf l
    , solveTopes topes (TEq x (pointFirst r))
    , solveTopes topes (TEq y (pointSecond r)) -> True
  TEq l r@(PPair x y)
    | KProd _ _ <- cubeOf r
    , solveTopes topes (TEq (pointFirst l) x)
    , solveTopes topes (TEq (pointSecond l) y) -> True
  TEq P2_0 PI_0 -> True
  TEq PI_0 P2_0 -> True
  TEq P2_1 PI_1 -> True
  TEq PI_1 P2_1 -> True
  TEq l r -> or
    [ l == r
    , tope `elem` topes
    , TEq r l `elem` topes ]
  TLeq l r
    | l == r -> True
    | solveTopes topes (TEq l r) -> True
    | solveTopes topes (TEq l P2_0) -> True
    | solveTopes topes (TEq l PI_0) -> True
    | solveTopes topes (TEq r P2_1) -> True
    | solveTopes topes (TEq r PI_1) -> True
  TAnd l r -> solveTopes topes l && solveTopes topes r
  TOr l r -> solveTopes topes l || solveTopes topes r
  _ -> tope `elem` topes

