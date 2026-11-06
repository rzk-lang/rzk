{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Showing a term to the user: the names of a context, and a core term
-- back to surface syntax under them.
--
-- A term of the core names its variables by 'Foil.Name', so anything
-- user-facing has to say what each name is called: a 'Naming' gives a
-- display name per name in scope and a supply of fresh names for the
-- binders the printer meets. A binder inside the term is freshened only
-- against the names the term mentions, so a type shows the binder it was
-- written with.
module Rzk.Check.Display
  ( Naming (..)
  , namingOfContext
  , displayOf
  , ppName
  , renderCore
  , ppCore
  , ppValue
  , ppValueIn
  , panicImpossible
  ) where

import           Control.Monad.Foil          (Distinct, NameMap, Scope)
import qualified Control.Monad.Foil          as Foil
import           Control.Monad.Foil.Internal (NameMap (..))
import           Control.Monad.Free.Foil     (AST (..), ScopedAST (..))
import           Data.Bifoldable             (bifoldMap)
import qualified Data.IntMap                 as IntMap
import           Data.List                   (nub, (\\))
import qualified Data.Set                    as Set

import           Language.Rzk.Foil.Names     (TModality (..), VarIdent,
                                              defaultVarIdents, fromMod,
                                              fromTModalityToModalColon,
                                              fromVarIdent, holeIdentToken,
                                              refreshVar, refreshVarIn)
import qualified Language.Rzk.Syntax         as Rzk
import           Rzk.Check.Context
import           Rzk.Check.Syntax
import           Rzk.Check.Value             (quoteFolded)

-- | What every name in scope is called, and what the printer may call the
-- binders it has yet to meet.
data Naming n = Naming
  { namingOf     :: NameMap n VarIdent
  , namingUsed   :: [VarIdent]
  , namingSupply :: [VarIdent]
  }

-- | Read the naming off a context: a named entry keeps its name, refreshed
-- if an outer one has taken it; an anonymous one draws from the supply.
-- Entries are named oldest binding first, so an outer binder keeps its
-- name and an inner one is refreshed away from it.
namingOfContext :: Context d n -> Naming n
namingOfContext ctx = Naming
  { namingOf = NameMap (IntMap.fromList entries)
  , namingUsed = used
  , namingSupply = defaultVarIdents \\ used
  }
  where
    (entries, usedSet) = go Set.empty defaultVarIdents (entriesInScope ctx)
    used = Set.toList usedSet
    go taken _ [] = ([], taken)
    go taken supply ((v, entry) : rest) = case entryName entry of
      Just x ->
        let x' = refreshVarIn taken x
        in named v x' supply
      Nothing -> case supply of
        x : supply' -> named v x supply'
        []          -> panicImpossible "not enough fresh variables"
      where
        named v' x supply' =
          let (acc, taken') = go (Set.insert x taken) supply' rest
          in ((Foil.nameId v', x) : acc, taken')

displayOf :: Naming n -> Foil.Name n -> VarIdent
displayOf naming name = Foil.lookupName name (namingOf naming)

ppName :: Naming n -> Foil.Name n -> String
ppName naming = Rzk.printTree . fromVarIdent . displayOf naming

-- | A core term as surface syntax, with the context's names.
renderCore :: Naming n -> Core n -> Rzk.Term
renderCore naming t = fromCore used supply (namingOf naming) t
  where
    used = nub [ displayOf naming v | v <- freeNamesOf t ]
    supply = defaultVarIdents \\ used

ppCore :: Naming n -> Core n -> String
ppCore naming = Rzk.printTree . renderCore naming

-- | A value shown to the user: read back with defined names folded.
ppValue :: Distinct n => Scope n -> Context d n -> Value n -> String
ppValue scope ctx = ppCore (namingOfContext ctx) . quoteFolded scope ctx

ppValueIn :: Distinct n => Context d n -> Value n -> String
ppValueIn ctx = ppValue (ctxScope ctx) ctx

-- | The printer proper: the names in use (which a binder must avoid), the
-- supply for anonymous binders, and the display name of every free name.
fromCore :: forall n. [VarIdent] -> [VarIdent] -> NameMap n VarIdent -> Core n -> Rzk.Term
fromCore used supply names = go
  where
    loc = Nothing

    go :: Core n -> Rzk.Term
    go = \case
      Var x -> Rzk.Var loc (fromVarIdent (Foil.lookupName x names))
      Universe -> Rzk.Universe loc
      UniverseCube -> Rzk.UniverseCube loc
      UniverseTope -> Rzk.UniverseTope loc
      CubeUnit -> Rzk.CubeUnit loc
      CubeUnitStar -> Rzk.CubeUnitStar loc
      Cube2 -> Rzk.Cube2 loc
      Cube2_0 -> Rzk.Cube2_0 loc
      Cube2_1 -> Rzk.Cube2_1 loc
      CubeI -> Rzk.CubeI loc
      CubeI_0 -> Rzk.CubeI_0 loc
      CubeI_1 -> Rzk.CubeI_1 loc
      CubeProduct l r -> Rzk.CubeProduct loc (go l) (go r)
      CubeSup l r -> Rzk.CubeSup loc (go l) (go r)
      CubeInf l r -> Rzk.CubeInf loc (go l) (go r)
      CubeFlip t -> Rzk.CubeFlip loc (go t)
      CubeUnflip t -> Rzk.CubeUnflip loc (go t)
      TopeTop -> Rzk.TopeTop loc
      TopeBottom -> Rzk.TopeBottom loc
      TopeEQ l r -> Rzk.TopeEQ loc (go l) (go r)
      TopeLEQ l r -> Rzk.TopeLEQ loc (go l) (go r)
      TopeAnd l r -> Rzk.TopeAnd loc (go l) (go r)
      TopeOr l r -> Rzk.TopeOr loc (go l) (go r)
      TopeInv t -> Rzk.TopeInv loc (go t)
      TopeUninv t -> Rzk.TopeUninv loc (go t)
      RecBottom -> Rzk.RecBottom loc
      RecOr rs -> Rzk.RecOr loc [ Rzk.Restriction loc (go tope) (go term) | (tope, term) <- rs ]
      Hole mname -> Rzk.Hole loc (Rzk.HoleIdent loc (Rzk.HoleIdentToken (holeIdentToken mname)))

      -- an anonymous binder the codomain does not use is not shown
      TypeFun Id arg Nothing ret@(ScopedAST (PatternVar Nothing _) _)
        | not (scopeUsesItsPattern ret) ->
            withScope ret $ \_ ret' -> Rzk.TypeFun loc (Rzk.ParamType loc (go arg)) ret'
      TypeFun md arg Nothing ret -> withScope ret $ \pat ret' ->
        let patTerm = patternTerm pat
        in case md of
             Id -> Rzk.TypeFun loc (Rzk.ParamTermType loc patTerm (go arg)) ret'
             _  -> Rzk.TypeFun loc (Rzk.ParamTermModalType loc patTerm (fromTModalityToModalColon md) (go arg)) ret'
      TypeFun md cube (Just shape) ret -> withScopeAndShape ret shape $ \pat tope ret' ->
        let patTerm = patternTerm pat
        in case md of
             Id -> Rzk.TypeFun loc (Rzk.ParamTermShape loc patTerm (go cube) tope) ret'
             _  -> Rzk.TypeFun loc (Rzk.ParamTermModalShape loc patTerm (fromTModalityToModalColon md) (go cube) tope) ret'

      TypeSigma md a b -> withScope b $ \pat b' ->
        case md of
          Id -> Rzk.TypeSigma loc pat (go a) b'
          _  -> Rzk.TypeSigmaModal loc pat (fromTModalityToModalColon md) (go a) b'

      TypeId l (Just tA) r -> Rzk.TypeId loc (go l) (go tA) (go r)
      TypeId l Nothing r -> Rzk.TypeIdSimple loc (go l) (go r)
      App l r -> Rzk.App loc (go l) (go r)

      Lambda Nothing body -> withScope body $ \pat body' ->
        Rzk.Lambda loc [Rzk.ParamPattern loc pat] body'
      Lambda (Just (Param md ty Nothing)) body -> withScope body $ \pat body' ->
        let param = case md of
              Id -> Rzk.ParamPatternType loc [pat] (go ty)
              _  -> Rzk.ParamPatternModalType loc [pat] (fromTModalityToModalColon md) (go ty)
        in Rzk.Lambda loc [param] body'
      Lambda (Just (Param md cube (Just shape))) body -> withScopeAndShape body shape $ \pat tope body' ->
        let param = case md of
              Id -> Rzk.ParamPatternShape loc [pat] (go cube) tope
              _  -> Rzk.ParamPatternModalShape loc [pat] (fromTModalityToModalColon md) (go cube) tope
        in Rzk.Lambda loc [param] body'

      Let mty val body -> withScope body $ \pat body' ->
        let bind = case mty of
              Nothing -> Rzk.BindPattern loc pat
              Just ty -> Rzk.BindPatternType loc pat (go ty)
        in Rzk.Let loc bind (go val) body'

      Pair l r -> Rzk.Pair loc (go l) (go r)
      First t -> Rzk.First loc (go t)
      Second t -> Rzk.Second loc (go t)
      TypeUnit -> Rzk.TypeUnit loc
      Unit -> Rzk.Unit loc
      Refl Nothing -> Rzk.Refl loc
      Refl (Just (t, Nothing)) -> Rzk.ReflTerm loc (go t)
      Refl (Just (t, Just ty)) -> Rzk.ReflTermType loc (go t) (go ty)
      IdJ a b c d e f -> Rzk.IdJ loc (go a) (go b) (go c) (go d) (go e) (go f)

      Match md scrut mmotive branches ->
        case (md, mmotive) of
          (Id, Nothing)     -> Rzk.Match loc (go scrut) (map goBranch branches)
          (Id, Just motive) -> Rzk.MatchInto loc (go scrut) (go motive) (map goBranch branches)
          (_, Nothing)      -> Rzk.MatchModal loc (fromMod md) (go scrut) (map goBranch branches)
          (_, Just motive)  -> Rzk.MatchModalInto loc (fromMod md) (go scrut) (go motive) (map goBranch branches)
        where
          goBranch (con, chain) = Rzk.MatchBranch loc (fromVarIdent con) pats body
            where (pats, body) = matchArms used supply names chain
      MatchArm{} -> panicImpossible "a match arm outside of a match branch"
      TypeAsc l r -> Rzk.TypeAsc loc (go l) (go r)
      TypeRestricted ty rs ->
        Rzk.TypeRestricted loc (go ty) [ Rzk.Restriction loc (go tope) (go term) | (tope, term) <- rs ]
      TypeModal m ty -> Rzk.ModType loc (fromMod m) (go ty)
      ModApp m t -> Rzk.ModApp loc (fromMod m) (go t)
      ModExtract app inn t -> Rzk.ModExtract loc (Rzk.Comp loc (fromMod app) (fromMod inn)) (go t)
      LetMod app inn mty mmotive val body -> withScope body $ \pat body' ->
        let bind = case mty of
              Nothing -> Rzk.BindPattern loc pat
              Just ty -> Rzk.BindPatternType loc pat (go ty)
        in case (app, mmotive) of
             (Id, Nothing)     -> Rzk.LetMod loc (fromMod inn) bind (go val) body'
             (Id, Just motive) -> Rzk.LetModInto loc (fromMod inn) bind (go val) (go motive) body'
             (_, Nothing)      -> Rzk.LetModFramed loc (fromMod app) (fromMod inn) bind (go val) body'
             (_, Just motive)  -> Rzk.LetModFramedInto loc (fromMod app) (fromMod inn) bind (go val) (go motive) body'

    -- Enter a scope: name its pattern and print the body under it.
    withScope :: ScopedCore n -> (Rzk.Pattern -> Rzk.Term -> r) -> r
    withScope (ScopedAST pat body) k =
      withPatternNames pat (scopeUses body) $ \pat' used' supply' names' ->
        k pat' (fromCore used' supply' names' body)

    -- A binder with a shape: the shape is a tope family, shown as its body
    -- under the binder's names when it is a λ over a pattern of the same
    -- shape, otherwise applied to the binder.
    withScopeAndShape :: ScopedCore n -> Core n -> (Rzk.Pattern -> Rzk.Term -> Rzk.Term -> r) -> r
    withScopeAndShape (ScopedAST pat body) shape k =
      withPatternNames pat (scopeUses body) $ \pat' used' supply' names' ->
        let body' = fromCore used' supply' names' body
            tope = case shape of
              Lambda _ (ScopedAST shapePat topeBody)
                | Just names'' <- sameShape shapePat pat names' ->
                    fromCore used' supply' names'' topeBody
              _ -> Rzk.App loc (go shape) (patternTerm pat')
        in k pat' tope body'

    -- The display names of a pattern's leaves given to another pattern of
    -- the same shape.
    sameShape :: Pattern x y -> Pattern n l -> NameMap l VarIdent -> Maybe (NameMap y VarIdent)
    sameShape shapePat pat names' =
      let leavesS = map snd (patternNames shapePat)
          leavesP = map snd (patternNames pat)
      in if length leavesS == length leavesP && shapeOf shapePat == shapeOf pat
           then Just $ NameMap $ foldr
                  (\(s, p) m -> IntMap.insert (Foil.nameId s) (Foil.lookupName p names') m)
                  (coerceNames names') (zip leavesS leavesP)
           else Nothing
    shapeOf :: Pattern x y -> String
    shapeOf = \case
      PatternVar _ _  -> "v"
      PatternUnit _   -> "u"
      PatternPair l r -> "(" <> shapeOf l <> "," <> shapeOf r <> ")"
    coerceNames :: NameMap a VarIdent -> IntMap.IntMap VarIdent
    coerceNames (NameMap m) = m

    -- Which bound names the body mentions, by id.
    scopeUses :: Core l -> [Int]
    scopeUses = nameIdsOf

    -- Name every leaf of a pattern: a named leaf keeps its name, refreshed
    -- against the names in use; an anonymous leaf is @_@ unless the body
    -- uses it, when it draws from the supply.
    withPatternNames
      :: forall l r'. Pattern n l -> [Int]
      -> (Rzk.Pattern -> [VarIdent] -> [VarIdent] -> NameMap l VarIdent -> r') -> r'
    withPatternNames pat0 usedIds k =
      let (pat', used', supply', entries) = goPat used supply [] pat0
      in k pat' used' supply' (NameMap (IntMap.union (IntMap.fromList entries) (coerceNames names)))
      where
        goPat :: [VarIdent] -> [VarIdent] -> [(Int, VarIdent)] -> Pattern x y
              -> (Rzk.Pattern, [VarIdent], [VarIdent], [(Int, VarIdent)])
        goPat u s acc = \case
          PatternVar (Just x) b ->
            let x' = refreshVar u x
            in (Rzk.PatternVar loc (fromVarIdent x'), x' : u, filter (/= x') s, (Foil.nameId (Foil.nameOf b), x') : acc)
          PatternVar Nothing b
            | Foil.nameId (Foil.nameOf b) `elem` usedIds, x : s' <- s ->
                (Rzk.PatternVar loc (fromVarIdent x), x : u, s', (Foil.nameId (Foil.nameOf b), x) : acc)
            | otherwise ->
                (Rzk.PatternVar loc (fromVarIdent "_"), u, s, (Foil.nameId (Foil.nameOf b), "_") : acc)
          PatternUnit b -> (Rzk.PatternUnit loc, u, s, (Foil.nameId (Foil.nameOf b), "unit") : acc)
          PatternPair l r ->
            let (l', u1, s1, acc1) = goPat u s acc l
                (r', u2, s2, acc2) = goPat u1 s1 acc1 r
            in (Rzk.PatternPair loc l' r', u2, s2, acc2)

-- | A surface pattern as a term, for a binder written as a term.
patternTerm :: Rzk.Pattern -> Rzk.Term
patternTerm = \case
  Rzk.PatternVar l x -> Rzk.Var l x
  Rzk.PatternPair l a b -> Rzk.Pair l (patternTerm a) (patternTerm b)
  Rzk.PatternUnit l -> Rzk.Unit l
  Rzk.PatternTuple l a b cs -> Rzk.Tuple l (patternTerm a) (patternTerm b) (map patternTerm cs)

-- | Peel a match branch's arm chain back into its patterns and body.
matchArms :: [VarIdent] -> [VarIdent] -> NameMap n VarIdent -> Core n -> ([Rzk.Pattern], Rzk.Term)
matchArms used supply names = \case
  MatchArm (ScopedAST pat body) ->
    case renderPattern used supply names pat (nameIdsOf body) of
      (pat', used', supply', names') ->
        let (pats, t) = matchArms used' supply' names' body
        in (pat' : pats, t)
  body -> ([], fromCore used supply names body)
  where
    renderPattern u s nm pat usedIds =
      let (pat', u', s', entries) = goPat u s [] pat
          NameMap m = nm
      in (pat', u', s', NameMap (IntMap.union (IntMap.fromList entries) m))
      where
        goPat :: [VarIdent] -> [VarIdent] -> [(Int, VarIdent)] -> Pattern x y
              -> (Rzk.Pattern, [VarIdent], [VarIdent], [(Int, VarIdent)])
        goPat u0 s0 acc = \case
          PatternVar (Just x) b ->
            let x' = refreshVar u0 x
            in (Rzk.PatternVar Nothing (fromVarIdent x'), x' : u0, filter (/= x') s0, (Foil.nameId (Foil.nameOf b), x') : acc)
          PatternVar Nothing b
            | Foil.nameId (Foil.nameOf b) `elem` usedIds, x : s1 <- s0 ->
                (Rzk.PatternVar Nothing (fromVarIdent x), x : u0, s1, (Foil.nameId (Foil.nameOf b), x) : acc)
            | otherwise ->
                (Rzk.PatternVar Nothing (fromVarIdent "_"), u0, s0, (Foil.nameId (Foil.nameOf b), "_") : acc)
          PatternUnit b -> (Rzk.PatternUnit Nothing, u0, s0, (Foil.nameId (Foil.nameOf b), "unit") : acc)
          PatternPair l r ->
            let (l', u1, s1, acc1) = goPat u0 s0 acc l
                (r', u2, s2, acc2) = goPat u1 s1 acc1 r
            in (Rzk.PatternPair Nothing l' r', u2, s2, acc2)

-- | Does a scope use the names its pattern binds?
scopeUsesItsPattern :: ScopedCore n -> Bool
scopeUsesItsPattern (ScopedAST pat body) =
  any (`elem` nameIdsOf body) (map (Foil.nameId . snd) (patternNames pat))

-- | Every name id occurring in a term, bound or free.
nameIdsOf :: Core l -> [Int]
nameIdsOf (Var x) = [Foil.nameId x]
nameIdsOf (Node sig) = bifoldMap (\(ScopedAST _ body) -> nameIdsOf body) nameIdsOf sig

panicImpossible :: String -> a
panicImpossible msg = error $ unlines
  [ "PANIC! Impossible happened (" <> msg <> ")!"
  , "Please, report a bug at https://github.com/rzk-lang/rzk/issues"
  ]
