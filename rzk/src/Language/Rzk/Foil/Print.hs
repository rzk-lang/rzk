{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The free-foil core back to surface syntax.
--
-- A transcription of @fromTermWith'@ from "Language.Rzk.Foil.Names". The
-- structure is the same, and so are the display rules:
--
--   * a binder's user-written name is kept, refreshed only against names already
--     in use; an anonymous binder draws from 'defaultVarIdents';
--   * a pattern binder is shown as the pattern, and projections of the variable
--     it binds are folded back to the component names, so a goal reads
--     @\\ (t , s) -> …@ and not @\\ x -> … π₁ x …@;
--   * an anonymous binder the codomain does not use is not shown at all, so
--     @(x₁ : A) → B@ prints as @A → B@.
--
-- What changes is the bookkeeping: a variable is a 'Foil.Name', so the display
-- names live in a 'Foil.NameMap' keyed by name, rather than being threaded
-- through de Bruijn shifts.
module Language.Rzk.Foil.Print where

import           Control.Monad.Foil       (NameMap)
import qualified Control.Monad.Foil       as Foil
import           Control.Monad.Free.Foil  (AST (..), ScopedAST (..))
import           Data.Bifoldable          (bifoldMap)

import           Language.Rzk.Foil.Syntax
import           Language.Rzk.Foil.Names (Binder (..), Display, Proj (..),
                                           TModality (..),
                                           VarIdent, binderIsCompound,
                                           binderLeaves, binderPaths,
                                           binderToPattern, defaultVarIdents,
                                           fromTModalityToModalColon,
                                           fromVarIdent, fromMod, holeIdentToken,
                                           modsToModComp, patternToTerm,
                                           refreshVar)
import qualified Language.Rzk.Syntax      as Rzk

-- | Print a closed term.
fromTermClosed :: Term Foil.VoidS -> Rzk.Term
fromTermClosed = fromTerm [] defaultVarIdents Foil.emptyNameMap

fromTerm :: forall n. [VarIdent] -> [VarIdent] -> NameMap n Display -> Term n -> Rzk.Term
fromTerm used supply names = go
  where
    loc = Nothing

    goMod :: TModality -> Rzk.Modality
    goMod = fromMod

    -- Enter a binder and print the scopes it binds over.
    --
    -- A Pi-type and a lambda each bind /two/ scopes under one binder (the shape
    -- tope and the body). In the old representation both were indexed by the
    -- same de Bruijn Z; here each 'ScopedAST' carries its own 'NameBinder'. That
    -- is operationally the same (the checker instantiates both with the same
    -- argument), but they must be /shown/ under one name, so each scope's binder
    -- is mapped to the same display name.
    withBinder1 :: Binder -> ScopedTerm n -> ((Binder, Rzk.Term) -> r) -> r
    withBinder1 z s k = withBinder z $ \z' printScope -> k (z', printScope s)

    withBinder2 :: Binder -> ScopedTerm n -> ScopedTerm n -> ((Binder, Rzk.Term, Rzk.Term) -> r) -> r
    withBinder2 z s1 s2 k =
      withBinder z $ \z' printScope -> k (z', printScope s1, printScope s2)

    withBinder
      :: Binder
      -> (Binder -> (ScopedTerm n -> Rzk.Term) -> r)
      -> r
    withBinder z k = k z' printScope
      where
        (z', supply') = freshenBinder used supply z
        x = displayNameOf z' supply'
        supply'' = case z' of
          BinderVar (Just _) -> supply'
          _                  -> drop 1 supply'
        used' = x : used <> binderLeaves z'
        printScope (ScopedAST binder body) =
          fromTerm used' supply'' (Foil.addNameBinder binder (x, z') names) body

    -- A projection chain over a pattern binder's variable is shown as the
    -- component's name: @π₁ x@ is @t@.
    projChain :: Term n -> Maybe ([Proj], Foil.Name n)
    projChain (First t)  = fmap (\(ps, x) -> (PFst : ps, x)) (projChain t)
    projChain (Second t) = fmap (\(ps, x) -> (PSnd : ps, x)) (projChain t)
    projChain (Var x)    = Just ([], x)
    projChain _          = Nothing

    foldedProjection :: Term n -> Maybe Rzk.Term
    foldedProjection t = do
      (ps, x) <- projChain t
      case ps of
        [] -> Nothing
        _  -> do
          let (_, binder) = Foil.lookupName x names
          leaf <- lookup (reverse ps) (binderPaths binder)
          pure (Rzk.Var loc (fromVarIdent leaf))

    go :: Term n -> Rzk.Term
    go t | Just t' <- foldedProjection t = t'
    go (Var x) =
      case Foil.lookupName x names of
        -- A bare use of a pattern binder's variable (the point itself) reads as
        -- the pattern, not as the placeholder.
        (_, binder) | binderIsCompound binder -> patternToTerm (binderToPattern binder)
        (name, _)                             -> Rzk.Var loc (fromVarIdent name)

    go Universe = Rzk.Universe loc
    go UniverseCube = Rzk.UniverseCube loc
    go UniverseTope = Rzk.UniverseTope loc
    go CubeUnit = Rzk.CubeUnit loc
    go CubeUnitStar = Rzk.CubeUnitStar loc
    go Cube2 = Rzk.Cube2 loc
    go Cube2_0 = Rzk.Cube2_0 loc
    go Cube2_1 = Rzk.Cube2_1 loc
    go CubeI = Rzk.CubeI loc
    go CubeI_0 = Rzk.CubeI_0 loc
    go CubeI_1 = Rzk.CubeI_1 loc
    go (CubeProduct l r) = Rzk.CubeProduct loc (go l) (go r)
    go (CubeSup l r) = Rzk.CubeSup loc (go l) (go r)
    go (CubeInf l r) = Rzk.CubeInf loc (go l) (go r)
    go (CubeFlip t) = Rzk.CubeFlip loc (go t)
    go (CubeUnflip t) = Rzk.CubeUnflip loc (go t)
    go TopeTop = Rzk.TopeTop loc
    go TopeBottom = Rzk.TopeBottom loc
    go (TopeEQ l r) = Rzk.TopeEQ loc (go l) (go r)
    go (TopeLEQ l r) = Rzk.TopeLEQ loc (go l) (go r)
    go (TopeAnd l r) = Rzk.TopeAnd loc (go l) (go r)
    go (TopeOr l r) = Rzk.TopeOr loc (go l) (go r)
    go (TopeInv t) = Rzk.TopeInv loc (go t)
    go (TopeUninv t) = Rzk.TopeUninv loc (go t)
    go RecBottom = Rzk.RecBottom loc
    go (RecOr rs) = Rzk.RecOr loc [Rzk.Restriction loc (go tope) (go term) | (tope, term) <- rs]
    go (Hole mname) = Rzk.Hole loc (Rzk.HoleIdent loc (Rzk.HoleIdentToken (holeIdentToken mname)))

    -- An anonymous binder the codomain does not use is not shown: @(x₁ : A) → B@
    -- reads better as @A → B@. A user-written name is kept even when unused.
    go (TypeFun z@(BinderVar Nothing) Id arg Nothing ret)
      | not (scopeUsesItsBinder ret) = withBinder1 z ret $ \(_z', ret') ->
          Rzk.TypeFun loc (Rzk.ParamType loc (go arg)) ret'
    go (TypeFun z md arg Nothing ret) = withBinder1 z ret $ \(z', ret') ->
      let pat = patternToTerm (binderToPattern z')
       in case md of
            Id -> Rzk.TypeFun loc (Rzk.ParamTermType loc pat (go arg)) ret'
            _  -> Rzk.TypeFun loc (Rzk.ParamTermModalType loc pat (fromTModalityToModalColon md) (go arg)) ret'
    go (TypeFun z md arg (Just tope) ret) = withBinder2 z tope ret $ \(z', tope', ret') ->
      let pat = patternToTerm (binderToPattern z')
       in case md of
            Id -> Rzk.TypeFun loc (Rzk.ParamTermShape loc pat (go arg) tope') ret'
            _  -> Rzk.TypeFun loc (Rzk.ParamTermModalShape loc pat (fromTModalityToModalColon md) (go arg) tope') ret'

    go (TypeSigma z md a b) = withBinder1 z b $ \(z', b') ->
      case md of
        Id -> Rzk.TypeSigma loc (binderToPattern z') (go a) b'
        _  -> Rzk.TypeSigmaModal loc (binderToPattern z') (fromTModalityToModalColon md) (go a) b'

    go (TypeId l (Just tA) r) = Rzk.TypeId loc (go l) (go tA) (go r)
    go (TypeId l Nothing r) = Rzk.TypeIdSimple loc (go l) (go r)
    go (App l r) = Rzk.App loc (go l) (go r)

    go (Lambda z Nothing body) = withBinder1 z body $ \(z', body') ->
      Rzk.Lambda loc [Rzk.ParamPattern loc (binderToPattern z')] body'
    go (Lambda z (Just (LambdaParam md ty Nothing)) body) = withBinder1 z body $ \(z', body') ->
      let pat = binderToPattern z'
          param = case md of
            Id -> Rzk.ParamPatternType loc [pat] (go ty)
            _  -> Rzk.ParamPatternModalType loc [pat] (fromTModalityToModalColon md) (go ty)
       in Rzk.Lambda loc [param] body'
    go (Lambda z (Just (LambdaParam md cube (Just tope))) body) =
      withBinder2 z tope body $ \(z', tope', body') ->
        let pat = binderToPattern z'
            param = case md of
              Id -> Rzk.ParamPatternShape loc [pat] (go cube) tope'
              _  -> Rzk.ParamPatternModalShape loc [pat] (fromTModalityToModalColon md) (go cube) tope'
         in Rzk.Lambda loc [param] body'

    go (Let z mty val body) = withBinder1 z body $ \(z', body') ->
      let bind = case mty of
            Nothing -> Rzk.BindPattern loc (binderToPattern z')
            Just ty -> Rzk.BindPatternType loc (binderToPattern z') (go ty)
       in Rzk.Let loc bind (go val) body'

    go (Pair l r) = Rzk.Pair loc (go l) (go r)
    go (First t) = Rzk.First loc (go t)
    go (Second t) = Rzk.Second loc (go t)
    go TypeUnit = Rzk.TypeUnit loc
    go Unit = Rzk.Unit loc
    go (Refl Nothing) = Rzk.Refl loc
    go (Refl (Just (t, Nothing))) = Rzk.ReflTerm loc (go t)
    go (Refl (Just (t, Just ty))) = Rzk.ReflTermType loc (go t) (go ty)
    go (IdJ a b c d e f) = Rzk.IdJ loc (go a) (go b) (go c) (go d) (go e) (go f)

    go (Match scrut mmotive branches) =
      case mmotive of
        Nothing     -> Rzk.Match loc (go scrut) (map goBranch branches)
        Just motive -> Rzk.MatchInto loc (go scrut) (go motive) (map goBranch branches)
      where
        goBranch (con, chain) = Rzk.MatchBranch loc (fromVarIdent con) pats body
          where (pats, body) = matchArms used supply names chain
    -- An arm never stands alone: the conversion only builds it inside a match
    -- branch, and 'matchArms' peels it before 'go' can see it.
    go MatchArm{} = error "fromTerm: MatchArm outside of a match branch"
    go (TypeAsc l r) = Rzk.TypeAsc loc (go l) (go r)
    go (TypeRestricted ty rs) =
      Rzk.TypeRestricted loc (go ty) [Rzk.Restriction loc (go tope) (go term) | (tope, term) <- rs]
    go (TypeModal m ty) = Rzk.ModType loc (goMod m) (go ty)
    go (ModApp m t) = Rzk.ModApp loc (goMod m) (go t)
    go (ModExtract app inn t) = Rzk.ModExtract loc (Rzk.Comp loc (goMod app) (goMod inn)) (go t)
    go (LetMod z app inn mty val body) = withBinder1 z body $ \(z', body') ->
      let bind = case mty of
            Nothing -> Rzk.BindPattern loc (binderToPattern z')
            Just ty -> Rzk.BindPatternType loc (binderToPattern z') (go ty)
       in Rzk.LetMod loc (modsToModComp app inn) bind (go val) body'

-- | Peel a match branch's arm chain back into its binder patterns and body.
--
-- Each 'MatchArm' contributes one pattern; the bookkeeping per binder is the
-- same as 'fromTerm' does for a λ (freshen the binder's leaves, record the
-- display name, spend a supply name for a placeholder).
matchArms :: [VarIdent] -> [VarIdent] -> NameMap n Display -> Term n -> ([Rzk.Pattern], Rzk.Term)
matchArms used supply names = \case
  MatchArm z (ScopedAST binder body) ->
    let (z', supply') = freshenBinder used supply z
        x = displayNameOf z' supply'
        supply'' = case z' of
          BinderVar (Just _) -> supply'
          _                  -> drop 1 supply'
        used' = x : used <> binderLeaves z'
        (pats, t) = matchArms used' supply'' (Foil.addNameBinder binder (x, z') names) body
     in (binderToPattern z' : pats, t)
  body -> ([], fromTerm used supply names body)

-- | The name standing for the variable itself. A single-variable binder uses
-- its own name; a pattern binder needs a placeholder, which is only shown if
-- the whole point is used (in a shape tope, say), and then it is printed as
-- the pattern anyway.
displayNameOf :: Binder -> [VarIdent] -> VarIdent
displayNameOf (BinderVar (Just x)) _ = x
displayNameOf _ (x : _)              = x
displayNameOf _ []                   = error "not enough fresh variables!"

-- | Refresh a binder's named leaves against the names already in use; draw
-- fresh names for anonymous leaves from the remaining supply.
freshenBinder :: [VarIdent] -> [VarIdent] -> Binder -> (Binder, [VarIdent])
freshenBinder _ stream (BinderVar Nothing) =
  case stream of
    x : xs -> (BinderVar (Just x), xs)
    _      -> error "not enough fresh variables!"
freshenBinder used' stream (BinderVar (Just z)) =
  (BinderVar (Just z'), filter (/= z') stream)
  where z' = refreshVar used' z
freshenBinder _ stream BinderUnit = (BinderUnit, stream)
freshenBinder used' stream (BinderPair l r) =
  let (l', s1) = freshenBinder used' stream l
      (r', s2) = freshenBinder (used' <> binderLeaves l') s1 r
   in (BinderPair l' r', s2)

-- | Does a scope actually use the variable it binds?
--
-- Compares name /ids/ rather than names, which sidesteps having to unsink the
-- inner scopes' names back into this one. Ids are unique per binder, so a hit is
-- an occurrence of exactly this binder's variable.
--
-- (free-foil's own @freeVarsOf@ would do, but it is not in the 0.2.0 release --
-- it is one of the unreleased helpers on free-foil's @main@.)
scopeUsesItsBinder :: ScopedAST Foil.NameBinder TermSig n -> Bool
scopeUsesItsBinder (ScopedAST binder body) =
  Foil.nameId (Foil.nameOf binder) `elem` nameIdsOf body

-- | Every name id occurring in a term, bound or free.
nameIdsOf :: Term l -> [Int]
nameIdsOf (Var x) = [Foil.nameId x]
nameIdsOf (Node sig) = bifoldMap goScoped nameIdsOf sig
  where
    goScoped (ScopedAST _binder body) = nameIdsOf body
