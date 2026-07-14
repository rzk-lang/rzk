{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Surface syntax to the free-foil core.
--
-- A transcription of @toTerm@ from "Language.Rzk.Foil.Names", with the variable
-- handling replaced. The environment is still a function from a surface
-- identifier to a term, as before; what changes is what happens at a binder:
--
--   * a binder is a fresh 'Foil.NameBinder' rather than the de Bruijn @Z@;
--   * the environment is carried into the binder's scope with 'Foil.sink', which
--     is a coercion, where the old representation shifted every entry with
--     @S \<$\>@ and so rebuilt every term it held.
--
-- A pattern binder still binds exactly /one/ variable, as before: the components
-- of @\\ (t , s) -> …@ are projections of it, and 'Binder' records the names so
-- they can be shown back to the user.
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Rzk.Foil.Convert where

import           Control.Monad.Foil       (Distinct, NameBinder, NameMap, Scope)
import qualified Control.Monad.Foil       as Foil
import           Control.Monad.Foil.Internal (NameMap (..))
import           Control.Monad.Free.Foil  (AST (..), ScopedAST (..))
import           Data.Data                (Data, cast, gmapQ)
import qualified Data.IntMap              as IntMap
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set

import           Language.Rzk.Foil.Syntax
import           Language.Rzk.Foil.Names (Binder (..), Display, TModality (..),
                                           VarIdent, holeName, toBinder, varIdent)
import qualified Language.Rzk.Foil.Names as Free
import qualified Language.Rzk.Syntax      as Rzk

-- | The environment: what a surface identifier stands for in the current scope.
-- A pattern binder maps its leaves to projections of the single variable it
-- binds, which is why this is a map to /terms/ and not to names.
type Env n = VarIdent -> Term n

-- | Translate a closed surface term.
toTermClosed :: Rzk.Term -> Term Foil.VoidS
toTermClosed = toTerm Foil.emptyScope unbound
  where
    unbound x = error ("undefined variable: " <> show x)

-- | Enter a pattern binder: bind one fresh name, map the pattern's leaves to
-- projections of it, and carry the rest of the environment in with 'Foil.sink'.
toScopedPattern
  :: Distinct n
  => Scope n -> Rzk.Pattern -> Env n -> Rzk.Term -> ScopedAST NameBinder TermSig n
toScopedPattern scope pat env body =
  Foil.withFresh scope $ \binder ->
    let scope' = Foil.extendScope binder scope
        bound = bindings pat (Var (Foil.nameOf binder))
        env' x = case lookup x bound of
          Just t  -> t
          Nothing -> Foil.sink (env x)   -- O(1): the old representation shifted every node
     in ScopedAST binder (toTerm scope' env' body)

-- | Enter an anonymous binder (a non-dependent function type binds nothing).
toScopedAnon
  :: Distinct n
  => Scope n -> Env n -> Rzk.Term -> ScopedAST NameBinder TermSig n
toScopedAnon scope env body =
  Foil.withFresh scope $ \binder ->
    let scope' = Foil.extendScope binder scope
     in ScopedAST binder (toTerm scope' (Foil.sink . env) body)

-- | What each leaf of a pattern stands for: a projection chain over the single
-- variable the pattern binds.
bindings :: Rzk.Pattern -> Term n -> [(VarIdent, Term n)]
bindings (Rzk.PatternUnit _loc) _ = []
bindings (Rzk.PatternVar _loc (Rzk.VarIdent _ "_")) _ = []
bindings (Rzk.PatternVar _loc x) t = [(varIdent x, t)]
bindings (Rzk.PatternPair _loc l r) t = bindings l (First t) <> bindings r (Second t)
bindings (Rzk.PatternTuple loc p1 p2 ps) t =
  bindings (Free.desugarTuple loc (reverse ps) p2 p1) t

toTerm :: forall n. Distinct n => Scope n -> Env n -> Rzk.Term -> Term n
toTerm scope env = go
  where
    go :: Rzk.Term -> Term n
    go = \case
      -- ASCII aliases and deprecations are desugared exactly as before.
      Rzk.RecOrDeprecated loc psi phi a_psi a_phi ->
        go (Rzk.RecOr loc [Rzk.Restriction loc psi a_psi, Rzk.Restriction loc phi a_phi])
      Rzk.TypeExtensionDeprecated loc shape type_ -> go (Rzk.TypeFun loc shape type_)
      Rzk.TypeFun loc (Rzk.ParamTermTypeDeprecated loc' pat type_) ret ->
        go (Rzk.TypeFun loc (Rzk.ParamTermType loc' (Free.patternToTerm pat) type_) ret)
      Rzk.TypeFun loc (Rzk.ParamVarShapeDeprecated loc' pat cube tope) ret ->
        go (Rzk.TypeFun loc (Rzk.ParamTermShape loc' (Free.patternToTerm pat) cube tope) ret)
      Rzk.Lambda loc (Rzk.ParamPatternShapeDeprecated loc' pat cube tope : params) body ->
        go (Rzk.Lambda loc (Rzk.ParamPatternShape loc' [pat] cube tope : params) body)

      Rzk.ASCII_CubeUnitStar loc -> go (Rzk.CubeUnitStar loc)
      Rzk.ASCII_Cube2_0 loc -> go (Rzk.Cube2_0 loc)
      Rzk.ASCII_Cube2_1 loc -> go (Rzk.Cube2_1 loc)
      Rzk.ASCII_TopeTop loc -> go (Rzk.TopeTop loc)
      Rzk.ASCII_TopeBottom loc -> go (Rzk.TopeBottom loc)
      Rzk.ASCII_TopeEQ loc l r -> go (Rzk.TopeEQ loc l r)
      Rzk.ASCII_TopeLEQ loc l r -> go (Rzk.TopeLEQ loc l r)
      Rzk.ASCII_TopeAnd loc l r -> go (Rzk.TopeAnd loc l r)
      Rzk.ASCII_TopeOr loc l r -> go (Rzk.TopeOr loc l r)
      Rzk.ASCII_TypeFun loc param ret -> go (Rzk.TypeFun loc param ret)
      Rzk.ASCII_TypeSigma loc pat ty ret -> go (Rzk.TypeSigma loc pat ty ret)
      Rzk.ASCII_TypeSigmaTuple loc p ps tN -> go (Rzk.TypeSigmaTuple loc p ps tN)
      Rzk.ASCII_Lambda loc pat ret -> go (Rzk.Lambda loc pat ret)
      Rzk.ASCII_TypeExtensionDeprecated loc shape type_ ->
        go (Rzk.TypeExtensionDeprecated loc shape type_)
      Rzk.ASCII_First loc term -> go (Rzk.First loc term)
      Rzk.ASCII_Second loc term -> go (Rzk.Second loc term)
      Rzk.ASCII_CubeI loc -> go (Rzk.CubeI loc)
      Rzk.ASCII_CubeI_0 loc -> go (Rzk.CubeI_0 loc)
      Rzk.ASCII_CubeI_1 loc -> go (Rzk.CubeI_1 loc)

      Rzk.Var _loc x -> env (varIdent x)
      Rzk.Universe _loc -> Universe
      Rzk.UniverseCube _loc -> UniverseCube
      Rzk.UniverseTope _loc -> UniverseTope
      Rzk.CubeUnit _loc -> CubeUnit
      Rzk.CubeUnitStar _loc -> CubeUnitStar
      Rzk.Cube2 _loc -> Cube2
      Rzk.Cube2_0 _loc -> Cube2_0
      Rzk.Cube2_1 _loc -> Cube2_1
      Rzk.CubeI _loc -> CubeI
      Rzk.CubeI_0 _loc -> CubeI_0
      Rzk.CubeI_1 _loc -> CubeI_1
      Rzk.CubeProduct _loc l r -> CubeProduct (go l) (go r)
      Rzk.TopeTop _loc -> TopeTop
      Rzk.TopeBottom _loc -> TopeBottom
      Rzk.TopeEQ _loc l r -> TopeEQ (go l) (go r)
      Rzk.TopeLEQ _loc l r -> TopeLEQ (go l) (go r)
      Rzk.TopeAnd _loc l r -> TopeAnd (go l) (go r)
      Rzk.TopeOr _loc l r -> TopeOr (go l) (go r)
      Rzk.TopeInv _loc t -> TopeInv (go t)
      Rzk.TopeUninv _loc t -> TopeUninv (go t)
      Rzk.CubeFlip _loc t -> CubeFlip (go t)
      Rzk.CubeUnflip _loc t -> CubeUnflip (go t)
      Rzk.RecBottom _loc -> RecBottom
      Rzk.RecOr _loc rs -> RecOr (map restriction rs)
      Rzk.TypeId _loc x tA y -> TypeId (go x) (Just (go tA)) (go y)
      Rzk.TypeIdSimple _loc x y -> TypeId (go x) Nothing (go y)
      Rzk.TypeUnit _loc -> TypeUnit
      Rzk.Unit _loc -> Unit
      Rzk.App _loc f x -> App (go f) (go x)
      Rzk.Pair _loc l r -> Pair (go l) (go r)
      Rzk.Tuple loc p1 p2 (p : ps) -> go (Rzk.Tuple loc (Rzk.Pair loc p1 p2) p ps)
      Rzk.Tuple loc p1 p2 [] -> go (Rzk.Pair loc p1 p2)
      Rzk.First _loc term -> First (go term)
      Rzk.Second _loc term -> Second (go term)
      Rzk.Refl _loc -> Refl Nothing
      Rzk.ReflTerm _loc term -> Refl (Just (go term, Nothing))
      Rzk.ReflTermType _loc x tA -> Refl (Just (go x, Just (go tA)))
      Rzk.IdJ _loc a b c d e f -> IdJ (go a) (go b) (go c) (go d) (go e) (go f)
      Rzk.TypeAsc _loc x t -> TypeAsc (go x) (go t)

      -- A binder may name several variables sharing a type, e.g. @(x y : A)@,
      -- which parses as an application spine; desugar into nested one-variable
      -- binders, as before.
      Rzk.TypeFun loc (Rzk.ParamTermType loc' patTerm arg) ret
        | _ : _ : _ <- vars ->
            go (foldr (\v -> Rzk.TypeFun loc (Rzk.ParamTermType loc' v arg)) ret vars)
        where vars = Free.flattenBinderApp patTerm
      Rzk.TypeFun loc (Rzk.ParamTermModalType loc' patTerm mc ty) ret
        | _ : _ : _ <- vars ->
            go (foldr (\v -> Rzk.TypeFun loc (Rzk.ParamTermModalType loc' v mc ty)) ret vars)
        where vars = Free.flattenBinderApp patTerm

      Rzk.TypeFun _loc (Rzk.ParamTermModalType _loc' patTerm mc ty) ret ->
        let pat = Free.unsafeTermToPattern patTerm
            md = Free.modalColonToTModality mc
         in TypeFun (toBinder pat) md (go ty) Nothing (toScopedPattern scope pat env ret)
      Rzk.TypeFun _loc (Rzk.ParamTermModalShape _loc' patTerm mc cube tope) ret ->
        let pat = Free.unsafeTermToPattern patTerm
            md = Free.modalColonToTModality mc
         in TypeFun (toBinder pat) md (go cube)
              (Just (toScopedPattern scope pat env tope))
              (toScopedPattern scope pat env ret)
      Rzk.TypeFun _loc (Rzk.ParamTermType _ patTerm arg) ret ->
        let pat = Free.unsafeTermToPattern patTerm
         in TypeFun (toBinder pat) Id (go arg) Nothing (toScopedPattern scope pat env ret)
      Rzk.TypeFun _loc (Rzk.ParamTermShape _loc' patTerm cube tope) ret ->
        let pat = Free.unsafeTermToPattern patTerm
         in TypeFun (toBinder pat) Id (go cube)
              (Just (toScopedPattern scope pat env tope))
              (toScopedPattern scope pat env ret)
      Rzk.TypeFun _loc (Rzk.ParamType _ arg) ret ->
        TypeFun (BinderVar Nothing) Id (go arg) Nothing (toScopedAnon scope env ret)

      Rzk.TypeSigma _loc pat tA tB ->
        TypeSigma (toBinder pat) Id (go tA) (toScopedPattern scope pat env tB)
      Rzk.TypeSigmaModal _loc pat mc ty body ->
        TypeSigma (toBinder pat) (Free.modalColonToTModality mc) (go ty)
          (toScopedPattern scope pat env body)
      Rzk.TypeSigmaTuple loc (Rzk.SigmaParamModal _loc' pat mc ty) rest body ->
        let tailSigma = case rest of
              []       -> body
              [sp]     -> Free.sigmaParamToTypeSigma loc sp body
              (sp:sps) -> Rzk.TypeSigmaTuple loc sp sps body
         in TypeSigma (toBinder pat) (Free.modalColonToTModality mc) (go ty)
              (toScopedPattern scope pat env tailSigma)
      Rzk.TypeSigmaTuple loc (Rzk.SigmaParam _ patA tA) (mp@Rzk.SigmaParamModal{} : rest) body ->
        go (Rzk.TypeSigma loc patA tA (case rest of
              [] -> Free.sigmaParamToTypeSigma loc mp body
              _  -> Rzk.TypeSigmaTuple loc mp rest body))
      Rzk.TypeSigmaTuple loc (Rzk.SigmaParam _ patA tA) (Rzk.SigmaParam _ patB tB : ps) tN ->
        go (Rzk.TypeSigmaTuple loc (Rzk.SigmaParam loc patX tX) ps tN)
        where
          patX = Rzk.PatternPair loc patA patB
          tX = Rzk.TypeSigma loc patA tA tB
      Rzk.TypeSigmaTuple loc (Rzk.SigmaParam _ pat tA) [] tB -> go (Rzk.TypeSigma loc pat tA tB)

      Rzk.Lambda loc (Rzk.ParamPatternModalType _ [] _mc _ty : params) body ->
        go (Rzk.Lambda loc params body)
      Rzk.Lambda loc (Rzk.ParamPatternModalType loc' (pat:pats) mc ty : params) body ->
        Lambda (toBinder pat) (Just (LambdaParam (Free.modalColonToTModality mc) (go ty) Nothing))
          (toScopedPattern scope pat env
            (Rzk.Lambda loc (if null pats then params else Rzk.ParamPatternModalType loc' pats mc ty : params) body))
      Rzk.Lambda loc (Rzk.ParamPatternModalShape _ [] _mc _cube _tope : params) body ->
        go (Rzk.Lambda loc params body)
      Rzk.Lambda loc (Rzk.ParamPatternModalShape loc' (pat:pats) mc cube tope : params) body ->
        Lambda (toBinder pat)
          (Just (LambdaParam (Free.modalColonToTModality mc) (go cube)
                   (Just (toScopedPattern scope pat env tope))))
          (toScopedPattern scope pat env
            (Rzk.Lambda loc (if null pats then params else Rzk.ParamPatternModalShape loc' pats mc cube tope : params) body))
      Rzk.Lambda _loc [] body -> go body
      Rzk.Lambda loc (Rzk.ParamPattern _ pat : params) body ->
        Lambda (toBinder pat) Nothing
          (toScopedPattern scope pat env (Rzk.Lambda loc params body))
      Rzk.Lambda loc (Rzk.ParamPatternType _ [] _ty : params) body ->
        go (Rzk.Lambda loc params body)
      Rzk.Lambda loc (Rzk.ParamPatternType loc' (pat:pats) ty : params) body ->
        Lambda (toBinder pat) (Just (LambdaParam Id (go ty) Nothing))
          (toScopedPattern scope pat env
            (Rzk.Lambda loc (Rzk.ParamPatternType loc' pats ty : params) body))
      Rzk.Lambda loc (Rzk.ParamPatternShape _ [] _cube _tope : params) body ->
        go (Rzk.Lambda loc params body)
      Rzk.Lambda loc (Rzk.ParamPatternShape loc' (pat:pats) cube tope : params) body ->
        Lambda (toBinder pat)
          (Just (LambdaParam Id (go cube) (Just (toScopedPattern scope pat env tope))))
          (toScopedPattern scope pat env
            (Rzk.Lambda loc (Rzk.ParamPatternShape loc' pats cube tope : params) body))

      Rzk.Let _loc (Rzk.BindPattern _ pat) val expr ->
        Let (toBinder pat) Nothing (go val) (toScopedPattern scope pat env expr)
      Rzk.Let _loc (Rzk.BindPatternType _ pat ty) val expr ->
        Let (toBinder pat) (Just (go ty)) (go val) (toScopedPattern scope pat env expr)

      Rzk.TypeRestricted _loc ty rs -> TypeRestricted (go ty) (map restriction rs)
      Rzk.Hole _loc (Rzk.HoleIdent _ (Rzk.HoleIdentToken tok)) -> Hole (holeName tok)
      Rzk.ModApp _loc md body -> ModApp (Free.toModality md) (go body)
      Rzk.ModType _loc md ty -> TypeModal (Free.toModality md) (go ty)
      Rzk.ModExtract{} -> error "$extract$ is an internal term and cannot appear in source"
      Rzk.LetMod _loc comp (Rzk.BindPattern _ pat) val body ->
        let (app, inn) = Free.modCompToMods comp
         in LetMod (toBinder pat) app inn Nothing (go val)
              (toScopedPattern scope pat env body)
      Rzk.LetMod _loc comp (Rzk.BindPatternType _ pat ty) val body ->
        let (app, inn) = Free.modCompToMods comp
         in LetMod (toBinder pat) app inn (Just (go ty)) (go val)
              (toScopedPattern scope pat env body)

    restriction = \case
      Rzk.Restriction _loc tope term       -> (go tope, go term)
      Rzk.ASCII_Restriction _loc tope term -> (go tope, go term)

-- * Open terms

-- | Elaborate a surface term whose free identifiers are not known in advance.
--
-- Each identifier occurring anywhere in the term gets a name in a fresh scope, so
-- an /open/ term (the annotation of a binder, say, taken out of its context) can be
-- put into the core, transformed, and printed back with the names it came in with.
-- The reference index needs this: it splits the annotation of a pair binder through
-- the core, and has no typing context to hand.
withOpenTerm
  :: forall r. Rzk.Term
  -> (forall n. Distinct n
        => Foil.Scope n -> NameMap n Display -> Term n -> r)
  -> r
withOpenTerm term k = go Foil.emptyScope [] Map.empty idents
  where
    idents = nubOrd (map varIdent (collectVarIdents term))

    go :: forall n. Distinct n
       => Foil.Scope n -> [(VarIdent, Foil.Name n)] -> Map VarIdent Display
       -> [VarIdent] -> r
    go scope bound names [] =
      k scope (namesOf bound names) (toTerm scope (envOf bound) term)
    go scope bound names (x : xs) =
      Foil.withFresh scope $ \binder ->
        let scope' = Foil.extendScope binder scope
            bound' = (x, Foil.nameOf binder) : map (fmap Foil.sink) bound
         in go scope' bound' (Map.insert x (x, BinderVar (Just x)) names) xs

    envOf bound x = case lookup x bound of
      Just v  -> Var v
      Nothing -> Var (snd (head bound))  -- unreachable: every identifier is bound above

    namesOf bound names = NameMap $ IntMap.fromList
      [ (Foil.nameId v, display)
      | (x, v) <- bound
      , Just display <- [Map.lookup x names]
      ]

-- | Every identifier occurring in a piece of surface syntax, bound or free.
collectVarIdents :: Data a => a -> [Rzk.VarIdent]
collectVarIdents x =
  maybe [] (:[]) (cast x) ++ concat (gmapQ collectVarIdents x)

nubOrd :: Ord a => [a] -> [a]
nubOrd = Set.toList . Set.fromList
