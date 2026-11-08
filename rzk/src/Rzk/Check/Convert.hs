{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Surface syntax to raw terms.
--
-- A transcription of "Language.Rzk.Foil.Convert" onto the pattern binder: a
-- pattern becomes a 'Pattern' with one fresh name per leaf, and the
-- environment maps each leaf to its name. A shape restriction becomes a
-- tope family, a λ over the same pattern with the tope as its body.
module Rzk.Check.Convert
  ( SurfaceEnv
  , toRaw
  , withPattern
  , desugarTuple
  ) where

import           Control.Monad.Foil       (DExt, Distinct, Scope)
import qualified Control.Monad.Foil       as Foil
import           Control.Monad.Free.Foil  (AST (..), ScopedAST (..))
import           Control.Monad.Free.Foil.Annotated (AnnSig (..))
import           Data.Functor             (void)
import           Debug.Trace              (trace)

import           Language.Rzk.Foil.Names  (RzkPosition (..), TModality (..),
                                           VarIdent, desugarTuple, flattenBinderApp,
                                           holeName, modalColonToTModality,
                                           patternToTerm, sigmaParamToTypeSigma,
                                           toModality, unsafeTermToPattern,
                                           varIdent)
import qualified Language.Rzk.Syntax      as Rzk
import           Rzk.Check.Syntax

-- | What a surface identifier stands for in the current scope: a variable of
-- the scope, or a term standing in for an identifier that is not in scope.
type SurfaceEnv n = VarIdent -> Raw n

-- | Enter a pattern: a fresh name per leaf, each mapped by the environment
-- to its name, and the rest of the environment carried in by coercion.
withPattern
  :: forall n r. Distinct n
  => Scope n -> Rzk.Pattern -> SurfaceEnv n
  -> (forall l. DExt n l => Pattern n l -> Scope l -> SurfaceEnv l -> r)
  -> r
withPattern scope pat env k = case pat of
  Rzk.PatternVar _loc (Rzk.VarIdent _ "_") ->
    Foil.withFresh scope $ \b ->
      k (PatternVar Nothing b) (Foil.extendScope b scope) (Foil.sink . env)
  Rzk.PatternVar _loc x ->
    Foil.withFresh scope $ \b ->
      let x' = varIdent x
          env' y
            | y == x'   = Var (Foil.nameOf b)
            | otherwise = Foil.sink (env y)
      in k (PatternVar (Just x') b) (Foil.extendScope b scope) env'
  Rzk.PatternUnit _loc ->
    Foil.withFresh scope $ \b ->
      k (PatternUnit b) (Foil.extendScope b scope) (Foil.sink . env)
  Rzk.PatternPair _loc l r ->
    withPattern scope l env $ \pl scope' env' ->
      withPattern scope' r env' $ \pr scope'' env'' ->
        let pat' = PatternPair pl pr
        in case (Foil.assertExt pat', Foil.assertDistinct pat') of
             (Foil.Ext, Foil.Distinct) -> k pat' scope'' env''
  Rzk.PatternTuple loc p1 p2 ps ->
    withPattern scope (desugarTuple loc (reverse ps) p2 p1) env k

-- | A pattern binder over a surface body.
scoped :: Distinct n => Scope n -> Rzk.Pattern -> SurfaceEnv n -> Rzk.Term -> ScopedRaw n
scoped scope pat env body =
  withPattern scope pat env $ \pat' scope' env' ->
    ScopedAST pat' (toRaw scope' env' body)

-- | An anonymous binder: the codomain of a non-dependent function type binds
-- nothing.
scopedAnon :: Distinct n => Scope n -> SurfaceEnv n -> Rzk.Term -> ScopedRaw n
scopedAnon scope env body =
  Foil.withFresh scope $ \b ->
    ScopedAST (PatternVar Nothing b) (toRaw (Foil.extendScope b scope) (Foil.sink . env) body)

-- | A shape restriction as a tope family: a λ over the binder's pattern whose
-- body is the tope.
shapeOf :: Distinct n => Scope n -> Rzk.Pattern -> SurfaceEnv n -> Rzk.Term -> Raw n
shapeOf scope pat env tope =
  rawNode (positionIn tope) (LambdaF Nothing (scoped scope pat env tope))

positionIn :: Rzk.Term -> RzkPosition
positionIn term = RzkPosition Nothing (Rzk.hasPosition term)

-- | Translate a surface term.
toRaw :: forall n. Distinct n => Scope n -> SurfaceEnv n -> Rzk.Term -> Raw n
toRaw scope env = go
  where
    ppPosition Nothing = ""
    ppPosition (Just (line_, col)) = " at line " <> show line_ <> " column " <> show col

    -- A notation that is fine, but that a simpler one says as well.
    lint orig suggestion = trace $ unlines
      [ "[HINT]:" <> ppPosition (Rzk.hasPosition orig) <> " consider replacing"
      , "  " <> Rzk.printTree orig
      , "with the following"
      , "  " <> Rzk.printTree suggestion
      ]

    -- Every node is tagged with where it was written. A node with no
    -- position of its own keeps whatever the conversion of its body found:
    -- desugaring hands surface nodes back without positions, and the λ a
    -- definition's parameters are wrapped in is spelled nowhere.
    go :: Rzk.Term -> Raw n
    go term = case Rzk.hasPosition term of
      Nothing  -> go' term
      Just pos -> atPosition (RzkPosition Nothing (Just pos)) (go' term)

    atPosition pos t@(Node (AnnSig (SrcPos old) sig))
      | Nothing <- rzkLineCol old = Node (AnnSig (SrcPos pos) sig)
      | otherwise = t
    atPosition _ t = t

    node :: Sig (ScopedRaw n) (Raw n) -> Raw n
    node = rawNode noPosition

    sc = scoped scope
    anon = scopedAnon scope env
    shape = shapeOf scope

    go' :: Rzk.Term -> Raw n
    go' = \case
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
      Rzk.ASCII_First loc term -> go (Rzk.First loc term)
      Rzk.ASCII_Second loc term -> go (Rzk.Second loc term)
      Rzk.ASCII_CubeI loc -> go (Rzk.CubeI loc)
      Rzk.ASCII_CubeI_0 loc -> go (Rzk.CubeI_0 loc)
      Rzk.ASCII_CubeI_1 loc -> go (Rzk.CubeI_1 loc)

      Rzk.Var _loc x -> env (varIdent x)
      Rzk.Universe _loc -> node UniverseF
      Rzk.UniverseCube _loc -> node UniverseCubeF
      Rzk.UniverseTope _loc -> node UniverseTopeF
      Rzk.CubeUnit _loc -> node CubeUnitF
      Rzk.CubeUnitStar _loc -> node CubeUnitStarF
      Rzk.Cube2 _loc -> node Cube2F
      Rzk.Cube2_0 _loc -> node Cube2_0F
      Rzk.Cube2_1 _loc -> node Cube2_1F
      Rzk.CubeI _loc -> node CubeIF
      Rzk.CubeI_0 _loc -> node CubeI_0F
      Rzk.CubeI_1 _loc -> node CubeI_1F
      Rzk.CubeProduct _loc l r -> node (CubeProductF (go l) (go r))
      Rzk.TopeTop _loc -> node TopeTopF
      Rzk.TopeBottom _loc -> node TopeBottomF
      Rzk.TopeEQ _loc l r -> node (TopeEQF (go l) (go r))
      Rzk.TopeLEQ _loc l r -> node (TopeLEQF (go l) (go r))
      Rzk.TopeAnd _loc l r -> node (TopeAndF (go l) (go r))
      Rzk.TopeOr _loc l r -> node (TopeOrF (go l) (go r))
      Rzk.TopeInv _loc t -> node (TopeInvF (go t))
      Rzk.TopeUninv _loc t -> node (TopeUninvF (go t))
      Rzk.CubeFlip _loc t -> node (CubeFlipF (go t))
      Rzk.CubeUnflip _loc t -> node (CubeUnflipF (go t))
      Rzk.CubeSup _loc l r -> node (CubeSupF (go l) (go r))
      Rzk.CubeInf _loc l r -> node (CubeInfF (go l) (go r))
      Rzk.RecBottom _loc -> node RecBottomF
      Rzk.RecOr _loc rs -> node (RecOrF (map restriction rs))
      Rzk.TypeId _loc x tA y -> node (TypeIdF (go x) (Just (go tA)) (go y))
      Rzk.TypeIdSimple _loc x y -> node (TypeIdF (go x) Nothing (go y))
      Rzk.TypeUnit _loc -> node TypeUnitF
      Rzk.Unit _loc -> node UnitF
      Rzk.App _loc f x -> node (AppF (go f) (go x))
      Rzk.Pair _loc l r -> node (PairF (go l) (go r))
      Rzk.Tuple loc p1 p2 (p : ps) -> go (Rzk.Tuple loc (Rzk.Pair loc p1 p2) p ps)
      Rzk.Tuple loc p1 p2 [] -> go (Rzk.Pair loc p1 p2)
      Rzk.First _loc term -> node (FirstF (go term))
      Rzk.Second _loc term -> node (SecondF (go term))
      Rzk.Refl _loc -> node (ReflF Nothing)
      Rzk.ReflTerm _loc term -> node (ReflF (Just (go term, Nothing)))
      Rzk.ReflTermType _loc x tA -> node (ReflF (Just (go x, Just (go tA))))
      Rzk.IdJ _loc a b c d e f -> node (IdJF (go a) (go b) (go c) (go d) (go e) (go f))
      Rzk.Match _loc scrut branches ->
        node (MatchF Id (go scrut) Nothing (map matchBranch branches))
      Rzk.MatchInto _loc scrut motive branches ->
        node (MatchF Id (go scrut) (Just (go motive)) (map matchBranch branches))
      Rzk.MatchModal _loc md scrut branches ->
        node (MatchF (toModality md) (go scrut) Nothing (map matchBranch branches))
      Rzk.MatchModalInto _loc md scrut motive branches ->
        node (MatchF (toModality md) (go scrut) (Just (go motive)) (map matchBranch branches))
      Rzk.TypeAsc _loc x t -> node (TypeAscF (go x) (go t))

      -- A binder may name several variables sharing a type, @(x y : A)@,
      -- which parses as an application spine: nested one-variable binders.
      Rzk.TypeFun loc (Rzk.ParamTermType loc' patTerm arg) ret
        | _ : _ : _ <- vars ->
            go (foldr (\v -> Rzk.TypeFun loc (Rzk.ParamTermType loc' v arg)) ret vars)
        where vars = flattenBinderApp patTerm
      Rzk.TypeFun loc (Rzk.ParamTermModalType loc' patTerm mc ty) ret
        | _ : _ : _ <- vars ->
            go (foldr (\v -> Rzk.TypeFun loc (Rzk.ParamTermModalType loc' v mc ty)) ret vars)
        where vars = flattenBinderApp patTerm

      Rzk.TypeFun _loc (Rzk.ParamTermModalType _loc' patTerm mc ty) ret ->
        let pat = unsafeTermToPattern patTerm
        in node (TypeFunF (modalColonToTModality mc) (go ty) Nothing (sc pat env ret))
      Rzk.TypeFun _loc (Rzk.ParamTermModalShape _loc' patTerm mc cube tope) ret ->
        let pat = unsafeTermToPattern patTerm
        in node (TypeFunF (modalColonToTModality mc) (go cube) (Just (shape pat env tope)) (sc pat env ret))
      Rzk.TypeFun _loc (Rzk.ParamTermType _ patTerm arg) ret ->
        let pat = unsafeTermToPattern patTerm
        in node (TypeFunF Id (go arg) Nothing (sc pat env ret))
      t@(Rzk.TypeFun loc (Rzk.ParamTermShape loc' patTerm cube tope) ret) ->
        let lint' = case tope of
              -- a shape whose tope is a predicate applied to exactly the
              -- binder is the type of that predicate, and says so directly
              Rzk.App _loc fun arg | void arg == void patTerm ->
                lint t (Rzk.TypeFun loc (Rzk.ParamTermType loc' patTerm fun) ret)
              _ -> id
            pat = unsafeTermToPattern patTerm
        in lint' $ node (TypeFunF Id (go cube) (Just (shape pat env tope)) (sc pat env ret))
      Rzk.TypeFun _loc (Rzk.ParamType _ arg) ret ->
        node (TypeFunF Id (go arg) Nothing (anon ret))

      Rzk.TypeSigma _loc pat tA tB ->
        node (TypeSigmaF Id (go tA) (sc pat env tB))
      Rzk.TypeSigmaModal _loc pat mc ty body ->
        node (TypeSigmaF (modalColonToTModality mc) (go ty) (sc pat env body))
      Rzk.TypeSigmaTuple loc (Rzk.SigmaParamModal _loc' pat mc ty) rest body ->
        let tailSigma = case rest of
              []       -> body
              [sp]     -> sigmaParamToTypeSigma loc sp body
              (sp:sps) -> Rzk.TypeSigmaTuple loc sp sps body
        in node (TypeSigmaF (modalColonToTModality mc) (go ty) (sc pat env tailSigma))
      Rzk.TypeSigmaTuple loc (Rzk.SigmaParam _ patA tA) (mp@Rzk.SigmaParamModal{} : rest) body ->
        go (Rzk.TypeSigma loc patA tA (case rest of
              [] -> sigmaParamToTypeSigma loc mp body
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
        node (LambdaF (Just (Param (modalColonToTModality mc) (go ty) Nothing))
          (sc pat env
            (Rzk.Lambda loc (if null pats then params else Rzk.ParamPatternModalType loc' pats mc ty : params) body)))
      Rzk.Lambda loc (Rzk.ParamPatternModalShape _ [] _mc _cube _tope : params) body ->
        go (Rzk.Lambda loc params body)
      Rzk.Lambda loc (Rzk.ParamPatternModalShape loc' (pat:pats) mc cube tope : params) body ->
        node (LambdaF (Just (Param (modalColonToTModality mc) (go cube) (Just (shape pat env tope))))
          (sc pat env
            (Rzk.Lambda loc (if null pats then params else Rzk.ParamPatternModalShape loc' pats mc cube tope : params) body)))
      Rzk.Lambda _loc [] body -> go body
      Rzk.Lambda loc (Rzk.ParamPattern _ pat : params) body ->
        node (LambdaF Nothing (sc pat env (Rzk.Lambda loc params body)))
      Rzk.Lambda loc (Rzk.ParamPatternType _ [] _ty : params) body ->
        go (Rzk.Lambda loc params body)
      Rzk.Lambda loc (Rzk.ParamPatternType loc' (pat:pats) ty : params) body ->
        node (LambdaF (Just (Param Id (go ty) Nothing))
          (sc pat env
            (Rzk.Lambda loc (Rzk.ParamPatternType loc' pats ty : params) body)))
      Rzk.Lambda loc (Rzk.ParamPatternShape _ [] _cube _tope : params) body ->
        go (Rzk.Lambda loc params body)
      t@(Rzk.Lambda loc (Rzk.ParamPatternShape loc' (pat:pats) cube tope : params) body) ->
        let lint' = case tope of
              Rzk.App _loc fun arg
                | null pats && void arg == void (patternToTerm pat) ->
                    lint t (Rzk.Lambda loc (Rzk.ParamPatternType loc' [pat] fun : params) body)
              _ -> id
        in lint' $ node (LambdaF (Just (Param Id (go cube) (Just (shape pat env tope))))
              (sc pat env
                (Rzk.Lambda loc (Rzk.ParamPatternShape loc' pats cube tope : params) body)))

      Rzk.Let _loc (Rzk.BindPattern _ pat) val expr ->
        node (LetF Nothing (go val) (sc pat env expr))
      Rzk.Let _loc (Rzk.BindPatternType _ pat ty) val expr ->
        node (LetF (Just (go ty)) (go val) (sc pat env expr))

      Rzk.TypeRestricted _loc ty rs -> node (TypeRestrictedF (go ty) (map restriction rs))
      Rzk.Hole _loc (Rzk.HoleIdent _ (Rzk.HoleIdentToken tok)) -> node (HoleF (holeName tok))
      Rzk.ModApp _loc md body -> node (ModAppF (toModality md) (go body))
      Rzk.ModType _loc md ty -> node (TypeModalF (toModality md) (go ty))
      Rzk.ModExtract{} -> error "$extract$ is an internal term and cannot appear in source"
      Rzk.LetModBind _loc md bind val body ->
        let m = toModality md
        in letMod bind Id m (node (ModAppF m (go val))) Nothing body
      Rzk.LetModBindInto _loc md bind val motive body ->
        let m = toModality md
        in letMod bind Id m (node (ModAppF m (go val))) (Just (go motive)) body
      Rzk.LetMod _loc inn bind val body ->
        letMod bind Id (toModality inn) (go val) Nothing body
      Rzk.LetModInto _loc inn bind val motive body ->
        letMod bind Id (toModality inn) (go val) (Just (go motive)) body
      Rzk.LetModFramed _loc ext inn bind val body ->
        letMod bind (toModality ext) (toModality inn) (go val) Nothing body
      Rzk.LetModFramedInto _loc ext inn bind val motive body ->
        letMod bind (toModality ext) (toModality inn) (go val) (Just (go motive)) body

    letMod bind app inn value mmotive body = case bind of
      Rzk.BindPattern _ pat ->
        node (LetModF app inn Nothing mmotive value (sc pat env body))
      Rzk.BindPatternType _ pat ty ->
        node (LetModF app inn (Just (go ty)) mmotive value (sc pat env body))

    restriction = \case
      Rzk.Restriction _loc tope term       -> (go tope, go term)
      Rzk.ASCII_Restriction _loc tope term -> (go tope, go term)

    -- One branch of a match: each pattern becomes one arm, and the last
    -- arm's scope holds the branch body.
    matchBranch (Rzk.MatchBranch _loc con pats body) = (varIdent con, arms scope env pats)
      where
        arms :: forall m. Distinct m => Scope m -> SurfaceEnv m -> [Rzk.Pattern] -> Raw m
        arms s e []       = toRaw s e body
        arms s e (p : ps) =
          node' (MatchArmF (withPattern s p e (\p' s' e' -> ScopedAST p' (arms s' e' ps))))
        node' = rawNode noPosition
