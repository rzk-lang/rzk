{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The surface-syntax scaffolding of @#data@ declarations, kept separate from
-- the monadic checker in "Rzk.TypeCheck.Decl".
--
-- Everything here is pure. Small builders over the surface syntax
-- (@surfaceApps@, @surfacePi@, …) support validation and preprocessing of a
-- declaration (the 'SortIndex' telescope and the 'DataConSurface'
-- constructors). The construction of the generated eliminators uses them: given
-- an 'ElimSpec' — the declaration's data together with the fresh binders of the
-- generated types and the validated endpoint images — 'elimTerms' produces the
-- surface types of @ind-D@ and @rec-D@ and the path computation rules. The
-- checker then pushes those surface terms through the ordinary elaborator, so
-- nothing here constructs core terms or touches the type-checking monad.
module Rzk.TypeCheck.Decl.Data where

import           Control.Monad      (forM, forM_, unless)
import           Data.Data          (Data, cast)
import           Data.List          (intercalate)
import qualified Data.Text          as T

import           Language.Rzk.Data      (foldData)
import           Language.Rzk.Foil.Names (TModality (..),
                                           fromMod,
                                           fromTModalityToModalColon,
                                           modalColonToTModality,
                                           patternToTerm)
import qualified Language.Rzk.Syntax as Rzk
import           Rzk.TypeCheck.Context (ModeTheory (comp))
import           Rzk.TypeCheck.Display (panicImpossible)

-- * Surface-syntax builders and preprocessed forms

addParamDecls :: [Rzk.ParamDecl] -> Rzk.Term -> Rzk.Term
addParamDecls [] = id
addParamDecls (paramDecl : paramDecls)
  = Rzk.TypeFun Nothing paramDecl . addParamDecls paramDecls

-- | One index of a family, as declared in the sort: its binder (when the
-- sort names it, as in @(n : nat) → U@) and its type.
data SortIndex = SortIndex
  { sortIndexVar  :: Maybe Rzk.VarIdent
  , sortIndexType :: Rzk.Term
  }

-- | The sort of a constructor: a point constructor returns the declared
-- type; a path constructor returns an identity type over it (spelled
-- @l =_{D …} r@), declaring an identification between the two endpoint
-- terms. Path β-rules are propositional (the generated @compute-@ lemmas),
-- following the HoTT-book treatment of higher inductive types.
data DataConSort
  = DataConPoint
  | DataConPath Rzk.Term Rzk.Term
    -- ^ the endpoints of the declared identification, as written

-- | A constructor, preprocessed at the surface level.
data DataConSurface = DataConSurface
  { dataConName       :: Rzk.VarIdent
  , dataConFields     :: [Rzk.ParamDecl]
    -- ^ the field telescope, one entry per bound variable
  , dataConFieldPats  :: [Rzk.Term]
    -- ^ the field patterns as terms, in the same order
  , dataConRecursive  :: [(Int, [Rzk.Term])]
    -- ^ the directly recursive fields (their type is the declared type
    -- applied to its parameters and some index terms): 0-based position
    -- and the index terms; each contributes an induction hypothesis to
    -- the eliminator's method
  , dataConRetIndices :: [Rzk.Term]
    -- ^ the index terms of the constructor's return type
  , dataConType       :: Rzk.Term
    -- ^ the constructor's full surface type: params → fields → D params
  , dataConProbe      :: Rzk.Term
    -- ^ the non-recursive fields → Unit, for the positivity and largeness
    -- checks (a directly recursive field mentions the type legitimately)
  , dataConNonRec     :: [Rzk.Term]
    -- ^ the non-recursive field types, for the per-field error message
  , dataConLocalNames :: [Rzk.VarIdentToken]
    -- ^ every binder token of the constructor, for freshening
  , dataConSort       :: DataConSort
  }

surfacePatternVars :: Rzk.Pattern -> [Rzk.VarIdent]
surfacePatternVars = \case
  Rzk.PatternVar _ v        -> [v]
  Rzk.PatternPair _ a b     -> surfacePatternVars a <> surfacePatternVars b
  Rzk.PatternTuple _ a b cs -> concatMap surfacePatternVars (a : b : cs)
  Rzk.PatternUnit _         -> []

identTokenOf :: Rzk.VarIdent -> Rzk.VarIdentToken
identTokenOf (Rzk.VarIdent _ tok) = tok

surfaceVar :: Rzk.VarIdent -> Rzk.Term
surfaceVar = Rzk.Var Nothing

-- | A λ over plain (untyped) variable patterns; only ever used in checking
-- position, where the domains come from the expected type.
surfaceLambda :: [Rzk.VarIdent] -> Rzk.Term -> Rzk.Term
surfaceLambda vs = Rzk.Lambda Nothing
  [ Rzk.ParamPattern Nothing (Rzk.PatternVar Nothing v) | v <- vs ]

-- | The wildcard binder, for a generated λ that ignores an argument.
underscoreIdent :: Rzk.VarIdent
underscoreIdent = Rzk.VarIdent Nothing (Rzk.VarIdentToken "_")

-- | Every variable-occurrence token in a surface term, collected
-- generically. Used for the endpoint well-formedness check of path
-- constructors, which is deliberately syntactic.
surfaceVarTokens :: Data a => a -> [Rzk.VarIdentToken]
surfaceVarTokens = foldData (maybe [] varTok . cast)
  where
    varTok :: Rzk.Term -> [Rzk.VarIdentToken]
    varTok = \case
      Rzk.Var _ v -> [identTokenOf v]
      _           -> []

surfaceApps :: Rzk.Term -> [Rzk.Term] -> Rzk.Term
surfaceApps f []       = f
surfaceApps f (x : xs) = surfaceApps (Rzk.App Nothing f x) xs

surfaceAppSpine :: Rzk.Term -> (Rzk.Term, [Rzk.Term])
surfaceAppSpine = go []
  where
    go acc (Rzk.App _ f x) = go (x : acc) f
    go acc t               = (t, acc)

surfaceArrow :: Rzk.Term -> Rzk.Term -> Rzk.Term
surfaceArrow a = Rzk.TypeFun Nothing (Rzk.ParamType Nothing a)

-- | The domains and codomain of a surface Π-chain (domain types only; for
-- the shaped and modal parameter forms the carrier is what matters here).
surfacePiSpine :: Rzk.Term -> ([Rzk.Term], Rzk.Term)
surfacePiSpine = go []
  where
    go acc (Rzk.TypeFun _ param ret)       = go (domainsOf param <> acc) ret
    go acc (Rzk.ASCII_TypeFun _ param ret) = go (domainsOf param <> acc) ret
    go acc t                               = (reverse acc, t)
    domainsOf = \case
      Rzk.ParamType _ t                 -> [t]
      Rzk.ParamTermType _ _ t           -> [t]
      Rzk.ParamTermShape _ _ cube _     -> [cube]
      Rzk.ParamTermModalType _ _ _ t    -> [t]
      Rzk.ParamTermModalShape _ _ _ c _ -> [c]

-- | Is the term the given name applied to exactly the given parameter
-- variables (in order, the uniformity requirement) and then exactly
-- @arity@ index terms? Returns those index terms. A syntactic check: this
-- is how constructor return types and directly recursive fields are
-- recognised.
dataAppliedIndices :: Rzk.VarIdent -> [Rzk.VarIdent] -> Int -> Rzk.Term -> Maybe [Rzk.Term]
dataAppliedIndices dataName paramVars arity t = case surfaceAppSpine t of
  (Rzk.Var _ h, args)
    | identTokenOf h == identTokenOf dataName
    , (paramArgs, indexArgs) <- splitAt (length paramVars) args
    , map (Just . identTokenOf) paramVars == map varTokenOf paramArgs
    , length indexArgs == arity
    -> Just indexArgs
  _ -> Nothing
  where
    varTokenOf (Rzk.Var _ v) = Just (identTokenOf v)
    varTokenOf _             = Nothing

matchesDataApplied :: Rzk.VarIdent -> [Rzk.VarIdent] -> Int -> Rzk.Term -> Bool
matchesDataApplied dataName paramVars arity =
  maybe False (const True) . dataAppliedIndices dataName paramVars arity

surfacePi :: Rzk.VarIdent -> Rzk.Term -> Rzk.Term -> Rzk.Term
surfacePi v ty = Rzk.TypeFun Nothing (Rzk.ParamTermType Nothing (surfaceVar v) ty)

prefixedIdent :: T.Text -> Rzk.VarIdent -> Rzk.VarIdent
prefixedIdent p (Rzk.VarIdent pos (Rzk.VarIdentToken t)) =
  Rzk.VarIdent pos (Rzk.VarIdentToken (p <> t))

modalityPrefix :: TModality -> T.Text
modalityPrefix = \case
  Id    -> ""
  Op    -> "op-"
  Flat  -> "b-"
  Sharp -> "sharp-"

elimIdent :: T.Text -> TModality -> Rzk.VarIdent -> Rzk.VarIdent
elimIdent kind md = prefixedIdent (kind <> "-" <> modalityPrefix md)

modalizeParamDecl :: TModality -> Rzk.ParamDecl -> Rzk.ParamDecl
modalizeParamDecl Id param = param
modalizeParamDecl md param = case param of
  Rzk.ParamType loc ty ->
    Rzk.ParamTermModalType loc (surfaceVar underscoreIdent)
      (fromTModalityToModalColon md) ty
  Rzk.ParamTermType loc pat ty ->
    Rzk.ParamTermModalType loc pat (fromTModalityToModalColon md) ty
  Rzk.ParamTermShape loc pat cube tope ->
    Rzk.ParamTermModalShape loc pat (fromTModalityToModalColon md) cube
      (Rzk.ModType Nothing (fromMod md) tope)
  Rzk.ParamTermModalType loc pat mc ty ->
    Rzk.ParamTermModalType loc pat
      (fromTModalityToModalColon (comp md (modalColonToTModality mc))) ty
  Rzk.ParamTermModalShape loc pat mc cube tope ->
    Rzk.ParamTermModalShape loc pat
      (fromTModalityToModalColon (comp md (modalColonToTModality mc))) cube
      (Rzk.ModType Nothing (fromMod md) tope)

surfaceArrowModal :: TModality -> Rzk.Term -> Rzk.Term -> Rzk.Term
surfaceArrowModal Id = surfaceArrow
surfaceArrowModal md = surfacePiModal md underscoreIdent

surfacePiModal :: TModality -> Rzk.VarIdent -> Rzk.Term -> Rzk.Term -> Rzk.Term
surfacePiModal Id v ty ret = surfacePi v ty ret
surfacePiModal md v ty ret =
  Rzk.TypeFun Nothing
    (Rzk.ParamTermModalType Nothing (surfaceVar v)
      (fromTModalityToModalColon md) ty) ret

-- | Index into a list that is long enough by construction, panicking with a
-- label instead of the opaque @Prelude.!!@ message if that invariant is ever
-- broken. Used where an index is derived from the same data as the list (the
-- induction-hypothesis binders, a constructor's field patterns).
nthByConstruction :: String -> [a] -> Int -> a
nthByConstruction what xs i = case drop i xs of
  x : _ -> x
  []    -> panicImpossible (what <> ": index " <> show i <> " out of range")

-- | The 0-based positions of a constructor's directly recursive fields.
recPositionsOf :: DataConSurface -> [Int]
recPositionsOf = map fst . dataConRecursive

-- * Declaration preprocessing

-- | Surface parameters as binders of a declaration's type.
paramDeclsOf :: Rzk.Param -> Either String [Rzk.ParamDecl]
paramDeclsOf = \case
  Rzk.ParamPatternShape loc pats cube tope ->
    Right [ Rzk.ParamTermShape loc (patternToTerm pat) cube tope | pat <- pats ]
  Rzk.ParamPatternType loc pats ty ->
    Right [ Rzk.ParamTermType loc (patternToTerm pat) ty | pat <- pats ]
  Rzk.ParamPattern{} -> Left "untyped pattern in parameters"
  Rzk.ParamPatternModalType loc pats mc ty ->
    Right [ Rzk.ParamTermModalType loc (patternToTerm pat) mc ty | pat <- pats ]
  Rzk.ParamPatternModalShape loc pats mc cube tope ->
    Right [ Rzk.ParamTermModalShape loc (patternToTerm pat) mc cube tope | pat <- pats ]

-- | The index telescope of a sort: @U@, or plain type indices ending in @U@.
dataSortIndices :: Rzk.DataSort -> Either String [SortIndex]
dataSortIndices = \case
  Rzk.NoDataSort _        -> pure []
  Rzk.SomeDataSort _ sort -> go sort
  where
    go = \case
      Rzk.Universe{} -> pure []
      Rzk.TypeFun _ param ret       -> (:) <$> indexOf param <*> go ret
      Rzk.ASCII_TypeFun _ param ret -> (:) <$> indexOf param <*> go ret
      sort -> Left $
        "the sort of a #data must be U, or an index telescope ending in U, got "
          <> Rzk.printTree sort
    indexOf = \case
      Rzk.ParamType _ ty -> pure (SortIndex Nothing ty)
      Rzk.ParamTermType _ (Rzk.Var _ v) ty -> pure (SortIndex (Just v) ty)
      Rzk.ParamTermType _ pat _ -> Left $
        "an index binder of a #data sort must be a plain variable, got "
          <> Rzk.printTree pat
      p -> Left $
        "an index of a #data sort must be a plain type, got " <> Rzk.printTree p

dataBodyParts :: Rzk.DataBody -> ([Rzk.Constructor], [Rzk.DataElim])
dataBodyParts = \case
  Rzk.NoDataBody _              -> ([], [])
  Rzk.SomeDataBody _ cons elims -> (cons, elims)

-- | Parameters must be plain typed variables, since the generated entries
-- apply the type former to them.
dataParamVars :: [Rzk.Param] -> Either String [Rzk.VarIdent]
dataParamVars = fmap concat . mapM paramVarsOf
  where
    paramVarsOf = \case
      Rzk.ParamPatternType _ pats _ty -> forM pats $ \case
        Rzk.PatternVar _ v -> pure v
        pat -> Left $
          "a parameter of a #data must be a plain variable, got "
            <> Rzk.printTree pat
      p -> Left $
        "a parameter of a #data must be a typed variable (x : A), got "
          <> Rzk.printTree p

dataFieldToParamDecl :: Rzk.VarIdent -> Rzk.Param -> Either String [Rzk.ParamDecl]
dataFieldToParamDecl cname = \case
  p@Rzk.ParamPatternType{}       -> paramDeclsOf p
  p@Rzk.ParamPatternShape{}      -> paramDeclsOf p
  p@Rzk.ParamPatternModalShape{} -> paramDeclsOf p
  p@Rzk.ParamPatternModalType{}  -> paramDeclsOf p
  Rzk.ParamPattern _ pat -> Left $
    "untyped field " <> Rzk.printTree pat <> " in constructor " <> Rzk.printTree cname

-- | Preprocess one constructor, recognising its return indices and directly
-- recursive fields syntactically.
dataConSurface
  :: Rzk.VarIdent -> [Rzk.VarIdent] -> [Rzk.ParamDecl] -> Int -> Rzk.Constructor
  -> Either String DataConSurface
dataConSurface dataName paramVars paramDecls indexArity (Rzk.Constructor _loc cname cparams ctype) = do
  fieldDecls <- concat <$> mapM (dataFieldToParamDecl cname) cparams
  let defaultRet = surfaceApps (surfaceVar dataName) (map surfaceVar paramVars)
      recIndicesOf = dataAppliedIndices dataName paramVars indexArity
  (ret, retIndices, sort) <- case ctype of
    Rzk.NoConstructorType _
      | indexArity == 0 -> pure (defaultRet, [], DataConPoint)
      | otherwise -> Left $
          "constructor " <> Rzk.printTree cname
            <> " must spell out its return type: the family has "
            <> show indexArity <> " index(es)"
    Rzk.SomeConstructorType _ ret
      | Just ixs <- recIndicesOf ret -> pure (ret, ixs, DataConPoint)
    Rzk.SomeConstructorType _ ret@(Rzk.TypeId _ l carrier r)
      | Just _ <- recIndicesOf carrier ->
          if indexArity == 0
            then pure (ret, [], DataConPath l r)
            else Left $
              "path constructor " <> Rzk.printTree cname
                <> " in an indexed family is not supported"
      | isIdentity carrier -> higherPathError
    Rzk.SomeConstructorType _ (Rzk.TypeIdSimple _ _ _) -> Left $
      "the return type of path constructor " <> Rzk.printTree cname
        <> " must spell out the carrier of the identification, like l =_{"
        <> Rzk.printTree defaultRet <> "} r"
    Rzk.SomeConstructorType _ _ -> Left $
      "the return type of constructor " <> Rzk.printTree cname
        <> " must be the declared type applied to its parameters and "
        <> show indexArity <> " index(es), like "
        <> Rzk.printTree defaultRet <> " …"
  let fieldTypes = [ ty | Rzk.ParamTermType _ _ ty <- fieldDecls ]
      recursive =
        [ (j, ixs) | (j, ty) <- zip [0 ..] fieldTypes, Just ixs <- [recIndicesOf ty] ]
      isRec ty = maybe False (const True) (recIndicesOf ty)
      nonRecTypes = [ ty | ty <- fieldTypes, not (isRec ty) ]
      nonRecDecls = [ d | d@(Rzk.ParamTermType _ _ ty) <- fieldDecls, not (isRec ty) ]
  forM_ fieldTypes $ \case
    fieldTy@(Rzk.TypeId _ _ carrier _)
      | matchesDataApplied dataName paramVars indexArity carrier -> Left $
          "constructor " <> Rzk.printTree cname
            <> " has a field of an identity type over the declared type ("
            <> Rzk.printTree fieldTy
            <> "); higher paths are not supported yet"
    _ -> pure ()
  pure DataConSurface
    { dataConName = cname
    , dataConFields = fieldDecls
    , dataConFieldPats = concatMap fieldPats cparams
    , dataConRecursive = recursive
    , dataConRetIndices = retIndices
    , dataConType = addParamDecls (paramDecls <> fieldDecls) ret
    , dataConProbe = addParamDecls nonRecDecls (Rzk.TypeUnit Nothing)
    , dataConNonRec = nonRecTypes
    , dataConLocalNames = identTokenOf cname
        : map identTokenOf (concatMap fieldVars cparams)
    , dataConSort = sort
    }
  where
    fieldVars = \case
      Rzk.ParamPatternType _ pats _           -> concatMap surfacePatternVars pats
      Rzk.ParamPatternShape _ pats _ _        -> concatMap surfacePatternVars pats
      Rzk.ParamPatternModalType _ pats _ _    -> concatMap surfacePatternVars pats
      Rzk.ParamPatternModalShape _ pats _ _ _ -> concatMap surfacePatternVars pats
      _                                       -> []
    fieldPats = \case
      Rzk.ParamPatternType _ pats _           -> map patternToTerm pats
      Rzk.ParamPatternShape _ pats _ _        -> map patternToTerm pats
      Rzk.ParamPatternModalType _ pats _ _    -> map patternToTerm pats
      Rzk.ParamPatternModalShape _ pats _ _ _ -> map patternToTerm pats
      _                                       -> []
    isIdentity = \case
      Rzk.TypeId{}       -> True
      Rzk.TypeIdSimple{} -> True
      _                  -> False
    higherPathError = Left $
      "constructor " <> Rzk.printTree cname
        <> " declares a path between paths; higher path constructors are not supported yet"

-- | The images of path-constructor endpoints under the section being defined.
-- Endpoints are built only from constructors, fields, and induction hypotheses.
pathEndpointImages
  :: Rzk.VarIdent -> [Rzk.VarIdent] -> [DataConSurface]
  -> [Rzk.VarIdent] -> Maybe [Rzk.VarIdent]
  -> Either String [Maybe ((Rzk.Term, Rzk.Term), (Rzk.Term, Rzk.Term))]
pathEndpointImages name paramVars consData ihNames methodVars = forM consData $ \con ->
  case dataConSort con of
    DataConPoint    -> pure Nothing
    DataConPath l r -> do
      iL <- imageOf con l
      iR <- imageOf con r
      pure (Just ((l, r), (iL, iR)))
  where
    conMethods = case methodVars of
      Just ms -> zip (map (identTokenOf . dataConName) consData) (zip ms consData)
      Nothing -> []
    dataTok = identTokenOf name
    conToks = map (identTokenOf . dataConName) consData
    ihByFieldTok c =
      [ (identTokenOf v, ih)
      | (ih, j) <- zip ihNames (recPositionsOf c)
      , Rzk.Var _ v <-
          [nthByConstruction "constructor field patterns" (dataConFieldPats c) j] ]
    endpointErr c t = Left $
      "in path constructor " <> Rzk.printTree (dataConName c)
        <> ": an endpoint must be built from the declaration's"
        <> " constructors and the constructor's fields, but "
        <> Rzk.printTree t <> " is not"
    ensureDFree c t =
      let bad = dataTok : conToks <> map fst (ihByFieldTok c)
       in unless (null [ () | tk <- surfaceVarTokens t, tk `elem` bad ])
            (endpointErr c t)
    imageOf c t = case surfaceAppSpine t of
      (Rzk.Var _ v, args)
        | Just (m, target) <- lookup (identTokenOf v) conMethods -> do
            let nP = length paramVars
                arity = length (dataConFields target)
                (pArgs, fArgs) = splitAt nP args
                varTokOf = \case
                  Rzk.Var _ u -> Just (identTokenOf u)
                  _           -> Nothing
            unless (length args == nP + arity
                && map varTokOf pArgs == map (Just . identTokenOf) paramVars) $
              endpointErr c t
            imgArgs <- forM (zip [0 :: Int ..] fArgs) $ \(j, a) ->
              if j `elem` recPositionsOf target
                then do { ia <- imageOf c a; pure [a, ia] }
                else do { ensureDFree c a; pure [a] }
            pure (surfaceApps (surfaceVar m) (concat imgArgs))
        | null args
        , Just ih <- lookup (identTokenOf v) (ihByFieldTok c) ->
            pure (surfaceVar ih)
      _ -> endpointErr c t

-- | Split re-ascription clauses by the generated entry they name.
splitDataClauses
  :: Rzk.VarIdent -> [Rzk.VarIdent] -> [Rzk.VarIdent] -> [Rzk.DataElim]
  -> Either String ([(Rzk.VarIdentToken, Rzk.Term)], [(Rzk.VarIdentToken, Rzk.Term)])
splitDataClauses name elimNames computeNames = go ([], [])
  where
    go acc [] = pure acc
    go (melims, mcomp) (Rzk.DataElim _loc elimName ty : rest)
      | any (sameIdent elimName) elimNames =
          if identTokenOf elimName `elem` map fst melims
            then duplicate elimName
            else go (melims <> [(identTokenOf elimName, ty)], mcomp) rest
      | otherwise = Left $
          "eliminate with clause for " <> Rzk.printTree elimName
            <> ", which is not an eliminator of " <> Rzk.printTree name
            <> " (the eliminators are "
            <> intercalate ", " (map Rzk.printTree elimNames) <> ")"
    go (melims, mcomp) (Rzk.DataCompute _loc ruleName ty : rest)
      | any (sameIdent ruleName) computeNames =
          if identTokenOf ruleName `elem` map fst mcomp
            then duplicate ruleName
            else go (melims, mcomp <> [(identTokenOf ruleName, ty)]) rest
      | otherwise = Left $
          "compute with clause for " <> Rzk.printTree ruleName
            <> ", which is not a computation rule of " <> Rzk.printTree name
            <> case computeNames of
                 [] -> " (the declaration has no path constructors, so no computation rules are generated)"
                 _  -> " (the computation rules are "
                         <> intercalate ", " (map Rzk.printTree computeNames)
                         <> ")"
    duplicate n = Left $ "duplicate re-ascription clause for " <> Rzk.printTree n
    sameIdent a b = identTokenOf a == identTokenOf b

-- | A fresh surface identifier, using the supplied top-level binding test.
freshIdentWith
  :: (Rzk.VarIdent -> Bool) -> [Rzk.VarIdentToken] -> T.Text -> Rzk.VarIdent
freshIdentWith isBound avoid base =
  case filter isFree candidates of
    cand : _ -> cand
    []       -> panicImpossible "no fresh identifier candidate"
  where
    candidates =
      [ Rzk.VarIdent Nothing (Rzk.VarIdentToken (base <> T.replicate p "'"))
      | p <- [0 ..] ]
    isFree cand = identTokenOf cand `notElem` avoid && not (isBound cand)

freshIdentsWith
  :: (Rzk.VarIdent -> Bool) -> Int -> [Rzk.VarIdentToken] -> T.Text -> [Rzk.VarIdent]
freshIdentsWith isBound count avoid base = go count avoid []
  where
    go 0 _ acc = reverse acc
    go k avoidNow acc =
      let ident = freshIdentWith isBound avoidNow base
       in go (k - 1) (identTokenOf ident : avoidNow) (ident : acc)

-- * Eliminator construction

-- | Everything the pure construction of the eliminators closes over: the
-- declaration's name, its parameters and index telescope, the fresh binders of
-- the generated types (the motive, the scrutinee, the induction hypotheses,
-- the path-method names, and the binders of the inlined transport), the
-- preprocessed constructors, and — for each path constructor — the images of
-- its endpoints under the section being defined. Assembled by
-- "Rzk.TypeCheck.Decl" once the fresh names are allocated and the endpoints
-- validated.
data ElimSpec = ElimSpec
  { esName       :: Rzk.VarIdent
  , esModality   :: TModality
  , esParamVars  :: [Rzk.VarIdent]
  , esParamDecls :: [Rzk.ParamDecl]
  , esIndexVars  :: [Rzk.VarIdent]
  , esIndexDecls :: [Rzk.ParamDecl]
  , esMotiveV    :: Rzk.VarIdent
  , esScrutV     :: Rzk.VarIdent
  , esIhNames    :: [Rzk.VarIdent]
  , esMethodVars :: Maybe [Rzk.VarIdent]
  , esEndpointV  :: Rzk.VarIdent
  , esPathV      :: Rzk.VarIdent
  , esTransportV :: Rzk.VarIdent
  , esConsData   :: [DataConSurface]
  , esPathData   :: [Maybe ((Rzk.Term, Rzk.Term), (Rzk.Term, Rzk.Term))]
  }

-- | The generated surface types of a declaration's eliminators and the path
-- computation rules, all as surface terms for the ordinary elaborator.
data ElimTerms = ElimTerms
  { indTypeTerm  :: Rzk.Term
    -- ^ the type of @ind-D@ (the dependent eliminator)
  , recTypeTerm  :: Rzk.Term
    -- ^ the type of @rec-D@ (the non-dependent eliminator)
  , computeRules :: [(Rzk.VarIdent, Rzk.Term)]
    -- ^ one @compute-ind-@/@compute-rec-@ lemma per path constructor
  }

elimTerms :: ElimSpec -> ElimTerms
elimTerms spec = ElimTerms
    { indTypeTerm  = elimTy True
    , recTypeTerm  = elimTy False
    , computeRules = computes
    }
  where
    ElimSpec
      { esName = name, esModality = elimMd
      , esParamVars = paramVars, esParamDecls = paramDecls
      , esIndexVars = indexVars, esIndexDecls = indexDecls, esMotiveV = motiveV
      , esScrutV = scrutV, esIhNames = ihNames, esMethodVars = methodVars
      , esEndpointV = endpointV, esPathV = pathV, esTransportV = transportV
      , esConsData = consData, esPathData = pathData } = spec
    dApplied = surfaceApps (surfaceVar name) (map surfaceVar paramVars)
    dAppliedIx = surfaceApps dApplied (map surfaceVar indexVars)
    motive = surfaceVar motiveV
    -- The motive abstracts over the indices (and, dependently, the
    -- scrutinee); a method's hypotheses and codomain instantiate it
    -- at the relevant index terms.
    motiveSort dependent = addParamDecls (map (modalizeParamDecl elimMd) indexDecls) $
      if dependent
        then surfaceArrowModal elimMd dAppliedIx (Rzk.Universe Nothing)
        else Rzk.Universe Nothing
    -- The constructor applied to the parameters and its own fields
    -- (for a path constructor, this is the declared identification).
    conApplied con = surfaceApps (surfaceVar (dataConName con))
      (map surfaceVar paramVars <> dataConFieldPats con)
    -- @transport@ in the motive along a path @p : l = rEnd@, spelled
    -- through idJ (rzk has no primitive transport):
    -- @idJ (D …, l, \y' _ → C l → C y', \y' → y', rEnd, p) u@.
    transportAlong l rEnd p u =
      Rzk.App Nothing
        (Rzk.IdJ Nothing dApplied l
          (surfaceLambda [transportV, underscoreIdent]
            (surfaceArrow (Rzk.App Nothing motive l)
              (Rzk.App Nothing motive (surfaceVar transportV))))
          (surfaceLambda [transportV] (surfaceVar transportV))
          rEnd p)
        u
    -- One method per constructor: its fields with an induction
    -- hypothesis interleaved after each recursive field (HoTT-book
    -- style). A point method then ends in the motive at the
    -- constructor's return indices (and, for @ind-D@, at the
    -- constructor applied to parameters and fields); a path method
    -- ends in an equation between the images of the endpoints, over
    -- the path for @ind-D@ (β on a path constructor stays
    -- propositional: the equation is the type of the generated
    -- @compute-@ lemma, not a rule the checker computes with).
    methodTy dependent (con, mpath) = wrapFields (0 :: Int)
      (zip [0 :: Int ..] (zip (dataConFields con) (dataConFieldPats con)))
      where
        wrapFields _ [] = case mpath of
          Nothing -> surfaceApps motive $
            dataConRetIndices con <> [ conApplied con | dependent ]
          Just ((l, r), (iL, iR))
            | dependent -> Rzk.TypeIdSimple Nothing
                (transportAlong l r (conApplied con) iL) iR
            | otherwise -> Rzk.TypeIdSimple Nothing iL iR
        wrapFields nRec ((j, (fieldDecl, fpat)) : more)
          | Just fieldIxs <- lookup j (dataConRecursive con) =
              Rzk.TypeFun Nothing (modalizeParamDecl elimMd fieldDecl) $
                surfacePi (nthByConstruction "induction hypotheses" ihNames nRec)
                  (surfaceApps motive (fieldIxs <> [ fpat | dependent ]))
                  (wrapFields (nRec + 1) more)
          | otherwise =
              Rzk.TypeFun Nothing (modalizeParamDecl elimMd fieldDecl) (wrapFields nRec more)
    elimTail dependent = addParamDecls (map (modalizeParamDecl elimMd) indexDecls) $
      if dependent
        then surfacePiModal elimMd scrutV dAppliedIx $
          surfaceApps motive (map surfaceVar indexVars <> [surfaceVar scrutV])
        else surfaceArrowModal elimMd dAppliedIx $
          surfaceApps motive (map surfaceVar indexVars)
    conWithPath = zip consData pathData
    methodsPis dependent inner = case methodVars of
      Nothing -> foldr (surfaceArrow . methodTy dependent) inner conWithPath
      Just ms -> foldr
        (\(m, cp) rest -> surfacePi m (methodTy dependent cp) rest)
        inner (zip ms conWithPath)
    elimTy dependent = addParamDecls (map (modalizeParamDecl elimMd) paramDecls) $
      surfacePi motiveV (motiveSort dependent) $
        methodsPis dependent (elimTail dependent)
    indName = elimIdent "ind" elimMd name
    recName = elimIdent "rec" elimMd name
    -- The propositional β-lemma per path constructor and eliminator:
    -- the section's action on the constructor's path (ap/apd, spelled
    -- through idJ) equals the path method at the fields, with the
    -- section applied to each recursive field as its hypothesis.
    -- Generated as opaque entries, like the eliminators themselves.
    sectionOf elimIdent = surfaceApps (surfaceVar elimIdent)
      (map surfaceVar paramVars <> [motive]
        <> maybe [] (map surfaceVar) methodVars)
    methodArgs f con = concat
      [ fpat : [ Rzk.App Nothing f fpat | j `elem` recPositionsOf con ]
      | (j, fpat) <- zip [0 :: Int ..] (dataConFieldPats con) ]
    computeTy dependent elimIdent m con l r =
      addParamDecls (map (modalizeParamDecl elimMd) paramDecls) $
        surfacePi motiveV (motiveSort dependent) $
          methodsPis dependent $
            addParamDecls (map (modalizeParamDecl elimMd) (dataConFields con)) $
              let f = sectionOf elimIdent
                  fAt = Rzk.App Nothing f
                  -- apd (dependent) or ap: the motive of the outer
                  -- idJ states what the section does to a path
                  motiveBody
                    | dependent = Rzk.TypeIdSimple Nothing
                        (transportAlong l (surfaceVar endpointV)
                          (surfaceVar pathV) (fAt l))
                        (fAt (surfaceVar endpointV))
                    | otherwise = Rzk.TypeIdSimple Nothing
                        (fAt l) (fAt (surfaceVar endpointV))
                  lhs = Rzk.IdJ Nothing dApplied l
                    (surfaceLambda [endpointV, pathV] motiveBody)
                    (Rzk.Refl Nothing) r (conApplied con)
                  rhs = surfaceApps (surfaceVar m) (methodArgs f con)
               in Rzk.TypeIdSimple Nothing lhs rhs
    computeNameFor kind con =
      let Rzk.VarIdent pos (Rzk.VarIdentToken d) = name
          Rzk.VarIdentToken c = identTokenOf (dataConName con)
       in Rzk.VarIdent pos
            (Rzk.VarIdentToken ("compute-" <> kind <> "-" <> modalityPrefix elimMd <> d <> "-" <> c))
    computes = case methodVars of
      Nothing -> []
      Just ms ->
        [ entry
        | (m, (con, Just ((l, r), _))) <- zip ms conWithPath
        , entry <-
            [ ( computeNameFor "ind" con
              , computeTy True indName m con l r )
            , ( computeNameFor "rec" con
              , computeTy False recName m con l r ) ] ]
