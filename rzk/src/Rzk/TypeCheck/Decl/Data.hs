{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The surface-syntax scaffolding of @#data@ declarations, kept separate from
-- the monadic checker in "Rzk.TypeCheck.Decl".
--
-- Two things live here, both pure. First, small builders over the surface
-- syntax (@surfaceApps@, @surfacePi@, …) and the preprocessed forms of a
-- declaration (the 'SortIndex' telescope and the 'DataConSurface'
-- constructors). Second, the construction of the generated eliminators: given
-- an 'ElimSpec' — the declaration's data together with the fresh binders of the
-- generated types and the validated endpoint images — 'elimTerms' produces the
-- surface types of @ind-D@ and @rec-D@ and the path computation rules. The
-- checker then pushes those surface terms through the ordinary elaborator, so
-- nothing here constructs core terms or touches the type-checking monad.
module Rzk.TypeCheck.Decl.Data where

import           Data.Data          (Data, cast, gmapQ)
import qualified Data.Text          as T

import qualified Language.Rzk.Syntax as Rzk
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
surfaceVarTokens x = concat
  [ maybe [] varTok (cast x)
  , concat (gmapQ surfaceVarTokens x)
  ]
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
      { esName = name, esParamVars = paramVars, esParamDecls = paramDecls
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
    motiveSort dependent = addParamDecls indexDecls $
      if dependent
        then surfaceArrow dAppliedIx (Rzk.Universe Nothing)
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
              Rzk.TypeFun Nothing fieldDecl $
                surfacePi (nthByConstruction "induction hypotheses" ihNames nRec)
                  (surfaceApps motive (fieldIxs <> [ fpat | dependent ]))
                  (wrapFields (nRec + 1) more)
          | otherwise =
              Rzk.TypeFun Nothing fieldDecl (wrapFields nRec more)
    elimTail dependent = addParamDecls indexDecls $
      if dependent
        then surfacePi scrutV dAppliedIx $
          surfaceApps motive (map surfaceVar indexVars <> [surfaceVar scrutV])
        else surfaceArrow dAppliedIx $
          surfaceApps motive (map surfaceVar indexVars)
    conWithPath = zip consData pathData
    methodsPis dependent inner = case methodVars of
      Nothing -> foldr (surfaceArrow . methodTy dependent) inner conWithPath
      Just ms -> foldr
        (\(m, cp) rest -> surfacePi m (methodTy dependent cp) rest)
        inner (zip ms conWithPath)
    elimTy dependent = addParamDecls paramDecls $
      surfacePi motiveV (motiveSort dependent) $
        methodsPis dependent (elimTail dependent)
    indName = prefixedIdent "ind-" name
    recName = prefixedIdent "rec-" name
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
      addParamDecls paramDecls $
        surfacePi motiveV (motiveSort dependent) $
          methodsPis dependent $
            addParamDecls (dataConFields con) $
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
    computeNameFor pfx con =
      let Rzk.VarIdent pos (Rzk.VarIdentToken d) = name
          Rzk.VarIdentToken c = identTokenOf (dataConName con)
       in Rzk.VarIdent pos (Rzk.VarIdentToken (pfx <> d <> "-" <> c))
    computes = case methodVars of
      Nothing -> []
      Just ms ->
        [ entry
        | (m, (con, Just ((l, r), _))) <- zip ms conWithPath
        , entry <-
            [ ( computeNameFor "compute-ind-" con
              , computeTy True indName m con l r )
            , ( computeNameFor "compute-rec-" con
              , computeTy False recName m con l r ) ] ]
