{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Rzk.VSCode.ReferenceIndex (
  Uri (..),
  Position (..),
  Range (..),
  Location (..),
  Binding (..),
  ReferenceIndex (..),
  indexModules,
  lookupAt,
  bindingSites,
  locationPath,
) where

import           Control.Applicative      ((<|>))
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (listToMaybe)
import qualified Data.Text                as T

import           Language.Rzk.Foil.Convert (withOpenTerm)
import qualified Language.Rzk.Foil.Names   as Free
import           Language.Rzk.Foil.Print   (fromTerm)
import           Language.Rzk.Foil.Syntax  (Term, instantiateUntyped,
                                            pattern CubeProduct, pattern First,
                                            pattern Pair, pattern Second,
                                            pattern TypeSigma)
import qualified Language.Rzk.Syntax      as Rzk

data Uri = Uri
  { uriPath :: FilePath
  }
  deriving (Eq, Ord, Show)

data Position = Position
  { positionLine      :: Int
  , positionCharacter :: Int
  }
  deriving (Eq, Ord, Show)

data Range = Range
  { rangeStart :: Position
  , rangeEnd   :: Position
  }
  deriving (Eq, Ord, Show)

data Location = Location
  { locationUri   :: Uri
  , locationRange :: Range
  }
  deriving (Eq, Ord, Show)

data Binding = Binding
  { bindingName :: T.Text
  , bindingDef  :: Location
  , bindingType :: Maybe T.Text
    -- ^ The printed surface annotation of the binder, when it has one
    -- (e.g. @A@ for @(x : A)@, @I | φ t@ for @(t : I | φ t)@). Top-level
    -- names carry 'Nothing'; their elaborated type comes from the
    -- typecheck cache instead.
  , bindingRefs :: [Location]
  }
  deriving (Eq, Show)

newtype ReferenceIndex = ReferenceIndex
  { occurrences :: Map.Map (FilePath, Int) [(Int, Int, Binding)]
    -- ^ Every occurrence (definition or reference), keyed by file and line,
    -- as column spans; identifiers never span lines. This is what makes
    -- 'lookupAt' a map lookup rather than a scan over all bindings.
  }
  deriving (Show)

-- | One resolved occurrence: name, definition site, occurrence site, and
-- (for the self-link of a binder) its printed type annotation.
data Link = Link T.Text Location Location (Maybe T.Text)

-- | Names in scope. Locals shadow globals by insertion.
type Env = Map.Map T.Text Location

varText :: Rzk.VarIdent' a -> T.Text
varText (Rzk.VarIdent _ (Rzk.VarIdentToken t)) = t

identLoc :: FilePath -> Rzk.VarIdent -> Maybe Location
identLoc file (Rzk.VarIdent pos (Rzk.VarIdentToken name)) = case pos of
  Just (l, c) ->
    let l0 = max 0 (l - 1)
        c0 = max 0 (c - 1)
        c1 = c0 + T.length name
    in Just (Location (Uri { uriPath = file }) (Range (Position l0 c0) (Position l0 c1)))
  Nothing -> Nothing

locationPath :: Location -> FilePath
locationPath (Location u _) = uriPath u

bindingSites :: Binding -> [Location]
bindingSites b = bindingDef b : bindingRefs b

lookupAt :: ReferenceIndex -> Uri -> Position -> Maybe Binding
lookupAt index (Uri path) (Position l c) =
  case Map.lookup (path, l) (occurrences index) of
    Nothing    -> Nothing
    Just spans -> listToMaybe [ b | (s, e, b) <- spans, s <= c, c < e ]

indexModules :: [(FilePath, Rzk.Module)] -> ReferenceIndex
indexModules modules = group $
  concat [ goCommand file env0 c | (file, m) <- modules, c <- moduleCommands m ]
  where
    -- On duplicate names, the first definition wins (as scope lookup would).
    env0 = Map.fromListWith (\_new old -> old)
      [ (varText v, loc)
      | (file, m) <- modules, v <- globalNames m, Just loc <- [identLoc file v] ]
    group links = ReferenceIndex
      { occurrences = Map.fromListWith (++)
          [ ((path, l), [(s, e, b)])
          | b <- bs
          , Location (Uri path) (Range (Position l s) (Position _ e)) <- bindingSites b
          ]
      }
      where
        -- Binders produce a self-link (definition site linked to itself) so
        -- that every binding has a key; keep it out of the reference list,
        -- which 'bindingSites' prepends the definition to.
        -- Accumulate by prepending (constant time per link) and restore the
        -- encounter order with one reverse at the end; appending would be
        -- quadratic in the number of references of a binding.
        bs = [ Binding n d ann (reverse rs)
             | ((n, d), (rs, ann)) <- Map.toList $ Map.fromListWith merge
                 [ ((n, d), (if r == d then [] else [r], a)) | Link n d r a <- links ]
             ]
        -- fromListWith combines as f new old: prepend the new references,
        -- prefer the earliest annotation (the binder's self-link).
        merge (rsNew, aNew) (rsOld, aOld) = (rsNew ++ rsOld, aOld <|> aNew)

moduleCommands :: Rzk.Module -> [Rzk.Command]
moduleCommands (Rzk.Module _ _ cmds) = cmds

globalNames :: Rzk.Module -> [Rzk.VarIdent]
globalNames = concatMap cmd . moduleCommands
  where
    cmd = \case
      Rzk.CommandDefine _ name _ _ _ _  -> [name]
      Rzk.CommandPostulate _ name _ _ _ -> [name]
      Rzk.CommandAssume _ vars _        -> vars
      _                                 -> []

use :: FilePath -> Env -> Rzk.VarIdent -> [Link]
use file env v = case (Map.lookup (varText v) env, identLoc file v) of
  (Just defLoc, Just occLoc) -> [Link (varText v) defLoc occLoc Nothing]
  _                          -> []

-- | A binder annotation, kept structured so that a pair pattern can be
-- matched against the shape of its type.
data BinderAnn
  = AnnType Rzk.Term            -- ^ @(x : A)@
  | AnnShape Rzk.Term Rzk.Term  -- ^ @(t : I | φ)@

typeAnn :: Rzk.Term -> Maybe BinderAnn
typeAnn = Just . AnnType

shapeAnn :: Rzk.Term -> Rzk.Term -> Maybe BinderAnn
shapeAnn cube tope = Just (AnnShape cube tope)

printAnn :: BinderAnn -> T.Text
printAnn (AnnType ty) = T.pack (Rzk.printTree ty)
printAnn (AnnShape cube tope) =
  T.pack (Rzk.printTree cube ++ " | " ++ Rzk.printTree tope)

-- | Decompose the annotation of a pair pattern into annotations of the two
-- components, through the scoped core: a cube product splits into its two
-- sides, and for a Σ-type the first component gets the base while the second
-- gets the family instantiated at the first component (@q : B p@, not
-- @q : B x@). Substitution in the core is capture-avoiding. For a shape
-- annotation, the tope constrains the components jointly, so the components
-- only inherit their cube.
splitPairAnn :: Rzk.Term -> BinderAnn -> Maybe (BinderAnn, BinderAnn)
splitPairAnn firstComp (AnnShape cube _tope) = splitPairAnn firstComp (AnnType cube)
splitPairAnn firstComp (AnnType ty) =
  -- The annotation is an open term (it mentions whatever is in scope where it was
  -- written), so every identifier in it gets a name, and the names are mapped back
  -- when it is printed.
  withOpenTerm (Rzk.Pair Nothing ty firstComp) $ \scope names paired ->
    case paired of
      Pair ty' first' -> case ty' of
        CubeProduct a b -> Just (render names a, render names b)
        TypeSigma _ _ a scoped ->
          Just ( render names a
               , render names (reduceProjections (instantiateUntyped scope scoped first'))
               )
        _ -> Nothing
      _ -> Nothing
  where
    render names = AnnType . fromTerm [] [] names

-- | Reduce projections of literal pairs (π₁ (a, b) → a). A pattern binder refers to
-- its components through projections, so substituting a pair for it leaves these
-- redexes behind.
reduceProjections :: Term n -> Term n
reduceProjections = \case
  First t -> case reduceProjections t of
    Pair a _ -> a
    t'       -> First t'
  Second t -> case reduceProjections t of
    Pair _ b -> b
    t'       -> Second t'
  t -> t

-- | A pattern as the term it matches.
patternTerm :: Rzk.Pattern -> Rzk.Term
patternTerm = \case
  Rzk.PatternUnit loc         -> Rzk.Unit loc
  Rzk.PatternVar loc v        -> Rzk.Var loc v
  Rzk.PatternPair loc a b     -> Rzk.Pair loc (patternTerm a) (patternTerm b)
  Rzk.PatternTuple loc a b cs -> Rzk.Tuple loc (patternTerm a) (patternTerm b) (map patternTerm cs)

bindVars :: FilePath -> Env -> [(Rzk.VarIdent, Maybe T.Text)] -> (Env, [Link])
bindVars file env vs =
  ( Map.union (Map.fromListWith (\_new old -> old) [ (n, loc) | (n, loc, _) <- binds ]) env
  , [ Link n loc loc ann | (n, loc, ann) <- binds ]
  )
  where
    binds = [ (varText v, loc, ann) | (v, ann) <- vs, Just loc <- [identLoc file v] ]

bindPat :: FilePath -> Env -> Maybe BinderAnn -> Rzk.Pattern -> (Env, [Link])
bindPat file env ann = bindVars file env . annotatedPatternVars ann

-- | Distribute an annotation over a pattern: a plain variable inherits it,
-- a pair pattern splits it along the type when the type's shape allows.
-- Tuples desugar to left-nested pairs, matching both 'Free.toTerm'''s
-- treatment of tuple patterns and its translation of Σ-tuples.
annotatedPatternVars :: Maybe BinderAnn -> Rzk.Pattern -> [(Rzk.VarIdent, Maybe T.Text)]
annotatedPatternVars ann = \case
  Rzk.PatternUnit _  -> []
  Rzk.PatternVar _ v -> [(v, printAnn <$> ann)]
  Rzk.PatternPair _ a b -> case ann >>= splitPairAnn (patternTerm a) of
    Just (annA, annB) ->
      annotatedPatternVars (Just annA) a ++ annotatedPatternVars (Just annB) b
    Nothing ->
      annotatedPatternVars Nothing a ++ annotatedPatternVars Nothing b
  Rzk.PatternTuple loc a b cs ->
    -- Reuse the core's own tuple desugaring (left-nested pairs), so this
    -- split cannot drift from how toTerm' scopes tuple patterns.
    annotatedPatternVars ann (Free.desugarTuple loc (reverse cs) b a)

annotatedTermPatVars :: Maybe BinderAnn -> Rzk.Term -> [(Rzk.VarIdent, Maybe T.Text)]
annotatedTermPatVars ann = \case
  Rzk.Var _ v -> [(v, printAnn <$> ann)]
  Rzk.Pair _ a b -> case ann >>= splitPairAnn a of
    Just (annA, annB) ->
      annotatedTermPatVars (Just annA) a ++ annotatedTermPatVars (Just annB) b
    Nothing ->
      annotatedTermPatVars Nothing a ++ annotatedTermPatVars Nothing b
  Rzk.Tuple loc a b cs ->
    annotatedTermPatVars ann (tuplePairs loc a b cs)
  t -> [ (v, Nothing) | v <- termPatVars t ]

-- | A tuple term as left-nested pairs, following the treatment of tuples in
-- 'Free.toTerm''.
tuplePairs :: a -> Rzk.Term' a -> Rzk.Term' a -> [Rzk.Term' a] -> Rzk.Term' a
tuplePairs loc t1 t2 []       = Rzk.Pair loc t1 t2
tuplePairs loc t1 t2 (t : ts) = tuplePairs loc (Rzk.Pair loc t1 t2) t ts

termPatVars :: Rzk.Term -> [Rzk.VarIdent]
termPatVars = \case
  Rzk.Var _ v        -> [v]
  Rzk.Pair _ a b     -> termPatVars a ++ termPatVars b
  Rzk.Tuple _ a b cs -> concatMap termPatVars (a : b : cs)
  _                  -> []

goCommand :: FilePath -> Env -> Rzk.Command -> [Link]
goCommand file env = \case
  Rzk.CommandDefine _ name _ ps ty body ->
    let (env', occs) = goParams file env ps
    in def name ++ occs ++ goTerm file env' ty ++ goTerm file env' body
  Rzk.CommandPostulate _ name _ ps ty ->
    let (env', occs) = goParams file env ps
    in def name ++ occs ++ goTerm file env' ty
  -- Assumptions (#assume, #variable, #variables) carry their declared type
  -- as the annotation; unlike #define, nothing is elaborated away.
  Rzk.CommandAssume _ vars ty ->
    [ Link (varText v) loc loc (Just (printAnn (AnnType ty)))
    | v <- vars, Just loc <- [identLoc file v] ]
      ++ goTerm file env ty
  Rzk.CommandCheck _ a b       -> goTerm file env a ++ goTerm file env b
  Rzk.CommandCompute _ a       -> goTerm file env a
  Rzk.CommandComputeWHNF _ a   -> goTerm file env a
  Rzk.CommandComputeNF _ a     -> goTerm file env a
  Rzk.CommandSetOption{}       -> []
  Rzk.CommandUnsetOption{}     -> []
  Rzk.CommandSection{}         -> []
  Rzk.CommandSectionEnd{}      -> []
  where
    def v = [ Link (varText v) loc loc Nothing | Just loc <- [identLoc file v] ]

goTerm :: FilePath -> Env -> Rzk.Term -> [Link]
goTerm file env = \case
  Rzk.Var _ v  -> use file env v
  Rzk.Hole _ _ -> []

  Rzk.Lambda _ ps body                      -> paramScope file env ps body
  Rzk.ASCII_Lambda _ ps body                -> paramScope file env ps body
  Rzk.Let _ bind val body                   -> letScope file env bind val body
  Rzk.LetMod _ _ bind val body              -> letScope file env bind val body
  Rzk.TypeSigma _ pat ty ret                -> sigmaScope file env pat ty ret
  Rzk.ASCII_TypeSigma _ pat ty ret          -> sigmaScope file env pat ty ret
  Rzk.TypeSigmaModal _ pat _ ty ret         -> sigmaScope file env pat ty ret
  Rzk.TypeSigmaTuple _ sp sps ret           -> sigmaTupleScope file env (sp : sps) ret
  Rzk.ASCII_TypeSigmaTuple _ sp sps ret     -> sigmaTupleScope file env (sp : sps) ret
  Rzk.TypeFun _ pd ret                      -> paramDeclScope file env pd ret
  Rzk.ASCII_TypeFun _ pd ret                -> paramDeclScope file env pd ret

  Rzk.CubeProduct _ a b         -> goTerm file env a ++ goTerm file env b
  Rzk.CubeSup _ a b             -> goTerm file env a ++ goTerm file env b
  Rzk.CubeInf _ a b             -> goTerm file env a ++ goTerm file env b
  Rzk.TopeEQ _ a b              -> goTerm file env a ++ goTerm file env b
  Rzk.TopeLEQ _ a b             -> goTerm file env a ++ goTerm file env b
  Rzk.TopeAnd _ a b             -> goTerm file env a ++ goTerm file env b
  Rzk.TopeOr _ a b              -> goTerm file env a ++ goTerm file env b
  Rzk.ASCII_TopeEQ _ a b        -> goTerm file env a ++ goTerm file env b
  Rzk.ASCII_TopeLEQ _ a b       -> goTerm file env a ++ goTerm file env b
  Rzk.ASCII_TopeAnd _ a b       -> goTerm file env a ++ goTerm file env b
  Rzk.ASCII_TopeOr _ a b        -> goTerm file env a ++ goTerm file env b
  Rzk.TopeInv _ a               -> goTerm file env a
  Rzk.TopeUninv _ a             -> goTerm file env a
  Rzk.CubeFlip _ a              -> goTerm file env a
  Rzk.CubeUnflip _ a            -> goTerm file env a
  Rzk.RecOr _ rs                -> concatMap (restriction file env) rs
  Rzk.TypeId _ a b c            -> concatMap (goTerm file env) [a, b, c]
  Rzk.TypeIdSimple _ a b        -> goTerm file env a ++ goTerm file env b
  Rzk.TypeRestricted _ a rs     -> goTerm file env a ++ concatMap (restriction file env) rs
  Rzk.App _ a b                 -> goTerm file env a ++ goTerm file env b
  Rzk.Pair _ a b                -> goTerm file env a ++ goTerm file env b
  Rzk.Tuple _ a b cs            -> concatMap (goTerm file env) (a : b : cs)
  Rzk.ModApp _ _ a              -> goTerm file env a
  Rzk.ModType _ _ a             -> goTerm file env a
  Rzk.ModExtract _ _ a          -> goTerm file env a
  Rzk.First _ a                 -> goTerm file env a
  Rzk.Second _ a                -> goTerm file env a
  Rzk.ASCII_First _ a           -> goTerm file env a
  Rzk.ASCII_Second _ a          -> goTerm file env a
  Rzk.ReflTerm _ a              -> goTerm file env a
  Rzk.ReflTermType _ a b        -> goTerm file env a ++ goTerm file env b
  Rzk.IdJ _ a b c d e f         -> concatMap (goTerm file env) [a, b, c, d, e, f]
  Rzk.TypeAsc _ a b             -> goTerm file env a ++ goTerm file env b

  Rzk.Universe{}           -> []
  Rzk.UniverseCube{}       -> []
  Rzk.UniverseTope{}       -> []
  Rzk.CubeUnit{}           -> []
  Rzk.CubeUnitStar{}       -> []
  Rzk.Cube2{}              -> []
  Rzk.Cube2_0{}            -> []
  Rzk.Cube2_1{}            -> []
  Rzk.CubeI{}              -> []
  Rzk.CubeI_0{}            -> []
  Rzk.CubeI_1{}            -> []
  Rzk.TopeTop{}            -> []
  Rzk.TopeBottom{}         -> []
  Rzk.RecBottom{}          -> []
  Rzk.TypeUnit{}           -> []
  Rzk.Unit{}               -> []
  Rzk.Refl{}               -> []
  Rzk.ASCII_CubeUnitStar{} -> []
  Rzk.ASCII_Cube2_0{}      -> []
  Rzk.ASCII_Cube2_1{}      -> []
  Rzk.ASCII_CubeI{}        -> []
  Rzk.ASCII_CubeI_0{}      -> []
  Rzk.ASCII_CubeI_1{}      -> []
  Rzk.ASCII_TopeTop{}      -> []
  Rzk.ASCII_TopeBottom{}   -> []

paramScope :: FilePath -> Env -> [Rzk.Param] -> Rzk.Term -> [Link]
paramScope file env ps body =
  let (env', occs) = goParams file env ps in occs ++ goTerm file env' body

letScope :: FilePath -> Env -> Rzk.Bind -> Rzk.Term -> Rzk.Term -> [Link]
letScope file env bind val body =
  let (env', occs) = goBind file env bind
  in goTerm file env val ++ occs ++ goTerm file env' body

sigmaScope :: FilePath -> Env -> Rzk.Pattern -> Rzk.Term -> Rzk.Term -> [Link]
sigmaScope file env pat ty ret =
  let (env', occs) = bindPat file env (typeAnn ty) pat
  in goTerm file env ty ++ occs ++ goTerm file env' ret

sigmaTupleScope :: FilePath -> Env -> [Rzk.SigmaParam] -> Rzk.Term -> [Link]
sigmaTupleScope file env sps ret =
  let (env', occs) = goSigmaParams file env sps in occs ++ goTerm file env' ret

paramDeclScope :: FilePath -> Env -> Rzk.ParamDecl -> Rzk.Term -> [Link]
paramDeclScope file env pd ret =
  let (env', occs) = goParamDecl file env pd in occs ++ goTerm file env' ret

restriction :: FilePath -> Env -> Rzk.Restriction -> [Link]
restriction file env = \case
  Rzk.Restriction _ a b       -> goTerm file env a ++ goTerm file env b
  Rzk.ASCII_Restriction _ a b -> goTerm file env a ++ goTerm file env b

goBind :: FilePath -> Env -> Rzk.Bind -> (Env, [Link])
goBind file env = \case
  Rzk.BindPattern _ pat -> bindPat file env Nothing pat
  Rzk.BindPatternType _ pat ty ->
    let (env', occs) = bindPat file env (typeAnn ty) pat in (env', goTerm file env ty ++ occs)

goParams :: FilePath -> Env -> [Rzk.Param] -> (Env, [Link])
goParams _    env []       = (env, [])
goParams file env (p : ps) =
  let (env1, o1) = goParam file env p
      (env2, o2) = goParams file env1 ps
  in (env2, o1 ++ o2)

goParam :: FilePath -> Env -> Rzk.Param -> (Env, [Link])
goParam file env = \case
  Rzk.ParamPattern _ pat -> bindPat file env Nothing pat
  Rzk.ParamPatternType _ pats ty ->
    let (env', occs) = bindVars file env (concatMap (annotatedPatternVars (typeAnn ty)) pats)
    in (env', goTerm file env ty ++ occs)
  Rzk.ParamPatternShape _ pats cube tope ->
    let (env', occs) = bindVars file env (concatMap (annotatedPatternVars (shapeAnn cube tope)) pats)
    in (env', goTerm file env cube ++ occs ++ goTerm file env' tope)
  Rzk.ParamPatternModalType _ pats _ ty ->
    let (env', occs) = bindVars file env (concatMap (annotatedPatternVars (typeAnn ty)) pats)
    in (env', goTerm file env ty ++ occs)
  Rzk.ParamPatternModalShape _ pats _ cube tope ->
    let (env', occs) = bindVars file env (concatMap (annotatedPatternVars (shapeAnn cube tope)) pats)
    in (env', goTerm file env cube ++ occs ++ goTerm file env' tope)

goParamDecl :: FilePath -> Env -> Rzk.ParamDecl -> (Env, [Link])
goParamDecl file env = \case
  Rzk.ParamType _ ty -> (env, goTerm file env ty)
  Rzk.ParamTermType _ patTerm ty ->
    let (env', occs) = bindVars file env (annotatedTermPatVars (typeAnn ty) patTerm)
    in (env', goTerm file env ty ++ occs)
  Rzk.ParamTermShape _ patTerm cube tope ->
    let (env', occs) = bindVars file env (annotatedTermPatVars (shapeAnn cube tope) patTerm)
    in (env', goTerm file env cube ++ occs ++ goTerm file env' tope)
  Rzk.ParamTermModalType _ patTerm _ ty ->
    let (env', occs) = bindVars file env (annotatedTermPatVars (typeAnn ty) patTerm)
    in (env', goTerm file env ty ++ occs)
  Rzk.ParamTermModalShape _ patTerm _ cube tope ->
    let (env', occs) = bindVars file env (annotatedTermPatVars (shapeAnn cube tope) patTerm)
    in (env', goTerm file env cube ++ occs ++ goTerm file env' tope)

goSigmaParams :: FilePath -> Env -> [Rzk.SigmaParam] -> (Env, [Link])
goSigmaParams _    env []       = (env, [])
goSigmaParams file env (p : ps) =
  let (env1, o1) = goSigmaParam file env p
      (env2, o2) = goSigmaParams file env1 ps
  in (env2, o1 ++ o2)

goSigmaParam :: FilePath -> Env -> Rzk.SigmaParam -> (Env, [Link])
goSigmaParam file env = \case
  Rzk.SigmaParam _ pat ty ->
    let (env', occs) = bindPat file env (typeAnn ty) pat in (env', goTerm file env ty ++ occs)
  Rzk.SigmaParamModal _ pat _ ty ->
    let (env', occs) = bindPat file env (typeAnn ty) pat in (env', goTerm file env ty ++ occs)
