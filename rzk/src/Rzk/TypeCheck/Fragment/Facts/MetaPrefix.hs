-- | The certificate of the meta-parameter layer check
-- ("Rzk.TypeCheck.MetaPrefix"): its use-site walk over the facts, position
-- by position. The walk never unfolds a definition; the only fact it asks is
-- whether the domain of a function applied is a schematic kind.
module Rzk.TypeCheck.Fragment.Facts.MetaPrefix (
  metaPrefixVerdict,
) where

import           Rzk.TypeCheck.Fragment.Facts.Expose
import           Rzk.TypeCheck.Fragment.Facts.Types

-- | How a position is classified under a rule.
data PosKind = MetaPos | ObjectPos
  deriving (Eq)

-- | How a position is classified under the structural rule and the strict one.
data Positions = Positions
  { posStructural :: PosKind
  , posStrict     :: PosKind
  }

typePositions, objectPositions :: Positions
typePositions = Positions MetaPos MetaPos
objectPositions = Positions ObjectPos ObjectPos

-- | The certificate of the meta-parameter layer check on a declaration,
-- under the strict rule ('True') or the structural one.
metaPrefixVerdict :: Bool -> DeclFacts -> Verdict
metaPrefixVerdict strict decl =
  go typePositions (declType decl) <> foldMap (go typePositions) (declValue decl)
  where
    go :: Positions -> Facts -> Verdict
    go pos f = case fShape f of
      SVar v -> checkHead pos v 0
      SParam _ -> Unclean "a parameter"
      SParamType _ _ -> Unclean "the type of an argument"
      SScoped _ _ -> Unclean "facts from a definition's body"
      SApp{} -> application pos f
      SLam l -> foldMap lambdaParam (lamParam l) <> go pos (lamBody l)
      SPi p ->
        go typePositions (piDomain p) <> foldMap (go objectPositions) (piTope p)
          <> go typePositions (piCodomain p)
      SSigma s -> go typePositions (sigmaFirst s) <> go typePositions (sigmaSecond s)
      -- The endpoints are terms; storing an unsaturated schema in an
      -- identity type is an object-level use.
      SId i ->
        go objectPositions (idLeft i) <> foldMap (go typePositions) (idType i)
          <> go objectPositions (idRight i)
      SRestricted ty faces -> go typePositions ty <> foldMap objectFace faces
      SLet l ->
        foldMap (go typePositions) (letAnnotation l) <> go objectPositions (letValue l)
          <> go pos (letBody l)
      SOther r -> Unclean r
      -- Every other node: its unscoped children are object positions.
      SUniverse -> Clean
      SCubeU -> Clean
      STopeU -> Clean
      SLeaf _ -> Clean
      SCubeOrTope _ cs -> foldMap (go objectPositions) cs
      SRecOr faces -> foldMap objectFace faces
      SPair a b -> go objectPositions a <> go objectPositions b
      SFirst p -> go objectPositions p
      SSecond p -> go objectPositions p
      SRefl me ->
        foldMap (\(Endpoint x mty) -> go objectPositions x <> foldMap (go objectPositions) mty) me
      SIdJ j ->
        foldMap (go objectPositions) [jType j, jStart j, jMotive j, jBase j, jEnd j, jProof j]
      STypeAsc v ty -> go objectPositions v <> go objectPositions ty
      STypeModal ty -> go objectPositions ty

    objectFace (Face a b) = go objectPositions a <> go objectPositions b

    lambdaParam (LamParam dom mtope) = go typePositions dom <> foldMap (go objectPositions) mtope

    -- The head, then each argument at the position its function's domain
    -- gives it: under the strict rule, a schematic argument must also sit in
    -- the head's meta prefix.
    application pos f = headVerdict <> mconcat (zipWith argument [1 ..] args)
      where
        (h, args) = spine f
        (prefix, headVerdict) = case fShape h of
          SVar v -> (tlMetaPrefix <$> vfTop v, checkHead pos v (length args))
          _ -> (Nothing, go objectPositions h)
        argument :: Int -> (Facts, Facts) -> Verdict
        argument i (fnode, arg) =
          let (domMeta, vs) = decide (domainMetaOf (fType fnode))
              strictPos = case prefix of
                Just k | domMeta && i <= k -> MetaPos
                _ -> ObjectPos
          in vs <> go (Positions (if domMeta then MetaPos else ObjectPos) strictPos) arg

    -- An unsaturated top-level head at an object position warns; at a
    -- position only the strict rule rejects, under the strict rule.
    checkHead :: Positions -> VarFacts -> Int -> Verdict
    checkHead pos v nargs = case vfTop v of
      Just tl
        | tlMetaPrefix tl > nargs
        , posStructural pos == ObjectPos || (posStrict pos == ObjectPos && strict) ->
            Unclean "unsaturated schematic parameters"
      _ -> Clean
