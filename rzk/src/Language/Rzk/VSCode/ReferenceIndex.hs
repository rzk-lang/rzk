{-# LANGUAGE LambdaCase        #-}
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
  locationCovers,
  identLoc,
) where

import           Data.Function                (on)
import           Data.List                    (find, nubBy)
import qualified Data.Text                    as T

import qualified Language.Rzk.Syntax          as Rzk

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
  deriving (Eq, Show)

data Location = Location
  { locationUri   :: Uri
  , locationRange :: Range
  }
  deriving (Eq, Show)

data Binding = Binding
  { bindingName :: T.Text
  , bindingDef  :: Location
  , bindingRefs :: [Location]
  }
  deriving (Eq, Show)

data ReferenceIndex = ReferenceIndex
  { bindings :: [Binding]
  }
  deriving (Show)

data Link = Link T.Text Location Location

type Env = [(T.Text, Location)]

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

locationCovers :: Uri -> Position -> Location -> Bool
locationCovers uri pos (Location u (Range (Position sl sc) (Position _ ec))) =
  u == uri && covers sl sc ec
  where
    Position cl cc = pos
    covers l s e = cl == l && s <= cc && cc < e

bindingSites :: Binding -> [Location]
bindingSites b = bindingDef b : bindingRefs b

lookupAt :: ReferenceIndex -> Uri -> Position -> Maybe Binding
lookupAt (ReferenceIndex bs) uri pos =
  find (\b -> locationCovers uri pos (bindingDef b) || any (locationCovers uri pos) (bindingRefs b)) bs

indexModules :: [(FilePath, Rzk.Module)] -> ReferenceIndex
indexModules modules = group $
  concat [ goCommand file env0 c | (file, m) <- modules, c <- moduleCommands m ]
  where
    env0 = [ (varText v, loc)
           | (file, m) <- modules, v <- globalNames m, Just loc <- [identLoc file v] ]
    group links =
      let keys = nubBy ((==) `on` (\(Link n d _) -> (n, d))) links
      in ReferenceIndex
        [ Binding n d [ r | Link n' d' r <- links, n' == n, d' == d ]
        | Link n d _ <- keys
        ]

moduleCommands :: Rzk.Module -> [Rzk.Command]
moduleCommands (Rzk.Module _ _ cmds) = cmds

globalNames :: Rzk.Module -> [Rzk.VarIdent]
globalNames = concatMap cmd . moduleCommands
  where
    cmd = \case
      Rzk.CommandDefine _ name _ _ _ _ _ _ -> [name]
      Rzk.CommandDefineNoParams _ name _ _ _ _ _ -> [name]
      Rzk.CommandDef _ name _ _ _ _ _ _    -> [name]
      Rzk.CommandDefNoParams _ name _ _ _ _ _ -> [name]
      Rzk.CommandPostulate _ name _ _ _ _ -> [name]
      Rzk.CommandPostulateNoParams _ name _ _ _ -> [name]
      Rzk.CommandAssume _ vars _ _        -> vars
      Rzk.CommandVariable _ name _ _      -> [name]
      Rzk.CommandVariables _ vars _ _     -> vars
      _                                 -> []

use :: FilePath -> Env -> Rzk.VarIdent -> [Link]
use file env v = case (lookup (varText v) env, identLoc file v) of
  (Just defLoc, Just occLoc) -> [Link (varText v) defLoc occLoc]
  _                          -> []

bindVars :: FilePath -> Env -> [Rzk.VarIdent] -> (Env, [Link])
bindVars file env vs = (binds ++ env, [ Link n loc loc | (n, loc) <- binds ])
  where
    binds = [ (varText v, loc) | v <- vs, Just loc <- [identLoc file v] ]

bindPat :: FilePath -> Env -> Rzk.Pattern -> (Env, [Link])
bindPat file env = bindVars file env . patternVars

patternVars :: Rzk.Pattern -> [Rzk.VarIdent]
patternVars = \case
  Rzk.PatternUnit _         -> []
  Rzk.PatternVar _ v        -> [v]
  Rzk.PatternPair _ a _ b     -> patternVars a ++ patternVars b
  Rzk.PatternTuple _ a _ b _ cs -> concatMap patternVars (a : b : cs)

termPatVars :: Rzk.Term -> [Rzk.VarIdent]
termPatVars = \case
  Rzk.Var _ v        -> [v]
  Rzk.Pair _ a _ b     -> termPatVars a ++ termPatVars b
  Rzk.Tuple _ a _ b _ cs -> concatMap termPatVars (a : b : cs)
  _                  -> []

goCommand :: FilePath -> Env -> Rzk.Command -> [Link]
goCommand file env = \case
  Rzk.CommandDefine _ name _ ps _ ty _ body ->
    defineCommand file env name ps ty body
  Rzk.CommandDefineNoParams _ name _ _ ty _ body ->
    defineCommand file env name [] ty body
  Rzk.CommandDef _ name _ ps _ ty _ body ->
    defineCommand file env name ps ty body
  Rzk.CommandDefNoParams _ name _ _ ty _ body ->
    defineCommand file env name [] ty body
  Rzk.CommandPostulate _ name _ ps _ ty ->
    let (env', occs) = goParams file env ps
    in def name ++ occs ++ goTerm file env' ty
  Rzk.CommandPostulateNoParams _ name _ _ ty ->
    def name ++ goTerm file env ty
  Rzk.CommandAssume _ vars _ ty  -> concatMap def vars ++ goTerm file env ty
  Rzk.CommandVariable _ name _ ty -> def name ++ goTerm file env ty
  Rzk.CommandVariables _ vars _ ty -> concatMap def vars ++ goTerm file env ty
  Rzk.CommandCheck _ a _ b       -> goTerm file env a ++ goTerm file env b
  Rzk.CommandCompute _ a       -> goTerm file env a
  Rzk.CommandComputeWHNF _ a   -> goTerm file env a
  Rzk.CommandComputeNF _ a     -> goTerm file env a
  Rzk.CommandSetOption{}       -> []
  Rzk.CommandUnsetOption{}     -> []
  Rzk.CommandSection{}         -> []
  Rzk.CommandSectionEnd{}      -> []
  where
    def v = [ Link (varText v) loc loc | Just loc <- [identLoc file v] ]
    defineCommand f e name ps ty body =
      let (env', occs) = goParams f e ps
      in def name ++ occs ++ goTerm f env' ty ++ goTerm f env' body

goTerm :: FilePath -> Env -> Rzk.Term -> [Link]
goTerm file env = \case
  Rzk.Var _ v  -> use file env v
  Rzk.Hole _ _ -> []

  Rzk.Lambda _ ps _ body                        -> paramScope file env ps body
  Rzk.ASCII_Lambda _ ps _ body                  -> paramScope file env ps body
  Rzk.Let _ bind _ val _ body                     -> letScope file env bind val body
  Rzk.LetMod _ _ _ bind _ val _ body                -> letScope file env bind val body
  Rzk.TypeSigma _ pat _ ty _ ret                  -> sigmaScope file env pat ty ret
  Rzk.ASCII_TypeSigma _ pat _ ty _ ret            -> sigmaScope file env pat ty ret
  Rzk.Unicode_TypeSigmaAlt _ pat _ ty _ ret       -> sigmaScope file env pat ty ret
  Rzk.TypeSigmaModal _ pat _ ty _ ret           -> sigmaScope file env pat ty ret
  Rzk.ASCII_TypeSigmaModal _ pat _ ty _ ret     -> sigmaScope file env pat ty ret
  Rzk.TypeSigmaTuple _ sp _ sps _ ret             -> sigmaTupleScope file env (sp : sps) ret
  Rzk.ASCII_TypeSigmaTuple _ sp _ sps _ ret       -> sigmaTupleScope file env (sp : sps) ret
  Rzk.Unicode_TypeSigmaTupleAlt _ sp _ sps _ ret  -> sigmaTupleScope file env (sp : sps) ret
  Rzk.TypeFun _ pd _ ret                        -> paramDeclScope file env pd ret
  Rzk.ASCII_TypeFun _ pd _ ret                  -> paramDeclScope file env pd ret
  Rzk.TypeExtensionDeprecated _ pd _ ty         -> paramDeclScope file env pd ty
  Rzk.ASCII_TypeExtensionDeprecated _ pd _ ty   -> paramDeclScope file env pd ty

  Rzk.CubeProduct _ a b         -> goTerm file env a ++ goTerm file env b
  Rzk.ASCII_CubeProduct _ a b   -> goTerm file env a ++ goTerm file env b
  Rzk.TopeEQ _ a _ b              -> goTerm file env a ++ goTerm file env b
  Rzk.TopeLEQ _ a _ b             -> goTerm file env a ++ goTerm file env b
  Rzk.TopeAnd _ a _ b             -> goTerm file env a ++ goTerm file env b
  Rzk.TopeOr _ a _ b              -> goTerm file env a ++ goTerm file env b
  Rzk.ASCII_TopeEQ _ a _ b        -> goTerm file env a ++ goTerm file env b
  Rzk.ASCII_TopeLEQ _ a _ b       -> goTerm file env a ++ goTerm file env b
  Rzk.ASCII_TopeAnd _ a _ b       -> goTerm file env a ++ goTerm file env b
  Rzk.ASCII_TopeOr _ a _ b        -> goTerm file env a ++ goTerm file env b
  Rzk.TopeInv _ a               -> goTerm file env a
  Rzk.ASCII_TopeInv _ a         -> goTerm file env a
  Rzk.TopeUninv _ a             -> goTerm file env a
  Rzk.ASCII_TopeUninv _ a       -> goTerm file env a
  Rzk.CubeFlip _ a              -> goTerm file env a
  Rzk.ASCII_CubeFlip _ a        -> goTerm file env a
  Rzk.CubeUnflip _ a            -> goTerm file env a
  Rzk.ASCII_CubeUnflip _ a      -> goTerm file env a
  Rzk.RecOr _ rs                -> concatMap (restriction file env) rs
  Rzk.RecOrDeprecated _ a _ b _ c _ d -> concatMap (goTerm file env) [a, b, c, d]
  Rzk.TypeId _ a _ b _ c            -> concatMap (goTerm file env) [a, b, c]
  Rzk.TypeIdSimple _ a _ b        -> goTerm file env a ++ goTerm file env b
  Rzk.TypeRestricted _ a rs     -> goTerm file env a ++ concatMap (restriction file env) rs
  Rzk.App _ a b                 -> goTerm file env a ++ goTerm file env b
  Rzk.Pair _ a _ b                -> goTerm file env a ++ goTerm file env b
  Rzk.Tuple _ a _ b _ cs            -> concatMap (goTerm file env) (a : b : cs)
  Rzk.ModApp _ _ _ a              -> goTerm file env a
  Rzk.ModType _ _ a             -> goTerm file env a
  Rzk.ModExtract _ _ a          -> goTerm file env a
  Rzk.First _ a                 -> goTerm file env a
  Rzk.Second _ a                -> goTerm file env a
  Rzk.ASCII_First _ a           -> goTerm file env a
  Rzk.ASCII_Second _ a          -> goTerm file env a
  Rzk.ReflTerm _ a              -> goTerm file env a
  Rzk.ReflTermType _ a _ b        -> goTerm file env a ++ goTerm file env b
  Rzk.IdJ _ a _ b _ c _ d _ e _ f         -> concatMap (goTerm file env) [a, b, c, d, e, f]
  Rzk.TypeAsc _ a _ b             -> goTerm file env a ++ goTerm file env b

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
  let (env', occs) = bindPat file env pat
  in goTerm file env ty ++ occs ++ goTerm file env' ret

sigmaTupleScope :: FilePath -> Env -> [Rzk.SigmaParam] -> Rzk.Term -> [Link]
sigmaTupleScope file env sps ret =
  let (env', occs) = goSigmaParams file env sps in occs ++ goTerm file env' ret

paramDeclScope :: FilePath -> Env -> Rzk.ParamDecl -> Rzk.Term -> [Link]
paramDeclScope file env pd ret =
  let (env', occs) = goParamDecl file env pd in occs ++ goTerm file env' ret

restriction :: FilePath -> Env -> Rzk.Restriction -> [Link]
restriction file env = \case
  Rzk.Restriction _ a _ b       -> goTerm file env a ++ goTerm file env b
  Rzk.ASCII_Restriction _ a _ b -> goTerm file env a ++ goTerm file env b

goBind :: FilePath -> Env -> Rzk.Bind -> (Env, [Link])
goBind file env = \case
  Rzk.BindPattern _ pat -> bindPat file env pat
  Rzk.BindPatternType _ pat _ ty ->
    let (env', occs) = bindPat file env pat in (env', goTerm file env ty ++ occs)

goParams :: FilePath -> Env -> [Rzk.Param] -> (Env, [Link])
goParams _    env []       = (env, [])
goParams file env (p : ps) =
  let (env1, o1) = goParam file env p
      (env2, o2) = goParams file env1 ps
  in (env2, o1 ++ o2)

goParam :: FilePath -> Env -> Rzk.Param -> (Env, [Link])
goParam file env = \case
  Rzk.ParamPattern _ pat -> bindPat file env pat
  Rzk.ParamPatternType _ pats _ ty ->
    let (env', occs) = bindVars file env (concatMap patternVars pats)
    in (env', goTerm file env ty ++ occs)
  Rzk.ParamPatternShape _ pats _ cube _ tope ->
    let (env', occs) = bindVars file env (concatMap patternVars pats)
    in (env', goTerm file env cube ++ occs ++ goTerm file env' tope)
  Rzk.ParamPatternShapeDeprecated _ pat _ cube _ tope ->
    let (env', occs) = bindPat file env pat
    in (env', goTerm file env cube ++ occs ++ goTerm file env' tope)
  Rzk.ParamPatternModalType _ pats _ ty ->
    let (env', occs) = bindVars file env (concatMap patternVars pats)
    in (env', goTerm file env ty ++ occs)
  Rzk.ParamPatternModalShape _ pats _ cube _ tope ->
    let (env', occs) = bindVars file env (concatMap patternVars pats)
    in (env', goTerm file env cube ++ occs ++ goTerm file env' tope)

goParamDecl :: FilePath -> Env -> Rzk.ParamDecl -> (Env, [Link])
goParamDecl file env = \case
  Rzk.ParamType _ ty -> (env, goTerm file env ty)
  Rzk.ParamTermType _ patTerm _ ty ->
    let (env', occs) = bindVars file env (termPatVars patTerm)
    in (env', goTerm file env ty ++ occs)
  Rzk.ParamTermShape _ patTerm _ cube _ tope ->
    let (env', occs) = bindVars file env (termPatVars patTerm)
    in (env', goTerm file env cube ++ occs ++ goTerm file env' tope)
  Rzk.ParamTermTypeDeprecated _ pat _ ty ->
    let (env', occs) = bindPat file env pat
    in (env', goTerm file env ty ++ occs)
  Rzk.ParamVarShapeDeprecated _ pat _ cube _ tope ->
    let (env', occs) = bindPat file env pat
    in (env', goTerm file env cube ++ occs ++ goTerm file env' tope)
  Rzk.ParamVarShapeDeprecatedAlt _ pat _ cube _ tope ->
    let (env', occs) = bindPat file env pat
    in (env', goTerm file env cube ++ occs ++ goTerm file env' tope)
  Rzk.ParamTermModalType _ patTerm _ ty ->
    let (env', occs) = bindVars file env (termPatVars patTerm)
    in (env', goTerm file env ty ++ occs)
  Rzk.ParamTermModalShape _ patTerm _ cube _ tope ->
    let (env', occs) = bindVars file env (termPatVars patTerm)
    in (env', goTerm file env cube ++ occs ++ goTerm file env' tope)

goSigmaParams :: FilePath -> Env -> [Rzk.SigmaParam] -> (Env, [Link])
goSigmaParams _    env []       = (env, [])
goSigmaParams file env (p : ps) =
  let (env1, o1) = goSigmaParam file env p
      (env2, o2) = goSigmaParams file env1 ps
  in (env2, o1 ++ o2)

goSigmaParam :: FilePath -> Env -> Rzk.SigmaParam -> (Env, [Link])
goSigmaParam file env = \case
  Rzk.SigmaParam _ pat _ ty ->
    let (env', occs) = bindPat file env pat in (env', goTerm file env ty ++ occs)
  Rzk.SigmaParamModal _ pat _ ty ->
    let (env', occs) = bindPat file env pat in (env', goTerm file env ty ++ occs)
