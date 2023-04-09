{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Free.Scoped.TH where

import           Control.Monad         (replicateM)
import           Language.Haskell.TH
import           Free.Scoped

makePatternsAll :: Name -> Q [Dec]
makePatternsAll ty = do
  TyConI tyCon <- reify ty
  case tyCon of
    DataD _ _ _ _ cs _ -> concat <$> do
      xs <- mapM makePatternFor cs
      xs' <- makeCompletePragma cs
      ys <- mapM makePatternEFor cs
      ys' <- makeCompletePragmaE cs
      zs <- mapM makePatternTFor cs
      zs' <- makeCompletePragmaT cs
      ws <- mapM makePatternTEFor cs
      ws' <- makeCompletePragmaTE cs
      return (xs ++ [xs'] ++ ys ++ [ys'] ++ zs ++ [zs'] ++ ws ++ [ws'])

    _                  -> fail "Can only make patterns for data types."


makeCompletePragma :: [Con] -> Q [Dec]
makeCompletePragma cs = do
  DataConI varName _ _ <- reify 'Pure
  let names = [mkName (removeF (nameBase name)) | NormalC name _ <- cs]
  return [PragmaD (CompleteP (varName : names) Nothing)]
  where
    removeF s = take (length s - 1) s

makeCompletePragmaE :: [Con] -> Q [Dec]
makeCompletePragmaE cs = do
  DataConI varName _ _ <- reify 'Pure
  PatSynI extName _ <- reify 'ExtE
  let names = [mkName (removeF (nameBase name)) | NormalC name _ <- cs]
  return [PragmaD (CompleteP (varName : extName : names) Nothing)]
  where
    removeF s = take (length s - 1) s <> "E"

makeCompletePragmaT :: [Con] -> Q [Dec]
makeCompletePragmaT cs = do
  DataConI varName _ _ <- reify 'Pure
  let names = [mkName (removeF (nameBase name)) | NormalC name _ <- cs]
  return [PragmaD (CompleteP (varName : names) Nothing)]
  where
    removeF s = take (length s - 1) s <> "T"

makeCompletePragmaTE :: [Con] -> Q [Dec]
makeCompletePragmaTE cs = do
  DataConI varName _ _ <- reify 'Pure
  let names = [mkName (removeF (nameBase name)) | NormalC name _ <- cs]
  return [PragmaD (CompleteP (varName : names) Nothing)]
  where
    removeF s = take (length s - 1) s <> "TE"

makePatternFor :: Con -> Q [Dec]
makePatternFor = \case
  NormalC name xs -> do
    args <- replicateM (length xs) (newName "x")
    let patName = mkName (removeF (nameBase name))
        patArgs = PrefixPatSyn args
        dir = ImplBidir
    pat <- [p| Free $(pure (ConP name [] (VarP <$> args))) |]
    return [PatSynD patName patArgs dir pat]
  _ -> fail "Can only make patterns for NormalC constructors"
  where
    removeF s = take (length s - 1) s

makePatternEFor :: Con -> Q [Dec]
makePatternEFor = \case
  NormalC name xs -> do
    args <- replicateM (length xs) (newName "x")
    let patName = mkName (removeF (nameBase name))
        patArgs = PrefixPatSyn args
        dir = ImplBidir
    pat <- [p| Free (InL $(pure (ConP name [] (VarP <$> args)))) |]
    return [PatSynD patName patArgs dir pat]
  _ -> fail "Can only make patterns for NormalC constructors"
  where
    removeF s = take (length s - 1) s <> "E"

makePatternTFor :: Con -> Q [Dec]
makePatternTFor = \case
  NormalC name xs -> do
    t <- newName "type_"
    args <- replicateM (length xs) (newName "x")
    let patName = mkName (removeF (nameBase name))
        patArgs = PrefixPatSyn (t : args)
        dir = ImplBidir
    pat <- [p| Free (AnnF $(pure (VarP t)) $(pure (ConP name [] (VarP <$> args)))) |]
    return [PatSynD patName patArgs dir pat]
  _ -> fail "Can only make patterns for NormalC constructors"
  where
    removeF s = take (length s - 1) s <> "T"

makePatternTEFor :: Con -> Q [Dec]
makePatternTEFor = \case
  NormalC name xs -> do
    t <- newName "type_"
    args <- replicateM (length xs) (newName "x")
    let patName = mkName (removeF (nameBase name))
        patArgs = PrefixPatSyn (t : args)
        dir = ImplBidir
    pat <- [p| Free (InL (AnnF $(pure (VarP t)) $(pure (ConP name [] (VarP <$> args))))) |]
    return [PatSynD patName patArgs dir pat]
  _ -> fail "Can only make patterns for NormalC constructors"
  where
    removeF s = take (length s - 1) s <> "TE"
