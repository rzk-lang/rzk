{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Drawing a term as an SVG diagram of a cube.
--
-- The geometry lives in "Rzk.Render.Geometry"; what is here is the part that
-- knows about terms: which subshape of the cube a tope carves out, which term
-- inhabits each of them, and what to label it with.
module Rzk.TypeCheck.Render where

import           Control.Monad            (forM)
import           Control.Monad.Reader     (asks)
import           Data.List                (intercalate, (\\))
import           Data.Maybe               (catMaybes)

import           Control.Monad.Foil       (Distinct)
import qualified Control.Monad.Foil       as Foil
import           Control.Monad.Free.Foil  (AST (Var))

import           Language.Rzk.Foil.Syntax
import           Language.Rzk.Foil.Names (Binder (..), TModality (..),
                                           binderName)
import           Rzk.Render.Geometry
import           Rzk.TypeCheck.Context
import           Rzk.TypeCheck.Display
import           Rzk.TypeCheck.Eval
import           Rzk.TypeCheck.Monad

-- * The subshapes of a cube

cube2powerT :: Int -> TermT n
cube2powerT 1   = cube2T
cube2powerT dim = cubeProductT (cube2powerT (dim - 1)) cube2T

splits :: [a] -> [([a], [a])]
splits [] = [([], [])]
splits (x:xs) = ([], x:xs) : [ (x : before, after) | (before, after) <- splits xs ]

verticesFrom :: [TermT n] -> [(ShapeId, TermT n)]
verticesFrom ts = combine <$> mapM mk ts
  where
    mk t = [("0", topeEQT t cube2_0T), ("1", topeEQT t cube2_1T)]
    combine xs = ([concat (map fst xs)], foldr1 topeAndT (map snd xs))

subTopes2 :: Int -> TermT n -> [(ShapeId, TermT n)]
-- 1-dim
subTopes2 1 t =
  [ (words "0", topeEQT t cube2_0T)
  , (words "1", topeEQT t cube2_1T)
  , (words "0 1", topeTopT) ]
-- 2-dim
subTopes2 2 ts =
  -- vertices
  [ (words "00", topeEQT t cube2_0T `topeAndT` topeEQT s cube2_0T)
  , (words "01", topeEQT t cube2_0T `topeAndT` topeEQT s cube2_1T)
  , (words "10", topeEQT t cube2_1T `topeAndT` topeEQT s cube2_0T)
  , (words "11", topeEQT t cube2_1T `topeAndT` topeEQT s cube2_1T)
  -- edges and the diagonal
  , (words "00 01", topeEQT t cube2_0T)
  , (words "10 11", topeEQT t cube2_1T)
  , (words "00 10", topeEQT s cube2_0T)
  , (words "01 11", topeEQT s cube2_1T)
  , (words "00 11", topeEQT s t)
  -- triangles
  , (words "00 01 11", topeLEQT t s)
  , (words "00 10 11", topeLEQT s t)
  ]
  where
    t = firstT cube2T ts
    s = secondT cube2T ts
-- 3-dim
subTopes2 3 t =
  -- vertices
  [ (words "000", topeEQT t1 cube2_0T `topeAndT` topeEQT t2 cube2_0T `topeAndT` topeEQT t3 cube2_0T)
  , (words "001", topeEQT t1 cube2_0T `topeAndT` topeEQT t2 cube2_0T `topeAndT` topeEQT t3 cube2_1T)
  , (words "010", topeEQT t1 cube2_0T `topeAndT` topeEQT t2 cube2_1T `topeAndT` topeEQT t3 cube2_0T)
  , (words "011", topeEQT t1 cube2_0T `topeAndT` topeEQT t2 cube2_1T `topeAndT` topeEQT t3 cube2_1T)
  , (words "100", topeEQT t1 cube2_1T `topeAndT` topeEQT t2 cube2_0T `topeAndT` topeEQT t3 cube2_0T)
  , (words "101", topeEQT t1 cube2_1T `topeAndT` topeEQT t2 cube2_0T `topeAndT` topeEQT t3 cube2_1T)
  , (words "110", topeEQT t1 cube2_1T `topeAndT` topeEQT t2 cube2_1T `topeAndT` topeEQT t3 cube2_0T)
  , (words "111", topeEQT t1 cube2_1T `topeAndT` topeEQT t2 cube2_1T `topeAndT` topeEQT t3 cube2_1T)
  -- edges
  , (words "000 001", topeEQT t1 cube2_0T `topeAndT` topeEQT t2 cube2_0T)
  , (words "010 011", topeEQT t1 cube2_0T `topeAndT` topeEQT t2 cube2_1T)
  , (words "000 010", topeEQT t1 cube2_0T `topeAndT` topeEQT t3 cube2_0T)
  , (words "001 011", topeEQT t1 cube2_0T `topeAndT` topeEQT t3 cube2_1T)
  , (words "100 101", topeEQT t1 cube2_1T `topeAndT` topeEQT t2 cube2_0T)
  , (words "110 111", topeEQT t1 cube2_1T `topeAndT` topeEQT t2 cube2_1T)
  , (words "100 110", topeEQT t1 cube2_1T `topeAndT` topeEQT t3 cube2_0T)
  , (words "101 111", topeEQT t1 cube2_1T `topeAndT` topeEQT t3 cube2_1T)
  , (words "000 100", topeEQT t2 cube2_0T `topeAndT` topeEQT t3 cube2_0T)
  , (words "001 101", topeEQT t2 cube2_0T `topeAndT` topeEQT t3 cube2_1T)
  , (words "010 110", topeEQT t2 cube2_1T `topeAndT` topeEQT t3 cube2_0T)
  , (words "011 111", topeEQT t2 cube2_1T `topeAndT` topeEQT t3 cube2_1T)
  -- face diagonals
  , (words "000 011", topeEQT t1 cube2_0T `topeAndT` topeEQT t2 t3)
  , (words "100 111", topeEQT t1 cube2_1T `topeAndT` topeEQT t2 t3)
  , (words "000 101", topeEQT t2 cube2_0T `topeAndT` topeEQT t1 t3)
  , (words "010 111", topeEQT t2 cube2_1T `topeAndT` topeEQT t1 t3)
  , (words "000 110", topeEQT t3 cube2_0T `topeAndT` topeEQT t1 t2)
  , (words "001 111", topeEQT t3 cube2_1T `topeAndT` topeEQT t1 t2)
  -- the long diagonal
  , (words "000 111", topeEQT t3 t2 `topeAndT` topeEQT t2 t1)
  -- face triangles
  , (words "000 001 011", topeEQT t1 cube2_0T `topeAndT` topeLEQT t2 t3)
  , (words "000 010 011", topeEQT t1 cube2_0T `topeAndT` topeLEQT t3 t2)
  , (words "100 101 111", topeEQT t1 cube2_1T `topeAndT` topeLEQT t2 t3)
  , (words "100 110 111", topeEQT t1 cube2_1T `topeAndT` topeLEQT t3 t2)
  , (words "000 001 101", topeEQT t2 cube2_0T `topeAndT` topeLEQT t1 t3)
  , (words "000 100 101", topeEQT t2 cube2_0T `topeAndT` topeLEQT t3 t1)
  , (words "010 011 111", topeEQT t2 cube2_1T `topeAndT` topeLEQT t1 t3)
  , (words "010 110 111", topeEQT t2 cube2_1T `topeAndT` topeLEQT t3 t1)
  , (words "000 010 110", topeEQT t3 cube2_0T `topeAndT` topeLEQT t1 t2)
  , (words "000 100 110", topeEQT t3 cube2_0T `topeAndT` topeLEQT t2 t1)
  , (words "001 011 111", topeEQT t3 cube2_1T `topeAndT` topeLEQT t1 t2)
  , (words "001 101 111", topeEQT t3 cube2_1T `topeAndT` topeLEQT t2 t1)
  -- diagonal triangles
  , (words "000 001 111", topeEQT t1 t2 `topeAndT` topeLEQT t2 t3)
  , (words "000 010 111", topeEQT t1 t3 `topeAndT` topeLEQT t1 t2)
  , (words "000 100 111", topeEQT t2 t3 `topeAndT` topeLEQT t2 t1)
  , (words "000 011 111", topeLEQT t1 t2 `topeAndT` topeEQT t2 t3)
  , (words "000 101 111", topeLEQT t2 t1 `topeAndT` topeEQT t1 t3)
  , (words "000 110 111", topeLEQT t3 t1 `topeAndT` topeEQT t1 t2)
  -- tetrahedra
  , (words "000 001 011 111", topeLEQT t1 t2 `topeAndT` topeLEQT t2 t3)
  , (words "000 010 011 111", topeLEQT t1 t3 `topeAndT` topeLEQT t3 t2)
  , (words "000 001 101 111", topeLEQT t2 t1 `topeAndT` topeLEQT t1 t3)
  , (words "000 100 101 111", topeLEQT t2 t3 `topeAndT` topeLEQT t3 t1)
  , (words "000 010 110 111", topeLEQT t3 t1 `topeAndT` topeLEQT t1 t2)
  , (words "000 100 110 111", topeLEQT t3 t2 `topeAndT` topeLEQT t2 t1)
  ]
  where
    t1 = firstT  cube2T (firstT (cube2powerT 2) t)
    t2 = secondT cube2T (firstT (cube2powerT 2) t)
    t3 = secondT cube2T t
subTopes2 dim _ = error (show dim <> " dimensions are not supported")

componentWiseEQT :: Int -> TermT n -> TermT n -> TermT n
componentWiseEQT 1 t s = topeEQT t s
componentWiseEQT 2 t s = topeAndT
  (componentWiseEQT 1 (firstT  cube2T t) (firstT  cube2T s))
  (componentWiseEQT 1 (secondT cube2T t) (secondT cube2T s))
componentWiseEQT 3 t s = topeAndT
  (componentWiseEQT 2 (firstT  (cube2powerT 2) t) (firstT (cube2powerT 2) s))
  (componentWiseEQT 1 (secondT cube2T t) (secondT cube2T s))
componentWiseEQT dim _ _ = error ("cannot work with " <> show dim <> " dimensions")
-- * Rendering

-- | Is the variable an anonymous one (written @_@)? Its cells are left
-- unlabelled.
isAnonymous :: Foil.Name n -> TypeCheck n Bool
isAnonymous x = asks ((== Just "_") . binderName . varOrig . lookupVarInfo x)

-- | Render a term in the current context.
ppInContext :: TermT n -> TypeCheck n String
ppInContext t = do
  naming <- asks namingOfContext
  pure (ppTerm naming (untyped t))

renderObjectsFor
  :: Distinct n
  => String -> Int -> TermT n -> TermT n
  -> TypeCheck n [(ShapeId, RenderObjectData)]
renderObjectsFor mainColor dim t term = fmap catMaybes $
  forM (subTopes2 dim t) $ \(shapeId, tope) ->
    checkTopeEntails tope >>= \case
      False -> return Nothing
      True -> typeOf term >>= \case
        UniverseTopeT{} -> localTope term $ checkTopeEntails tope >>= \case
          False -> return Nothing
          True -> return $ Just (shapeId, RenderObjectData
            { renderObjectDataLabel = ""
            , renderObjectDataFullLabel = ""
            , renderObjectDataColor = "orange"  -- FIXME: orange for topes?
            })
        _ -> do
          term' <- localTope tope $ whnfT term
          let argIsOfT arg =
                null (freeVarsOfTermT arg \\ freeVarsOfTermT t)
          label <- case term' of
            AppT _ (Var z) arg -> isAnonymous z >>= \case
              True -> return ""
              False
                | argIsOfT arg -> ppInContext (Var z)
                | otherwise    -> ppInContext term'
            _ -> ppInContext term'
          color <- case term' of
            Var{} -> return "purple"
            AppT _ (Var x) arg -> isAnonymous x >>= \case
              True -> return mainColor
              False
                | argIsOfT arg -> return "purple"
                | otherwise    -> return mainColor
            _ -> return mainColor
          hide <- asks ctxRenderHideTerm
          return $ Just (shapeId, hideTermData hide mainColor RenderObjectData
            { renderObjectDataLabel = label
            , renderObjectDataFullLabel = label
            , renderObjectDataColor = color
            })

renderObjectsInSubShapeFor
  :: Distinct n
  => String -> Int -> [Foil.Name n] -> Foil.Name n
  -> TermT n -> TermT n -> TermT n
  -> TypeCheck n [(ShapeId, RenderObjectData)]
renderObjectsInSubShapeFor mainColor dim sub super retType f x = fmap catMaybes $ do
  let reduceContext
        = foldr topeOrT topeBottomT
        . map (foldr topeAndT topeTopT)
        . map (filter (\tope -> all (`notElem` freeVarsOfTermT tope) sub))
        . map (map tTope)
        . map saturateTopes
        . simplifyLHSwithDisjunctions
  contextTopes  <- asks (reduceContext . ctxTopesNF)
  contextTopes' <- localTope (componentWiseEQT dim (Var super) x) $
    asks (reduceContext . ctxTopesNF)
  forM (subTopes2 dim (Var super)) $ \(shapeId, tope) ->
    checkEntails tope contextTopes >>= \case
      False -> return Nothing
      True -> do
        term <- localTope tope (whnfT (appT retType f (Var super)))
        let argIsSuper arg = null (freeVarsOfTermT arg \\ [super])
        label <- typeOf term >>= \case
          UniverseTopeT{} -> return ""
          _ -> case term of
            AppT _ (Var z) arg -> isAnonymous z >>= \case
              True -> return ""
              False
                | argIsSuper arg -> ppInContext (Var z)
                | otherwise      -> ppInContext term
            _ -> ppInContext term
        color <- checkEntails tope contextTopes' >>= \case
          True -> case term of
            Var{} -> return "purple"
            AppT _ (Var z) arg -> isAnonymous z >>= \case
              True -> return mainColor
              False
                | argIsSuper arg -> return "purple"
                | otherwise      -> return mainColor
            _ -> return mainColor
          False -> return "gray"
        hide <- asks ctxRenderHideTerm
        return $ Just (shapeId, hideTermData hide mainColor RenderObjectData
          { renderObjectDataLabel = label
          , renderObjectDataFullLabel = label
          , renderObjectDataColor = color
          })

renderForSubShapeSVG
  :: Distinct n
  => String -> Int -> [Foil.Name n] -> Foil.Name n
  -> TermT n -> TermT n -> TermT n
  -> TypeCheck n String
renderForSubShapeSVG mainColor dim sub super retType f x = do
  objects <- renderObjectsInSubShapeFor mainColor dim sub super retType f x
  pure (drawCube dim (map mk objects))
  where
    mk (shapeId, renderData) = (intercalate "-" (map fill shapeId), renderData)
    fill xs = xs <> replicate (3 - length xs) '1'

renderForSVG
  :: Distinct n => String -> Int -> TermT n -> TermT n -> TypeCheck n String
renderForSVG mainColor dim t term = do
  objects <- renderObjectsFor mainColor dim t term
  pure (drawCube dim (map mk objects))
  where
    mk (shapeId, renderData) = (intercalate "-" (map fill shapeId), renderData)
    fill xs = xs <> replicate (3 - length xs) '1'

drawCube :: Int -> [(String, RenderObjectData)] -> String
drawCube dim objects =
  renderCube defaultCamera rotation (`lookup` objects)
  where
    rotation :: Double
    rotation = if dim > 2 then pi/7 else 0

-- | The dimension of a cube, if it is a power of the directed interval.
dimOf :: TermT n -> Maybe Int
dimOf = \case
  Cube2T{}           -> Just 1
  CubeProductT _ l r -> (+) <$> dimOf l <*> dimOf r
    -- WARNING: breaks for 2 * (2 * 2)
  _                  -> Nothing

maxRenderDim :: Int
maxRenderDim = 3

renderTermSVGFor
  :: Distinct n
  => String                              -- ^ main colour
  -> Int                                 -- ^ dimensions accumulated so far (0 to 3)
  -> (Maybe (TermT n, TermT n), [Foil.Name n])  -- ^ the accumulated point, and its cube
  -> TermT n                             -- ^ the term to render
  -> TypeCheck n (Maybe String)
renderTermSVGFor mainColor accDim (mp, xs) t = do
  t' <- whnfT t
  ty <- typeOf t'
  case t of -- check the unevaluated term
    AppT _info f x -> renderApp f x
    TypeFunT _ _orig' md' _ _ _
      | null xs -> withBinder (BinderVar (Just "_")) md' t' $ \binder ->
          renderTermSVGFor "blue" 0 (Nothing, []) (Var (Foil.nameOf binder))  -- blue for types

    _ -> case t' of -- check the evaluated term
      AppT _info f x -> renderApp f x
      TypeFunT _ _orig' md' _ _ _
        | null xs -> withBinder (BinderVar (Just "_")) md' t' $ \binder ->
            renderTermSVGFor "blue" 0 (Nothing, []) (Var (Foil.nameOf binder))

      _ -> case ty of -- check the type of the term
        TypeFunT _ orig md arg mtope ret
          | Just dim <- dimOf arg, accDim + dim <= maxRenderDim ->
              underArg orig md arg mtope ret t' (accDim + dim) True
          | null xs ->
              underArg orig md arg mtope ret t' accDim False
        _ -> renderAccumulated t'
  where
    renderAccumulated t' = traverse (\(p', _) -> renderForSVG mainColor accDim p' t') mp

    renderApp f x = typeOf f >>= \case
      TypeFunT _ fOrig md fArg mtopeArg ret
        | Just dim <- dimOf fArg, dim <= maxRenderDim ->
            inScopeMaybeTope fOrig md fArg mtopeArg $ \binder -> do
              ret' <- openScoped binder ret
              -- FIXME: breaks for 2 * (2 * 2), but works for 2 * 2 * 2 = (2 * 2) * 2
              Just <$> renderForSubShapeSVG mainColor dim
                (Foil.sink1 xs) (Foil.nameOf binder)
                ret' (Foil.sink f) (Foil.sink x)
      _ -> do
        t' <- whnfT t
        renderAccumulated t'

    -- Go under the domain of a function type, assuming its shape tope if it has
    -- one, and render the body there.
    underArg orig md arg mtope ret t' accDim' extend =
      inScopeMaybeTope orig md arg mtope $ \binder -> do
        let z = Var (Foil.nameOf binder)
            arg' = Foil.sink arg
        body <- case t' of
          LambdaT _ _orig _marg lamBody -> openScoped binder lamBody
          _ -> do
            ret' <- openScoped binder ret
            pure (appT ret' (Foil.sink t') z)
        let mp' | extend = join' (fmap (both Foil.sink) mp) arg' z
                | otherwise = fmap (both Foil.sink) mp
            xs' | extend = Foil.nameOf binder : Foil.sink1 xs
                | otherwise = Foil.sink1 xs
        renderTermSVGFor mainColor accDim' (mp', xs') body

    both f (x, y) = (f x, f y)

    join' Nothing Cube2T{} x = Just (x, cube2T)
    join' (Just (p, pt)) Cube2T{} x = Just (p', pt')
      where
        pt' = cubeProductT pt cube2T
        p' = pairT pt' p x
    join' p (CubeProductT _ l r) x =
      join' (join' p l (firstT l x)) r (secondT r x)
    join' _ _ _ = Nothing -- FIXME: error?

-- | Enter a binder and assume the shape tope it carries, if any.
inScopeMaybeTope
  :: Distinct n
  => Binder -> TModality -> TermT n -> Maybe (ScopedTermT n)
  -> (forall l. (Foil.DExt n l, Distinct l) => Foil.NameBinder n l -> TypeCheck l a)
  -> TypeCheck n a
inScopeMaybeTope orig md ty mtope k =
  withBinder orig md ty $ \binder ->
    case mtope of
      Nothing   -> k binder
      Just tope -> do
        tope' <- openScoped binder tope
        localTope tope' (k binder)

renderTermSVG :: Distinct n => TermT n -> TypeCheck n (Maybe String)
renderTermSVG = renderTermSVGFor "red" 0 (Nothing, [])  -- red for terms, by default

-- | Render the goal /cell/ for a (shape) type: introduce an abstract inhabitant
-- and render it with the proof term hidden. Under a boundary tope an abstract
-- inhabitant of an extension type reduces to the prescribed face value, so the
-- cell shows its given edges with a blank interior — the shape to inhabit, not an
-- answer. 'Nothing' for a non-shape type (a 0-cell, or a non-cube goal).
renderGoalCellSVG :: Distinct n => TermT n -> TypeCheck n (Maybe String)
renderGoalCellSVG ty =
  hidingTerm $ withBinder (BinderVar (Just "_")) Id ty $ \binder ->
    renderTermSVG' (Var (Foil.nameOf binder))

renderTermSVG' :: forall n. Distinct n => TermT n -> TypeCheck n (Maybe String)
renderTermSVG' t = whnfT t >>= \t' -> typeOf t >>= \case
  TypeFunT _ orig md arg mtope ret ->
    inScopeMaybeTope orig md arg mtope $ \binder ->
      case t' of
        LambdaT _ _orig _marg lamBody ->
          openScoped binder lamBody >>= \case
            AppT _info f x -> typeOf f >>= \case
              TypeFunT _ fOrig md2 fArg mtope2 _ret
                | Just dim <- dimOf fArg -> do
                    ret' <- openScoped binder ret
                    inScopeMaybeTope fOrig md2 fArg mtope2 $ \binder2 ->
                      Just <$> renderForSubShapeSVG "red" dim
                        [Foil.sink (Foil.nameOf binder)] (Foil.nameOf binder2)
                        (Foil.sink ret') (Foil.sink f) (Foil.sink x)
              _ -> renderApplied binder t' arg ret
            _ -> renderApplied binder t' arg ret
        _ -> renderApplied binder t' arg ret
  _t' -> return Nothing

-- | Render a term of a function type by applying it to the variable it abstracts
-- over, and drawing that.
renderApplied
  :: Foil.DExt n l
  => Foil.NameBinder n l -> TermT n -> TermT n -> ScopedTermT n
  -> TypeCheck l (Maybe String)
renderApplied binder t' arg ret = do
  ret' <- openScoped binder ret
  let z = Var (Foil.nameOf binder)
      applied = appT ret' (Foil.sink t') z
  case dimOf arg of
    Just dim | dim <= maxRenderDim ->
      Just <$> renderForSVG "red" dim z applied
    _ -> renderTermSVG' applied
