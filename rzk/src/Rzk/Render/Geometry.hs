{-# LANGUAGE RecordWildCards #-}

-- | The geometry behind the SVG rendering of cubes, and nothing else.
--
-- Rzk renders a (sub)shape of a cube as an SVG diagram: the vertices, edges and
-- faces of the unit cube are projected to the plane through a camera, and each
-- of them is labelled with the term that inhabits it. This module holds the part
-- of that story which knows nothing about terms: the projection matrices, the
-- camera, and 'renderCube', which draws a cube given only a function saying what
-- (if anything) to draw on each of its parts.
--
-- Nothing here mentions the type checker, so it is shared by both term
-- representations during the free-foil migration.
module Rzk.Render.Geometry where

import           Data.List (intercalate, tails)

-- | The name of a vertex of the unit cube, as a string of coordinates
-- (e.g. @"010"@).
type PointId = String

-- | The name of a subshape of the unit cube: its vertices, in order, joined by
-- dashes (e.g. @"000-011"@ for an edge, @"000"@ for a vertex).
type ShapeId = [PointId]

type Point2D a = (a, a)
type Point3D a = (a, a, a)
type Edge3D a = (Point3D a, Point3D a)
type Face3D a = (Point3D a, Point3D a, Point3D a)
type Volume3D a = (Point3D a, Point3D a, Point3D a, Point3D a)

data CubeCoords2D a b = CubeCoords2D
  { vertices :: [(Point3D a, Point2D b)]
  , edges    :: [(Edge3D a, (Point2D b, Point2D b))]
  , faces    :: [(Face3D a, (Point2D b, Point2D b, Point2D b))]
  , volumes  :: [(Volume3D a, (Point2D b, Point2D b, Point2D b, Point2D b))]
  }

data Matrix3D a = Matrix3D
  a a a
  a a a
  a a a

data Matrix4D a = Matrix4D
  a a a a
  a a a a
  a a a a
  a a a a

data Vector3D a = Vector3D a a a

data Vector4D a = Vector4D a a a a

rotateX :: Floating a => a -> Matrix3D a
rotateX theta = Matrix3D
  1 0 0
  0 (cos theta) (- sin theta)
  0 (sin theta) (cos theta)

rotateY :: Floating a => a -> Matrix3D a
rotateY theta = Matrix3D
  (cos theta) 0 (sin theta)
  0 1 0
  (- sin theta) 0 (cos theta)

rotateZ :: Floating a => a -> Matrix3D a
rotateZ theta = Matrix3D
  (cos theta) (- sin theta) 0
  (sin theta) (cos theta) 0
  0 0 1

data Camera a = Camera
  { cameraPos         :: Point3D a
  , cameraFoV         :: a
  , cameraAspectRatio :: a
  , cameraAngleY      :: a
  , cameraAngleX      :: a
  }

viewRotateX :: Floating a => Camera a -> Matrix4D a
viewRotateX Camera{..} = matrix3Dto4D (rotateX cameraAngleX)

viewRotateY :: Floating a => Camera a -> Matrix4D a
viewRotateY Camera{..} = matrix3Dto4D (rotateY cameraAngleY)

viewTranslate :: Num a => Camera a -> Matrix4D a
viewTranslate Camera{..} = Matrix4D
  1 0 0 0
  0 1 0 0
  0 0 1 0
  (-x) (-y) (-z) 1
  where
    (x, y, z) = cameraPos

project2D :: Floating a => Camera a -> Matrix4D a
project2D Camera{..} = Matrix4D
  (2 * n / (r - l)) 0 ((r + l) / (r - l)) 0
  0 (2 * n / (t - b)) ((t + b) / (t - b)) 0
  0 0 (- (f + n) / (f - n)) (- 2 * f * n / (f - n))
  0 0 (-1) 0
  where
    n = 1
    f = 2
    r = n * tan (cameraFoV / 2)
    l = -r
    t = r * cameraAspectRatio
    b = -t


matrixVectorMult4D :: Num a => Matrix4D a -> Vector4D a -> Vector4D a
matrixVectorMult4D
  (Matrix4D
    a1 a2 a3 a4
    b1 b2 b3 b4
    c1 c2 c3 c4
    d1 d2 d3 d4)
  (Vector4D a b c d)
    = Vector4D a' b' c' d'
  where
    a' = sum (zipWith (*) [a1, b1, c1, d1] [a, b, c, d])
    b' = sum (zipWith (*) [a2, b2, c2, d2] [a, b, c, d])
    c' = sum (zipWith (*) [a3, b3, c3, d3] [a, b, c, d])
    d' = sum (zipWith (*) [a4, b4, c4, d4] [a, b, c, d])

matrix3Dto4D :: Num a => Matrix3D a -> Matrix4D a
matrix3Dto4D
  (Matrix3D
    a1 b1 c1
    a2 b2 c2
    a3 b3 c3) = Matrix4D
      a1 b1 c1 0
      a2 b2 c2 0
      a3 b3 c3 0
      0 0 0 1

fromAffine :: Fractional a => Vector4D a -> (Point2D a, a)
fromAffine (Vector4D a b c d) = ((x, y), zIndex)
  where
    x = a / d
    y = b / d
    zIndex = c / d

point3Dto2D :: Floating a => Camera a -> a -> Point3D a -> (Point2D a, a)
point3Dto2D camera rotY (x, y, z) = fromAffine $
  foldr matrixVectorMult4D (Vector4D x y z 1) $ reverse
    [ matrix3Dto4D (rotateY rotY)
    , viewTranslate camera
    , viewRotateY camera
    , viewRotateX camera
    , project2D camera
    ]

-- | What to draw on one part (vertex, edge or face) of a cube.
data RenderObjectData = RenderObjectData
  { renderObjectDataLabel     :: String
  , renderObjectDataFullLabel :: String
  , renderObjectDataColor     :: String
  }

limitLength :: Int -> String -> String
limitLength n s
  | length s > n = take (n - 1) s <> "…"
  | otherwise    = s

-- | Apply the term-hiding policy to a cell's render data: drop the @\<title\>@
-- (the full term) from every cell, and blank the visible label of a
-- proof-coloured (interior) cell. Boundary cells (coloured otherwise) keep
-- their given labels. A no-op when not hiding.
hideTermData :: Bool -> String -> RenderObjectData -> RenderObjectData
hideTermData False _ d = d
hideTermData True  mainColor d
  | renderObjectDataColor d == mainColor =
      d { renderObjectDataLabel = "", renderObjectDataFullLabel = "" }
  | otherwise = d { renderObjectDataFullLabel = "" }

renderCube
  :: (Floating a, Show a)
  => Camera a
  -> a
  -> (String -> Maybe RenderObjectData)
  -> String
renderCube camera rotY renderDataOf' = unlines $ filter (not . null)
  [ "<svg class=\"rzk-render\" viewBox=\"-175 -200 350 375\" width=\"150\" height=\"150\">"
  , intercalate "\n"
      [ "  <path d=\"M " <> show x1 <> " " <> show y1
                <> " L " <> show x2 <> " " <> show y2
                <> " L " <> show x3 <> " " <> show y3
                <> " Z\" style=\"fill: " <> renderObjectDataColor <> "; opacity: 0.2\"><title>" <> renderObjectDataFullLabel <> "</title></path>" <> "\n" <>
        "  <text x=\"" <> show x <> "\" y=\"" <> show y <> "\" fill=\"" <> renderObjectDataColor <> "\">" <> renderObjectDataLabel <> "</text>"
      | (faceId, (((x1, y1), (x2, y2), (x3, y3)), _)) <- faces
      , Just RenderObjectData{..} <- [renderDataOf faceId]
      , let x = (x1 + x2 + x3) / 3
      , let y = (y1 + y2 + y3) / 3 ]
  , intercalate "\n"
      [ "  <polyline points=\"" <> show x1 <> "," <> show y1 <> " " <> show x2 <> "," <> show y2
        <> "\" stroke=\"" <> renderObjectDataColor <> "\" stroke-width=\"3\" marker-end=\"url(#arrow)\"><title>" <> renderObjectDataFullLabel <> "</title></polyline>" <> "\n" <>
        "  <text x=\"" <> show x <> "\" y=\"" <> show y <> "\" fill=\"" <> renderObjectDataColor <> "\" stroke=\"white\" stroke-width=\"10\" stroke-opacity=\".8\" paint-order=\"stroke\">" <> renderObjectDataLabel <> "</text>"
      | (edge, (((x1, y1), (x2, y2)), _)) <- edges
      , Just RenderObjectData{..} <- [renderDataOf edge]
      , let x = (x1 + x2) / 2
      , let y = (y1 + y2) / 2 ]
  , intercalate "\n"
      [ "  <text x=\"" <> show x <> "\" y=\"" <> show y <> "\" fill=\"" <> renderObjectDataColor <> "\">" <> renderObjectDataLabel <> "</text>"
      | (v, ((x, y), _)) <- vertices
      , Just RenderObjectData{..} <- [renderDataOf v]]
  , "</svg>" ]
  where
    renderDataOf shapeId =
      case renderDataOf' shapeId of
        Nothing -> Nothing
        Just RenderObjectData{..} -> Just RenderObjectData
          -- FIXME: move constants to configurable parameters
          { renderObjectDataLabel = hideWhenLargerThan shapeId 5 renderObjectDataLabel
          , renderObjectDataFullLabel = limitLength 30 renderObjectDataFullLabel
          , .. }

    hideWhenLargerThan shapeId n s
      | null s || length s > n = if '-' `elem` shapeId then "" else "•"
      | otherwise = s

    vertices =
      [ (show x <> show y <> show z, ((500 * x'', 500 * y''), zIndex))
      | x <- [0,1]
      , y <- [0,1]
      , z <- [0,1]
      , let f c = 2 * fromInteger c - 1
      , let x' = f x
      , let y' = f (1-y)
      , let z' = f z
      , let ((x'', y''), zIndex) = point3Dto2D camera rotY (x', y', z') ]

    radius = 20

    mkEdge r (x1, y1) (x2, y2) = ((x1 + dx, y1 + dy), ((x2 - dx), (y2 - dy)))
      where
        d = sqrt ((x2 - x1)^(2 :: Int) + (y2 - y1)^(2 :: Int))
        dx = r * (x2 - x1) / d
        dy = r * (y2 - y1) / d

    scaleAround (cx, cy) s (x, y) = (cx + s * (x - cx), cy + s * (y - cy))

    mkFace (x1, y1) (x2, y2) (x3, y3) = (p1, p2, p3)
      where
        cx = (x1 + x2 + x3) / 3
        cy = (y1 + y2 + y3) / 3
        p1 = scaleAround (cx, cy) 0.85 (x1, y1)
        p2 = scaleAround (cx, cy) 0.85 (x2, y2)
        p3 = scaleAround (cx, cy) 0.85 (x3, y3)

    edges =
      [ (intercalate "-" [fromName, toName], (mkEdge radius from to, 0 :: Int))
      | (fromName, (from, _)) : vs <- tails vertices
      , (toName, (to, _)) <- vs
      , and (zipWith (<=) fromName toName)
      ]

    faces =
      [ (intercalate "-" [name1, name2, name3], (mkFace v1 v2 v3, 0 :: Int))
      | (name1, (v1, _)) : vs <- tails vertices
      , (name2, (v2, _)) : vs' <- tails vs
      , and (zipWith (<=) name1 name2)
      , (name3, (v3, _)) <- vs'
      , and (zipWith (<=) name2 name3)
      ]


defaultCamera :: Floating a => Camera a
defaultCamera = Camera
  { cameraPos = (0, 7, 10)
  , cameraAngleY = pi
  , cameraAngleX = pi/5
  , cameraFoV = pi/15
  , cameraAspectRatio = 1
  }
