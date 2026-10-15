module Scenes (sceneOne, sceneTwo) where

import Color
import Vec3
import Objects.Object
import Objects.Sphere
import Objects.Triangle
import Scene
import Ray
import Objects.Material
import Light

matte :: Color -> Material
matte surfaceColor = Material {
  baseColor = surfaceColor,
  ambient = 0.12,
  diffuse = 0.85,
  specular = 0.12,
  shininess = 16,
  reflectivity = 0.02,
  transparency = 0,
  refractiveIndex = 1,
  reflectionRoughness = 0,
  transmissionRoughness = 0
}

glass :: Color -> Material
glass surfaceColor = Material {
  baseColor = surfaceColor,
  ambient = 0.005,
  diffuse = 0.05,
  specular = 1.0,
  shininess = 96,
  reflectivity = 0.04,
  transparency = 0.9,
  refractiveIndex = 1.35,
  reflectionRoughness = 0.08,
  transmissionRoughness = 0.12
}

mirror :: Color -> Material
mirror surfaceColor = Material {
  baseColor = surfaceColor,
  ambient = 0.04,
  diffuse = 0.35,
  specular = 0.95,
  shininess = 128,
  reflectivity = 0.55,
  transparency = 0,
  refractiveIndex = 1,
  reflectionRoughness = 0.18,
  transmissionRoughness = 0
}

skyBackground :: Ray -> Color
skyBackground ray =
  let
    Vec3 _ y _ = unit (direction ray)
    a = 0.5 * (y + 1)
  in (Vec3 0.08 0.1 0.14 *^ (1 - a)) +^ (Vec3 0.55 0.68 0.9 *^ a)

floorTriangles :: Material -> [Object]
floorTriangles mat = [
    TriangleObj (Triangle (Vec3 (-7) (-1.2) (-2)) (Vec3 7 (-1.2) (-2)) (Vec3 7 (-1.2) (-11)) mat),
    TriangleObj (Triangle (Vec3 (-7) (-1.2) (-2)) (Vec3 7 (-1.2) (-11)) (Vec3 (-7) (-1.2) (-11)) mat)
  ]

colorTarget :: Double -> Double -> Double -> [Object]
colorTarget cx cy z =
  let
    colors = [
        Vec3 1 0.75 0.08,
        Vec3 0.15 0.45 1,
        Vec3 0.12 0.8 0.35,
        Vec3 0.95 0.18 0.55,
        Vec3 0.05 0.85 0.9,
        Vec3 1 0.35 0.08
      ]
    columns = 7
    rows = 5
    tileWidth = 3.6 / fromIntegral columns
    tileHeight = 2.8 / fromIntegral rows
    left = cx - 1.8
    bottom = cy - 1.4
    tile x y =
      let
        x0 = left + fromIntegral x * tileWidth
        x1 = x0 + tileWidth
        y0 = bottom + fromIntegral y * tileHeight
        y1 = y0 + tileHeight
        mat = matte (colors !! ((x + y * columns) `mod` length colors))
        bottomLeft = Vec3 x0 y0 z
        bottomRight = Vec3 x1 y0 z
        topRight = Vec3 x1 y1 z
        topLeft = Vec3 x0 y1 z
      in [
        TriangleObj (Triangle bottomLeft bottomRight topRight mat),
        TriangleObj (Triangle bottomLeft topRight topLeft mat)
      ]
  in concat [tile x y | y <- [0 .. rows - 1], x <- [0 .. columns - 1]]

sceneOne :: Scene
sceneOne = Scene {
  background = skyBackground,
  objects =
    floorTriangles (matte (Vec3 0.42 0.48 0.52)) ++
    colorTarget (-1.15) 0.15 (-7.1) ++ [
      SphereObj (Sphere (Vec3 (-1.15) (-0.15) (-5)) 1.05 (glass (Vec3 1 1 1))),
      SphereObj (Sphere (Vec3 1.45 (-0.05) (-4.8)) 1.0 (mirror (Vec3 0.15 0.28 0.95))),
      TriangleObj (Triangle (Vec3 0.25 (-1.2) (-6.3)) (Vec3 0.85 0.65 (-6.5)) (Vec3 1.45 (-1.2) (-6.3)) (matte (Vec3 0.22 0.72 1)))
    ],
  lights = [
    AreaLight (Vec3 (-3) 4 (-2)) (Vec3 3 0 0) (Vec3 0 0 (-3)) (Vec3 1 0.96 0.9)
  ]
}

sceneTwo :: Scene
sceneTwo = Scene {
  background = skyBackground,
  objects =
    floorTriangles (matte (Vec3 0.22 0.24 0.28)) ++
    colorTarget 0 0.2 (-7.0) ++ [
      SphereObj (Sphere (Vec3 0 (-0.1) (-4.25)) 1.1 ((glass (Vec3 1 1 1)) { transmissionRoughness = 0.24 })),
      SphereObj (Sphere (Vec3 2.25 (-0.45) (-5.2)) 0.75 ((mirror (Vec3 0.95 0.95 1)) { reflectionRoughness = 0.28 })),
      TriangleObj (Triangle (Vec3 (-2.25) (-1.2) (-3.15)) (Vec3 (-1.45) 0.05 (-4.05)) (Vec3 (-0.7) (-1.2) (-3.15)) (matte (Vec3 0.95 0.85 0.16)))
    ],
  lights = [
    AreaLight (Vec3 (-1) 3.5 (-1.5)) (Vec3 3 0 0) (Vec3 0 0 (-2.5)) (Vec3 1 0.98 0.94)
  ]
}
