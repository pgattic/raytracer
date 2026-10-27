module Scenes (sceneOne, sceneTwo, cameraOne, cameraTwo,
               pathSceneOne, pathSceneTwo, pathCameraOne, pathCameraTwo) where

import Camera
import Color
import Vec3
import Objects.Object
import Objects.Sphere
import Objects.Triangle
import Scene
import Ray
import Objects.Material
import Light
import Sampling (random01)

matte :: Color -> Material
matte surfaceColor = Material surfaceColor 0.16 0.75 0.08 24 0 0 1 0 0

glass :: Double -> Material
glass roughness = Material (Vec3 1 1 1) 0 0.02 1 128 0.07 0.92 1.5 0 roughness

metal :: Color -> Double -> Material
metal surfaceColor roughness = Material surfaceColor 0.04 0.18 0.8 100 0.88 0 1 roughness 0

ball :: Double -> Double -> Double -> Material -> Object
ball x z r mat = SphereObj (Sphere (Vec3 x r z) r mat)

floorTriangles :: Color -> [Object]
floorTriangles surfaceColor =
  let a = Vec3 (-100) 0 (-100)
      b = Vec3 100 0 (-100)
      c = Vec3 100 0 100
      d = Vec3 (-100) 0 100
      mat = matte surfaceColor
  in [TriangleObj (Triangle a c b mat), TriangleObj (Triangle a d c mat)]

palette :: [Color]
palette = [
    Vec3 0.72 0.08 0.12, Vec3 0.08 0.42 0.67, Vec3 0.12 0.62 0.38,
    Vec3 0.65 0.15 0.64, Vec3 0.9 0.48 0.12, Vec3 0.55 0.67 0.17,
    Vec3 0.35 0.22 0.7, Vec3 0.12 0.7 0.72, Vec3 0.82 0.4 0.5,
    Vec3 0.7 0.74 0.85, Vec3 0.22 0.3 0.45, Vec3 0.42 0.2 0.12
  ]

paletteAt :: Int -> Color
paletteAt index = case drop (mod index (length palette)) palette of
  col:_ -> col
  [] -> Vec3 1 1 1

sky :: Ray -> Color
sky ray =
  let Vec3 _ y _ = unit (direction ray)
      blend = max 0 (min 1 (0.5 * (y+1)))
  in Vec3 0.7 0.79 0.9 *^ (1-blend) +^ Vec3 0.94 0.97 1 *^ blend

-- Horizontal clearance also prevents the small spheres intersecting the heroes.
clearOf :: [Object] -> Double -> Double -> Double -> Bool
clearOf heroes x z r = all separated heroes
  where
    separated (SphereObj (Sphere (Vec3 hx _ hz) hr _)) =
      let dx = x-hx; dz = z-hz; gap = hr+r+0.12
      in dx*dx + dz*dz > gap*gap
    separated _ = True

gardenHeroes :: [Object]
gardenHeroes = [
    ball 1.4 0.6 1.25 (glass 0),
    ball (-1.8) 0 1.25 (metal (Vec3 0.95 0.97 1) 0),
    ball (-6.5) (-3.5) 1.25 (matte (Vec3 0.42 0.24 0.12)),
    ball (-2.8) 3.2 0.48 (glass 0.1),
    ball 3.7 1.8 0.5 (metal (Vec3 0.78 0.86 0.98) 0.1)
  ]

gardenSpheres :: [Object]
gardenSpheres = [
    ball x z r (matte (paletteAt (floor (random01 seed 3 * 12000)))) |
    row <- [-9..7 :: Int], column <- [-8..8 :: Int],
    let seed = fromIntegral ((row+10)*100 + column+10),
    let x = fromIntegral column + 0.4 * (random01 seed 0 - 0.5),
    let z = fromIntegral row + 0.4 * (random01 seed 1 - 0.5),
    let r = 0.16 + 0.1 * random01 seed 2,
    clearOf gardenHeroes x z r
  ]

sceneOne :: Scene
sceneOne = Scene {
  background = sky,
  objects = floorTriangles (Vec3 0.48 0.51 0.55) ++ gardenSpheres ++ gardenHeroes,
  lights = [
    AreaLight (Vec3 (-4) 8 4) (Vec3 5 0 0) (Vec3 0 0 (-4)) (Vec3 0.9 0.88 0.84),
    DirectionalLight (Vec3 (-1) (-1) (-0.5)) (Vec3 0.1 0.13 0.18)
  ]
}

galleryHeroes :: [Object]
galleryHeroes = [
    ball (-1.45) 1 1 (glass 0),
    ball 1.45 1 1 (glass 0.09),
    ball (-3.7) (-2.6) 1 (metal (Vec3 0.95 0.97 1) 0),
    ball 3.7 (-2.6) 1 (metal (Vec3 0.95 0.97 1) 0.13)
  ]

galleryBackdrop :: [Object]
galleryBackdrop = [
    SphereObj (Sphere (Vec3 x (0.24 + fromIntegral row*0.48) (-1.5)) 0.24 (matte (paletteAt (column+row*3)))) |
    (column,x) <- zip [0..] [-2.05,-1.45,-0.85,0.85,1.45,2.05],
    row <- [0..3 :: Int]
  ]

galleryBorder :: [Object]
galleryBorder = [
    ball x z 0.22 (matte (paletteAt (column+row*5))) |
    row <- [-4..3 :: Int], column <- [-5..5 :: Int],
    let x = fromIntegral column * 1.1,
    let z = fromIntegral row * 1.2,
    abs x > 3 || z < -3,
    clearOf galleryHeroes x z 0.22
  ]

sceneTwo :: Scene
sceneTwo = Scene {
  background = sky,
  objects = floorTriangles (Vec3 0.55 0.57 0.61) ++ galleryBackdrop ++ galleryBorder ++ galleryHeroes,
  lights = [
    AreaLight (Vec3 (-4) 7 4) (Vec3 5 0 0) (Vec3 0 0 (-4)) (Vec3 0.95 0.93 0.9),
    DirectionalLight (Vec3 1 (-1) (-0.2)) (Vec3 0.08 0.1 0.14)
  ]
}

cameraOne :: Int -> Camera
cameraOne h = Camera (16/9) h 0.38 1 (Vec3 8 2.2 12) (Vec3 0 0.95 0) (Vec3 0 1 0)

cameraTwo :: Int -> Camera
cameraTwo h = Camera (16/9) h 0.44 1 (Vec3 0 3.6 13) (Vec3 0 0.9 0) (Vec3 0 1 0)

-- These scenes are lit exclusively by the environment in path mode.
pathSceneOne :: Scene
pathSceneOne = sceneOne { lights = [] }

pathMatte :: Color -> Material
pathMatte col = (matte col) { ambient = 0, diffuse = 1, specular = 0 }

quad :: Vec3 -> Vec3 -> Vec3 -> Vec3 -> Material -> [Object]
quad a b c d mat =
  [TriangleObj (Triangle a b c mat), TriangleObj (Triangle a c d mat)]

roofPanel :: Double -> Double -> Double -> Double -> Material -> [Object]
roofPanel x0 x1 z0 z1 =
  quad (Vec3 x0 4 z0) (Vec3 x1 4 z0) (Vec3 x1 4 z1) (Vec3 x0 4 z1)

pathSceneTwo :: Scene
pathSceneTwo =
  let neutral = pathMatte (Vec3 0.74 0.74 0.72)
      red = pathMatte (Vec3 0.75 0.08 0.06)
      green = pathMatte (Vec3 0.06 0.6 0.3)
      floorObjects = quad (Vec3 (-3) 0 2) (Vec3 3 0 2)
                          (Vec3 3 0 (-4)) (Vec3 (-3) 0 (-4)) neutral
      back = quad (Vec3 (-3) 0 (-4)) (Vec3 3 0 (-4))
                  (Vec3 3 4 (-4)) (Vec3 (-3) 4 (-4)) neutral
      left = quad (Vec3 (-3) 0 2) (Vec3 (-3) 0 (-4))
                  (Vec3 (-3) 4 (-4)) (Vec3 (-3) 4 2) red
      right = quad (Vec3 3 0 (-4)) (Vec3 3 0 2)
                   (Vec3 3 4 2) (Vec3 3 4 (-4)) green
      -- A large opening in the roof admits sky light deep into the room.
      roofObjects = concat [
          roofPanel (-3) (-1.6) (-4) 2 neutral,
          roofPanel 1.6 3 (-4) 2 neutral,
          roofPanel (-1.6) 1.6 (-4) (-2.8) neutral,
          roofPanel (-1.6) 1.6 0.6 2 neutral
        ]
  in Scene {
    background = \ray -> sky ray *^ 1.2,
    objects = floorObjects ++ back ++ left ++ right ++ roofObjects ++ [
      ball (-1.65) (-1.2) 0.95 (pathMatte (Vec3 0.82 0.82 0.82)),
      ball 1.25 (-1.4) 0.85 (glass 0),
      ball 0.2 0.8 0.45 (pathMatte (Vec3 0.12 0.22 0.7))
    ],
    lights = []
  }

pathCameraOne :: Int -> Camera
pathCameraOne = cameraOne

pathCameraTwo :: Int -> Camera
pathCameraTwo h = Camera (16/9) h 0.5 1 (Vec3 0 2.05 7.5) (Vec3 0 1.5 (-1.1)) (Vec3 0 1 0)
