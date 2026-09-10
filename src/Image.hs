module Image (Image(..), toPPM, exampleImage) where

import Color
import Vec3
import Data.List (intercalate)

data Image = Image {
  width :: Int,
  height :: Int,
  pixels :: [Color]
}

toPPM :: Image -> String
toPPM image =
  "P3\n" ++
  show (width image) ++ " " ++ show (height image) ++
  "\n255\n" ++
  intercalate "\n" (map printColor (pixels image))

exampleImage :: Image
exampleImage = Image {
  width = 3,
  height = 2,
  pixels = [
    (Vec3 1 0 0),
    (Vec3 0 1 0),
    (Vec3 1 0 0),
    (Vec3 1 0 0),
    (Vec3 1 0 0),
    (Vec3 1 0 0)
  ]
}

