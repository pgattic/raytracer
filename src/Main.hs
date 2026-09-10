module Main (main) where

import Image (toPPM, exampleImage)
import Vec3(Vec3(..))
import Camera

main :: IO ()
main = let
    cam = Camera {
      aspectRatio = 16 / 9,
      imageHeight = 360,
      viewportHeight = 2,
      focalLength = 1,
      cameraCenter = Vec3 0 0 0
    }
  in do
  putStrLn (toPPM exampleImage)
  putStrLn (show (imageHeight cam))
  putStrLn (show (imageWidth cam))

