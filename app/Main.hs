module Main where

import Image
import Vec3
import Camera
import Hittables.Hittable
import Hittables.Sphere
import Renderer
import Scene

main :: IO ()
main = let
    cam = Camera {
      aspectRatio = 16 / 9,
      imageHeight = 360,
      viewportHeight = 2,
      focalLength = 1,
      cameraCenter = Vec3 0 0 0
    };
    scene = Scene {
      backgroundColor = (Vec3 0.5 0.7 1.0),
      objects = [
        SphereObj Sphere { center = (Vec3 0 0 (-10)), radius = 5 },
        SphereObj Sphere { center = (Vec3 4 4 (-7)), radius = 3 },
        SphereObj Sphere { center = (Vec3 0 (-100.5) (-1)), radius = 100 }
      ]
    }
  in do
    writeFile "image.ppm" (toPPM (render cam scene))
    putStrLn "image.ppm successfully written"
