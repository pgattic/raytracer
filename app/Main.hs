module Main where

import Image
import Vec3
import Camera
import Hittables.Hittable
import Hittables.Sphere
import Renderer
import Scene
import Ray
import Interval
import Hittables.Material
import Light

main :: IO ()
main = let
    -- Blue Sky shader
    bgFn ray = let
        (Vec3 _ y _) = unit (direction ray);
        a = 0.5 * (y + 1);
      in ((Vec3 1 1 1) *^ (1-a)) +^ ((Vec3 0.5 0.7 1.0) *^ a);

    rendererConf = RendererConfig {
      rayBounds = (Interval 0.001 100)
    };
    cam = Camera {
      aspectRatio = 16 / 9,
      imageHeight = 720,
      viewportHeight = 2,
      focalLength = 1,
      cameraCenter = Vec3 0 0 0
    };
    scene = Scene {
      background = \_ -> (Vec3 0 0 0), -- Black BG
      objects = [
        SphereObj (Sphere (Vec3 0 0 (-5)) 2.5 (Lambertian (Vec3 1 0 0))),
        SphereObj (Sphere (Vec3 2 2 (-3.5)) 1.5 (Lambertian (Vec3 0 0 1))),
        SphereObj (Sphere (Vec3 0 (-100.5) (-1)) 100 (Lambertian (Vec3 0.3 0.8 0.4)))
      ],
      lights = [
        (PointLight (Vec3 (-3) 15 (-4)) (Vec3 1 1 1)),
        (PointLight (Vec3 (4) 0 (-2)) (Vec3 1 1 1))
      ]
    }
  in do
    writeFile "image.ppm" (toPPM (render rendererConf cam scene))
    putStrLn "image.ppm successfully written"
