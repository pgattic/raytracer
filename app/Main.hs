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
    -- Background shader
    bgFn ray = let
        (Vec3 _ y _) = unit (direction ray);
        a = 0.5 * (y + 1);
      in ((Vec3 1 1 1) *^ (1-a)) +^ ((Vec3 0.5 0.7 1.0) *^ a);

    rendererConf = RendererConfig {
      rayBounds = (Interval 0.001 100)
    };
    cam = Camera {
      aspectRatio = 16 / 9,
      imageHeight = 360,
      viewportHeight = 2,
      focalLength = 1,
      cameraCenter = Vec3 0 0 0
    };
    scene = Scene {
      background = bgFn,
      objects = [
        SphereObj (Sphere (Vec3 0 0 (-10)) 5 (Lambertian (Vec3 1 0 0))),
        SphereObj (Sphere (Vec3 4 4 (-7)) 3 (Lambertian (Vec3 1 0 0))),
        SphereObj (Sphere (Vec3 0 (-100.5) (-1)) 100 (Lambertian (Vec3 1 0 0)))
      ],
      lights = [
        (PointLight (Vec3 0 20 (-8)) (Vec3 1 1 1))
      ]
    }
  in do
    writeFile "image.ppm" (toPPM (render rendererConf cam scene))
    putStrLn "image.ppm successfully written"
