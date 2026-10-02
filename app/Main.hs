module Main where

import Image
import Vec3
import Camera
import Objects.Object
import Objects.Sphere
import Renderer
import Scene
import Interval
import Objects.Material
import Light

main :: IO ()
main = let
    rendererConf = RendererConfig {
      rayBounds = (Interval 0.001 100),
      maxDepth = 3
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
        SphereObj (Sphere (Vec3 0 0 (-5)) 2.5 Material {
          baseColor = Vec3 1 0 0,
          ambient = 0.1,
          diffuse = 0.7,
          specular = 0.4,
          shininess = 32,
          reflectivity = 0.15
        }),
        SphereObj (Sphere (Vec3 2 2 (-3.5)) 1.5 Material {
          baseColor = Vec3 0 0 1,
          ambient = 0.1,
          diffuse = 0.7,
          specular = 0.7,
          shininess = 64,
          reflectivity = 0.25
        }),
        SphereObj (Sphere (Vec3 0 (-100.5) (-1)) 100 Material {
          baseColor = Vec3 0.3 0.6 0.2,
          ambient = 0.1,
          diffuse = 0.8,
          specular = 0.1,
          shininess = 16,
          reflectivity = 0.05
        })
      ],
      lights = [
        (DirectionalLight (Vec3 0 (-1) 0) (Vec3 1 1 1))
      ]
    }
  in do
    writeFile "image.ppm" (toPPM (render rendererConf cam scene))
    putStrLn "image.ppm successfully written"
