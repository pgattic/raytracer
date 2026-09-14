module Main where

import Image (Image(..), toPPM)
import Vec3 (Vec3(..), unit, (*^), (+^))
import Camera
import Ray
import Color (Color)
import Hittables.Hittable (Hittable(..), hit, HitRecord(..))
import Hittables.Sphere (Sphere(..))
import Interval

import Data.List (sortOn)
import Data.Maybe (catMaybes)

see :: [Hittable] -> Ray -> Color
see objects ray = let
    (Vec3 _ y _) = unit (direction ray);
    a = 0.5 * (y + 1);
    hits = sortOn t (catMaybes (map (hit (Interval 0.01 100) ray) objects));
  in
    case hits of
      (h : _ ) -> let (Vec3 nx ny nz) = normal h
        in (Vec3 (nx+1) (ny+1) (nz+1)) *^ 0.5
      _ -> ((Vec3 1 1 1) *^ (1-a)) +^ ((Vec3 0.5 0.7 1.0) *^ a)

main :: IO ()
main = let
    cam = Camera {
      aspectRatio = 16 / 9,
      imageHeight = 360,
      viewportHeight = 2,
      focalLength = 1,
      cameraCenter = Vec3 0 0 0
    };
    spheres = [
        SphereObj Sphere { center = (Vec3 0 0 (-10)), radius = 5 },
        SphereObj Sphere { center = (Vec3 4 4 (-7)), radius = 3 },
        SphereObj Sphere { center = (Vec3 0 (-100.5) (-1)), radius = 100 }
      ]
    grid = [(x, y) | y <- [0 .. imageHeight cam - 1], x <- [0 .. imageWidth cam - 1]]
    rays = map (\(x, y) -> (xyRay cam) x y) grid;
    colors = map (see spheres) rays;
    image = Image { width = imageWidth cam, height = imageHeight cam, pixels = colors };
  in do
    writeFile "image.ppm" (toPPM image)
    putStrLn "image.ppm successfully written"
