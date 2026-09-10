module Main (main) where

import Image (Image(..), toPPM)
import Vec3(Vec3(..), unit, (*^), (+^), (-^), (.^))
import Camera
import Ray
import Color (Color)

hitSphere :: Vec3 -> Double -> Ray -> Bool
hitSphere center rad ray = let
    oc = center -^ origin ray;
    a = (direction ray) .^ (direction ray);
    b = (direction ray .^ oc) * (-2.0);
    c = (oc .^ oc) - (rad * rad)
    discriminant = b*b - (4 * a * c)
  in
    discriminant >= 0

rayColor :: Ray -> Color
rayColor r = let
    (Vec3 _ y _) = unit (direction r);
    a = 0.5 * (y + 1)
  in
    if hitSphere (Vec3 0 0 (-1)) 0.5 r
      then Vec3 1 0 0
      else ((Vec3 1 1 1) *^ (1-a)) +^ ((Vec3 0.5 0.7 1.0) *^ a)

main :: IO ()
main = let
    cam = Camera {
      aspectRatio = 16 / 9,
      imageHeight = 360,
      viewportHeight = 2,
      focalLength = 1,
      cameraCenter = Vec3 0 0 0
    };
    grid = [(x, y) | y <- [0 .. imageHeight cam - 1], x <- [0 .. imageWidth cam - 1]]
    rays = map (\(x, y) -> (xyRay cam) x y) grid;
    colors = map rayColor rays;
    image = Image { width = imageWidth cam, height = imageHeight cam, pixels = colors };
  in do
  putStrLn (toPPM image)

