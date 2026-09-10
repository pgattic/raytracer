module Main (main) where

import Image (Image(..), toPPM)
import Vec3 (Vec3(..), unit, mag_squared, (*^), (+^), (-^), (.^))
import Camera
import Ray
import Color (Color)

hitSphere :: Vec3 -> Double -> Ray -> Double
hitSphere center rad ray = let
    oc = center -^ origin ray;
    a = mag_squared (direction ray)
    h = (direction ray .^ oc);
    c = (mag_squared oc) - (rad * rad);
    discriminant = h*h - a*c
  in
    if discriminant < 0
      then (-1)
      else (h - (sqrt discriminant)) / a

rayColor :: Ray -> Color
rayColor r = let
    (Vec3 _ y _) = unit (direction r);
    a = 0.5 * (y + 1)
    t = hitSphere (Vec3 0 0 (-1)) 0.5 r
  in
    if t > 0
      then let (Vec3 nx ny nz) = unit ((at r t) -^ (Vec3 0 0 (-1)))
        in (Vec3 (nx+1) (ny+1) (nz+1)) *^ 0.5
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

