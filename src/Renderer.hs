module Renderer(render) where

import Camera
import Scene
import Image
import Vec3
import Ray
import Color
import Hittables.Hittable
import Interval

import Data.List (sortOn)
import Data.Maybe (catMaybes)

see :: Scene -> Ray -> Color
see scene ray = let
    (Vec3 _ y _) = unit (direction ray);
    a = 0.5 * (y + 1);
    hit = hitScene scene ray (Interval 0.01 100);
  in
    case hit of
      Just h -> let (Vec3 nx ny nz) = normal h
        in (Vec3 (nx+1) (ny+1) (nz+1)) *^ 0.5 -- RGB Effect
      Nothing -> ((Vec3 1 1 1) *^ (1-a)) +^ ((backgroundColor scene) *^ a) -- Background shader

render :: Camera -> Scene -> Image
render cam scene = let
    grid = [(x, y) | y <- [0 .. imageHeight cam - 1], x <- [0 .. imageWidth cam - 1]]
    rays = map (\(x, y) -> (xyRay cam) x y) grid;
    colors = map (see scene) rays;
  in Image { width = imageWidth cam, height = imageHeight cam, pixels = colors };

