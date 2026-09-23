module Renderer(RendererConfig(..), render) where

import Camera
import Scene
import Image
import Vec3
import Ray
import Color
import Hittables.Hittable
import Interval

data RendererConfig = RendererConfig {
  rayBounds :: Interval
}

rayColor :: RendererConfig -> Scene -> Ray -> Color
rayColor (RendererConfig rI) scene ray =
  case hitScene scene ray rI of
    Just h -> let (Vec3 nx ny nz) = normal h
      in (Vec3 (nx+1) (ny+1) (nz+1)) *^ 0.5 -- RGB Effect
    Nothing -> (background scene) ray

render :: RendererConfig -> Camera -> Scene -> Image
render rendConf cam scene = let
    grid = [(x, y) | y <- [0 .. imageHeight cam - 1], x <- [0 .. imageWidth cam - 1]]
    rays = map (\(x, y) -> (xyRay cam) x y) grid;
    colors = map (rayColor rendConf scene) rays;
  in Image { width = imageWidth cam, height = imageHeight cam, pixels = colors };
