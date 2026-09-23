module Renderer(RendererConfig(..), render) where

import Camera
import Scene
import Image
import Vec3
import Ray
import Color
import Hittables.Hittable
import Hittables.Material
import Interval
import Light

data RendererConfig = RendererConfig {
  rayBounds :: Interval
}

multiplyColor :: Color -> Color -> Color
multiplyColor (Vec3 r0 g0 b0) (Vec3 r1 g1 b1) = Vec3 (r0 * r1) (g0 * g1) (b0 * b1)

lightContribution :: Scene -> HitRecord -> Light -> Color
lightContribution scene rec light =
  let
    toLight = position light -^ point rec
    lightDistance = mag toLight
    lightDirection = unit toLight
    brightness = max 0 (normal rec .^ lightDirection)
    shadowRay = Ray { origin = point rec, direction = lightDirection }
    visible = case hitScene scene shadowRay (Interval 0.001 lightDistance) of
      Nothing -> True
      Just _ -> False
  in
    if visible
      then color light *^ brightness
      else Vec3 0 0 0

shade :: Scene -> HitRecord -> Color
shade scene rec =
  case material rec of
    Lambertian albedo ->
      let
        lightColors = map (lightContribution scene rec) (lights scene)
        totalLight = foldl (+^) (Vec3 0 0 0) lightColors
      in multiplyColor albedo totalLight

rayColor :: RendererConfig -> Scene -> Ray -> Color
rayColor (RendererConfig rI) scene ray =
  case hitScene scene ray rI of
    Just h -> shade scene h
    Nothing -> background scene ray

render :: RendererConfig -> Camera -> Scene -> Image
render rendConf cam scene = let
    grid = [(x, y) | y <- [0 .. imageHeight cam - 1], x <- [0 .. imageWidth cam - 1]]
    rays = map (\(x, y) -> (xyRay cam) x y) grid;
    colors = map (rayColor rendConf scene) rays;
  in Image { width = imageWidth cam, height = imageHeight cam, pixels = colors };
