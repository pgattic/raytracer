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
  rayBounds :: Interval,
  maxDepth :: Int
}

multiplyColor :: Color -> Color -> Color
multiplyColor (Vec3 r0 g0 b0) (Vec3 r1 g1 b1) = Vec3 (r0 * r1) (g0 * g1) (b0 * b1)

reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v -^ (n *^ (2 * (v .^ n)))

ambientContribution :: Material -> Color
ambientContribution mat = baseColor mat *^ ambient mat

lightContribution :: Scene -> Ray -> HitRecord -> Light -> Color
lightContribution scene ray rec light =
  let
    (surfaceToLight, shadowRange, lightColor) =
      case light of
        PointLight lightPosition col ->
          let
            toLight = lightPosition -^ point rec
          in (unit toLight, Interval 0.001 (mag toLight), col)
        DirectionalLight lightRayDirection col ->
          (unit (lightRayDirection *^ (-1)), Interval 0.001 (1 / 0), col)
    diffuseStrength = diffuse mat * max 0 (normal rec .^ surfaceToLight)
    viewDirection = unit (direction ray *^ (-1))
    reflectedLight = reflect (surfaceToLight *^ (-1)) (normal rec)
    specularStrength = specular mat * ((max 0 (viewDirection .^ reflectedLight)) ** shininess mat)
    shadowRay = Ray { origin = point rec, direction = surfaceToLight }
    mat = material rec
    visible = case hitScene scene shadowRay shadowRange of
      Nothing -> True
      Just _ -> False
  in
    if visible
      then multiplyColor (baseColor mat) lightColor *^ diffuseStrength
        +^ lightColor *^ specularStrength
      else Vec3 0 0 0

shade :: RendererConfig -> Int -> Scene -> Ray -> HitRecord -> Color
shade rendConf depth scene ray rec =
  let
    mat = material rec
    lightColors = map (lightContribution scene ray rec) (lights scene)
    directLight = foldl (+^) (ambientContribution mat) lightColors
    reflectedColor =
      if depth <= 0 || reflectivity mat <= 0
        then Vec3 0 0 0
        else
          let
            reflectedRay = Ray {
              origin = point rec,
              direction = reflect (unit (direction ray)) (normal rec)
            }
          in rayColorWithDepth rendConf (depth - 1) scene reflectedRay
  in directLight *^ (1 - reflectivity mat) +^ reflectedColor *^ reflectivity mat

rayColorWithDepth :: RendererConfig -> Int -> Scene -> Ray -> Color
rayColorWithDepth rendConf depth scene ray =
  case hitScene scene ray rI of
    Just h -> shade rendConf depth scene ray h
    Nothing -> background scene ray
  where
    rI = rayBounds rendConf

rayColor :: RendererConfig -> Scene -> Ray -> Color
rayColor rendConf = rayColorWithDepth rendConf (maxDepth rendConf)

render :: RendererConfig -> Camera -> Scene -> Image
render rendConf cam scene = let
    grid = [(x, y) | y <- [0 .. imageHeight cam - 1], x <- [0 .. imageWidth cam - 1]]
    rays = map (\(x, y) -> (xyRay cam) x y) grid;
    colors = map (rayColor rendConf scene) rays;
  in Image { width = imageWidth cam, height = imageHeight cam, pixels = colors };
