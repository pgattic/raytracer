module Renderer(RendererConfig(..), render) where

import Camera
import Scene
import Image
import Vec3
import Ray
import Color
import Objects.Object
import Objects.Material
import Interval
import Light
import Optics
import Sampling

data RendererConfig = RendererConfig {
  rayBounds :: Interval,
  maxDepth :: Int,
  samplesPerPixel :: Int
}

ambientContribution :: Material -> Color
ambientContribution mat = baseColor mat *^ ambient mat

-- Straight shadow transmission is an approximation; it does not model caustics.
shadowTransmission :: Scene -> Ray -> Interval -> Color
shadowTransmission scene ray bounds =
  case hitScene scene ray bounds of
    Nothing -> Vec3 1 1 1
    Just rec ->
      let mat = material rec
          amount = max 0 (min 1 (transparency mat))
          rest = Interval (t rec + rayEpsilon) (maxT bounds)
      in if amount == 0 then Vec3 0 0 0
         else multiplyColor (baseColor mat *^ amount) (shadowTransmission scene ray rest)

lightContribution :: Scene -> Ray -> HitRecord -> Light -> Color
lightContribution scene ray rec light =
  let
    (surfaceToLight, shadowRange, lightColor) =
      case light of
        PointLight lightPosition col ->
          let
            toLight = lightPosition -^ point rec
          in (unit toLight, Interval rayEpsilon (max 0 (mag toLight - rayEpsilon)), col)
        DirectionalLight lightRayDirection col ->
          (unit (lightRayDirection *^ (-1)), Interval rayEpsilon infinity, col)
    diffuseStrength = diffuse mat * max 0 (normal rec .^ surfaceToLight)
    viewDirection = unit (direction ray *^ (-1))
    reflectedLight = reflect (surfaceToLight *^ (-1)) (normal rec)
    specularStrength = specular mat * ((max 0 (viewDirection .^ reflectedLight)) ** shininess mat)
    shadowRay = spawnRay (point rec) (normal rec) surfaceToLight
    mat = material rec
    transmission = shadowTransmission scene shadowRay shadowRange
  in
    if normal rec .^ surfaceToLight > 0
      then multiplyColor transmission (multiplyColor (baseColor mat) lightColor *^ diffuseStrength
        +^ lightColor *^ specularStrength)
      else Vec3 0 0 0

shade :: RendererConfig -> Int -> Scene -> Ray -> HitRecord -> Color
shade rendConf depth scene ray rec =
  let
    mat = material rec
    lightColors = map (lightContribution scene ray rec) (lights scene)
    directLight = foldl' (+^) (ambientContribution mat) lightColors
    unitDirection = unit (direction ray)
    reflectedColor =
      if depth <= 0 || reflectivity mat <= 0
        then Vec3 0 0 0
        else
          let
            reflectedRay = spawnRay (point rec) (normal rec) (reflect unitDirection (normal rec))
          in rayColorWithDepth rendConf (depth - 1) scene reflectedRay
    refractedColor =
      if depth <= 0 || transparency mat <= 0
        then Vec3 0 0 0
        else
          let
            refractionRatio =
              if frontFace rec
                then 1 / refractiveIndex mat
                else refractiveIndex mat
            refractedDirection = refract unitDirection (normal rec) refractionRatio
            fallbackDirection = reflect unitDirection (normal rec)
            outgoing = case refractedDirection of
              Nothing -> fallbackDirection
              Just dir -> dir
            refractedRay = spawnRay (point rec) (normal rec) outgoing
          in rayColorWithDepth rendConf (depth - 1) scene refractedRay
    localWeight = max 0 (1 - reflectivity mat - transparency mat)
  in directLight *^ localWeight
    +^ reflectedColor *^ reflectivity mat
    +^ multiplyColor (baseColor mat) refractedColor *^ transparency mat

rayColorWithDepth :: RendererConfig -> Int -> Scene -> Ray -> Color
rayColorWithDepth rendConf depth scene ray =
  case hitScene scene ray rI of
    Just h -> shade rendConf depth scene ray h
    Nothing -> background scene ray
  where
    rI = Interval rayEpsilon infinity

rayColor :: RendererConfig -> Scene -> Ray -> Color
rayColor rendConf scene ray =
  case hitScene scene ray (rayBounds rendConf) of
    Nothing -> background scene ray
    Just rec -> shade rendConf (maxDepth rendConf) scene ray rec

averageColor :: [Color] -> Color
averageColor colors =
  foldl' (+^) (Vec3 0 0 0) colors /^ fromIntegral (length colors)

pixelColor :: RendererConfig -> Camera -> Scene -> Int -> Int -> Color
pixelColor rendConf cam scene x y =
  let
    offsets = sampleOffsets (samplesPerPixel rendConf)
    rays = map (\(xOffset, yOffset) -> xyRaySample cam x y xOffset yOffset) offsets
  in averageColor (map (rayColor rendConf scene) rays)

render :: RendererConfig -> Camera -> Scene -> Image
render rendConf cam originalScene = let
    scene = accelerateScene originalScene
    grid = [(x, y) | y <- [0 .. imageHeight cam - 1], x <- [0 .. imageWidth cam - 1]]
    colors = map (\(x, y) -> pixelColor rendConf cam scene x y) grid;
  in Image { width = imageWidth cam, height = imageHeight cam, pixels = colors };
