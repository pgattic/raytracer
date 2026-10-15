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
  samplesPerPixel :: Int,
  shadowSamples :: Int,
  renderSeed :: Seed
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

pointContribution :: Scene -> Ray -> HitRecord -> Light -> Color
pointContribution scene ray rec light =
  let
    (surfaceToLight, shadowRange, lightColor) =
      case light of
        PointLight lightPosition col ->
          let
            toLight = lightPosition -^ point rec
          in (unit toLight, Interval rayEpsilon (max 0 (mag toLight - rayEpsilon)), col)
        DirectionalLight lightRayDirection col ->
          (unit (lightRayDirection *^ (-1)), Interval rayEpsilon infinity, col)
        AreaLight p _ _ col ->
          (unit (p -^ point rec), Interval rayEpsilon (mag (p -^ point rec)), col)
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

lightContribution :: RendererConfig -> Seed -> Scene -> Ray -> HitRecord -> Light -> Color
lightContribution config seed scene ray rec light =
  case light of
    AreaLight p u v col ->
      averageColor [pointContribution scene ray rec (PointLight (p +^ u *^ x +^ v *^ y) col)
                   | (x,y) <- sampleOffsets seed (shadowSamples config)]
    _ -> pointContribution scene ray rec light

shade :: RendererConfig -> Seed -> Int -> Scene -> Ray -> HitRecord -> Color
shade rendConf seed depth scene ray rec =
  let
    mat = material rec
    lightColors = zipWith (\i light -> lightContribution rendConf (deriveSeed seed i) scene ray rec light) [10..] (lights scene)
    directLight = foldl' (+^) (ambientContribution mat) lightColors
    unitDirection = unit (direction ray)
    reflectedColor =
      if depth <= 0 || reflectivity mat <= 0
        then Vec3 0 0 0
        else
          let
            reflectedRay = spawnRay (point rec) (normal rec) (jitterDirection (deriveSeed seed 1) (reflectionRoughness mat) (reflect unitDirection (normal rec)) (normal rec))
          in rayColorWithDepth rendConf (deriveSeed seed 2) (depth - 1) scene reflectedRay
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
              Nothing -> jitterDirection (deriveSeed seed 3) (reflectionRoughness mat) fallbackDirection (normal rec)
              Just dir -> jitterDirection (deriveSeed seed 3) (transmissionRoughness mat) dir (normal rec *^ (-1))
            refractedRay = spawnRay (point rec) (normal rec) outgoing
          in rayColorWithDepth rendConf (deriveSeed seed 4) (depth - 1) scene refractedRay
    localWeight = max 0 (1 - reflectivity mat - transparency mat)
  in directLight *^ localWeight
    +^ reflectedColor *^ reflectivity mat
    +^ multiplyColor (baseColor mat) refractedColor *^ transparency mat

rayColorWithDepth :: RendererConfig -> Seed -> Int -> Scene -> Ray -> Color
rayColorWithDepth rendConf seed depth scene ray =
  case hitScene scene ray rI of
    Just h -> shade rendConf seed depth scene ray h
    Nothing -> background scene ray
  where
    rI = Interval rayEpsilon infinity

rayColor :: RendererConfig -> Seed -> Scene -> Ray -> Color
rayColor rendConf seed scene ray =
  case hitScene scene ray (rayBounds rendConf) of
    Nothing -> background scene ray
    Just rec -> shade rendConf seed (maxDepth rendConf) scene ray rec

averageColor :: [Color] -> Color
averageColor colors =
  foldl' (+^) (Vec3 0 0 0) colors /^ fromIntegral (length colors)

pixelColor :: RendererConfig -> Camera -> Scene -> Int -> Int -> Color
pixelColor rendConf cam scene x y =
  let
    seed = deriveSeed (deriveSeed (renderSeed rendConf) (fromIntegral x)) (fromIntegral y)
    offsets = sampleOffsets seed (samplesPerPixel rendConf)
    rays = map (\(xOffset, yOffset) -> xyRaySample cam x y xOffset yOffset) offsets
  in averageColor (zipWith (\i ray -> rayColor rendConf (deriveSeed seed (i+100)) scene ray) [0..] rays)

render :: RendererConfig -> Camera -> Scene -> Image
render rendConf cam originalScene = let
    scene = accelerateScene originalScene
    grid = [(x, y) | y <- [0 .. imageHeight cam - 1], x <- [0 .. imageWidth cam - 1]]
    colors = map (\(x, y) -> pixelColor rendConf cam scene x y) grid;
  in Image { width = imageWidth cam, height = imageHeight cam, pixels = colors };
