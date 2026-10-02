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

data RendererConfig = RendererConfig {
  rayBounds :: Interval,
  maxDepth :: Int,
  samplesPerPixel :: Int
}

reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v -^ (n *^ (2 * (v .^ n)))

refract :: Vec3 -> Vec3 -> Double -> Maybe Vec3
refract uv n refractionRatio =
  let
    cosTheta = min ((uv *^ (-1)) .^ n) 1
    rayOutPerpendicular = (uv +^ (n *^ cosTheta)) *^ refractionRatio
    perpendicularLengthSquared = mag_squared rayOutPerpendicular
  in
    if perpendicularLengthSquared > 1
      then Nothing
      else
        let rayOutParallel = n *^ (negate (sqrt (abs (1 - perpendicularLengthSquared))))
        in Just (rayOutPerpendicular +^ rayOutParallel)

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
          (unit (lightRayDirection *^ (-1)), Interval 0.001 infinity, col)
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
    unitDirection = unit (direction ray)
    reflectedColor =
      if depth <= 0 || reflectivity mat <= 0
        then Vec3 0 0 0
        else
          let
            reflectedRay = Ray {
              origin = point rec,
              direction = reflect unitDirection (normal rec)
            }
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
            refractedRay = Ray {
              origin = point rec,
              direction = case refractedDirection of
                Nothing -> fallbackDirection
                Just dir -> dir
            }
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
    rI = rayBounds rendConf

rayColor :: RendererConfig -> Scene -> Ray -> Color
rayColor rendConf = rayColorWithDepth rendConf (maxDepth rendConf)

sampleOffsets :: Int -> [(Double, Double)]
sampleOffsets sampleCount =
  let
    safeSampleCount = max 1 sampleCount
    samplesPerAxis :: Int
    samplesPerAxis = ceiling (sqrt (fromIntegral safeSampleCount :: Double))
    axisSamples :: [Int]
    axisSamples = [0 .. samplesPerAxis - 1]
    centeredOffset :: Int -> Double
    centeredOffset sampleIndex =
      (fromIntegral sampleIndex + 0.5) / fromIntegral samplesPerAxis
  in take safeSampleCount [
    (centeredOffset x, centeredOffset y) |
    y <- axisSamples,
    x <- axisSamples
  ]

averageColor :: [Color] -> Color
averageColor colors =
  foldl (+^) (Vec3 0 0 0) colors /^ fromIntegral (length colors)

pixelColor :: RendererConfig -> Camera -> Scene -> Int -> Int -> Color
pixelColor rendConf cam scene x y =
  let
    offsets = sampleOffsets (samplesPerPixel rendConf)
    rays = map (\(xOffset, yOffset) -> xyRaySample cam x y xOffset yOffset) offsets
  in averageColor (map (rayColor rendConf scene) rays)

render :: RendererConfig -> Camera -> Scene -> Image
render rendConf cam scene = let
    grid = [(x, y) | y <- [0 .. imageHeight cam - 1], x <- [0 .. imageWidth cam - 1]]
    colors = map (\(x, y) -> pixelColor rendConf cam scene x y) grid;
  in Image { width = imageWidth cam, height = imageHeight cam, pixels = colors };
