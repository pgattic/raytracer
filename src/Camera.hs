module Camera(Camera(..), imageWidth, xyRay, xyRaySample) where

import Point3
import Vec3
import Ray

data Camera = Camera {
  aspectRatio :: Double,
  imageHeight :: Int,
  viewportHeight :: Double,
  focalLength :: Double,
  cameraCenter :: Point3,
  lookAt :: Point3,
  cameraUp :: Vec3
}

imageWidth :: Camera -> Int
imageWidth c = floor((aspectRatio c) * (fromIntegral (imageHeight c)))

viewportWidth :: Camera -> Double
viewportWidth c = (aspectRatio c) * viewportHeight c

viewportU :: Camera -> Vec3
viewportU c = unit (cameraUp c `cross` backward c) *^ viewportWidth c

viewportV :: Camera -> Vec3
viewportV c = unit (backward c `cross` unit (viewportU c)) *^ negate (viewportHeight c)

backward :: Camera -> Vec3
backward c = unit (cameraCenter c -^ lookAt c)

pxDeltaU :: Camera -> Vec3
pxDeltaU c = (viewportU c) /^ (fromIntegral (imageWidth c))

pxDeltaV :: Camera -> Vec3
pxDeltaV c = (viewportV c) /^ (fromIntegral (imageHeight c))

viewportUpperLeft :: Camera -> Point3
viewportUpperLeft c = cameraCenter c -^ (backward c *^ focalLength c) -^ (viewportV c /^ 2) -^ (viewportU c /^ 2)

px2ray :: Camera -> Point3 -> Ray
px2ray c px =
  let center = cameraCenter c
  in Ray { origin = center, direction = px -^ center }

xyPixelSample :: Camera -> Int -> Int -> Double -> Double -> Point3
xyPixelSample c xi yi xOffset yOffset =
  let
    x = fromIntegral xi + xOffset;
    y = fromIntegral yi + yOffset
  in (viewportUpperLeft c) +^ (((pxDeltaU c) *^ x) +^ ((pxDeltaV c) *^ y))

xyPixel :: Camera -> Int -> Int -> Point3
xyPixel c xi yi = xyPixelSample c xi yi 0.5 0.5

xyRaySample :: Camera -> Int -> Int -> Double -> Double -> Ray
xyRaySample c x y xOffset yOffset = px2ray c (xyPixelSample c x y xOffset yOffset)

xyRay :: Camera -> Int -> Int -> Ray
xyRay c x y = px2ray c (xyPixel c x y)
