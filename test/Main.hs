module Main where

import Control.Monad (unless, forM_)
import Bounds
import Camera
import Color
import Image
import Interval
import Optics
import Objects.HitRecord (t, normal, frontFace)
import Objects.Material
import Objects.Object (Object(..))
import Objects.Sphere (Sphere(..), hitSphere)
import Objects.Triangle (Triangle(..), hitTriangle)
import Ray
import Renderer
import Sampling
import Scene
import Light (Light(PointLight, AreaLight))
import Vec3

assert :: String -> Bool -> IO ()
assert label ok = unless ok (fail label)

near :: Double -> Double -> Bool
near a b = abs (a-b) < 1e-8

nearVec :: Vec3 -> Vec3 -> Bool
nearVec a b = mag (a -^ b) < 1e-7

opaque :: Material
opaque = Material (Vec3 1 1 1) 0.1 0.8 0.2 32 0 0 1 0 0

main :: IO ()
main = do
  let bounds = Interval 0.00001 infinity
      sphere = Sphere (Vec3 0 0 (-3)) 1 opaque
      forward = Ray (Vec3 0 0 0) (Vec3 0 0 (-1))
      triangle = Triangle (Vec3 (-1) (-1) (-2)) (Vec3 1 (-1) (-2)) (Vec3 0 1 (-2)) opaque
  assert "sphere entry" (maybe False (near 2 . t) (hitSphere bounds forward sphere))
  assert "sphere exit and normal" (maybe False (\h -> near 1 (t h) && not (frontFace h) && nearVec (normal h) (Vec3 0 0 1))
    (hitSphere bounds (Ray (Vec3 0 0 (-3)) (Vec3 0 0 (-1))) sphere))
  assert "sphere miss" (maybe True (const False) (hitSphere bounds (Ray (Vec3 4 0 0) (Vec3 0 0 (-1))) sphere))
  assert "triangle center" (maybe False (near 2 . t) (hitTriangle bounds forward triangle))
  assert "triangle vertex" (maybe False (near 2 . t) (hitTriangle bounds (Ray (Vec3 0 1 0) (Vec3 0 0 (-1))) triangle))
  assert "triangle miss" (maybe True (const False) (hitTriangle bounds (Ray (Vec3 2 0 0) (Vec3 0 0 (-1))) triangle))
  assert "normal incidence" (maybe False (nearVec (Vec3 0 0 (-1))) (refract (Vec3 0 0 (-1)) (Vec3 0 0 1) (1/1.5)))
  assert "total internal reflection" (refract (unit (Vec3 0.9 0 (-0.3))) (Vec3 0 0 1) 1.5 == Nothing)
  forM_ [1..20] $ \count -> do
    let offsets = sampleOffsets 42 count
    assert "sample count and bounds" (length offsets == count && all (\(x,y) -> x>0 && x<1 && y>0 && y<1) offsets)
    let bins values = [floor (value * fromIntegral count) :: Int | value <- values]
    assert "one sample per axis stratum" (all (\i -> length (filter (== i) (bins (map fst offsets))) == 1
      && length (filter (== i) (bins (map snd offsets))) == 1) [0..count-1])
  assert "gamma conversion" (to256 (Vec3 0.25 0 1) == (127,0,255))
  let camera = Camera 1 5 2 1 (Vec3 0 0 0) (Vec3 1 0 0) (Vec3 0 1 0)
  assert "look-at center ray" (nearVec (unit (direction (xyRay camera 2 2))) (Vec3 1 0 0))
  let box = Bounds (Vec3 (-1) (-1) (-3)) (Vec3 1 1 (-2))
  assert "parallel slab hit" (hitBounds bounds forward box)
  assert "parallel slab miss" (not (hitBounds bounds (Ray (Vec3 2 0 0) (Vec3 0 0 (-1))) box))
  let scene = Scene (const (Vec3 0 0 0)) [SphereObj sphere, TriangleObj triangle] []
      accelerated = accelerateScene scene
  forM_ [Ray (Vec3 0 0 0) (Vec3 (fromIntegral x/8) (fromIntegral y/8) (-1)) | x <- [-12..12 :: Int], y <- [-12..12 :: Int]] $ \ray ->
    assert "BVH matches linear hits" (case (hitScene scene ray bounds, hitScene accelerated ray bounds) of
      (Nothing,Nothing) -> True
      (Just a,Just b) -> near (t a) (t b) && nearVec (normal a) (normal b)
      _ -> False)
  let clear = opaque { ambient = 0, diffuse = 0, specular = 0, transparency = 1, refractiveIndex = 1 }
      backdrop ray = let Vec3 x y _ = unit (direction ray) in Vec3 (0.5+x/4) (0.5+y/4) 0.6
      empty = Scene backdrop [] []
      glassScene = empty { objects = [SphereObj (Sphere (Vec3 0 0 (-3)) 1 clear)] }
      cam = Camera 1 9 2 1 (Vec3 0 0 0) (Vec3 0 0 (-1)) (Vec3 0 1 0)
      config = RendererConfig (Interval 0.001 4) 8 1 16 42
  assert "index-one glass preserves background" (and (zipWith nearVec (pixels (render config cam empty)) (pixels (render config cam glassScene))))
  let distant = TriangleObj (Triangle (Vec3 (-20) (-20) (-12)) (Vec3 20 (-20) (-12)) (Vec3 0 20 (-12))
        (opaque { baseColor = Vec3 1 0 0, ambient = 1 }))
      through = glassScene { objects = distant : objects glassScene }
      onePixel = Camera 1 1 0.000001 1 (Vec3 0 0 0) (Vec3 0 0 (-1)) (Vec3 0 1 0)
      result sceneValue = case pixels (render config onePixel sceneValue) of
        [p] -> p
        _ -> Vec3 (-1) (-1) (-1)
  assert "secondary rays exceed camera far bound" (nearVec (result through) (Vec3 1 0 0))
  let surface = TriangleObj (Triangle (Vec3 (-10) (-10) (-2)) (Vec3 10 (-10) (-2)) (Vec3 0 10 (-2))
        (opaque { ambient = 0, specular = 0 }))
      lit = Scene (const (Vec3 0 0 0)) [surface] [PointLight (Vec3 4 0 2) (Vec3 1 1 1)]
      blocked mat = lit { objects = surface : [SphereObj (Sphere (Vec3 2 0 0) 0.5 mat)] }
      energy (Vec3 r g b) = r+g+b
  assert "opaque shadow" (nearVec (result (blocked opaque)) (Vec3 0 0 0))
  assert "transparent shadow" (energy (result (blocked (clear { transparency = 0.8 }))) > 0
    && energy (result (blocked (clear { transparency = 0.8 }))) < energy (result lit))
  let backLit = lit { lights = [PointLight (Vec3 4 0 (-3)) (Vec3 1 1 1)] }
  assert "back-facing light contributes nothing" (nearVec (result backLit) (Vec3 0 0 0))
  assert "same seed repeats" (sampleOffsets 42 16 == sampleOffsets 42 16)
  assert "different seeds differ" (sampleOffsets 42 16 /= sampleOffsets 43 16)
  let n = Vec3 0 0 1
      ideal = unit (Vec3 0.5 0 1)
  forM_ [0..100] $ \seed -> do
    assert "zero roughness preserves direction" (nearVec (jitterDirection seed 0 ideal n) ideal)
    let reflected = jitterDirection seed 0.8 ideal n
        transmitted = jitterDirection seed 0.8 (ideal *^ (-1)) (n *^ (-1))
    assert "reflection stays normalized and above surface" (near 1 (mag reflected) && reflected .^ n >= 0)
    assert "transmission stays on transmitted side" (near 1 (mag transmitted) && transmitted .^ n <= 0)
  let area = lit { lights = [AreaLight (Vec3 4 0 2) (Vec3 0 0 0) (Vec3 0 0 0) (Vec3 1 1 1)] }
  assert "collapsed area light matches point" (nearVec (result area) (result lit))
  let soft = lit { lights = [AreaLight (Vec3 0 0 2) (Vec3 8 0 0) (Vec3 0 2 0) (Vec3 1 1 1)] }
      softBlocked = soft { objects = surface : [SphereObj (Sphere (Vec3 2 0 0) 0.5 opaque)] }
  assert "partial area occlusion creates penumbra" (energy (result softBlocked) > 0
    && energy (result softBlocked) < energy (result soft))
  let repeated = pixels (render config cam glassScene)
  assert "render repeatability" (repeated == pixels (render config cam glassScene))
  let gradientScene = Scene backdrop [SphereObj (Sphere (Vec3 0 0 (-3)) 1 (opaque { reflectivity = 1 }))] []
      roughScene = gradientScene { objects = [SphereObj (Sphere (Vec3 0 0 (-3)) 1
        (opaque { reflectivity = 1, reflectionRoughness = 0.4 }))] }
      frostedScene = glassScene { objects = [SphereObj (Sphere (Vec3 0 0 (-3)) 1
        (clear { transmissionRoughness = 0.4 }))] }
  assert "glossy reflection changes render" (pixels (render config cam gradientScene) /= pixels (render config cam roughScene))
  assert "rough transmission changes render" (pixels (render config cam glassScene) /= pixels (render config cam frostedScene))
  putStrLn "All regression tests passed."
