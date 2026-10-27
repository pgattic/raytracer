module PathTracer (tracePath) where

import Color
import Interval
import Objects.HitRecord
import Objects.Material
import Optics
import Ray
import Sampling
import Scene
import Vec3

-- A depth of N permits N surface scatters. A miss still sees the sky at depth 0.
tracePath :: Int -> Seed -> Scene -> Interval -> Ray -> Color
tracePath depth seed scene bounds ray =
  case hitScene scene ray bounds of
    Nothing -> background scene ray
    Just rec
      | depth <= 0 -> Vec3 0 0 0
      | otherwise ->
          let mat = material rec
              clamp01 = max 0 . min 1
              reflection = clamp01 (reflectivity mat)
              transmission = min (1-reflection) (clamp01 (transparency mat))
              lambert = (1-reflection-transmission) * clamp01 (diffuse mat)
              total = reflection + transmission + lambert
              choice = random01 seed 0 * total
              incoming = unit (direction ray)
              n = normal rec
              scatteringSeed = deriveSeed seed 1
              ratio = if frontFace rec then 1 / refractiveIndex mat else refractiveIndex mat
              (outgoing, tint)
                | choice < lambert =
                    (cosineHemisphere scatteringSeed n, baseColor mat)
                | choice < lambert + reflection =
                    (jitterDirection scatteringSeed (reflectionRoughness mat) (reflect incoming n) n,
                     Vec3 1 1 1)
                | otherwise =
                    case refract incoming n ratio of
                      Nothing ->
                        (jitterDirection scatteringSeed (reflectionRoughness mat) (reflect incoming n) n,
                         Vec3 1 1 1)
                      Just refracted ->
                        (jitterDirection scatteringSeed (transmissionRoughness mat) refracted (n *^ (-1)),
                         baseColor mat)
              bounced = spawnRay (point rec) n outgoing
              returning = tracePath (depth-1) (deriveSeed seed 2) scene
                            (Interval rayEpsilon infinity) bounced
          -- Branch i has probability weight_i/total, so weight_i/probability = total.
          in if total <= 0 then Vec3 0 0 0
             else multiplyColor (tint *^ total) returning
