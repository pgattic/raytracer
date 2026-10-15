module Sampling (Seed, deriveSeed, random01, sampleOffsets, jitterDirection) where

import Data.Bits (xor, shiftR)
import Data.Word (Word64)
import Vec3

type Seed = Word64

-- SplitMix64 finalizer: counter-based streams keep rendering order independent.
deriveSeed :: Seed -> Word64 -> Seed
deriveSeed seed stream =
  let z0 = seed + 0x9e3779b97f4a7c15 * (stream + 1)
      z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
  in z2 `xor` (z2 `shiftR` 31)

random01 :: Seed -> Word64 -> Double
random01 seed stream =
  fromIntegral (deriveSeed seed stream `shiftR` 11) / 9007199254740992

-- Jittered Latin hypercube: each axis visits every stratum exactly once.
sampleOffsets :: Seed -> Int -> [(Double, Double)]
sampleOffsets seed requested =
  let count = max 1 requested
      candidates = [max 1 (count `div` 2) .. count]
      stride = case [k | k <- candidates, gcd k count == 1] of
        k:_ -> k
        [] -> 1
      offset i jitter = (fromIntegral i + jitter) / fromIntegral count
  in [(offset i (random01 seed (fromIntegral (2*i))),
       offset ((i * stride) `mod` count) (random01 seed (fromIntegral (2*i+1))))
     | i <- [0 .. count - 1]]

-- Uniform solid-angle cone, folded into the desired surface hemisphere.
jitterDirection :: Seed -> Double -> Vec3 -> Vec3 -> Vec3
jitterDirection seed roughness ideal allowedNormal
  | roughness <= 0 = unit ideal
  | otherwise =
      let axis = unit ideal
          Vec3 _ _ az = axis
          helper = if abs az < 0.9 then Vec3 0 0 1 else Vec3 0 1 0
          tangent = unit (axis `cross` helper)
          bitangent = axis `cross` tangent
          coneAngle = min 1 roughness * pi / 2
          cosine = 1 - random01 seed 0 * (1 - cos coneAngle)
          sine = sqrt (max 0 (1 - cosine*cosine))
          phi = 2*pi*random01 seed 1
          candidate = axis *^ cosine +^ tangent *^ (sine*cos phi) +^ bitangent *^ (sine*sin phi)
          projection = candidate .^ allowedNormal
      in unit (if projection < 0 then candidate -^ allowedNormal *^ (2*projection) else candidate)
