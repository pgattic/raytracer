module Sampling (sampleOffsets) where

-- Latin hypercube samples cover both axes for every positive count.
sampleOffsets :: Int -> [(Double, Double)]
sampleOffsets requested =
  let count = max 1 requested
      candidates = [max 1 (count `div` 2) .. count]
      stride = case [k | k <- candidates, gcd k count == 1] of
        k:_ -> k
        [] -> 1
      offset i = (fromIntegral i + 0.5) / fromIntegral count
  in [(offset i, offset ((i * stride) `mod` count)) | i <- [0 .. count - 1]]
