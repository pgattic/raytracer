module Color (Color, (+^), (-^), to256, printColor) where

import Data.Word
import Vec3 (Vec3(..), (+^), (-^))

type Color = Vec3

clamp :: Double -> Double -> Double -> Double
clamp low high x = max low (min high x)

to256 :: Color -> (Word8, Word8, Word8)
to256 (Vec3 r g b) = let
    conv x = floor (255.999 * (clamp 0 1 x))
  in (conv r, conv g, conv b)

printColor :: Color -> String
printColor c = let (r, g, b) = to256 c in unwords [show (r), show (g), show (b)]
