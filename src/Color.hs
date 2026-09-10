module Color (Color, (+^), (-^), to256) where

import Data.Word
import Vec3 (Vec3(..), (+^), (-^))

type Color = Vec3

to256 :: Color -> (Word8, Word8, Word8)
to256 (Vec3 r g b) = (floor (255.999 * r), floor (255.999 * g), floor (255.999 * b))
