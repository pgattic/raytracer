module Scene where

import Color
import Hittables.Hittable

data Scene = Scene {
  backgroundColor :: Color,
  objects :: [Hittable]
}
