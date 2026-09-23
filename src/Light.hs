module Light(Light(..)) where

import Point3
import Color

data Light = PointLight {
  position :: Point3,
  color :: Color
}
