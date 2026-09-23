module Light(Light(..)) where

import Vec3
import Point3
import Color

data Light = PointLight {
  position :: Point3,
  color :: Color
} | DirectionalLight {
  lightDirection :: Vec3,
  color :: Color
}
