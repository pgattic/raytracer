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
} | AreaLight {
  corner :: Point3,
  edgeU :: Vec3,
  edgeV :: Vec3,
  color :: Color
}
