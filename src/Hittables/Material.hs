module Hittables.Material(Material(..)) where

import Color

data Material = Material {
  baseColor :: Color,
  ambient :: Double,
  diffuse :: Double,
  specular :: Double,
  shininess :: Double,
  reflectivity :: Double
}
