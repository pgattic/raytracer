module Ray (Ray(..), at) where

import Vec3 (Vec3(..), (+^), (*^))

data Ray = Ray {
  origin :: Vec3,
  direction :: Vec3
}

at :: Ray -> Double -> Vec3
at (Ray orig dir) pos = orig +^ (dir *^ pos)

