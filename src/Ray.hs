module Ray (Ray(..), at) where

import Point3
import Vec3

data Ray = Ray {
  origin :: Point3,
  direction :: Vec3
}

at :: Ray -> Double -> Point3
at (Ray orig dir) pos = orig +^ (dir *^ pos)
