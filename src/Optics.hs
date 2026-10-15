module Optics (reflect, refract, rayEpsilon, spawnRay) where

import Vec3
import Ray

rayEpsilon :: Double
rayEpsilon = 0.00001

reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v -^ (n *^ (2 * (v .^ n)))

refract :: Vec3 -> Vec3 -> Double -> Maybe Vec3
refract incoming n ratio =
  let cosine = max 0 (min 1 ((incoming *^ (-1)) .^ n))
      perpendicular = (incoming +^ n *^ cosine) *^ ratio
      remaining = 1 - mag_squared perpendicular
  in if remaining < 0 then Nothing
     else Just (perpendicular -^ n *^ sqrt remaining)

spawnRay :: Vec3 -> Vec3 -> Vec3 -> Ray
spawnRay p n outgoing =
  let side = if outgoing .^ n >= 0 then 1 else -1
  in Ray (p +^ n *^ (side * rayEpsilon)) (unit outgoing)
