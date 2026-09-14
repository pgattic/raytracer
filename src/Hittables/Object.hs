module Hittables.Object(Object(..)) where

import Ray
import Hittables.Hittable (Hittable(..), HitRecord (t))
import Hittables.Sphere (Sphere)
import Data.List (sortOn)
import Data.Maybe (catMaybes)

data Object = SphereObj Sphere

instance Hittable Object where
  hit tMin tMax ray obj =
    case obj of
      SphereObj sphere -> hit tMin tMax ray sphere

-- type Scene = [Object]
--
-- hitScene :: Scene -> Ray -> Double -> Double -> Maybe HitRecord
-- hitScene objects ray tMin tMax = let
--     hits = sortOn t (catMaybes (map (hit tMin tMax ray) objects));
--   in
--     case hits of
--       (h : _ ) -> Just h
--       _ -> Nothing
--
