module Hittables.Hittable(Hittable(..), hit, HitRecord(..), createHitRecord) where

import Ray
import Hittables.Sphere
import Hittables.HitRecord
-- import Data.List (sortOn)
-- import Data.Maybe (catMaybes)

data Hittable = SphereObj Sphere

hit :: Double -> Double -> Ray -> Hittable -> Maybe HitRecord
hit tMin tMax ray obj =
  case obj of
    SphereObj sphere -> hitSphere tMin tMax ray sphere

-- type Scene = [Hittable]
--
-- hitScene :: Scene -> Ray -> Double -> Double -> Maybe HitRecord
-- hitScene objects ray tMin tMax = let
--     hits = sortOn t (catMaybes (map (hit tMin tMax ray) objects));
--   in
--     case hits of
--       (h : _ ) -> Just h
--       _ -> Nothing
--

