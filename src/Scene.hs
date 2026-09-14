module Scene (Scene(..), hitScene) where

import Color
import Hittables.Hittable
import Vec3
import Ray
import Interval

import Data.List (sortOn)
import Data.Maybe (catMaybes, listToMaybe)

data Scene = Scene {
  background :: Ray -> Color,
  objects :: [Hittable]
}

hitScene :: Scene -> Ray -> Interval -> Maybe HitRecord
hitScene (Scene _ objects) ray interval = let
    hits = sortOn t (catMaybes (map (hit interval ray) objects));
  in
    listToMaybe hits
