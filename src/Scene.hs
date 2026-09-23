module Scene (Scene(..), hitScene) where

import Color
import Hittables.Hittable
import Ray
import Interval

data Scene = Scene {
  background :: Ray -> Color,
  objects :: [Hittable]
}

findClosest :: Maybe HitRecord -> Maybe HitRecord -> Maybe HitRecord
findClosest Nothing Nothing = Nothing;
findClosest Nothing (Just hr1) = Just hr1;
findClosest (Just hr0) Nothing = Just hr0;
findClosest (Just hr0) (Just hr1) = Just (if (t hr0) < (t hr1) then hr0 else hr1)

hitScene :: Scene -> Ray -> Interval -> Maybe HitRecord
hitScene (Scene _ objs) ray interval =
  foldl findClosest Nothing (map (hit interval ray) objs)
