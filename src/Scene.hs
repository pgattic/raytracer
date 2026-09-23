module Scene (Scene(..), hitScene) where

import Color
import Hittables.Hittable
import Ray
import Interval
import Light

data Scene = Scene {
  background :: Ray -> Color,
  objects :: [Hittable],
  lights :: [Light]
}

findClosest :: Ray -> (Maybe HitRecord, Interval) -> Hittable -> (Maybe HitRecord, Interval)
findClosest ray accumVal obj = let
    accumInt = snd accumVal;
  in
    case hit accumInt ray obj of
      Nothing -> accumVal
      Just record -> (Just record, Interval (minT accumInt) (t record))

hitScene :: Scene -> Ray -> Interval -> Maybe HitRecord
hitScene (Scene _ objs _) ray interval =
  fst (foldl (findClosest ray) (Nothing, interval) objs)
