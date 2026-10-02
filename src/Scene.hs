module Scene (Scene(..), hitScene) where

import Color
import Objects.Object
import Ray
import Interval
import Light

data Scene = Scene {
  background :: Ray -> Color,
  objects :: [Object],
  lights :: [Light]
}

findClosest :: Ray -> (Maybe HitRecord, Interval) -> Object -> (Maybe HitRecord, Interval)
findClosest ray accumVal@(_, accumInt) obj =
  case hit accumInt ray obj of
    Nothing -> accumVal
    Just record -> (Just record, Interval (minT accumInt) (t record))

hitScene :: Scene -> Ray -> Interval -> Maybe HitRecord
hitScene (Scene _ objs _) ray interval =
  fst (foldl (findClosest ray) (Nothing, interval) objs)
