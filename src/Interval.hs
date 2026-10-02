module Interval(Interval(..), infinity) where

data Interval = Interval {
  minT :: Double,
  maxT :: Double
}

infinity :: Double
infinity = 1 / 0
