module Main (main) where

import Image

main :: IO ()
main = putStrLn (toPPM exampleImage)

