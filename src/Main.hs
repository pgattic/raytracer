module Main (main) where
import Data.Word

data Color = Color {
  r :: Word8,
  g :: Word8,
  b :: Word8
} deriving (Show, Eq)

newColor :: Word8 -> Word8 -> Word8 -> Color
newColor red green blue = Color { r = red, g = green, b = blue }

printColor :: Color -> String
printColor c = unwords [show (r c), show (g c), show (b c)] ++ "\n"

data Image = Image {
  width :: Int,
  height :: Int,
  pixels :: [Color]
}

toPPM :: Image -> String
toPPM image =
  "P3\n" ++
  show (width image) ++ " " ++ show (height image) ++
  "\n255\n" ++
  concat (map printColor (pixels image))

exampleImage :: Image
exampleImage = Image {
  width = 3,
  height = 2,
  pixels = [
    (newColor 255 0 0),
    (newColor 0 255 0),
    (newColor 255 0 0),
    (newColor 255 0 0),
    (newColor 255 0 0),
    (newColor 255 0 0)
  ]
}

main :: IO ()
main = putStrLn (toPPM exampleImage)

