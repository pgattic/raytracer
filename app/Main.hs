module Main where

import Camera
import Image
import Interval
import Renderer
import Scenes
import Vec3
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)

data Options = Options Int Int Int String (Maybe FilePath) Vec3 Vec3

defaults :: Options
defaults = Options 720 9 6 "both" Nothing (Vec3 0 0 0) (Vec3 0 0 (-1))

usage :: String
usage = "Usage: raytracer [--scene 1|2|both] [--height N] [--samples N] [--depth N] [--output PATH] [--eye X Y Z] [--target X Y Z]"

parse :: Options -> [String] -> Either String Options
parse opts [] = Right opts
parse (Options h s d which output eye target) (flag:value:rest)
  | flag == "--scene" && value `elem` ["1","2","both"] = next h s d value output eye target
  | flag == "--output" = next h s d which (Just value) eye target
  | flag `elem` ["--height","--samples","--depth"] =
      case readMaybe value of
        Just n | n > 0 || (flag == "--depth" && n == 0) ->
          next (if flag == "--height" then n else h)
               (if flag == "--samples" then n else s)
               (if flag == "--depth" then n else d) which output eye target
        _ -> Left ("Invalid value for " ++ flag)
  | flag `elem` ["--eye","--target"] =
      case rest of
        y:z:remaining -> case (readMaybe value, readMaybe y, readMaybe z) of
          (Just x', Just y', Just z')
            | all (\v -> not (isNaN v || isInfinite v)) [x',y',z'] ->
                let p = Vec3 x' y' z'
                in parse (Options h s d which output
                    (if flag == "--eye" then p else eye)
                    (if flag == "--target" then p else target)) remaining
          _ -> Left "Camera coordinates must be finite numbers"
        _ -> Left usage
  | otherwise = Left usage
  where next h' s' d' w' o' e' t' = parse (Options h' s' d' w' o' e' t') rest
parse _ _ = Left usage

main :: IO ()
main = do
  args <- getArgs
  if args == ["--help"] then putStrLn usage else
    case parse defaults args of
      Left message -> die message
      Right (Options h s d which output eye target)
        | mag_squared (eye -^ target) == 0 -> die "Eye and target must differ"
        | mag_squared ((eye -^ target) `cross` Vec3 0 1 0) < 1e-12 ->
            die "Camera direction must not be parallel to its up vector"
        | which == "both" && output /= Nothing -> die "--output requires --scene 1 or --scene 2"
        | otherwise -> do
            let config = RendererConfig (Interval 0.001 100) d s
                camera = Camera (16/9) h 2 1 eye target (Vec3 0 1 0)
                jobs = case which of
                  "1" -> [(maybe "scene1.ppm" id output, sceneOne)]
                  "2" -> [(maybe "scene2.ppm" id output, sceneTwo)]
                  _ -> [("scene1.ppm",sceneOne),("scene2.ppm",sceneTwo)]
            mapM_ (\(path, scene) -> do
              writeFile path (toPPM (render config camera scene))
              putStrLn (path ++ " successfully written")) jobs
