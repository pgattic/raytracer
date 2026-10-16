module Main where

import Camera
import Image
import Interval
import Renderer
import Scenes
import Sampling (Seed)
import Vec3
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)

data Options = Options Int Int Int String (Maybe FilePath) (Maybe Vec3) (Maybe Vec3) Int Seed

defaults :: Options
defaults = Options 360 16 4 "both" Nothing Nothing Nothing 4 42

usage :: String
usage = "Usage: raytracer [--scene 1|2|both] [--height N] [--samples N] [--depth N] [--shadow-samples N] [--seed N] [--output PATH] [--eye X Y Z] [--target X Y Z]"

parse :: Options -> [String] -> Either String Options
parse opts [] = Right opts
parse (Options h s d which output eye target shadows seed) (flag:value:rest)
  | flag == "--seed" =
      case readMaybe value :: Maybe Integer of
        Just n | n >= 0 && n <= 18446744073709551615 -> parse (Options h s d which output eye target shadows (fromIntegral n)) rest
        _ -> Left "Seed must be an unsigned 64-bit integer"
  | flag == "--shadow-samples" =
      case readMaybe value of
        Just n | n > 0 -> parse (Options h s d which output eye target n seed) rest
        _ -> Left "Shadow sample count must be positive"
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
                    (if flag == "--eye" then Just p else eye)
                    (if flag == "--target" then Just p else target) shadows seed) remaining
          _ -> Left "Camera coordinates must be finite numbers"
        _ -> Left usage
  | otherwise = Left usage
  where next h' s' d' w' o' e' t' = parse (Options h' s' d' w' o' e' t' shadows seed) rest
parse _ _ = Left usage

main :: IO ()
main = do
  args <- getArgs
  if args == ["--help"] then putStrLn usage else
    case parse defaults args of
      Left message -> die message
      Right (Options h s d which output eye target shadows seed)
        | which == "both" && output /= Nothing -> die "--output requires --scene 1 or --scene 2"
        | otherwise -> do
            let config = RendererConfig (Interval 0.001 100) d s shadows seed
                jobs = case which of
                  "1" -> [(maybe "scene1.ppm" id output, sceneOne, cameraOne h)]
                  "2" -> [(maybe "scene2.ppm" id output, sceneTwo, cameraTwo h)]
                  _ -> [("scene1.ppm",sceneOne,cameraOne h),("scene2.ppm",sceneTwo,cameraTwo h)]
                prepareCamera preset = preset {
                  cameraCenter = maybe (cameraCenter preset) id eye,
                  lookAt = maybe (lookAt preset) id target
                }
                validCamera camera =
                  let view = cameraCenter camera -^ lookAt camera
                  in mag_squared view > 0 && mag_squared (cross view (cameraUp camera)) > 1e-12
            if not (all (\(_,_,preset) -> validCamera (prepareCamera preset)) jobs)
              then die "Camera eye and target must differ and viewing direction must not be parallel to up"
              else mapM_ (\(path, scene, preset) -> do
                writeFile path (toPPM (render config (prepareCamera preset) scene))
                putStrLn (path ++ " successfully written")) jobs
