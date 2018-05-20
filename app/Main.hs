{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import GameLoop
import Control.Monad (unless)
import Context
import Grid
import Foreign.C.Types
import Control.Monad.IO.Class
import SDL.Vect
import Text.Read
import System.Random
import qualified SDL
import qualified Data.Vector as V
import System.IO
import Timer as T

data Settings = Settings { gridSize :: V2 Int, cellSize :: Int, genDuration :: Float }

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (1366, 768)

windowConfig :: SDL.WindowConfig
windowConfig =
  SDL.defaultWindow { 
    SDL.windowInitialSize = V2 screenWidth screenHeight,
    SDL.windowResizable = True
    }  

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  putStrLn "AYYLMA3O"
  hFlush stdout

  (Settings gSize cSize genDur) <- getSettings

  window <- SDL.createWindow "Conway's Game of Life" windowConfig
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedRenderer }
  initialTimer <- T.createTimer
  gen <- getStdGen

  let 
    ctx = Context window renderer initialTimer genDur grid
    grid = setCellPositions (createGrid gSize cSize (Just gen))

  gameLoop ctx


  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit


getInput :: (Num a) => (String -> IO (Maybe a)) -> String -> String -> IO a
getInput cb message errMessage = do
  putStrLn message
  hFlush stdout


  input <- getLine >>= cb

  let 
    hasDesiredInput = not (null input)
    
  if not hasDesiredInput then do
    putStrLn errMessage
    hFlush stdout
    getInput cb message errMessage
  else do
    let 
      (Just result) = input
    return result

-- getInput cb message errMessage = do
--   putStrLn message
--   hFlush stdout

--   input <- getLine >>= cb

--   putStrLn $ show input
--   hFlush stdout

--   let 
--     hasDesiredInput = not $ (null input)

--   if hasDesiredInput then do
--     let 
--       (Just result) = input
--     return ()
--   else do
--     putStrLn errMessage
--     hFlush stdout
--     getInput cb message errMessage


-- getInput' :: (String -> IO (Maybe Int)) -> String -> String -> IO ()
-- getInput' cb message errMessage = do
--   putStrLn message
--   hFlush stdout

--   input <- getLine >>= cb

--   putStrLn $ show input
--   hFlush stdout

--   let 
--     hasDesiredInput = not $ (null input)

--   if hasDesiredInput then do
--     let 
--       (Just result) = input
--     return ()
--   else do
--     putStrLn errMessage
--     hFlush stdout
--     getInput' cb message errMessage
        


getInputi :: String -> String-> IO Int
getInputi message errMessage = getInput (\x -> return $ (readMaybe x :: Maybe Int)) message errMessage

getInputf :: String -> String -> IO Float
getInputf message errMessage = getInput (\x -> return $ (readMaybe x :: Maybe Float)) message errMessage

getSettings :: IO Settings
getSettings = do
  let errMessage = "Invalid input, please enter a valid number"

  gWidth <- getInputi "Enter Grid Width" errMessage
  gHeight <- getInputi "Enter Grid Height" errMessage
  cSize <- getInputi "Enter Cell Sizes" errMessage
  genTime <- getInputf "Enter Generation Time" errMessage

  return $ Settings (V2 gWidth gHeight) cSize genTime