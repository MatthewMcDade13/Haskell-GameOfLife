{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import GameLoop
import Context
import Grid
import Foreign.C.Types
import SDL.Vect
import System.Random
import qualified SDL
import qualified Data.Vector as V
import System.IO
import Timer as T

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

windowConfig :: SDL.WindowConfig
windowConfig =
  SDL.defaultWindow { 
    SDL.windowInitialSize = V2 screenWidth screenHeight,
    SDL.windowResizable = True
    }

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "MEMES" windowConfig
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedRenderer }
  initialTimer <- T.createTimer
  gen <- getStdGen

  let 
    ctx = createContext window renderer initialTimer (Just grid)
    grid = setCellPositions (createGrid (V2 500 500) (8) (Just gen))

  gameLoop ctx
  SDL.quit