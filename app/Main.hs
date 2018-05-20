{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import GameLoop
import Context
import Foreign.C.Types
import SDL.Vect
import qualified SDL
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
  gameLoop (createContext window renderer initialTimer Nothing)
  SDL.quit