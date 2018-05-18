{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Init
import GameLoop
import Control.Concurrent (threadDelay)
import Foreign.C.Types
import SDL.Vect
import qualified SDL
import System.IO
import Game as G
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
  initSDL
  window <- SDL.createWindow "MEMES" windowConfig
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedRenderer }
  initialTimer <- T.createTimer
  gameLoop (G.createContext window renderer initialTimer Nothing)
  SDL.quit