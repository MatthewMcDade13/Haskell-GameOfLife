{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Init
import GameLoop
import Control.Concurrent (threadDelay)
import Foreign.C.Types
import SDL.Vect
import qualified SDL
import System.IO

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

main :: IO ()
main = do
  initSDL
  window <- SDL.createWindow "MEMES" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedRenderer }
  gameLoop renderer