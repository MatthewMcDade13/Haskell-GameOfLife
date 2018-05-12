module Init (
 initSDL
) where

import Data.Text
import qualified SDL

initSDL :: IO ()
initSDL = SDL.initialize [SDL.InitVideo]

