module Context (
    Context (..),
    setGrid,
    setTimer
) where

import qualified SDL
import qualified Data.Vector as V
import SDL.Vect
import Util
import Timer
import Grid


data Context = Context { 
    window :: SDL.Window, 
    renderer :: SDL.Renderer,
    genTimer :: Timer, 
    genDuration :: Float,
    grid :: Grid 
}

setGrid :: Context -> Grid -> Context
setGrid ctx g = ctx { grid = g }

setTimer :: Context -> Timer -> Context 
setTimer ctx t = ctx { genTimer = t }
