module Context (
    Context (..),
    createContext,
    setGrid,
    setTimer
) where

import qualified SDL
import qualified Data.Vector as V
import SDL.Vect
import Util
import Timer
import Grid


data Context = Context { window :: SDL.Window, renderer :: SDL.Renderer, tickTimer :: Timer, grid :: Grid }
createContext :: SDL.Window -> SDL.Renderer -> Timer -> Maybe Grid -> Context

createContext win ren timer (Just grid) = Context win ren timer grid
createContext win ren timer Nothing = Context win ren timer grid
    where
        cell = Cell False rect
        rect = makeRect (sqVec 0) (sqVec 5)
        grid = Grid (V.replicate 50 cell) (V2 5 10) 5

setGrid :: Context -> Grid -> Context
setGrid ctx g = ctx { grid = g }

setTimer :: Context -> Timer -> Context 
setTimer ctx t = ctx { tickTimer = t }
