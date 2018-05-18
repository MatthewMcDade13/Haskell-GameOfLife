{-# LANGUAGE PatternSynonyms #-}

module Game (
    createContext,
    createGrid,
    update,
    getCell,
    setGrid,
    setTimer,
    Context (..),
    Grid (..), 
    Cell (..)
) where

import qualified SDL
import qualified Timer as T
import qualified Data.Vector as V
import qualified System.Random as R
import SDL.Vect
import Foreign.C.Types

data Cell = Cell { isAlive :: Bool, rect :: (SDL.Rectangle CInt) }
data Grid = Grid { cells :: (V.Vector Cell), size :: (V2 CInt), cellSize :: CInt }

data Context = Context { window :: SDL.Window, renderer :: SDL.Renderer, tickTimer :: T.Timer, grid :: Grid }


update :: Grid -> Grid
update = id


createContext :: SDL.Window -> SDL.Renderer -> T.Timer -> Maybe Grid -> Context
createContext win ren timer (Just ctx) = Context win ren timer ctx
createContext win ren timer Nothing = 
    Context win ren timer (Grid (V.replicate 50 (Cell False $ makeRect (sqVec 0) (sqVec 5))) (V2 5 10) 5) 

setGrid :: Context -> Grid -> Context
setGrid ctx g = ctx { grid = g }

setTimer :: Context -> T.Timer -> Context 
setTimer ctx t = ctx { tickTimer = t }


createGrid :: V2 CInt -> CInt -> Maybe R.StdGen -> Grid
createGrid gridSize@(V2 gw gh) cellSize Nothing = 
    Grid (V.replicate (fromIntegral (gw * gh)) (Cell False $ makeRect (sqVec 0) (sqVec 0))) (V2 gw gh) (cellSize)

createGrid gsize@(V2 gw gh) cellSize (Just gen) = 
    Grid (V.fromList $ randGrid (gw * gh) cellSize [] gen) gsize cellSize
    where
        randGrid 0 s l _ = l 
        randGrid n s l gen =
            let 
                (val, newGen) = (R.random gen :: (Bool, R.StdGen))
                newCell = Cell val $ makeRect (sqVec 0) (sqVec s)
            in randGrid (n - 1) s (newCell:l) newGen
         

getCell :: Grid -> CInt -> CInt -> Cell
getCell (Grid cells (V2 w h) _) x y = cells V.! fromIntegral (w * y + x)

makeRect :: V2 CInt -> V2 CInt -> SDL.Rectangle CInt
makeRect pos size = SDL.Rectangle (SDL.P pos) (size)

-- makes vector with same with and height as given int
sqVec :: CInt -> V2 CInt
sqVec size = V2 size size
