module Grid (
    Grid (..),
    Cell (..),
    createGrid,
    getCell
) where

import qualified SDL
import qualified Data.Vector as V
import Util
import SDL.Vect
import Foreign.C.Types
import System.Random

data Cell = Cell { isAlive :: Bool, rect :: (SDL.Rectangle CInt) }
data Grid = Grid { cells :: (V.Vector Cell), size :: (V2 CInt), cellSize :: CInt }


createGrid :: V2 CInt -> CInt -> Maybe StdGen -> Grid
createGrid gridSize@(V2 gw gh) cellSize Nothing = 
    Grid { cells = (V.replicate vecSize cell), size = gridSize, cellSize = cellSize }
    where
        cell = Cell False rect
        rect = makeRect (sqVec 0) (sqVec 0)
        vecSize = fromIntegral (gw * gh)

createGrid gsize@(V2 gw gh) cellSize (Just gen) = 
    Grid (V.fromList $ randGrid (gw * gh) cellSize [] gen) gsize cellSize
    where
        randGrid 0 s l _ = l 
        randGrid n s l gen =
            let 
                (val, newGen) = (random gen :: (Bool, StdGen))
                newCell = Cell val $ makeRect (sqVec 0) (sqVec s)
            in randGrid (n - 1) s (newCell:l) newGen
         

getCell :: Grid -> CInt -> CInt -> Cell
getCell (Grid cells (V2 w h) _) x y = cells V.! fromIntegral (w * y + x)