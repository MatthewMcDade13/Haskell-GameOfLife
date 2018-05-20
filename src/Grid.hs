module Grid (
    Grid (..),
    Cell (..),
    createGrid,
    asRects,
    setCellPositions,
    getCell
) where

import qualified SDL
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Util
import SDL.Vect
import Foreign.C.Types
import System.Random

data Cell = Cell { isAlive :: Bool, rect :: (SDL.Rectangle CInt) }
data Grid = Grid { cells :: (V.Vector Cell), size :: (V2 CInt), cellSize :: CInt } 

instance Show Cell where
    show (Cell _ (SDL.Rectangle (SDL.P (V2 x y)) _)) = "(" ++ show x ++ ", " ++ show y ++ ")"

createGrid :: V2 CInt -> CInt -> Maybe StdGen -> Grid
createGrid gridSize@(V2 gw gh) cellSize Nothing = 
    Grid { cells = (V.replicate vecSize cell), size = gridSize, cellSize = cellSize }
    where
        cell = Cell False rect
        (cw, ch) = ((gw `quot` cellSize), (gh `quot` cellSize))
        rect = makeRect (sqVec 0) (V2 cw ch)
        vecSize = fromIntegral (cw * ch)

createGrid gsize@(V2 gw gh) cellSize (Just gen) = 
    Grid (V.fromList $ randGrid (cw * ch) cellSize [] gen) gsize cellSize
    where
        randGrid 0 s l _ = l 
        randGrid n s l gen =
            let 
                (val, newGen) = (randomR (0, 100) gen :: (Int, StdGen))                
                newCell = Cell roll $ makeRect (sqVec 0) (V2 cw ch)
                roll = False -- if val <= 25 then True else False
            in randGrid (n - 1) s (newCell:l) newGen
        (cw, ch) = ((gw `quot` cellSize), (gh `quot` cellSize))


setCellPositions :: Grid -> Grid
setCellPositions grid = grid { cells = (cells grid) V.// reverse (set 0 []) }
    where 
        set 0 l = set 1 (setCell:l)
            where 
                cell = getCell grid 0 0
                setCell = (0, setCellPos (cell) (sqVec 0))
        set n l 
            | n < cellsLength = 
                let 
                    (_, prevCell) = head l
                    currCell = gridCells V.! n
                    gCellSize = (cellSize grid)
                    (V2 px py) = cellPos prevCell
                    (V2 gWidth _) = size grid                
                    hasOverflow = (px + gCellSize) >= (gWidth)
                in 
                    if hasOverflow then 
                        set (n + 1) ((n, (setCellPos currCell (V2 0 (py + gCellSize)))):l)
                    else 
                        set (n + 1) ((n, (setCellPos currCell (V2 (px + gCellSize) py))):l)
            | otherwise = l
            
        gridCells = cells grid
        cellsLength = V.length gridCells
         

getCell :: Grid -> CInt -> CInt -> Cell
getCell (Grid cells (V2 w h) _) x y = cells V.! fromIntegral (w * y + x)

setCellPos :: Cell -> V2 CInt -> Cell
setCellPos cell pos = cell { rect = makeRect (pos) (size) }
    where size = getRectSize $ rect cell

cellPos :: Cell -> V2 CInt
cellPos Cell { rect = r } = getRectPos r


asRects :: V.Vector Cell -> VS.Vector (SDL.Rectangle CInt)
asRects cells = make cells
    where 
        make = VS.fromList . V.foldl' (\acc (Cell { rect = r }) -> r:acc) []