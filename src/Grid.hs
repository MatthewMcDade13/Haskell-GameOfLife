module Grid (
    Grid (..),
    Cell (..),
    createGrid,
    asRects,
    setCellPositions,
    numLiveNeighbors,
    getCell
) where

import qualified SDL
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.List
import Util
import SDL.Vect
import Foreign.C.Types
import System.Random

data Cell = Cell { isAlive :: Bool, rect :: (SDL.Rectangle CInt), gridPos :: V2 Int }
data Grid = Grid { cells :: (V.Vector Cell), size :: (V2 Int), cellSize :: Int } 

instance Show Cell where
    show (Cell _ _ (V2 x y)) = "(" ++ show x ++ ", " ++ show y ++ ")"

createGrid :: V2 Int -> Int -> Maybe StdGen -> Grid
createGrid gridSize@(V2 gw gh) cellSize Nothing = 
    Grid { cells = (V.replicate vecSize cell), size = gridSize, cellSize = cellSize }
    where
        cell = Cell False rect $ sqVec 0
        (cw, ch) = ((gw `quot` cellSize), (gh `quot` cellSize))
        rect = makeRect (sqVec 0) (sqVec $ fromIntegral cellSize)
        vecSize = fromIntegral (cw * ch)

createGrid gsize@(V2 gw gh) cellSize (Just gen) = 
    Grid (V.fromList $ randGrid (cw * ch) cellSize [] gen) gsize cellSize
    where
        randGrid 0 s l _ = l 
        randGrid n s l gen =
            let 
                (val, newGen) = (randomR (0, 100) gen :: (Int, StdGen))  
                rs = gw `quot` cellSize               
                newCell = Cell roll rect $ V2 ((n - 1) `rem` rs) ((n - 1) `quot` rs)
                rect = makeRect (sqVec 0) (sqVec $ fromIntegral cellSize)
                roll = val <= 25
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
                    (V2 cw ch) = getCellSize prevCell
                    (V2 px py) = cellPos prevCell
                    (V2 gWidth _) = size grid                
                    hasOverflow = (px + cw) >= (fromIntegral gWidth)
                in 
                    if hasOverflow then 
                        let 
                            vec = (V2 0 (py + ch))
                            result = (setCellPos currCell vec)
                        in 
                            set (n + 1) $ (n, result):l
                    else 
                        let 
                            vec = (V2 (px + cw) py)
                            result = (setCellPos currCell vec)
                        in
                            set (n + 1) $ (n, result):l
            | otherwise = l
            
        gridCells = cells grid
        cellsLength = V.length gridCells



getRowSize :: Grid -> Int
getRowSize (Grid { size = (V2 gw gh), cellSize = cs }) = fromIntegral $ gw `quot` cs

getColSize :: Grid -> Int
getColSize (Grid { size = (V2 gw gh), cellSize = cs }) = fromIntegral $ gh `quot` cs
         
getCell :: Grid -> Int -> Int -> Cell
getCell grid@(Grid { cells = cs }) x y = cs V.! fromIntegral (clamp (w * y + x))
    where 
        w = getRowSize grid
        cellsLength = V.length cs
        clamp n = 
            if n >= cellsLength then 
                n - cellsLength
            else if n < 0 then
                n + cellsLength
            else n

setCellPos :: Cell -> V2 CInt -> Cell
setCellPos cell pos = cell { rect = makeRect (pos) (size) }
    where size = getRectSize $ rect cell

cellPos :: Cell -> V2 CInt
cellPos Cell { rect = r } = getRectPos r

getCellSize :: Cell -> V2 CInt
getCellSize (Cell { rect = r }) = getRectSize r

asRects :: V.Vector Cell -> VS.Vector (SDL.Rectangle CInt)
asRects cells = make cells
    where 
        make = VS.fromList . V.foldl' (\acc (Cell { rect = r }) -> r:acc) []


numLiveNeighbors :: Cell -> Grid -> Int
numLiveNeighbors cell grid = 
    let 
        indxs = delete (0,0) [ (i, j) | i <- [-1..1], j <- [-1..1] ]
    in 
        foldl accumNeighbors 0 indxs
    where
        accumNeighbors acc (i, j) =
            let 
                alive = isAlive $ getCell grid (cx + i) (cy + j)
                cellsLength = V.length $ cells grid
                (V2 cx cy) = gridPos cell                
            in
                if alive then succ acc else acc
                    
