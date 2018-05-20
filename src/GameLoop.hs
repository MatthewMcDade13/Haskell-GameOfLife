{-# LANGUAGE PatternSynonyms #-}

module GameLoop (
    gameLoop
) where

import qualified SDL
import qualified Data.Vector as V
import Timer
import Grid
import Context
import Keyboard
import Control.Monad (unless, when)
import SDL.Vect
import Foreign.C.Types
import System.IO

gameLoop :: Context -> IO ()
gameLoop ctx@(Context _ renderer genTimer genDur g) = do

    events <- SDL.pollEvents    
    currTime <- getElapsedTime genTimer

    let 
        shouldExit = any shouldClose events
        shouldTick = currTime >= genDur

    nextTimer <- if shouldTick then createTimer else return genTimer
    
    let newGrid = if shouldTick then update g else g
    
    render renderer g
    
    unless shouldExit (gameLoop $ newCtx nextTimer newGrid)

    where 
        getDt t = do 
            newTimer <- createTimer
            dt <- getElapsedTime t
            return (dt, newTimer)
        newCtx t g = 
            let nctx = setGrid ctx g
            in setTimer nctx t
        shouldClose event = 
            isKeyPressed SDL.KeycodeEscape event ||
            flip isEvent SDL.QuitEvent event

update :: Grid -> Grid
update grid = grid { cells = newCells }
    where
        checkNeighbors cell =
            let 
                liveNeighbors = numLiveNeighbors cell grid
                willDie = liveNeighbors < 2 || liveNeighbors > 3
                willSpawn = liveNeighbors == 3
            in
                if willDie then
                    cell { isAlive = False }
                else if willSpawn then
                    cell { isAlive = True }
                else 
                    cell
        newCells = V.map checkNeighbors gridCells
        gridCells = cells grid


render :: SDL.Renderer -> Grid -> IO ()
render renderer grid = do
    SDL.clear renderer
    
    let (alive, dead) = V.partition (\c -> isAlive c) $ cells grid
    
    setColor $ V4 67 31 31 255
    SDL.fillRects renderer (asRects alive)
    setColor $ V4 105 105 105 255
    SDL.drawRects renderer (asRects dead)
    
    setColor $ V4 0 0 0 0 
    -- Some rendering shit
    SDL.present renderer

    where 
        setColor color = SDL.rendererDrawColor renderer SDL.$= color
