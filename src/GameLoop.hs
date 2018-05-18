{-# LANGUAGE PatternSynonyms #-}

module GameLoop (
    gameLoop
) where

import qualified Timer
import qualified SDL
import qualified Game as G
import qualified Data.Vector as V
import Keyboard
import Control.Monad (unless, when)
import SDL.Vect
import Foreign.C.Types
import System.IO
import Control.Monad.State
import Control.Monad.IO.Class

dt :: Float
dt = 1.0 / 60.0

move :: SDL.Rectangle CInt -> V2 CInt -> SDL.Rectangle CInt
move (SDL.Rectangle (SDL.P pos) size) offset = 
    SDL.Rectangle (SDL.P (pos + offset)) (size)


gameLoop :: G.Context -> IO ()
gameLoop ctx@(G.Context win renderer tickTimer g) = do

    events <- SDL.pollEvents    
    let shouldExit = any shouldClose events
    
    (dt, newTimer) <- getDt tickTimer

    let newGrid = G.update g

    render renderer g

    unless shouldExit (gameLoop $ newCtx newTimer newGrid)

    where 
        getDt t = do 
            newTimer <- Timer.createTimer
            dt <- Timer.getElapsedTime t
            return (dt, newTimer)
        newCtx t g = 
            let nctx = G.setGrid ctx g
            in G.setTimer nctx t
        shouldClose event = 
            isKeyPressed SDL.KeycodeEscape event ||
            flip isEvent SDL.QuitEvent event


render :: SDL.Renderer -> G.Grid -> IO ()
render renderer grid = do
    SDL.clear renderer
    SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 0 

    let (alive, dead) = V.partition (\c -> G.isAlive c) $ G.cells grid

    -- Some rendering shit
    SDL.present renderer
