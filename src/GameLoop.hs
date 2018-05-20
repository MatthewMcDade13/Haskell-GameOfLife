{-# LANGUAGE PatternSynonyms #-}

module GameLoop (
    gameLoop
) where

import qualified SDL
import qualified Data.Vector as V
import Timer
import Game
import Grid
import Context
import Keyboard
import Control.Monad (unless, when)
import SDL.Vect
import Foreign.C.Types
import System.IO
import Control.Monad.State
import Control.Monad.IO.Class

move :: SDL.Rectangle CInt -> V2 CInt -> SDL.Rectangle CInt
move (SDL.Rectangle (SDL.P pos) size) offset = 
    SDL.Rectangle (SDL.P (pos + offset)) (size)


gameLoop :: Context -> IO ()
gameLoop ctx@(Context win renderer tickTimer g) = do

    events <- SDL.pollEvents    
    let shouldExit = any shouldClose events
    
    (dt, newTimer) <- getDt tickTimer

    let newGrid = update g

    render renderer g

    unless shouldExit (gameLoop $ newCtx newTimer newGrid)

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


render :: SDL.Renderer -> Grid -> IO ()
render renderer grid = do
    SDL.clear renderer
    SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 0 

    let (alive, dead) = V.partition (\c -> isAlive c) $ cells grid

    -- Some rendering shit
    SDL.present renderer
