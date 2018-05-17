{-# LANGUAGE PatternSynonyms #-}

module GameLoop (
    gameLoop
) where

import qualified Timer
import qualified SDL
import qualified Game as G
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
    
    let isEscPressed = any (isKeyPressed SDL.KeycodeEscape) events
    
    (dt, newTimer) <- getDt tickTimer
    SDL.clear renderer
    SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 0 

    print dt

    SDL.present renderer

    unless isEscPressed (gameLoop $ newCtx newTimer)

    where 
        getDt t = do 
            newTimer <- Timer.createTimer
            dt <- Timer.getElapsedTime t
            return (dt, newTimer)
        newCtx t = 
            let nctx = G.setGrid ctx (G.createGrid 5 5 Nothing)
            in G.setTimer nctx t

