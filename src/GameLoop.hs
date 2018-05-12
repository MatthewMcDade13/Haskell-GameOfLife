module GameLoop (
    gameLoop
) where

import qualified Timer
import Control.Monad (unless)
import SDL.Vect
import qualified SDL
import System.IO

gameLoop :: SDL.Renderer -> IO ()
gameLoop renderer = do
    timer <- Timer.createTimer
    events <- SDL.pollEvents

    let isEscPressed = any (isKeyPressed SDL.KeycodeEscape) events

    SDL.rendererDrawColor renderer SDL.$= V4 0 0 255 255
    SDL.clear renderer
    SDL.present renderer

    Timer.getElapsedTime timer >>= print

    unless isEscPressed (gameLoop renderer)



isKeyPressed :: SDL.Keycode -> SDL.Event -> Bool
isKeyPressed key event =
    case SDL.eventPayload event of
        SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == key
        _ -> False

