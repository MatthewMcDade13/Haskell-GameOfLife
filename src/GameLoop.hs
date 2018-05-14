module GameLoop (
    gameLoop
) where

import qualified Timer
import Control.Monad (unless, when)
import SDL.Vect
import qualified SDL
import Foreign.C.Types
import System.IO

data Context = Context SDL.Window SDL.Renderer

move :: SDL.Rectangle CInt -> V2 CInt -> SDL.Rectangle CInt
move (SDL.Rectangle (SDL.P (V2 rx ry)) (V2 xx yy)) (V2 ox oy) = 
    SDL.Rectangle (SDL.P (V2 (rx + ox) (ry + oy))) (V2 xx yy)


gameLoop :: SDL.Rectangle CInt -> SDL.Renderer -> IO ()
gameLoop rect renderer = do
    timer <- Timer.createTimer
    events <- SDL.pollEvents

    let checkKey k = any (isKeyPressed k) events 
        isEscPressed = checkKey SDL.KeycodeEscape
        isLeftPressed = checkKey SDL.KeycodeLeft
        isRightPressed = checkKey SDL.KeycodeRight
        isUpPressed = checkKey SDL.KeycodeUp
        isDownPressed = checkKey SDL.KeycodeDown
        newRect = if isLeftPressed
                  then move rect (V2 (-5) 0)
                  else if isRightPressed
                  then move rect (V2 5 0)
                  else if isUpPressed 
                  then move rect (V2 0 (-5))
                  else if isDownPressed
                  then move rect (V2 0 5)
                  else rect


    SDL.rendererDrawColor renderer SDL.$= V4 255 0 0 0   
    print newRect            
    SDL.fillRect renderer (Just newRect)
    SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 0
    SDL.clear renderer
    SDL.present renderer

    unless isEscPressed (gameLoop newRect renderer)

isKeyPressed :: SDL.Keycode -> SDL.Event -> Bool
isKeyPressed key event =
    case SDL.eventPayload event of
        SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == key
        _ -> False

