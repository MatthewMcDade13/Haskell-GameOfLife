module GameLoop (
    gameLoop
) where

import qualified Timer
import Control.Monad (unless, when)
import SDL.Vect
import qualified SDL
import Foreign.C.Types
import System.IO
import Control.Monad.State
import Control.Monad.IO.Class

data Context = Context SDL.Window SDL.Renderer

move :: SDL.Rectangle CInt -> V2 CInt -> SDL.Rectangle CInt
move (SDL.Rectangle (SDL.P pos) size) offset = 
    SDL.Rectangle (SDL.P (pos + offset)) (size)



move' :: V2 CInt -> StateT (SDL.Rectangle CInt) IO ()
move' offset = state $ \r -> ((), move r offset)

defaultRect :: SDL.Rectangle CInt
defaultRect = SDL.Rectangle (SDL.P (V2 0 0)) (V2 0 0)

proccessEvents' :: StateT (SDL.Rectangle CInt) IO ()
proccessEvents' = do
    left <- liftIO (isKeyPressed' SDL.ScancodeLeft)
    right <- liftIO (isKeyPressed' SDL.ScancodeRight)
    up <- liftIO (isKeyPressed' SDL.ScancodeUp)
    down <- liftIO (isKeyPressed' SDL.ScancodeDown)
    if left then move' (V2 (-1) 0) else noop
    if right then move' (V2 1 0) else noop
    if up then move' (V2 0 (-1)) else noop
    if down then move' (V2 0 1) else noop

    where noop = move' (V2 0 0)
    


gameLoop :: SDL.Rectangle CInt -> SDL.Renderer -> IO ()
gameLoop rect renderer = do
    timer <- Timer.createTimer
    events <- SDL.pollEvents

    (_, newRect) <- runStateT proccessEvents' rect

    let isEscPressed = any (isKeyPressed SDL.KeycodeEscape) events

    print newRect
                  
    SDL.clear renderer
    SDL.rendererDrawColor renderer SDL.$= V4 255 0 0 0       
    SDL.fillRect renderer (Just newRect)
    SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 0
    SDL.present renderer

    unless isEscPressed (gameLoop newRect renderer)

    where 
        proccessEvents acc e = 
            case getKeyPressed e of
                Just SDL.KeycodeLeft -> move acc (V2 (-5) 0)
                Just SDL.KeycodeRight -> move acc (V2 5 0)
                Just SDL.KeycodeUp -> move acc (V2 0 (-5))
                Just SDL.KeycodeDown -> move acc (V2 0 5)
                _ -> acc



getKeyPressed :: SDL.Event -> Maybe SDL.Keycode
getKeyPressed SDL.Event { SDL.eventPayload = ep } =
    case ep of 
        SDL.KeyboardEvent kbe ->
            if SDL.keyboardEventKeyMotion kbe == SDL.Pressed
            then Just $ SDL.keysymKeycode $ SDL.keyboardEventKeysym kbe
            else Nothing
        _ -> Nothing

isKeyPressed' :: SDL.Scancode -> IO Bool
isKeyPressed' sc = do
    SDL.pumpEvents
    SDL.getKeyboardState >>= (\f -> return (f sc))

isKeyPressed :: SDL.Keycode -> SDL.Event -> Bool
isKeyPressed key event =
    case SDL.eventPayload event of
        SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == key
        _ -> False

