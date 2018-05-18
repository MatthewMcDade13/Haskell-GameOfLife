module Keyboard (
    getKeyPressed,
    isKeyPressedLive,
    isKeyPressed,
    isEvent
) where

import qualified SDL

getKeyPressed :: SDL.Event -> Maybe SDL.Keycode
getKeyPressed SDL.Event { SDL.eventPayload = ep } =
    case ep of 
        SDL.KeyboardEvent kbe ->
            if SDL.keyboardEventKeyMotion kbe == SDL.Pressed
            then Just $ SDL.keysymKeycode $ SDL.keyboardEventKeysym kbe
            else Nothing
        _ -> Nothing

isKeyPressedLive :: SDL.Scancode -> IO Bool
isKeyPressedLive sc = do
    SDL.pumpEvents
    SDL.getKeyboardState >>= (\f -> return (f sc))

isKeyPressed :: SDL.Keycode -> SDL.Event -> Bool
isKeyPressed key event =
    case SDL.eventPayload event of
        SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == key
        _ -> False

isEvent :: SDL.Event -> SDL.EventPayload -> Bool
isEvent (SDL.Event { SDL.eventPayload = ep }) targetType = ep == targetType
