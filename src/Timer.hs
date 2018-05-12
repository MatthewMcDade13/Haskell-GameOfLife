module Timer (
    createTimer,
    getElapsedTime
) where

import qualified SDL
import qualified SDL.Time as SDL

data Timer = Timer Float

createTimer :: IO Timer
createTimer = do
    currTime <- SDL.time
    return (Timer currTime)

getElapsedTime :: Timer -> IO Float
getElapsedTime (Timer time) = do
    currTime <- SDL.time
    return (currTime - time)


