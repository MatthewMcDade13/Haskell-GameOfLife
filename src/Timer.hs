{-# LANGUAGE BangPatterns #-}
module Timer (
    createTimer,
    getElapsedTime
) where

import qualified SDL
import qualified SDL.Time as SDL

newtype Timer = Timer Float

createTimer :: IO Timer
createTimer = do
    !currTime <- SDL.ticks
    return (Timer (fromIntegral currTime))

getElapsedTime :: Timer -> IO Float
getElapsedTime (Timer !time) = do
    !currTime <- SDL.ticks
    let 
        delta = (fromIntegral currTime) - time 
        in return (delta * 0.001)

