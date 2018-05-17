{-# LANGUAGE BangPatterns #-}
module Timer (
    createTimer,
    getElapsedTime,
    Timer
) where

import qualified SDL
import qualified SDL.Time as SDL

newtype Timer = Timer Float deriving (Show)

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


instance Num Timer where
    (Timer tl) + (Timer tr) = Timer (tl + tr)
    (Timer tl) - (Timer tr) = Timer (tl - tr)
    (Timer tl) * (Timer tr) = Timer (tl * tr)
    negate (Timer t) = Timer (negate t)
    abs (Timer t) = Timer (abs t)
    signum (Timer t) = Timer (signum t)
    fromInteger x = Timer (fromInteger x)