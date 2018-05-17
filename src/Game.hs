{-# LANGUAGE PatternSynonyms #-}

module Game (
    createContext,
    createGrid,
    setGrid,
    getGrid,
    setTimer,
    Context,
    Grid, 
    pattern Context,
    pattern Grid
) where

import qualified SDL
import qualified Timer as T
import qualified Data.Vector as V
import qualified System.Random as R

pattern Context w r t g = RContext w r t g
pattern Grid v = RGrid v

newtype Grid = RGrid (V.Vector Bool)

data Context = RContext { window :: SDL.Window, renderer :: SDL.Renderer, tickTimer :: T.Timer, grid :: Grid }


createContext :: SDL.Window -> SDL.Renderer -> T.Timer -> Maybe Grid -> Context
createContext win ren timer (Just ctx) = RContext win ren timer ctx
createContext win ren timer Nothing = RContext win ren timer (RGrid (V.replicate 50 False)) 

setGrid :: Context -> Grid -> Context
setGrid (RContext win ren timer _) grid = RContext win ren timer grid

setTimer :: Context -> T.Timer -> Context
setTimer (RContext win ren _ g) t = RContext win ren t g

getGrid :: Context -> Grid
getGrid (RContext _ _ _ game) = game

createGrid :: Int -> Int -> Maybe R.StdGen -> Grid
createGrid width height Nothing = RGrid (V.replicate (width * height) False)
createGrid width height (Just gen) = RGrid (V.fromList $ randGrid (width * height) [] gen)
    where
        randGrid 0 l _ = l 
        randGrid n l gen =
            let (val, newGen) = (R.random gen :: (Bool, R.StdGen))
            in randGrid (n - 1) (val:l) newGen

