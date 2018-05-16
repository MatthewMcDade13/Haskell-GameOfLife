{-# LANGUAGE PatternSynonyms #-}

module Game (
    createContext,
    setGameContext,
    Context,
    pattern Context,
    GameContext (..)
) where

import qualified SDL
import qualified Timer as T

pattern Context w r t g = RContext w r t g

data Context = RContext { window :: SDL.Window, renderer :: SDL.Renderer, tickTimer :: T.Timer, game :: GameContext }
data GameContext = GameContext Bool


createContext :: SDL.Window -> SDL.Renderer -> T.Timer -> Maybe GameContext -> Context
createContext win ren timer (Just ctx) = RContext win ren timer ctx
createContext win ren timer Nothing = RContext win ren timer (GameContext True) 

setGameContext :: Context -> GameContext -> Context
setGameContext (Context win ren timer _) gtx = Context win ren timer gtx