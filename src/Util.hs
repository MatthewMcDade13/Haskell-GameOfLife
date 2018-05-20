module Util (
    makeRect,
    sqVec
) where
    
import qualified SDL
import SDL.Vect
import Foreign.C.Types

makeRect :: V2 CInt -> V2 CInt -> SDL.Rectangle CInt
makeRect pos size = SDL.Rectangle (SDL.P pos) (size)

-- makes vector with same with and height as given int
sqVec :: CInt -> V2 CInt
sqVec size = V2 size size
