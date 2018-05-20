module Util (
    makeRect,
    getRectPos,
    getRectSize,
    sqVec
) where
    
import qualified SDL
import SDL.Vect
import Foreign.C.Types

makeRect :: V2 CInt -> V2 CInt -> SDL.Rectangle CInt
makeRect pos size = SDL.Rectangle (SDL.P pos) (size)

getRectPos :: SDL.Rectangle CInt -> V2 CInt
getRectPos (SDL.Rectangle (SDL.P pos) _) = pos

getRectSize :: SDL.Rectangle CInt -> V2 CInt
getRectSize (SDL.Rectangle _ size) = size

-- makes vector with same with and height as given int
sqVec :: a -> V2 a
sqVec size = V2 size size
