{-# LANGUAGE PatternSynonyms #-}

module Game (
    update
) where

import qualified SDL
import Grid

update :: Grid -> Grid
update = id

