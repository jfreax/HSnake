{-# OPTIONS_GHC -XPackageImports  #-}

module Keyboard ( handleKeyboard )
    where
        
import Control.Monad
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Reader

import Graphics.UI.SDL

import State
import Modify


handleKeyboard :: Event -> [Snake] -> Status -> ([Snake],Status)
handleKeyboard (KeyDown (Keysym key _ _)) snakes@(s1:s2:sx) status
    -- Control the snake
    | key == SDLK_UP    = if rD s1 == South then ret else (s1 { dir = North }:s2:sx, status)
    | key == SDLK_DOWN  = if rD s1 == North then ret else (s1 { dir = South }:s2:sx, status)
    | key == SDLK_LEFT  = if rD s1 == East  then ret else (s1 { dir = West  }:s2:sx, status)
    | key == SDLK_RIGHT = if rD s1 == West  then ret else (s1 { dir = East  }:s2:sx, status)
    | key == SDLK_w     = if rD s2 == South then ret else (s1:s2 { dir = North }:sx, status)
    | key == SDLK_s     = if rD s2 == North then ret else (s1:s2 { dir = South }:sx, status)
    | key == SDLK_a     = if rD s2 == East  then ret else (s1:s2 { dir = West  }:sx, status)
    | key == SDLK_d     = if rD s2 == West  then ret else (s1:s2 { dir = East  }:sx, status)
    -- Control the game
    | key == SDLK_p     = (snakes,togglePause status)
    | otherwise = ret
    where ret = (snakes,status)
handleKeyboard _ d e = (d,e)


-- |Real direction of the snake
rD :: Snake -> Direction
rD snake@Snake { pos=[_], dir=d } = d
rD snake@Snake { pos=((x,y):(x2,y2):xs), dir=_ }
    | x == x2-1 = West
    | x == x2+1 = East
    | y == y2-1 = North
    | otherwise = South
    
-- changeDirection :: [Snake] -> Direction -> [Snake]
-- changeDirection snakes direction = map (\snake@Snake { dir=dir } -> snake { dir=direction } ) snakes

-- trick :: MonadState AppData m => m () -> Bool
trick x y = foldr (\a b -> b) y [x]
