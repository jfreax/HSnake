{-# OPTIONS_GHC -XPackageImports  #-}

module State
    where

import System.Random

import Data.Word
import Data.Int

import Control.Monad
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Reader

import Graphics.UI.SDL
import Graphics.UI.SDL.TTF
import qualified Graphics.UI.SDL.Framerate as FPS

import Timer


data Direction = North | South | West | East deriving (Eq, Show)
data Status = Main | Highscore | Play | Pause | Death deriving (Eq, Ord)

data Snake = Snake
    { pos         :: [(Int, Int)]
    , dir         :: Direction
    , grow        :: Int
    , color       :: Color
    , colorShadow :: Color
    }

data Map = Map
    { applePos     :: [(Int16,Int16)]
    , bombPos      :: [(Int16,Int16,Int)]
    , explosionPos :: [(Int16,Int16,Int)]
    , mapTimer     :: Timer
    , seed         :: StdGen
    }

data Sprites = Sprites
    { fontBig     :: Font
    , font        :: Font
    , textPause   :: Surface
    , apple       :: Surface
    , applePoison :: Surface
    , bomb        :: Surface
    , explosion   :: Surface
    }

data AppData = AppData
    { status     :: Status
    , sprites    :: Sprites
    , gameMap    :: Map
    , snakes     :: [Snake]
    , time       :: Timer
    , oldTime    :: Word32
    }

data AppConfig = AppConfig
    { screen     :: Surface
    , fpsManager :: FPS.FPSManager 
    }


type AppState = StateT AppData IO
type AppEnv = ReaderT AppConfig AppState


togglePause :: Status -> Status
togglePause s 
    | s == Pause = Play
    | otherwise  = Pause 
