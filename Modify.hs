{-# OPTIONS_GHC -XFlexibleContexts -XPackageImports  #-}

module Modify
    where

import Data.Word

import Control.Monad
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Reader

import Graphics.UI.SDL
import Graphics.UI.SDL.TTF
import qualified Graphics.UI.SDL.Framerate as FPS

import Timer
import State


getScreen :: MonadReader AppConfig m => m Surface
getScreen = liftM screen ask

getSnake :: MonadState AppData m => m [Snake]
getSnake = liftM snakes get

getFPSManager :: MonadReader AppConfig m => m FPS.FPSManager
getFPSManager = liftM fpsManager ask

-- getSnakeLength :: [Snake] -> Int
-- getSnakeLength [] = 0
-- getSnakeLength (Snake { pos = xs }:sx) = (length xs) + (getSnakeLength sx)

getSnakeLength :: Snake -> Int
getSnakeLength Snake { pos = xs } = length xs

putSnake :: MonadState AppData m => [Snake] -> m ()
putSnake t = modify $ \s -> s { snakes = t }

modifySnake :: MonadState AppData m => ([Snake] -> [Snake]) -> m ()
modifySnake fn = fn `liftM` getSnake >>= putSnake

modifySnakes :: MonadState AppData m => ([Snake] -> Status -> ([Snake],Status)) -> m ()
modifySnakes fn = do
    sOrg   <- getSnake
    status <- getStatus
    return (fst $ hfn sOrg status) >>= putSnake
    return (snd $ hfn sOrg status) >>= putStatus
    where hfn s s2 = fn s s2

getMap :: MonadState AppData m => m Map
getMap = liftM gameMap get

putMap :: MonadState AppData m => Map -> m ()
putMap t = modify $ \s -> s { gameMap = t }

modifyMap :: MonadState AppData m => (Map -> Map) -> m ()
modifyMap fn = fn `liftM` getMap >>= putMap

getTime :: MonadState AppData m => m Timer
getTime = liftM time get

resetTime :: Word32 -> Word32
resetTime timer = 0x00

getMapTime :: MonadState Map m => m Timer
getMapTime = liftM mapTimer get

putOldTime :: (MonadIO m, MonadState AppData m) => t -> m ()
putOldTime t = do
    time  <- liftIO . getTimerTicks =<< getTime
    modify $ \s -> s { oldTime = time }

getOldTime :: MonadState AppData m => m Word32
getOldTime = liftM oldTime get

modifyOldTime :: (MonadState AppData m, MonadIO m) => (Word32 -> Word32) -> m ()
modifyOldTime fn = fn `liftM` getOldTime >>= putOldTime

getStatus :: MonadState AppData m => m Status
getStatus = liftM status get

putStatus :: MonadState AppData m => Status -> m ()
putStatus t = modify $ \s -> s { status = t }

modifyStatus :: MonadState AppData m => (Status -> Status) -> m ()
modifyStatus fn = fn `liftM` getStatus >>= putStatus

getSprites :: MonadState AppData m => m Sprites
getSprites = liftM sprites get

getTextPause :: Sprites -> Surface
getTextPause Sprites { textPause = tp } = tp

