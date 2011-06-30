module Init{- ( AppConfig, initEnv )-}
    where

import System.Random

import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Graphics.UI.SDL.TTF
import qualified Graphics.UI.SDL.Framerate as FPS

import Timer
import Settings
import State
-- import Misc

startMap :: IO Map
startMap = do
    ticks <- getTicks
    return Map { applePos = [(22,22)], bombPos = [(20,20,0)], explosionPos = [(15,15,8)], mapTimer = defaultTimer, seed = mkStdGen (fromIntegral ticks) }

startSnake :: Snake
startSnake  = Snake { 
    pos         = [(10,10),(9,10),(8,10)],
    dir         = East,
    grow        = 0, 
    color       = Color 0xff 0x0f 0x1f,
    colorShadow = Color 0xaf 0x00 0x0f
}

startSnake2 :: Snake
startSnake2 = Snake {
    pos         = [(28,10),(29,10),(30,10)],
    dir         = West,
    grow        = 0,
    color       = Color 0x00 0x5f 0xff,
    colorShadow = Color 0x00 0x1f 0xaf
}

-- |
startSprites :: IO Sprites
startSprites = do
    fontBig_  <- openFont "data/Battlev2p.ttf" 72
    font_     <- openFont "data/Battlev2p.ttf" 14
    pause_    <- renderTextBlended fontBig_ "PAUSE" textColorBig
    
    apple_       <- load "data/apple.png"
    applePoison_ <- load "data/apple_poison.png"
    bomb_        <- load "data/bomb.png"
    explosion_   <- load "data/explosion.png"
    
    return Sprites { fontBig = fontBig_, font = font_, textPause = pause_, apple = apple_, applePoison = applePoison_, bomb = bomb_,
        explosion = explosion_ }


-- |Initialize the screen and start timer
initEnv ::IO (AppConfig, AppData)
initEnv = do
    screen_ <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Haskell Snake" []
    
    timer_   <- start defaultTimer
    gameMap_ <- startMap
    sprites_ <- startSprites
    
    frManager <- FPS.new 
    FPS.init frManager
    
    return (AppConfig screen_ frManager, AppData Play sprites_ gameMap_ [startSnake,startSnake2] timer_ 0x00)


