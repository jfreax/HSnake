{-# OPTIONS_GHC -XPackageImports  #-}

module Draw ( render, applySurface, applySurface' )
    where

import Control.Monad (when)
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Reader

import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Graphics.UI.SDL.Primitives
import Graphics.UI.SDL.Sprig

import Graphics.UI.SDL.TTF

import Timer
import Modify
import Settings
import State
import Misc

-- |Render all graphics
render :: AppEnv ()
render = do
    screen  <- getScreen
    status  <- getStatus
    gameMap <- getMap
    sprites <- getSprites
    snakes@(snake:snake2:sx)   <- getSnake

    liftIO $ do
        jrect <- Just `liftM` getClipRect screen

        -- Set background color
        fillRect screen jrect bgColor

        -- Draw sprites
        renderSprites screen gameMap sprites snakes

        -- Draw Score
        renderScore screen sprites snakes

        -- Show 'Pause' on screen
        when ( status == Pause ) $ do
            let height = surfaceGetHeight $ getTextPause sprites
            let width  = surfaceGetWidth  $ getTextPause sprites
            let x      = (screenWidth - width) `div` 2
            applySurface x 200 (getTextPause sprites) screen Nothing
            rectRoundFilledBlend screen (x-15) 200 (x+width+15) (200+height) 16.0 (rgbColor 100 100 100) 50

        Graphics.UI.SDL.flip screen


-- |
renderSprites :: Surface -> Map -> Sprites -> [Snake] -> IO ()
renderSprites screen gameMap sprites snakes = do
    showApple gameMap screen $ apple sprites
    showBomb  gameMap screen $ bomb sprites
    showExplosion gameMap screen $ explosion sprites
    mapM (\a -> showSnake a screen) snakes
    return ()


-- |
renderScore :: Surface -> Sprites -> [Snake] -> IO ()
renderScore screen sprites (snake:snake2:sx) = do
    let font_ = font sprites
    score <- renderTextBlended  font_ "Score: " textColor
    score1 <- renderTextBlended font_ (show $ getSnakeLength snake) player1color
    score2 <- renderTextBlended font_ (show $ getSnakeLength snake2) player2color
    vs <- renderTextBlended font_ " vs. " textColor
    
    -- TODO function 
    applySurface 10 10 score screen Nothing
    applySurface (12 + (surfaceGetWidth score)) 10 score1 screen Nothing
    applySurface (12 + (surfaceGetWidth' [score,score1])) 10 vs screen Nothing
    applySurface (12 + (surfaceGetWidth' [score,score1,vs])) 10 score2 screen Nothing
    
    return ()


-- |
showSnake :: Snake -> Surface -> IO Bool
showSnake Snake { pos=[] } _ = return False
showSnake snake@Snake { pos=((px,py):xs), dir=d, grow=g, color=c, colorShadow=cs } screen = do
    ret <- box screen Rect { rectX = x+2, rectY = y+2, rectW = x2+2, rectH = y2+2 } colShadow
    ret2 <- box screen Rect { rectX = x, rectY = y, rectW = x2, rectH = y2 } color
    if ret && xs /= [] then showSnake snake { pos=xs, dir=d, grow=g } screen
           else return ret
    where 
        color = color2pixel c
        colShadow = color2pixel cs
        x = px * 16
        y = py * 16
        x2 = x + 13
        y2 = y + 13


-- |
showApple :: Map -> Surface -> Surface -> IO Bool
showApple Map { applePos = [] } _ _ = return False
showApple map@Map { applePos = ((px,py):xs) } screen apple = do
    ret <- applySurface x y apple screen Nothing
--     aaCircle screen x y 7 color
--     col <- mapRGBA (surfaceGetPixelFormat screen) 100 255 100 255
--     filledCircle screen 10 10 7 col
    if ret && xs /= [] then showApple map { applePos=xs } screen apple
                       else return ret
    where color = rgbColor 0xff 0x00 0x00
          x = int16ToInt $ px * 16
          y = int16ToInt $ py * 16
          

-- |
showBomb :: Map -> Surface -> Surface -> IO Bool
showBomb Map { bombPos = [] } _ _ = return False
showBomb map@Map { bombPos = ((px,py,t):xs) } screen bomb = do
    ret <- applySurface x y bomb screen (Just (Rect r1 0 16 16))
    if ret && xs /= [] then showBomb map { bombPos=xs } screen bomb
                       else return ret
    where color = rgbColor 0xff 0x00 0x00
          x = int16ToInt $ px * 16
          y = int16ToInt $ py * 16
          r1 = (t`div`2)*16


-- |
showExplosion :: Map -> Surface -> Surface -> IO Bool
showExplosion Map { explosionPos = [] } _ _ = return False
showExplosion map@Map { explosionPos = ((px,py,t):xs) } screen explosion = do
    ret <- applySurface x y explosion screen (Just (Rect r1 0 16 16))
    if ret && xs /= [] then showExplosion map { explosionPos=xs } screen explosion
                       else return ret
    where color = rgbColor 0xff 0x00 0x00
          x = int16ToInt $ px * 16
          y = int16ToInt $ py * 16
          r1 = (t`div`2)*16



applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
    where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

applySurface' :: MonadIO m => Int -> Int -> Surface -> Surface -> Maybe Rect -> m Bool
applySurface' x y src dst = liftIO . applySurface x y src dst