{-# OPTIONS_GHC -XPackageImports  #-}

module Main
    where

import System.Random

import Control.Monad (when)
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Reader

import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Graphics.UI.SDL.Primitives
import Graphics.UI.SDL.Sprig

import Graphics.UI.SDL.TTF
import qualified Graphics.UI.SDL.Framerate as FPS
import qualified Graphics.UI.SDL.TTF.General as TTFG

import Timer
import State
import Init
import Event
import Keyboard
import Settings
import Modify
import Misc
import Draw


-- |
modifyM :: MonadState s m => (s -> m s) -> m ()
modifyM act = get >>= act >>= put


main = withInit [InitEverything] $ do
    result <- TTFG.init
    if not result
        then putStrLn "Failed to init ttf"
        else do
            (env, state) <- initEnv
            runLoop env state

            TTFG.quit
            Graphics.UI.SDL.quit
            
            free' env state
            

-- | Free all surfaces
free' :: AppConfig -> AppData -> IO ()
free' = evalStateT . runReaderT ( do
    sprites <- getSprites
    liftIO $ do
        freeSurface $ textPause sprites
        freeSurface $ apple sprites
        freeSurface $ applePoison sprites
        freeSurface $ bomb sprites
        freeSurface $ explosion sprites
        
        closeFont $ fontBig sprites
        closeFont $ fontBig sprites
    )


-- |
runLoop :: AppConfig -> AppData -> IO ()
runLoop = evalStateT . runReaderT loop



{------------------------------------------------------------------------------}
{-------------------------------------Game loop--------------------------------}
{------------------------------------------------------------------------------}
loop :: AppEnv ()
loop = do
    quit <- whileEvents $ modifySnakes . handleKeyboard
    
    fpsManager <- getFPSManager
    liftIO $ do
        -- Set framelimit
        FPS.delay fpsManager

    calc
    render

    -- Read data
    snake   <- getSnake
    
    unless (quit || checkCollision snake ) loop


-- |Calculate the game logic
calc :: AppEnv ()
calc =  do
    status    <- getStatus
    time      <- liftIO . getTimerTicks =<< getTime
    oldTime   <- getOldTime
    
   -- Calc only all 200ms
    when ( status == Play && time > oldTime+200 ) $ do
        
        oldSnakes@(oldSnake:oldSnake2:_)  <- getSnake
        gameMap   <- getMap
        
        -- Move the snakes
        modifySnake  $ move . hitApple gameMap
        
        -- Delete eated apples
        modifyMap $ delApple oldSnakes
--         modifyMap $ delApple oldSnake2
        
        -- Reset timer
        modifyOldTime $ resetTime
        
        -- Add random new apples
        modifyMap $ addApple
        
        -- Animate the bombs / add
        modifyMap $ animBom . addBomb
        
        -- Animate explosion
        modifyMap $ animExplosion





-- | Add random apple
addBomb :: Map -> Map
addBomb gameMap@Map { bombPos=p, seed = ls } = gameMap { bombPos = added, seed = ss }
    where     
        added
            | fst (rand (0,50) ls) > 3 = p
            | otherwise = (intToInt16 newX, intToInt16 newY,0) : p
        ((newX,fs),(newY,ss)) = ((rand (0,30) ls),(rand (0,40) fs))


-- |
animBom :: Map -> Map
animBom gameMap@Map { bombPos = bp, explosionPos = ep, seed = ls } = gameMap { bombPos = fst calced, explosionPos = snd calced }
    where
        calced = changed bp ([],ep)
        changed [] acc = acc
        changed ((x,y,t):xs) acc@(ab,ae)
            | t+1 > 9   = changed xs (ab,(x,y,7):((addExp True ls)++ae))
            | otherwise = changed xs ((x,y,t+1):ab,ae)
            where 
                addExp False _  = []
                addExp _  curSeed = let ((newX,fs),(newY,ss),(newB,rs),(newT,ts)) = (rand (-1,1) curSeed, rand (-1,1) fs, rand (0,2) ss, rand (0,7) rs) 
                                    in (x + (intToInt16 newX),y + (intToInt16 newY),newT) : 
                                    addExp (if newB >= 1 then False else True ) ts


-- |
animExplosion :: Map -> Map
animExplosion gameMap@Map { explosionPos = bp } = gameMap { explosionPos = (changed bp []) }
    where
        changed [] acc = acc
        changed ((x,y,t):xs) acc
            | t+1 > 18  = changed xs acc
            | otherwise = (x,y,t+1) : changed xs acc


-- |Return a new snake if it hits an apple (grow+1)
hitApple :: Map -> [Snake] -> [Snake]
hitApple gameMap [] = []
hitApple gameMap@Map { applePos=[] } snakes = snakes
hitApple gameMap@Map { applePos=xr } (snake@Snake { pos=((x,y):xs), grow=g }:sx) = 
    snake { grow = newG } : hitApple gameMap sx
        where newG = foldl (\acc (a,b) -> if fromIntegral a /= x || fromIntegral b /= y then acc else acc+1 ) g xr


checkCollision :: [Snake] -> Bool
checkCollision snakes = rec snakes snakes
    where
        rec [] _ = False
        rec (s@Snake{pos=opos@((x,y):xopos)}:sx) acc = any (\Snake{pos=poss} -> any (\(x2,y2) -> x == x2 && y == y2) poss ) filtered || rec sx acc
            where filtered = s{pos=xopos} : filter (\Snake{pos=fpos} -> fpos /= opos ) acc



move :: [Snake] -> [Snake]
move [] = []
move (snake@Snake { pos=((x,y):xs), dir=d, grow=g }:sx) = snake { pos=(x'', y''):(x,y):rest, grow=newG } : move sx
        where
            x'  = x + deltaH
            y'  = y + deltaV
            x'' = if x' < 0 then maxH else if x' > maxH then 0 else x'
            y'' = if y' < 0 then maxV else if y' > maxV then 0 else y'
            deltaH = if d == East then 1 else if d == West then -1 else 0
            deltaV = if d == South then 1 else if d == North then -1 else 0
            maxH = screenWidth  `div` 16 -1
            maxV = screenHeight `div` 16 -1
            newG = if g <= 1 then 0 else g-1
            rest 
                | null xs = []
                | g > 0 = xs
                | otherwise = Prelude.init xs


-- | Add random apple
addApple :: Map -> Map
addApple gameMap@Map { applePos=p, seed = ls } = gameMap { applePos = added, seed = ss }
    where     
        added
            | fst (rand (0,50) ls) > 3 = p
            | otherwise = (intToInt16 newX, intToInt16 newY) : p
        rand :: (Int,Int) -> StdGen -> (Int,StdGen)
        rand x g = randomR x g
        ((newX,fs),(newY,ss)) = ((rand (0,30) ls),(rand (0,40) fs))


-- | Delete hitted apple
delApple :: [Snake] -> Map -> Map
delApple [] gameMap = gameMap
delApple _ gameMap@Map { applePos=[] } = gameMap
delApple (Snake { pos=((x,y):_) }:sx) gameMap@Map { applePos=xr } = 
    delApple sx (gameMap { applePos=(filter (\(a,b) -> fromIntegral a /= x || fromIntegral b /= y ) xr) })




