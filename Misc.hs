module Misc
    where

import System.Random

import Graphics.UI.SDL
import Graphics.UI.SDL.Image

import Data.Bits
import Data.Int
import Data.Word


rgbColor::Word8 -> Word8 -> Word8 -> Pixel
rgbColor r g b = Pixel (shiftL (fi r) 24 .|. shiftL (fi g) 16 .|. shiftL (fi b) 8 .|. (fi 255))
    where fi a = fromIntegral a

mapRGB' :: Surface -> Word8 -> Word8 -> Word8 -> IO Pixel
mapRGB' = mapRGB . surfaceGetPixelFormat

setColorKey' :: Maybe (Word8, Word8, Word8) -> Surface -> IO Surface
setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = mapRGB' surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

--loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
--loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

surfaceGetWidth' :: [Surface] -> Int
surfaceGetWidth' xs = foldl (\acc x -> acc + (surfaceGetWidth x)) 0 xs 

intToInt16 :: Int -> Int16
intToInt16 x = read $ show x

int16ToInt :: Int16 -> Int
int16ToInt x = read $ show x

rand :: (Int,Int) -> StdGen -> (Int,StdGen)
rand x g = randomR x g