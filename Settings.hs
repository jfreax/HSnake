module Settings
    where

import Graphics.UI.SDL

import Misc

screenWidth  = 640 :: Int
screenHeight = 480 :: Int
screenBpp    = 32 :: Int

bgColor = color2pixel $ Color 0xff 0xc0 0xff

player1color = Color 0xff 0x0f 0x1f
player1pixel = color2pixel player1color
player1shadow = Color 0xaf 0x00 0x0f
player1pixelShadow = color2pixel player1shadow

player2color = Color 0x00 0x5f 0xff
player2pixel = color2pixel player2color
player2shadow = Color 0x00 0x1f 0xaf
player2pixelShadow = color2pixel player2shadow


textColor = Color 10 10 10
textColorBig = Color 50 50 120

color2pixel Color { colorRed = r, colorGreen = g, colorBlue = b } = rgbColor r g b