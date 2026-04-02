module ColorScheme where

import Graphics.Vty (Attr)
import qualified Graphics.Vty as V
import Grid (Grid)
import qualified Grid
import Brick

type ColorScheme = Grid -> ColorGetter 
type ColorGetter = Int -> V.Color
type ColorMap = [V.Color]

-- Thx gemini
hslToRgb :: Float -> Float -> Float -> V.Color
hslToRgb h s l = V.rgbColor (round $ r * 255) (round $ g * 255) (round $ b * 255)
  where
    c = (1 - abs (2 * l - 1)) * s
    x = c * (1 - abs (fmod (h / 60) 2 - 1))
    m = l - c / 2

    (r', g', b')
      | h < 60    = (c, x, 0)
      | h < 120   = (x, c, 0)
      | h < 180   = (0, c, x)
      | h < 240   = (0, x, c)
      | h < 300   = (x, 0, c)
      | otherwise = (c, 0, x)

    r = r' + m
    g = g' + m
    b = b' + m

    -- Helper for float modulo
    fmod _x _y = _x - fromIntegral (floor (_x / _y)) * _y

-- Total colors can be found by doing (rows-1 + cols-1)
genRainbowColorMap :: Int -> ColorMap
genRainbowColorMap n = 
    let step = 360.0 / fromIntegral n
    in [ hslToRgb (step * fromIntegral i) 1.0 0.4 | i <- [0 .. (n - 1)] ]

_getFringeMapIndex :: Grid -> Int -> Int 
_getFringeMapIndex g val = 
    let (row, col) = Grid.getOriginalPos val g
    in  if row <= col then row * 2 
        else col * 2 + 1

fringe :: ColorScheme
fringe g value = 
    let (rows, cols) = Grid.gridSize g
        colorMap = genRainbowColorMap (rows + cols - 2)
    in (colorMap !! _getFringeMapIndex g value) 

    -- in V.withStyle (bg (colorMap !! _getFringeMapIndex g value)) V.bold

applyCGAsFg :: ColorGetter -> Int -> Attr
applyCGAsFg _ 0 = V.defAttr 
applyCGAsFg cg val = V.withStyle (fg $ cg val) V.bold

applyCGAsBg :: ColorGetter -> Int -> Attr
applyCGAsBg _ 0 = V.defAttr 
applyCGAsBg cg val = V.withStyle (bg $ cg val) V.bold