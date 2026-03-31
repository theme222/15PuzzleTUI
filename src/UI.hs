module UI where

import Brick
import qualified Graphics.Vty as V
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (border)
import Data.List (intersperse)
import qualified Grid
import Grid (Grid)
import Ipair
import Data.Array ((!))

data WidgetName = Tilename Ipair | Background deriving (Eq, Ord, Show)

_getPaddingSize :: Char -> Int -> Int
_getPaddingSize 'l' v = 5 - (v `div` 2)
_getPaddingSize 'r' v | even v = 6 - ceiling (fromIntegral v / 2)
                      | odd v  = 5 - (v `div` 2)
                      | otherwise = error "bruh"
_getPaddingSize _ _ = error "idc"

-- Put spaces in strings with even length to make them odd and center able
_standardizeText :: String -> String 
_standardizeText "0" = " " -- Also make 0 blank
_standardizeText text = if odd $ length text then text else intersperse ' ' text

numberTileUI :: Grid -> Ipair -> Widget WidgetName
numberTileUI g pos =
    let inArr = Grid.gridArr g
        val = inArr ! pos
        display = _standardizeText $ show val
        leftPadding = padLeft $ Pad $ _getPaddingSize 'l' $ length display
        rightPadding = padRight $ Pad $ _getPaddingSize 'r' $ length display
    in clickable (Tilename pos) (border $ leftPadding $ rightPadding $ padTopBottom 2 $ str display)
    
-- recursively build the columns
_genGridColUI :: Grid -> Ipair -> Widget UI.WidgetName  
_genGridColUI g pos = 
    let size = Grid.gridSize g
        inArr = Grid.gridArr g
        (_, colCount) = size
        (_, currentCol) = pos
    in if currentCol == colCount - 1 then numberTileUI g pos
       else numberTileUI g pos <+> _genGridColUI g (pos ~+ (0, 1))
    
-- recursively build the rows 
_genGridRowUI :: Grid -> Ipair -> Widget WidgetName  
_genGridRowUI g pos  = 
    let size = Grid.gridSize g
        (rowCount, _) = size
        (currentRow, _) = pos
    in if currentRow == rowCount - 1 then _genGridColUI g pos
    else _genGridColUI g pos <=> _genGridRowUI g (pos ~+ (1, 0))
    
gridUI :: Grid -> Widget WidgetName  
gridUI g = _genGridRowUI g (0, 0)