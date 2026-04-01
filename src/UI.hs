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
import ColorScheme (ColorScheme, ColorGetter)

data WidgetName = Tilename Ipair | Background deriving (Eq, Ord, Show)
type WN = WidgetName

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

_applyInlineStyle :: ColorGetter -> Int -> Widget WN -> Widget WN
_applyInlineStyle cg val = modifyDefAttr (\_ -> cg val) 

_applyPadding :: Int -> Widget WN -> Widget WN
_applyPadding len w = 
    let  
        leftPadding = padLeft $ Pad $ _getPaddingSize 'l' len
        rightPadding = padRight $ Pad $ _getPaddingSize 'r' len
    in
        leftPadding $ rightPadding $ padTopBottom 2 w

numberTileUI :: Grid -> ColorGetter -> Ipair -> Widget WN
numberTileUI g cg pos =
    let inArr = Grid.gridArr g
        val = inArr ! pos
        display = _standardizeText $ show val
    in clickable 
        (Tilename pos) 
            (_applyInlineStyle cg val $ _applyPadding (length display) $ str display)
    
-- recursively build the columns
_genGridColUI :: Grid -> ColorGetter -> Ipair -> Widget WidgetName  
_genGridColUI g cg pos = 
    let size = Grid.gridSize g
        inArr = Grid.gridArr g
        (_, colCount) = size
        (_, currentCol) = pos
    in if currentCol == colCount - 1 then numberTileUI g cg pos
       else numberTileUI g cg pos <+> _genGridColUI g cg (pos ~+ (0, 1))
    
-- recursively build the rows 
_genGridRowUI :: Grid -> ColorGetter -> Ipair -> Widget WN
_genGridRowUI g cg pos  = 
    let size = Grid.gridSize g
        (rowCount, _) = size
        (currentRow, _) = pos
    in if currentRow == rowCount - 1 then _genGridColUI g cg pos
    else _genGridColUI g cg pos <=> _genGridRowUI g cg (pos ~+ (1, 0))
    
gridUI :: Grid -> ColorGetter -> Widget WN 
gridUI g cg = _genGridRowUI g cg (0, 0)