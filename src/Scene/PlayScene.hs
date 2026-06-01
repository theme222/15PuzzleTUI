module Scene.PlayScene where

import qualified Grid
import Ipair
import UI (WN, WidgetName (..))
import ColorScheme (ColorGetter, applyCGAsFg, applyCGAsBg, ColorScheme (..))
import State (GameState(..), PlayState(..), SettingsState(..), DebugState (..))
import Save ( TileType(..), Leaderboard(..), Settings (..) )

import Brick
import Data.List (intersperse)
import Data.Array.Unboxed ((!))
import Brick.Widgets.Center
import Text.Printf (printf)
import Brick.Widgets.Border (border)
import Brick.Widgets.Border.Style (unicodeBold)
import qualified Graphics.Vty as V
import Data.List.Index (imap)

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

_applyInlineStyle :: TileType -> ColorGetter -> Int -> Widget WN -> Widget WN
_applyInlineStyle Fill cg val = modifyDefAttr (\_ -> applyCGAsBg cg val)
_applyInlineStyle Border cg val = modifyDefAttr (\_ -> applyCGAsFg cg val)

_applyTileType :: TileType -> Widget WN -> Widget WN
_applyTileType tt w = if tt == Border then withBorderStyle unicodeBold $ border w else w

_applyPadding :: TileType -> Int -> Widget WN -> Widget WN
_applyPadding tt len w =
    let paddingOffset = if tt == Border then -1 else 0
        leftPadding = padLeft $ Pad $ _getPaddingSize 'l' len + paddingOffset
        rightPadding = padRight $ Pad $ _getPaddingSize 'r' len + paddingOffset
    in leftPadding $ rightPadding $ padTopBottom (2 + paddingOffset) w

numberTileUI :: GameState -> Ipair -> Widget WN
numberTileUI state pos =
    let grid = playGrid . gamePlay $ state
        val = Grid.getTile grid pos
        display = _standardizeText $ show val
        cg = (colorSchemeFunc . settingsColorScheme) (gameSettings state) grid
        tt = settingsTileType (gameSettings state)
    in clickable
        (Tilename pos)
            ( _applyInlineStyle tt cg val $ _applyTileType tt $ _applyPadding tt (length display) $ str display)

-- recursively build the columns
_genGridColUI :: GameState -> Ipair -> Widget WN
_genGridColUI state pos =
    let size = (Grid.gridSize . playGrid . gamePlay) state
        (_, colCount) = size
        (_, currentCol) = pos
    in if currentCol == colCount - 1 then numberTileUI state pos
       else numberTileUI state pos <+> _genGridColUI state (pos ~+ (0, 1))

-- recursively build the rows 
_genGridRowUI :: GameState -> Ipair -> Widget WN
_genGridRowUI state pos  =
    let size = (Grid.gridSize . playGrid . gamePlay) state
        (rowCount, _) = size
        (currentRow, _) = pos
    in if currentRow == rowCount - 1 then _genGridColUI state pos
    else _genGridColUI state pos <=> _genGridRowUI state (pos ~+ (1, 0))

gridUI :: GameState -> Widget WN
gridUI state = _genGridRowUI state (0, 0)

getFormattedTime :: Int -> String
getFormattedTime totalMs =
    let seconds = (totalMs `div` 1000) `mod` 60
        minutes = totalMs `div` 60000
        ms      = totalMs `mod` 1000
    in printf "%02d:%02d.%03d" minutes seconds ms

gameStatUI :: GameState -> Widget WN
gameStatUI state =
    let totalMs = playTimerMs (gamePlay state)
        moveCount = (Grid.gridMoveCount . playGrid . gamePlay) state

        timeStr = getFormattedTime totalMs
    in border $ str ("Current Time: " ++ timeStr ++ " Current moves: " ++ show moveCount)

_inlineControlStyle :: Widget WN -> Widget WN
_inlineControlStyle = modifyDefAttr (\_ -> fg V.red)

controlsUI :: Widget WN
controlsUI = border $ padLeftRight 4 $ padTopBottom 1 $ hLimit 23 $ modifyDefAttr (`V.withStyle` V.bold) (
        (hCenter (str "Controls") <=> str " ") <=>
        (str "Move up:    " <+> _inlineControlStyle (str "w, ⬆")) <=>
        (str "Move down:  " <+> _inlineControlStyle (str "s, ⬇")) <=>
        (str "Move left:  " <+> _inlineControlStyle (str "a, ⬅")) <=>
        (str "Move right: " <+> _inlineControlStyle (str "d, ➡")) <=>
        (str "Reset:      " <+> _inlineControlStyle (str "r, <space>")) <=>
        (str "Settings:   " <+> _inlineControlStyle (str "m")) <=>
        (str "Help:       " <+> _inlineControlStyle (str "h")) <=>
        (str "Quit:       " <+> _inlineControlStyle (str "q"))
    )

_formatLeaderboardPos :: Int -> Int -> Widget WN
_formatLeaderboardPos index item =
    str (show (index + 1) ++ ". " ++ getFormattedTime item)

leaderboardUI :: GameState -> Widget WN
leaderboardUI state =
    let leaderboard = (playLeaderboard . gamePlay) state
        leaderboardEntries = vBox $ imap _formatLeaderboardPos (leaderboardRankings leaderboard)
    in  border $ padLeftRight 4 $ padTopBottom 1 $ hLimit 23 $
        hCenter (str ("Fastest Times For " ++ show (leaderboardSize leaderboard)) <=> str " ") <=> leaderboardEntries

debugUI :: GameState -> Widget WN
debugUI state = str $ (debugStr . gameDebug) state

draw :: GameState -> Widget WN
draw state = center $ (padLeftRight 4 (gridUI state <=> gameStatUI state) <+> (controlsUI <=> leaderboardUI state)) <=> debugUI state
