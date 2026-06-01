{-# LANGUAGE LambdaCase #-} 
module Scene.SettingsScene where

import UI (WN)
import State (GameState (..), SettingsState (..))
import Save (TileType (..), Settings (..))

import Data.List.Index (imap)
import Brick
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Border
import ColorScheme (ColorScheme (..), fringe, row, col)
import qualified Graphics.Vty as V
import qualified Data.Ord as Ord 

-- Each row has a label, a getter and setter 
data SettingValue = VInt Int | VHz Int | VColorScheme ColorScheme | VTileType TileType
type SettingRow = (String, SettingsState -> SettingValue, SettingsState -> SettingValue -> SettingsState)

-- Takes the state and checks if the current row is the selected row, applying a highlight if so
_applyHighlightIfOnRow :: SettingsState -> Int -> Widget WN -> Widget WN
_applyHighlightIfOnRow state rowNum w = let selectedRow = settingsRowHover state
                                        in if rowNum == selectedRow
                                            then modifyDefAttr (\_ -> V.black `on` V.white) w
                                            else w

settingsIncrement :: SettingValue -> SettingValue
settingsIncrement (VInt v) = VInt (v + 1)
settingsIncrement (VHz v) = VHz (Ord.clamp (15, 240) (v * 2))
-- Color scheme enum
settingsIncrement (VColorScheme (ColorScheme name _)) | name == "fringe" = VColorScheme row
                                            | name == "row" = VColorScheme col
                                            | name == "col" = VColorScheme fringe
-- Tile type enum
settingsIncrement (VTileType v) | v == Fill = VTileType Border
                                | v == Border = VTileType Fill
settingsIncrement v = v
                                
settingsDecrement :: SettingValue -> SettingValue
-- Num can't be less than 2
settingsDecrement (VInt 2) = VInt 2
settingsDecrement (VInt v) = VInt (v - 1)
settingsDecrement (VHz v) = VHz (Ord.clamp (15, 240) (v `div` 2))
-- Color scheme enum
settingsDecrement (VColorScheme (ColorScheme name _)) | name == "fringe" = VColorScheme col
                                            | name == "col" = VColorScheme row
                                            | name == "row" = VColorScheme fringe
-- Tile type enum
settingsDecrement (VTileType v) | v == Fill = VTileType Border
                                | v == Border = VTileType Fill
settingsDecrement v = v

settingRows :: [SettingRow]
settingRows = [ -- I just gotta say haskell gotta be the most unsightreadable language I've ever used
        ("Rows", VInt . fst . settingsGridSize, \s -> \case 
            (VInt v) -> s { settingsGridSize = (v, snd (settingsGridSize s)) }
            _ -> s
        ), -- Int
        ("Columns", VInt . snd . settingsGridSize, \s -> \case
            (VInt v) -> s { settingsGridSize = (fst (settingsGridSize s), v) }
            _ -> s
        ), -- Int
        ("Color Scheme", VColorScheme . settingsColorScheme, \s -> \case
            (VColorScheme v) -> s { settingsColorScheme = v }
            _ -> s
        ), -- Enum
        ("Tile Type", VTileType . settingsTileType, \s -> \case
            (VTileType v) -> s { settingsTileType = v }
            _ -> s
        ), -- Enum
        ("Refresh Rate", VHz . settingsRefreshRate, \s -> \case 
            (VHz v) -> s { settingsRefreshRate = v } 
            _ -> s
        ) -- Enum
    ]
    
_drawSettingValue :: SettingValue -> Widget WN
_drawSettingValue (VInt v) = str $ "- " ++ show v ++ " +"
_drawSettingValue (VHz v) = str $ "- " ++ show v ++ " +"
_drawSettingValue (VColorScheme cs) = str $ "← " ++ show cs ++  " →" 
_drawSettingValue (VTileType tt) = str $ "← " ++ show tt ++ " →" 

_drawSettingRow :: SettingsState -> Int -> SettingRow -> Widget WN
_drawSettingRow ss rowIndex (name, getter, _) = 
    let value = getter ss
        isHovered = settingsRowHover ss == rowIndex
        attr = if isHovered then bg V.brightBlue else V.defAttr
    in modifyDefAttr (const attr) $ str name <+> fill ' ' <+> _drawSettingValue value 


-- Changing anything in this will change the settings state but will not change the grid until we shuffle (reset)
draw :: GameState -> Widget WN
draw gs = 
    let ss = gameSettings gs
    in center $ border $ padLeftRight 2 $ vLimit (1 + length settingRows) $ hLimit 25 $ vBox $
        hCenter (modifyDefAttr (const $ V.withStyle (fg V.brightBlue) V.bold) $ str "Settings")
        : imap (_drawSettingRow ss) settingRows
