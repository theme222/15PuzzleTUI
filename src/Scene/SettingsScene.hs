{-# LANGUAGE LambdaCase #-} 
module Scene.SettingsScene where

import UI (WN)
import State (GameState, SettingsState (..), TileType (..))

import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Border
import ColorScheme (ColorScheme, fringe, row, col)
import qualified Graphics.Vty as V

-- Takes the state and checks if the current row is the selected row, applying a highlight if so
_applyHighlightIfOnRow :: SettingsState -> Int -> Widget WN -> Widget WN
_applyHighlightIfOnRow state rowNum w = let selectedRow = settingsRowHover state
                                        in if rowNum == selectedRow
                                            then modifyDefAttr (\_ -> V.black `on` V.white) w
                                            else w
                                            
-- Each row has a label, a getter and a function to update the state
data SettingValue = VInt Int | VColorScheme ColorScheme | VTileType TileType
type SettingRow = (String, SettingsState -> SettingValue, SettingsState -> SettingValue -> SettingsState)

settingsIncrement :: SettingValue -> SettingValue
settingsIncrement (VInt v) = VInt (v + 1)

-- Color scheme enum
settingsIncrement (VColorScheme (name, _)) | name == "fringe" = VColorScheme row
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

-- Color scheme enum
settingsDecrement (VColorScheme (name, cg)) | name == "fringe" = VColorScheme col
                                            | name == "row" = VColorScheme fringe
                                            | name == "col" = VColorScheme fringe
                                            | otherwise = VColorScheme (name, cg)

-- Tile type enum
settingsDecrement (VTileType v) | v == Fill = VTileType Border
                                | v == Border = VTileType Fill

settingsDecrement v = v

_settingRows :: [SettingRow]
_settingRows = [ -- I just gotta say haskell gotta be the most unsightreadable language I've ever used
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
        ) -- Enum
    ]

-- Changing anything in this will change the settings state but will not change the grid until we shuffle (reset)
draw :: GameState -> Widget WN
draw _ = center $ border $ str "Settings"


