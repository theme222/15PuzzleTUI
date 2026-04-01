module State where

import Grid (Grid)
import Ipair (Ipair)
import Graphics.Vty (Attr)
import ColorScheme (ColorScheme)

data Scene = PlayScene | SettingsScene deriving (Show, Eq)
data TileType = Fill | Border deriving (Show, Eq)

data Settings = Settings {
    settingsTileType :: TileType,
    settingsColorScheme :: ColorScheme, -- Pass through both the value and the position (just in case tho idk)
    settingsGridSize :: Ipair -- Rows, columns
}

-- Game State
data GameState = GameState {
    gameScene :: Scene,
    gameStartTime :: Maybe Int,
    gameGrid :: Grid,
    gameSettings :: Settings
} 