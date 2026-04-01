module State where

import Grid (Grid)
import Ipair (Ipair)
import Graphics.Vty (Attr)
import ColorScheme (ColorScheme)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)

data Scene = PlayScene | SettingsScene deriving (Show, Eq)
data TileType = Fill | Border deriving (Show, Eq)
data CustomEvent = Tick UTCTime

data Settings = Settings {
    settingsTileType :: TileType,
    settingsColorScheme :: ColorScheme, -- Pass through both the value and the position (just in case tho idk)
    settingsGridSize :: Ipair -- Rows, columns
}

-- Game State
data GameState = GameState {
    gameScene :: Scene,
    gameGrid :: Grid,
    gameSettings :: Settings,
    gameIsRunning :: Bool,
    gameTimerMs :: Int,
    gameLastTickTime :: Maybe UTCTime
} 