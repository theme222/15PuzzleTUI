module State where

import Grid (Grid)
import Ipair (Ipair)
import ColorScheme (ColorScheme)

import Data.Time.Clock (UTCTime)

data Scene = PlayScene | SettingsScene deriving (Show, Eq)
data TileType = Fill | Border deriving (Show, Eq)
data CustomEvent = Tick UTCTime

data SettingsState = SettingsState {
    settingsTileType :: TileType,
    settingsColorScheme :: ColorScheme, -- Pass through both the value and the position (just in case tho idk)
    settingsGridSize :: Ipair, -- Rows, columns
    settingsRowHover :: Int
}

data PlayState = PlayState {
    playGrid :: Grid,
    playIsRunning :: Bool,
    playIsFinished :: Bool,
    playTimerMs :: Int,
    playLastTickTime :: Maybe UTCTime
}

-- Game State
data GameState = GameState {
    gameScene :: Scene,
    gameSettings :: SettingsState,
    gamePlay :: PlayState -- What a name btw
} 