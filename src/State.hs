module State where

import Grid (Grid)
import Ipair (Ipair)
import ColorScheme (ColorScheme)

import Data.Time.Clock (UTCTime)
import Save (Leaderboard)
import qualified Data.Vector as V

data Scene = PlayScene | SettingsScene deriving (Show, Eq)
data TileType = Fill | Border deriving (Show, Eq)
data CustomEvent = Tick UTCTime

data SettingsState = SettingsState {
    settingsTileType :: TileType,
    settingsColorScheme :: ColorScheme, -- Pass through both the value and the position (just in case tho idk)
    settingsGridSize :: Ipair, -- Rows, columns
    settingsRowHover :: Int
}

data Helper = Helper {
    helperGridVec :: V.Vector Grid,
    helperCurrentVecIdx :: Int,
    helperIsHelping :: Bool,
    helperLastRenderTime :: Maybe UTCTime
}

data PlayState = PlayState {
    playGrid :: Grid,
    playIsRunning :: Bool,
    playIsFinished :: Bool,
    playTimerMs :: Int,
    playLastTickTime :: Maybe UTCTime,
    playLeaderboard :: Leaderboard,
    playHelper :: Helper
}

data DebugState = DebugState {
    debugStr :: String
}

-- Game State
data GameState = GameState {
    gameScene :: Scene,
    gameSettings :: SettingsState,
    gamePlay :: PlayState,
    gameDebug :: DebugState
} 