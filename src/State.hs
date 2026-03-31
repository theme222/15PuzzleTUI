module State where

import Grid (Grid)
import Data.Maybe (Maybe)

data Scene = PlayScene | SettingsScene deriving (Show, Eq)

-- Game State
data GameState = GameState {
    gameScene :: Scene,
    gameStartTime :: Maybe Int,
    gameGrid :: Grid
} deriving (Show)