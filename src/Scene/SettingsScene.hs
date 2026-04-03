module Scene.SettingsScene where

import UI (WN)
import State (GameState)

import Brick

-- Changing anything in this will change the settings state but will not change the grid until we shuffle (reset)
draw :: GameState -> Widget WN
draw _ = str "settings"


