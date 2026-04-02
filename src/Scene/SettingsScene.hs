module Scene.SettingsScene where

import UI (WN)
import State (GameState)

import Brick

draw :: GameState -> Widget WN
draw _ = str "settings"
