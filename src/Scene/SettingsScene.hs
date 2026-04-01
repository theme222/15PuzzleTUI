module Scene.SettingsScene where

import Brick
import qualified Graphics.Vty as V
import UI (WN)
import State (GameState)

draw :: GameState -> Widget WN
draw _ = str "settings"
