module Game where

import Brick
import qualified Graphics.Vty as V
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (border)

import Grid (Grid)
import qualified UI
import UI (gridUI)

data Scene = PlayScene | SettingsScene deriving (Show, Eq)
-- Game State
data GameState = GameState {
    gameScene :: Scene,
    gameStartTime :: Maybe Int,
    gameGrid :: Grid
} deriving (Show)

-- UI renderer
drawUI :: GameState -> [Widget ()]
drawUI state = [ 
     center $ gridUI $ gameGrid state 
    ]

-- Event handler
handleEvent :: BrickEvent () e -> EventM () GameState ()
-- handleEvent (VtyEvent (V.EvKey V.KUp []))         = modify (\s -> s { moves = moves s + 1 })
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent _                                     = return () -- Ignore all other keys


appConfig :: App GameState e ()
appConfig = App { 
    appDraw = drawUI
    , appChooseCursor = neverShowCursor      -- Hides the blinking terminal cursor
    , appHandleEvent = handleEvent 
    , appStartEvent = return ()            -- Do nothing on startup
    , appAttrMap = const $ attrMap V.defAttr [] -- Default colors
    }
