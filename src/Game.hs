module Game where

import Brick
import qualified Graphics.Vty as V
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (border)

import Grid (Grid)
import qualified UI
import UI (gridUI)
import Action 
import State
import Ipair (nilPair)

-- UI renderer
drawUI :: GameState -> [Widget UI.WidgetName]
drawUI state = [ 
        center $ gridUI $ gameGrid state 
    ]

-- Event handler
handleEvent :: BrickEvent UI.WidgetName e -> EventM UI.WidgetName GameState ()
-- handleEvent (VtyEvent (V.EvKey V.KUp []))         = modify (\s -> s { moves = moves s + 1 })
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') []))     = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'w') []))     = Action.dispatch $ action Action.Up
handleEvent (VtyEvent (V.EvKey (V.KChar 'a') []))     = Action.dispatch $ action Action.Left
handleEvent (VtyEvent (V.EvKey (V.KChar 's') []))     = Action.dispatch $ action Action.Down
handleEvent (VtyEvent (V.EvKey (V.KChar 'd') []))     = Action.dispatch $ action Action.Right
handleEvent (VtyEvent (V.EvKey V.KUp []))             = Action.dispatch $ action Action.Up
handleEvent (VtyEvent (V.EvKey V.KLeft []))           = Action.dispatch $ action Action.Left
handleEvent (VtyEvent (V.EvKey V.KDown []))           = Action.dispatch $ action Action.Down
handleEvent (VtyEvent (V.EvKey V.KRight []))          = Action.dispatch $ action Action.Right
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') []))     = Action.dispatch $ action Action.Reset
handleEvent (VtyEvent (V.EvKey (V.KChar ' ') []))     = Action.dispatch $ action Action.Reset
handleEvent (VtyEvent (V.EvKey (V.KChar 'b') []))     = Action.dispatch $ action Action.Back
handleEvent (MouseDown (UI.Tilename pos) V.BLeft _ _) = Action.dispatch $ Action Action.Point pos
handleEvent _                                         = return () -- Ignore all other keys


appConfig :: App GameState e UI.WidgetName
appConfig = App { 
    appDraw = drawUI, 
    appChooseCursor = neverShowCursor,      -- Hides the blinking terminal cursor
    appHandleEvent = handleEvent,
    appStartEvent = return (),            -- Do nothing on startup
    appAttrMap = const $ attrMap V.defAttr [] -- Default colors
}
