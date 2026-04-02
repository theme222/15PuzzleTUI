module Game where

import Brick
import qualified Graphics.Vty as V
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (border)

import Grid (Grid)
import qualified UI 
import qualified Scene.PlayScene as PlayScene
import Action 
import State
import qualified State as Scene
import qualified Scene.SettingsScene as SettingsScene
import Control.Monad (when)
import Data.Time.Clock (diffUTCTime)

-- UI renderer
drawUI :: GameState -> [Widget UI.WidgetName]
drawUI state = 
    let currentDrawer = case gameScene state of 
            Scene.PlayScene -> PlayScene.draw
            Scene.SettingsScene -> SettingsScene.draw
    in [ currentDrawer state ]

-- Event handler
handleEvent :: BrickEvent UI.WidgetName CustomEvent -> EventM UI.WidgetName GameState ()
---- Key events ----
handleEvent (VtyEvent (V.EvKey (V.KChar c) [])) | c == 'q' = halt
                                                | c == 'w' = Action.dispatch $ action Action.Up
                                                | c == 'a' = Action.dispatch $ action Action.Left
                                                | c == 's' = Action.dispatch $ action Action.Down
                                                | c == 'd' = Action.dispatch $ action Action.Right
                                                | c == 'r' = Action.dispatch $ action Action.Reset
                                                | c == ' ' = Action.dispatch $ action Action.Reset
                                                | c == 'm' = Action.dispatch $ action Action.Menu
handleEvent (VtyEvent (V.EvKey V.KUp []))                  = Action.dispatch $ action Action.Up
handleEvent (VtyEvent (V.EvKey V.KLeft []))                = Action.dispatch $ action Action.Left
handleEvent (VtyEvent (V.EvKey V.KDown []))                = Action.dispatch $ action Action.Down
handleEvent (VtyEvent (V.EvKey V.KRight []))               = Action.dispatch $ action Action.Right
---- Key events ----
---- Mouse events ----
handleEvent (MouseDown (UI.Tilename pos) V.BLeft _ _)      = Action.dispatch $ Action Action.Point pos
---- Mouse events ----
---- Custom events ----
handleEvent (AppEvent (Tick currentTime)) = do 
    state <- get
    when (gameIsRunning state) $
        case gameLastTickTime state of 
            Just lastTime -> do
                let diffSeconds = realToFrac (diffUTCTime currentTime lastTime)
                    diffMs = round (diffSeconds * 1000)
                
                put $ state {
                    gameTimerMs = gameTimerMs state + diffMs,
                    gameLastTickTime = Just currentTime
                }
            Nothing -> put $ state { gameLastTickTime = Just currentTime }
---- Custom events ----
handleEvent _                                              = return () -- Ignore all other keys


appConfig :: App GameState CustomEvent UI.WidgetName
appConfig = App { 
    appDraw = drawUI, 
    appChooseCursor = neverShowCursor,      -- Hides the blinking terminal cursor
    appHandleEvent = handleEvent,
    appStartEvent = return (),            -- Do nothing on startup
    appAttrMap = const $ attrMap V.defAttr [] -- Default colors
}
