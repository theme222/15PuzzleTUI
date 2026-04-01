module Action where
import State
import Ipair (Ipair, nilPair)
import qualified Grid
import Brick (EventM, modify, get)
import Control.Monad.IO.Class (liftIO)
import qualified UI

data ActionType = Left | Right | Up | Down | Point | Back | Reset deriving (Eq, Show)

data Action = Action {
    actionType :: ActionType,
    actionPosition :: Ipair
} deriving (Eq, Show)

action :: ActionType -> Action
action t = Action t nilPair

-- Send the action to the appropriate scene handler
dispatch :: Action -> EventM UI.WidgetName GameState ()
dispatch a = do
    state <- get -- Grab the current state from the monad
    if gameScene state == PlayScene  then _playSceneActionHandler a
    else if gameScene state == SettingsScene then _settingsSceneActionHandler a
    else pure ()

_playSceneActionHandler :: Action -> EventM UI.WidgetName GameState ()
_playSceneActionHandler a = do
    state <- get -- Grab the state
    case actionType a of
        Action.Left  -> modify (\s -> s { gameGrid = Grid.offset (0, 1) (gameGrid s) })
        Action.Right -> modify (\s -> s { gameGrid = Grid.offset (0, -1) (gameGrid s) })
        Action.Up    -> modify (\s -> s { gameGrid = Grid.offset (1, 0) (gameGrid s) })
        Action.Down  -> modify (\s -> s { gameGrid = Grid.offset (-1, 0) (gameGrid s) })
        Action.Point -> modify (\s -> s { gameGrid = Grid.move (actionPosition a) (gameGrid s) })
        
        -- The Magic Happens Here! (Thx chatgpt)
        Action.Reset -> do
            -- 1. liftIO temporarily opens the IO portal so we can shuffle
            newGrid <- liftIO $ Grid.shuffle (gameGrid state)
            -- 2. Modify the state with our pure, newly shuffled grid
            modify (\s -> s { gameGrid = newGrid, gameStartTime = Nothing })
            
        _ -> return ()

_settingsSceneActionHandler :: Action -> EventM UI.WidgetName GameState ()
_settingsSceneActionHandler _ = return ()
