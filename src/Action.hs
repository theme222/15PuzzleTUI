module Action where
import State
import Ipair (Ipair, nilPair)
import qualified Grid
import Grid (Grid)
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
        Action.Left  -> modify (\s -> _checkBoardAndApplyMove s $ Grid.offset (0, 1) (gameGrid s))
        Action.Right -> modify (\s -> _checkBoardAndApplyMove s $ Grid.offset (0, -1) (gameGrid s))
        Action.Up    -> modify (\s -> _checkBoardAndApplyMove s $ Grid.offset (1, 0) (gameGrid s))
        Action.Down  -> modify (\s -> _checkBoardAndApplyMove s $ Grid.offset (-1, 0) (gameGrid s))
        Action.Point -> modify (\s -> _checkBoardAndApplyMove s $ Grid.move (actionPosition a) (gameGrid s))
        
        -- The Magic Happens Here! (Thx chatgpt)
        Action.Reset -> do
            -- 1. liftIO temporarily opens the IO portal so we can shuffle
            newGrid <- liftIO $ Grid.shuffle (gameGrid state)
            -- 2. Modify the state with our pure, newly shuffled grid
            modify (\s -> s { gameGrid = newGrid, gameIsRunning = False, gameTimerMs = 0 })
            
        _ -> return ()

_settingsSceneActionHandler :: Action -> EventM UI.WidgetName GameState ()
_settingsSceneActionHandler _ = return ()

_checkBoardAndApplyMove :: GameState -> Grid -> GameState
_checkBoardAndApplyMove state newGrid = -- new Grid here is what the grid would be if it had done the move
    let isRunning = gameIsRunning state
    in  
        if gameTimerMs state == 0 && not isRunning then -- First move
            state { gameIsRunning = True }
        else if not isRunning then -- Already solved
            state
        else if Grid.isSolved newGrid then -- Just finished solving
            state { gameIsRunning = False, gameGrid = newGrid }
        else -- Still not solved
            state { gameGrid = newGrid }