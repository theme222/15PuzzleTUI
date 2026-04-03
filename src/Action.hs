module Action where

import State
import Ipair (Ipair, nilPair)
import qualified Grid
import Grid (Grid)
import qualified UI

import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Brick (EventM, modify, get, put)
import Control.Monad (when)

data ActionType = Left | Right | Up | Down | Point | Refresh | Menu | Reset deriving (Eq, Show)

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
    gameState <- get -- Grab the state
    time <- liftIO getCurrentTime
    let 
        oldState = gamePlay gameState
        g = playGrid oldState
        state =  -- Update time
            if playIsRunning oldState then
                case playLastTickTime oldState of 
                    Just lastTime -> do
                        let diffSeconds = realToFrac (diffUTCTime time lastTime)
                            diffMs = round (diffSeconds * 1000)
                        
                        oldState {
                            playTimerMs = playTimerMs oldState + diffMs,
                            playLastTickTime = Just time
                        }
                    Nothing -> oldState { playLastTickTime = Just time }
            else oldState { playLastTickTime = Just time }
    
    case actionType a of
        Action.Left  -> do 
            when (Grid.validOffsetPos g (0, 1)) (put gameState { gamePlay = _checkBoardAndApplyMove state $ Grid.offset (0, 1) g})
        Action.Right -> do
            when (Grid.validOffsetPos g (0, -1)) (put gameState { gamePlay = _checkBoardAndApplyMove state $ Grid.offset (0, -1) g})
        Action.Up    -> do
            when (Grid.validOffsetPos g (1, 0)) (put gameState { gamePlay = _checkBoardAndApplyMove state $ Grid.offset (1, 0) g})
        Action.Down  -> do
            when (Grid.validOffsetPos g (-1, 0)) (put gameState { gamePlay = _checkBoardAndApplyMove state $ Grid.offset (-1, 0) g})
        Action.Point -> do
            when (Grid.validMovePos g (actionPosition a))
                (put gameState { gamePlay = _checkBoardAndApplyMove state $ Grid.move (actionPosition a) g})
        Action.Menu  -> modify (\s -> s { gameScene = SettingsScene })
        
        -- The Magic Happens Here! (Thx chatgpt)
        Action.Reset -> do
            -- 1. liftIO temporarily opens the IO portal so we can shuffle
            newGrid <- liftIO $ Grid.shuffle g
            -- 2. Modify the state with our pure, newly shuffled grid
            modify (\s -> s { gamePlay = state { playGrid = newGrid, playIsRunning = False, playIsFinished = False, playLastTickTime = Nothing, playTimerMs = 0 }})
        
        Action.Refresh -> put gameState { gamePlay = state } -- Force UI refresh

_settingsSceneActionHandler :: Action -> EventM UI.WidgetName GameState ()
_settingsSceneActionHandler a = do
    state <- get
    case actionType a of
        Action.Menu  -> modify (\s -> s { gameScene = PlayScene })
        _ -> pure ()

_checkBoardAndApplyMove :: PlayState -> Grid -> PlayState
_checkBoardAndApplyMove ps newGrid  -- new Grid here is what the grid would be if it had done the move
    | playIsFinished ps = ps
    | Grid.isSolved newGrid = ps { playIsRunning = False, playIsFinished = True, playGrid = newGrid }
    | otherwise = ps { playIsRunning = True, playGrid = newGrid }