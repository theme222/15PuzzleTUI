module Action where

import State
import Ipair (Ipair, nilPair)
import qualified Grid
import Grid (Grid, grid)
import qualified UI

import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Brick (EventM, modify, get, put, halt)
import Control.Monad (when)
import Scene.SettingsScene (getSettingValueByIndex, settingsIncrement, settingsDecrement, settingRows)
import Save (loadLeaderboard, Leaderboard (..), storeLeaderboard, formatLeaderboardRankings)
import Solver (idaStar, linearConflict, manhattanDistance)
import Text.Printf (printf)

data ActionType = Left | Right | Up | Down | Point | Refresh | Menu | Help | Reset deriving (Eq, Show)

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
    if gameScene state == PlayScene  then playSceneActionHandler a
    else if gameScene state == SettingsScene then settingsSceneActionHandler a
    else pure ()
    
checkBoardAndApplyMove :: GameState -> PlayState -> Ipair -> EventM UI.WidgetName GameState ()
checkBoardAndApplyMove gameState newPlayState pos = do  -- new Grid here is what the grid would be if it had done the move
    let oldPlayState = gamePlay gameState
        oldGrid = playGrid oldPlayState
        moveIsValid = Grid.validMovePos oldGrid pos 
        newGrid = Grid.move pos oldGrid

    if not moveIsValid || playIsFinished oldPlayState then pure () -- if the move is invalid or the game is already finished
    
    else if Grid.isSolved newGrid then do -- if the move caused the grid to be solved
    
        let newLeaderboard = (playLeaderboard oldPlayState) { 
            leaderboardRankings = formatLeaderboardRankings ( -- Add the new score to the leaderboard rankings then format 
                playTimerMs oldPlayState : (leaderboardRankings . playLeaderboard) oldPlayState
            )
        }
        liftIO $ storeLeaderboard newLeaderboard
        
        put gameState { gamePlay = oldPlayState { playIsRunning = False, playIsFinished = True, playGrid = newGrid, playLeaderboard = newLeaderboard } }  
        
    else 
        put gameState { gamePlay = oldPlayState { playIsRunning = True, playGrid = newGrid } }


playSceneActionHandler :: Action -> EventM UI.WidgetName GameState ()
playSceneActionHandler a = do
    gameState <- get -- Grab the state
    time <- liftIO getCurrentTime
    let 
        ss = gameSettings gameState
        oldState = gamePlay gameState
        oldGrid = playGrid oldState
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
    
        Action.Left  -> checkBoardAndApplyMove gameState state $ Grid.getMovePosFromOffset (0,1) oldGrid
        Action.Right -> checkBoardAndApplyMove gameState state $ Grid.getMovePosFromOffset (0,-1) oldGrid
        Action.Up    -> checkBoardAndApplyMove gameState state $ Grid.getMovePosFromOffset (1,0) oldGrid
        Action.Down  -> checkBoardAndApplyMove gameState state $ Grid.getMovePosFromOffset (-1,0) oldGrid
        Action.Point -> checkBoardAndApplyMove gameState state $ actionPosition a
        Action.Menu  -> modify (\s -> s { gameScene = SettingsScene })
        Action.Help  -> do
            let searchResult = idaStar oldGrid
            error $ show searchResult
            -- let linearConfH = linearConflict oldGrid
            --     manhattanDistH = manhattanDistance oldGrid
            -- modify(\s -> s { gameDebug = DebugState { debugStr = printf "Linear conflict: %d, Manhattan distance: %d\n" linearConfH manhattanDistH } })
        
        Action.Reset -> do
            let newGridSize = settingsGridSize ss
        
            newGrid <- liftIO $ Grid.shuffle $ grid newGridSize -- Read the latest grid size from settings
            
            newLeaderboard <- 
                if newGridSize /= (leaderboardSize . playLeaderboard) oldState 
                then liftIO $ loadLeaderboard newGridSize
                else pure (playLeaderboard state)
                
            modify (\s -> s {
                gamePlay = state { 
                    playGrid = newGrid,
                    playIsRunning = False,
                    playIsFinished = False,
                    playLastTickTime = Nothing,
                    playTimerMs = 0,
                    playLeaderboard = newLeaderboard
                }
            })
        
        Action.Refresh -> put gameState { gamePlay = state } -- Force UI refresh

settingsSceneActionHandler :: Action -> EventM UI.WidgetName GameState ()
settingsSceneActionHandler a = do
    gs <- get
    let ss = gameSettings gs
        rIdx = settingsRowHover ss
        (_, getter, setter) = settingRows !! rIdx
        val = getter ss
        set = setter ss 
        
    case actionType a of
        Action.Left -> modify (\s -> s { gameSettings = set (settingsDecrement val) })
        Action.Right -> modify (\s -> s { gameSettings = set (settingsIncrement val) })
        Action.Up -> modify (\s -> s { gameSettings = ss { settingsRowHover = max 0 (rIdx - 1) }})
        Action.Down -> modify (\s -> s { gameSettings = ss { settingsRowHover = min (length settingRows - 1) (rIdx + 1) }})
        Action.Menu  -> modify (\s -> s { gameScene = PlayScene })
        _ -> pure ()
        