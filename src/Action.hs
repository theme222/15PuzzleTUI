module Action where

import State
import Ipair (Ipair, nilPair)
import qualified Grid
import Grid (Grid, grid)
import qualified UI

import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Brick (EventM, modify, get, put, halt, continueWithoutRedraw)
import Control.Monad (when)
import Scene.SettingsScene (settingsIncrement, settingsDecrement, settingRows)
import Save (loadLeaderboard, Leaderboard (..), storeLeaderboard, formatLeaderboardRankings, Settings (..), storeSettings)
import Solver (idaStar, linearConflict)
import Text.Printf (printf)
import Config (_REFRESH_HELPER_MS)
import qualified Data.Vector as V

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
        
    continueWithoutRedraw


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
            
        updateRender = case playLastTickTime oldState of
            Nothing -> True
            Just lastTickTime ->
                let diffSeconds = realToFrac $ diffUTCTime time lastTickTime
                    diffMs = diffSeconds * 1000
                in diffMs >= 1.0 / (fromIntegral . settingsRefreshRate) ss * 1000


    case actionType a of

        Action.Left  -> checkBoardAndApplyMove gameState state $ Grid.getMovePosFromOffset (0,1) oldGrid
        Action.Right -> checkBoardAndApplyMove gameState state $ Grid.getMovePosFromOffset (0,-1) oldGrid
        Action.Up    -> checkBoardAndApplyMove gameState state $ Grid.getMovePosFromOffset (1,0) oldGrid
        Action.Down  -> checkBoardAndApplyMove gameState state $ Grid.getMovePosFromOffset (-1,0) oldGrid
        Action.Point -> checkBoardAndApplyMove gameState state $ actionPosition a
        Action.Menu  -> modify (\s -> s { gameScene = SettingsScene })
        Action.Help  -> do
            let searchResult = idaStar oldGrid -- TODO: Consider putting this in a separate thread somehow

            if playIsFinished state then
                put gameState { gamePlay = state }
            else
                put gameState {  -- If they call the helper we consider the run invalid and don't save to leaderboards
                    gamePlay = state {
                        playIsRunning = False,
                        playIsFinished = True,
                        playHelper = Helper {
                            helperGridVec = searchResult,
                            helperCurrentVecIdx = 0,
                            helperLastRenderTime = Nothing,
                            helperIsHelping = True
                        }
                    }
                }
                
            continueWithoutRedraw

        Action.Reset -> do
            let newGridSize = settingsGridSize ss

            newGrid <- liftIO $ Grid.shuffle $ grid newGridSize -- Read the latest grid size from settings

            newLeaderboard <-
                if newGridSize /= (leaderboardSize . playLeaderboard) oldState
                then liftIO $ loadLeaderboard newGridSize
                else pure (playLeaderboard state)

            put gameState {
                gamePlay = state {
                    playGrid = newGrid,
                    playIsRunning = False,
                    playIsFinished = False,
                    playLastTickTime = Nothing,
                    playTimerMs = 0,
                    playLeaderboard = newLeaderboard,
                    playHelper = Helper {
                        helperGridVec = V.empty,
                        helperCurrentVecIdx = 0,
                        helperLastRenderTime = Nothing,
                        helperIsHelping = False
                    }
                }
            }

        Action.Refresh -> do
            let incrementHelper = case (helperLastRenderTime . playHelper) state of
                    Nothing -> True
                    Just lastHelperTime ->
                        let diffSeconds = realToFrac (diffUTCTime time lastHelperTime)
                            diffMs = diffSeconds * 1000
                        in diffMs >= _REFRESH_HELPER_MS

            if incrementHelper && (helperIsHelping . playHelper) state then do -- Show the next solver path.
                let helper = playHelper state
                    newIdx = helperCurrentVecIdx helper + 1
                put gameState {
                        gamePlay = state {
                            playGrid = helperGridVec helper V.! newIdx,
                            playHelper = helper {
                                helperCurrentVecIdx = newIdx,
                                helperLastRenderTime = Just time,
                                helperIsHelping = newIdx + 1 < V.length (helperGridVec helper)
                            }
                        }
                    }
            else if updateRender then do
                put gameState { gamePlay = state } -- Force UI refresh
            else do
                continueWithoutRedraw

settingsSceneActionHandler :: Action -> EventM UI.WidgetName GameState ()
settingsSceneActionHandler a = do
    gameState <- get
    let ss = gameSettings gameState
        rIdx = settingsRowHover ss
        (_, getter, setter) = settingRows !! rIdx
        val = getter ss
        set = setter ss

        newGameState = case actionType a of
            Action.Left -> gameState { gameSettings = set (settingsDecrement val) }
            Action.Right -> gameState { gameSettings = set (settingsIncrement val) }
            Action.Up -> gameState { gameSettings = ss { settingsRowHover = max 0 (rIdx - 1) }}
            Action.Down -> gameState { gameSettings = ss { settingsRowHover = min (length settingRows - 1) (rIdx + 1 )}} 
            Action.Menu  -> gameState { gameScene = PlayScene }
            _ -> gameState 

    liftIO $ storeSettings (gameSettings newGameState)
    put newGameState 
    
