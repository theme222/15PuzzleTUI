module Main where

import Game 
import State
import Grid (grid, shuffle)
import qualified ColorScheme
import qualified Config

import Brick (customMain)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Data.Time.Clock (getCurrentTime)
import Save (loadLeaderboard, Leaderboard (..), Settings (..), TileType (..), loadSettings)
import qualified Data.Vector as V

main :: IO ()   
main = do
    -- Init settings
    let initialHelper = Helper {
            helperGridVec = V.empty,
            helperCurrentVecIdx = 0,
            helperIsHelping = False,
            helperLastRenderTime = Nothing
        }

    currentSettings <- loadSettings        
        
    randomGrid <- shuffle $ grid $ settingsGridSize currentSettings

    -- load leaderboard
    currentLeaderboard <- loadLeaderboard (settingsGridSize currentSettings)
    
    let initialPlay = PlayState {
            playGrid = randomGrid,
            playIsRunning = False,
            playIsFinished = False,
            playTimerMs = 0,
            playLastTickTime = Nothing,
            playLeaderboard = currentLeaderboard,
            playHelper = initialHelper
        }
        
    -- Init initial state
    let initialState = State.GameState { 
            gameScene = PlayScene,
            gameSettings = currentSettings,
            gamePlay = initialPlay,
            gameDebug = DebugState { debugStr = "" }
        }
        
    -- Create a channel for timer (idk what this means)
    timerUpdateChannel <- newBChan 10
    
    _ <- forkIO $ forever $ do
        threadDelay $ round (Config._REFRESH_RATE_MS * 1000.0)
        now <- getCurrentTime
        writeBChan timerUpdateChannel (Tick now) 
    
    let buildVty = VCP.mkVty V.defaultConfig
    initialVty <- buildVty
    V.setMode (V.outputIface initialVty) V.Mouse True
    
    _ <- customMain initialVty buildVty (Just timerUpdateChannel) appConfig initialState
    
    putStrLn "Game gracefully closed."