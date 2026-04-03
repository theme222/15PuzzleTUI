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

main :: IO ()   
main = do
    -- Init settings
    let initialSettings = SettingsState {
            settingsTileType = Fill,
            settingsColorScheme = ColorScheme.fringe,
            settingsGridSize = (4, 4),
            settingsRowHover = 0
        }
        
    randomGrid <- shuffle $ grid $ settingsGridSize initialSettings
    let initialPlay = PlayState {
            playGrid = randomGrid,
            playIsRunning = False,
            playIsFinished = False,
            playTimerMs = 0,
            playLastTickTime = Nothing
        }
        
    -- Init initial state
    let initialState = State.GameState { 
            gameScene = PlayScene,
            gameSettings = initialSettings,
            gamePlay = initialPlay
        }
        
    -- Create a channel for timer (idk what this means)
    timerUpdateChannel <- newBChan 10
    
    _ <- forkIO $ forever $ do
        threadDelay $ Config._REFRESH_RATE_MS * 1000
        now <- getCurrentTime
        writeBChan timerUpdateChannel (Tick now) 
    
    let buildVty = VCP.mkVty V.defaultConfig
    initialVty <- buildVty
    V.setMode (V.outputIface initialVty) V.Mouse True
    
    _ <- customMain initialVty buildVty (Just timerUpdateChannel) appConfig initialState
    
    putStrLn "Game gracefully closed."