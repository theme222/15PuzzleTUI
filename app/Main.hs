module Main where
import Brick (customMain)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP

import Game 
import State
import Grid (grid, shuffle)
import ColorScheme (fringe)
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Data.Time.Clock (getCurrentTime)

main :: IO ()   
main = do
    -- Init settings
    let initialSettings = Settings {
            settingsTileType = Fill,
            settingsColorScheme = fringe,
            settingsGridSize = (4, 4)
        }
        
    -- Init initial state
    randomGrid <- shuffle $ grid $ settingsGridSize initialSettings
    let initialState = State.GameState { 
            gameScene = PlayScene,
            gameGrid = randomGrid,
            gameSettings = initialSettings,
            gameIsRunning = False,
            gameTimerMs = 0,
            gameLastTickTime = Nothing
        }
        
    -- Create a channel for timer (idk what this means)
    timerUpdateChannel <- newBChan 10
    
    _ <- forkIO $ forever $ do
        threadDelay 1000
        now <- getCurrentTime
        writeBChan timerUpdateChannel (Tick now) 
    
    let buildVty = VCP.mkVty V.defaultConfig
    initialVty <- buildVty
    V.setMode (V.outputIface initialVty) V.Mouse True
    
    _ <- customMain initialVty buildVty (Just timerUpdateChannel) appConfig initialState
    
    putStrLn "Game gracefully closed."