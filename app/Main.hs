module Main where

import Game
import State
import Grid (grid, shuffle)
import qualified ColorScheme
import qualified Config
import CLIx
import Ipair (nilPair, Ipair)
import Save (loadLeaderboard, Leaderboard (..), Settings (..), TileType (..), loadSettings)

import Brick (customMain)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Data.Time.Clock (getCurrentTime)
import qualified Data.Vector as V
import System.Environment (getArgs)
import Data.List (elemIndex)
import Data.Maybe (mapMaybe, catMaybes)

loadGame :: Ipair -> IO()
loadGame size = do

    let initialHelper = Helper {
            helperGridVec = V.empty,
            helperCurrentVecIdx = 0,
            helperIsHelping = False,
            helperLastRenderTime = Nothing
        }

    currentSettings' <- loadSettings

    let currentSettings = currentSettings' {
            settingsGridSize = if size == nilPair then settingsGridSize currentSettings' else size
        }

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

main :: IO ()
main = do

    args <- getArgs

    -- very simple and braindead CLI
    let showHelp = "-h" `elem` args || "--help" `elem` args
        showVersion = "-v" `elem` args || "--version" `elem` args
        handleSize = "-s" `elem` args || "--size" `elem` args
        sizeIdx = head $ map (+ 1) $ catMaybes [elemIndex "-s" args, elemIndex "--size" args]

    if showHelp then 
        putStrLn helpText
    else if showVersion then
        putStrLn versionText
    else if handleSize then
        loadGame $ parseSize $ args !! sizeIdx
    else
        loadGame nilPair
        