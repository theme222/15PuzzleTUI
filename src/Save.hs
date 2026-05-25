-- Saves all relevent records to JSON

module Save where

import System.Directory (getXdgDirectory, XdgDirectory(XdgData), createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Ipair
import Text.Read
import Data.Maybe (isJust, mapMaybe)
import Data.List (sort)

data Leaderboard = Leaderboard {
    leaderboardSize :: Ipair,
    leaderboardRankings :: [Int]
}

formatLeaderboardRankings :: [Int] -> [Int]
formatLeaderboardRankings rankings = take 5 $ sort rankings

-- Gets the leaderboard json file path
leaderboardPath :: Ipair -> IO FilePath
leaderboardPath size = do 
    dir <- getXdgDirectory XdgData "15p"
    createDirectoryIfMissing True (dir </> "leaderboards")
    pure (dir </> "leaderboards" </> show size ++ ".txt")
    
-- Loads the leaderboard from the json file
loadLeaderboard :: Ipair -> IO Leaderboard
loadLeaderboard size = do
    path <- leaderboardPath size
    exists <- doesFileExist path
    if not exists then pure $ Leaderboard size []
    else do  
        content <- readFile path
        pure $ Leaderboard size $ formatLeaderboardRankings $ mapMaybe readMaybe $ splitOn "\n" content  
        
-- Lets see how long this bind operator lasts in my source code before I think its too unreadable and I change it back to a do
-- 15:06 17 may 2026
-- 16:03 18 may 2026 i gotta remove it lol the file path became the content and the content became the filepath :skull:
storeLeaderboard :: Leaderboard -> IO ()
storeLeaderboard leaderboard = do
    path <- leaderboardPath (leaderboardSize leaderboard)
    writeFile path (unlines $ map show (leaderboardRankings leaderboard))
