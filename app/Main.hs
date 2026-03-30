module Main where
import Brick

import Game 
import Grid (grid, shuffle)

testGrid = grid (4,4)

main :: IO ()
main = do
    randomGrid <- shuffle testGrid
    let initialState = Game.GameState { gameScene = PlayScene, gameStartTime = pure 0, gameGrid = randomGrid}
    
    -- defaultMain hijacks the terminal, runs the app until 'halt' is called, 
    -- and then returns the very last state.
    finalState <- defaultMain Game.appConfig initialState
   
    putStrLn "End"