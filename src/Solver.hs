module Solver where

import Grid (Grid)
import qualified Grid
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Ipair

-- returns the maximum value found of the row / column and the current conflict count
-- check is boolean function to check whether the value at the provided index is at the correct row or col
_conflictReducer :: Grid -> (Int -> Bool) -> (Int, Int) -> Ipair -> (Int, Int)
_conflictReducer grid check (maxVal, conflictCount) index =
    let val = Grid.getTile grid index
    in
        if val == 0 || not (check val) then (maxVal, conflictCount)
        else if val > maxVal then (val, conflictCount)
        else (maxVal, conflictCount + 2)

-- Incase this breaks (which it probably will because haskell is an ugly unreadable language)
-- Go here to see what you did wrong: https://github.com/MilanPecov/15-Puzzle-Solvers/blob/master/fifteen_puzzle_solvers/services/puzzle/heuristic.py
linearConflict :: Grid -> Int
linearConflict g =
    let (rows, cols) = Grid.gridSize g
        _conflictReducer' = _conflictReducer g
        rowConflicts = sum $
            map (\currentRow -> snd $ foldl (\msg currentCol ->
                    _conflictReducer' 
                        (\val -> (val - 1) `div` cols == currentRow) 
                        msg
                        (currentRow, currentCol)
                    ) (-1, 0) [0..cols-1]
                ) [0..rows-1]

        colConflicts = sum $
            map (\currentCol -> snd $ foldl (\msg currentRow ->
                    _conflictReducer' 
                        (\val -> (val - 1) `mod` cols == currentCol) 
                        msg
                        (currentRow, currentCol)
                    ) (-1, 0) [0..rows-1]
                ) [0..cols-1]
    in rowConflicts + colConflicts

_getManhattanDistanceOfTile :: Grid -> Int -> Int -> Int
_getManhattanDistanceOfTile grid index val =
    if val == 0 then 0
    else
        let originalPos = Grid.getOriginalPos grid val
            delta = originalPos ~- Grid.expandIndex grid index
        in (abs . fst) delta + (abs . snd) delta

manhattanDistance :: Grid -> Int
manhattanDistance grid = VU.foldl' (+) 0 $ VU.imap (_getManhattanDistanceOfTile grid) (Grid.gridVec grid)

-- Manhattan distance + Linear Conflict 
heuristic :: Grid -> Int
heuristic grid = manhattanDistance grid + linearConflict grid

getPossibleMoves :: Grid -> V.Vector Grid
getPossibleMoves grid = V.map (`Grid.offset` grid) $ V.filter (Grid.validOffsetPos grid) $ V.fromList [(0, 1), (0, -1), (1, 0), (-1, 0)]

-- https://en.wikipedia.org/wiki/Iterative_deepening_A*#:~:text=Pseudocode
_iterativeSearch :: V.Vector Grid -> Int -> (Int, V.Vector Grid)
_iterativeSearch path bound = 
    let currentGrid = V.last path
        estimatedGoalPathH = Grid.gridMoveCount currentGrid + heuristic currentGrid
    in  if estimatedGoalPathH > bound then (estimatedGoalPathH, path)
        else if Grid.isSolved currentGrid then (-1, path) -- solved
        else V.foldl' 
            (\(h, p) (minH, minP) -> if h < minH then (h, p) else (minH, minP)) (maxBound, path) 
            $ V.map (\move -> _iterativeSearch (V.snoc path move) bound) $ V.filter (`V.notElem` path) (getPossibleMoves currentGrid)

_idaStarLoop :: V.Vector Grid -> Int -> V.Vector Grid
_idaStarLoop path bound = 
    let searchResult = _iterativeSearch path bound
    in  if fst searchResult == -1 then snd searchResult -- found a path
        else _idaStarLoop path (fst searchResult)
        
-- An iterative deepening A* search algorithm
idaStar :: Grid -> V.Vector Grid
idaStar grid = 
    let path = V.singleton grid
    in  _idaStarLoop path (heuristic grid)



