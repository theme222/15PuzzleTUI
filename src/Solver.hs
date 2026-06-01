module Solver where

import Grid (Grid)
import qualified Grid
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Ipair
    
-- Manhattan distance + Linear Conflict 
heuristic :: Grid -> Int
heuristic grid = Grid.gridMDCache grid + Grid.linearConflict grid
-- heuristic grid = Grid.gridMDCache grid + Grid.gridLCCache grid

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
    in  _idaStarLoop path (Grid.gridMoveCount grid + heuristic grid)



