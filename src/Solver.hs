module Solver where

import Grid (Grid)
import qualified Grid
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Sequence as S
import Ipair

-- Manhattan distance + Linear Conflict $ No walking distance $ very sad
heuristic :: Grid -> Int
heuristic grid = Grid.gridMDCache grid + Grid.linearConflict grid
-- heuristic grid = Grid.gridMDCache grid + Grid.gridLCCache grid

getPossibleOffsetedMovePos :: Grid -> V.Vector Ipair
getPossibleOffsetedMovePos grid = V.filter (Grid.validOffsetPos grid) $ V.fromList [(0, 1), (0, -1), (1, 0), (-1, 0)]

-- https://en.wikipedia.org/wiki/Iterative_deepening_A*#:~:text=Pseudocode
_iterativeSearch :: V.Vector Grid -> Int -> (Int, V.Vector Grid)
_iterativeSearch path bound =
    let currentGrid = V.last path
        estimatedGoalPathH = Grid.gridMoveCount currentGrid + heuristic currentGrid
    in  if estimatedGoalPathH > bound then (estimatedGoalPathH, path)
        else if Grid.isSolved currentGrid then (-1, path) -- solved
        else V.foldl'
            (\(h, p) (minH, minP) -> if h < minH then (h, p) else (minH, minP)) (maxBound, path)
            $ V.map (\move -> _iterativeSearch (V.snoc path move) bound) $ V.filter (`V.notElem` path) (V.map (`Grid.offset` currentGrid) $ getPossibleOffsetedMovePos currentGrid)

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


-- A BFS to find a path to move the empty tile to a given position (visited here also includes "locked positions")
-- The sequence (queue) is the value of (nextGrid, nextOrigin) which we can use to backtrack our way once we find the final position
_bfsEmptyPath :: (Grid, Grid) -> VU.Vector Bool -> S.Seq (Grid, Grid) -> Ipair -> V.Vector (Grid, Grid)
_bfsEmptyPath (currentGrid, prevGrid) visited queue target
    -- | otherwise = error $ show target 
    | Grid.getTile currentGrid target == 0 = V.singleton (currentGrid, prevGrid)
    | otherwise =
        let  offsetPositions = getPossibleOffsetedMovePos currentGrid
             pushedQueue = V.foldl' (S.|>) queue $ V.map (\os -> (Grid.offset os currentGrid, currentGrid)) $ V.filter (not . (visited VU.!) . Grid.squashIndex currentGrid . (`Grid.getMovePosFromOffset` currentGrid)) offsetPositions -- add possible moves to the queue
             nextGridSet | S.null pushedQueue = error $ show offsetPositions
                         | otherwise  =  S.index pushedQueue 0
             poppedQueue = S.drop 1 pushedQueue
             newVisited  = visited VU.// [(Grid.squashIndex currentGrid pos, True) | pos <- V.toList $ V.map (`Grid.getMovePosFromOffset` currentGrid) offsetPositions]
             result = _bfsEmptyPath nextGridSet newVisited poppedQueue target
         in  if (snd . V.head) result == currentGrid then V.singleton (currentGrid, prevGrid) V.++ result
             else result

-- Returns the steps to take to move the tile at (fromX, fromY) to (toX, toY)
-- This function can handle all ending positions that aren't the final tile in a row or column
_slideTo :: Grid -> VU.Vector Bool -> Ipair -> Ipair -> V.Vector Grid
_slideTo grid locked from to =
    let locked' = locked VU.// [(Grid.squashIndex grid from, True)]
    in  if from == to then V.empty
        else if from `isAdjacent` to then -- Holy shit look at this perfect use of the infix operator 
            let emptyTilePath = V.map fst $ _bfsEmptyPath (grid, Grid.nullGrid) locked' S.empty to
            in  V.tail $ V.snoc emptyTilePath $ Grid.move from $ if V.null emptyTilePath then grid else V.last emptyTilePath
        else
            let normalize n = if n > 0 then 1 else -1
                deltaX = snd to - snd from
                deltaY = fst to - fst from
                xIsBlocked = any (\pos -> locked VU.! Grid.squashIndex grid pos) [from ~+ (0, x * normalize deltaX) | x <- [1.. abs deltaX]]
                yIsBlocked = any (\pos -> locked VU.! Grid.squashIndex grid pos) [from ~+ (y * normalize deltaY, 0) | y <- [1.. abs deltaY]]
                nextAdjMovePos | deltaX == 0 = from ~+ (normalize deltaY, 0)
                               | deltaY == 0 = from ~+ (0, normalize deltaX)
                               | xIsBlocked = from ~+ (normalize deltaY, 0) 
                               | otherwise  = from ~+ (0, normalize deltaX)
                emptyTilePath = V.map fst $ _bfsEmptyPath (grid, Grid.nullGrid) locked' S.empty nextAdjMovePos
                emptyTilePath' = V.snoc emptyTilePath $ Grid.move from $ if V.null emptyTilePath then grid else V.last emptyTilePath
            -- in  error $ show nextAdjMovePos
            in  V.tail emptyTilePath' V.++ _slideTo (V.last emptyTilePath') locked nextAdjMovePos to


-- This will handle the last tile in a row where it has atleast a 3x3 space available
-- s s to
-- b d from
-- up right right down left up left down
-- s  b
-- s  d
-- to from
-- left down down right up left up right
_handleLastTile :: Grid -> VU.Vector Bool -> Ipair -> Ipair -> V.Vector Grid
_handleLastTile grid locked from to =
    let offsetMovesX = [upOS, rightOS, rightOS, downOS, leftOS, upOS, leftOS, downOS]
        deltaX = snd to - snd from
        deltaY = fst to - fst from
        xAxisSolve = deltaX == 0
        yAxisSolve = deltaY == 0
        blankPos | xAxisSolve && yAxisSolve  = error "handle last tile called with invalid from and to"
                 | xAxisSolve = from ~+ (0, -2)
                 | yAxisSolve = from ~+ (-2, 0)
                 | otherwise  = error "handle last tile called with invalid from and to"
        locked' = locked VU.// [(Grid.squashIndex grid from, True), (Grid.squashIndex grid to, True)]
        ignoreLastTile = Grid.getTile grid to == 0
        emptyTilePath = V.tail $ V.map fst $ _bfsEmptyPath (grid, Grid.nullGrid) locked' S.empty blankPos
        lastTileOffsets | xAxisSolve = V.fromList offsetMovesX
                        | yAxisSolve = V.fromList $ map flipDiagonal offsetMovesX 
        lastTilePath = Grid.applyOffsets (if V.null emptyTilePath then grid else V.last emptyTilePath) lastTileOffsets -- Since it also returns the current grid
    -- in error $ show (emptyTilePath)
    in  if ignoreLastTile then V.singleton (Grid.move from grid) else emptyTilePath V.++ lastTilePath

_NMpathGen :: Grid -> Ipair -> V.Vector Ipair -> V.Vector Int -> Ipair -> V.Vector Grid
_NMpathGen grid (rowBounds, colBounds) positions values os =
    let (pathExceptLast, lockedExceptLast) =
            V.foldl' (
                \(path, locked) index ->
                    let currentGrid = V.last path
                        to = positions V.! index
                        val = values V.! index
                        from = Grid.getPos val currentGrid
                        currentPath = _slideTo currentGrid locked from to
                        locked' = locked VU.// [(Grid.squashIndex grid to, True)]
                    in (path V.++ currentPath, locked')
            ) (V.singleton grid, _getLockedFromBounds grid (rowBounds, colBounds)) $ V.generate (V.length positions - 1) id
        lastGridofExceptLast = V.last pathExceptLast
        lastIndex = V.length positions - 1
        lastTempPos = (positions V.! lastIndex) ~+ os
        ignorePathLast = Grid.getTile lastGridofExceptLast (positions V.! lastIndex) == values V.! lastIndex
        pathLast = _slideTo lastGridofExceptLast lockedExceptLast (Grid.getPos (values V.! lastIndex) lastGridofExceptLast) lastTempPos 
        pathLastHandled = _handleLastTile (if V.null pathLast then lastGridofExceptLast else V.last pathLast) lockedExceptLast lastTempPos (positions V.! lastIndex)
        path = if ignorePathLast then pathExceptLast else V.concat [pathExceptLast, pathLast, pathLastHandled]
    -- in error $ show (pathLast) 
    in V.tail path -- Since it starts of with the initial grid

-- This will handle solving the current row / col (based on the order) of the grid. The bounds must be at least 3x3
_shrinkNxM :: Grid -> Ipair -> Order -> V.Vector Grid
_shrinkNxM grid (rowBounds, colBounds) Row =
    let (rows, cols) = Grid.gridSize grid
        positions = V.reverse $ V.fromList [(rows-rowBounds, cols - c) | c <- [1..colBounds]]
        values = V.map (Grid.getOriginalTile grid) positions
        doPathGen = V.map (Grid.getTile grid) positions /= values
    in  if doPathGen then _NMpathGen grid (rowBounds, colBounds) positions values downOS 
        else V.empty 
_shrinkNxM grid (rowBounds, colBounds) Col =
    let (rows, cols) = Grid.gridSize grid
        positions = V.reverse $ V.fromList [(rows-r, cols - colBounds) | r <- [1..rowBounds]]
        values = V.map (Grid.getOriginalTile grid) positions
        doPathGen = V.map (Grid.getTile grid) positions /= values
    in if doPathGen then _NMpathGen grid (rowBounds, colBounds) positions values rightOS
        else V.empty

-- This will handle solving the case where one dimension is of size 2. The resulting grid will have 2 x (n-1) being unsolved
-- x Axis solve
-- 1 d
-- d 2
-- d b
-- up up left down right down left up up right down 
-- y axis solve
-- 1 d d
-- d 2 b
-- left left up right down right up left left down right
_shrink2xN :: Grid -> VU.Vector Bool -> (Ipair, Ipair) -> (Ipair, Ipair) -> V.Vector Grid
_shrink2xN grid locked (from1, to1) (from2, to2) =
    let offsetMovesX = [upOS, upOS, leftOS, downOS, rightOS, downOS, leftOS, upOS, upOS, rightOS, downOS]
        fromVal1 = Grid.getTile grid from1
        toVal1 = Grid.getTile grid to1
        fromVal2 = Grid.getTile grid from2
        toVal2 = Grid.getTile grid to2
        deltaX = snd to1 - snd to2
        deltaY = fst to1 - fst to2
        xAxisSolve = deltaY == 0
        yAxisSolve = deltaX == 0
        tile1Path = _slideTo grid locked from1 to1
        tile1PathLastGrid = if V.null tile1Path then grid else V.last tile1Path
        tile2Locked = locked VU.// [(Grid.squashIndex grid to1, True), (Grid.squashIndex grid to2, True)]
        ignoreTile2Path = Grid.getTile tile1PathLastGrid to2 == fromVal2
        from2' = Grid.getPos fromVal2 tile1PathLastGrid
        to2' = to2 ~+ (if xAxisSolve then downOS else rightOS)
        tile2Path = _slideTo tile1PathLastGrid tile2Locked from2' to2'
        tile2PathLastGrid = if V.null tile2Path then tile1PathLastGrid else V.last tile2Path
        ignoreHandle2xN = Grid.getTile tile2PathLastGrid to2 == 0
        emptyTileLocked = tile2Locked VU.// [(Grid.squashIndex grid to2', True)]
        emptyTilePath = V.tail $ V.map fst $ _bfsEmptyPath (tile2PathLastGrid, Grid.nullGrid) emptyTileLocked S.empty (to2' ~+ (if xAxisSolve then downOS else rightOS))
        emptyTilePathLastGrid = if V.null emptyTilePath then tile2PathLastGrid else V.last emptyTilePath
        handle2xNOffsets | xAxisSolve = V.fromList offsetMovesX
                         | yAxisSolve = V.fromList $ map flipDiagonal offsetMovesX
        handle2xNPath = Grid.applyOffsets emptyTilePathLastGrid handle2xNOffsets
    -- in error $ show (emptyTilePath)
    in  if fromVal1 == toVal1 && fromVal2 == toVal2 then V.empty
        else if ignoreTile2Path then tile1Path
        else if ignoreHandle2xN then V.snoc (tile1Path V.++ tile2Path) (Grid.move to2' tile2PathLastGrid)
        else V.concat [tile1Path, tile2Path, emptyTilePath, handle2xNPath]

_handle2x2 :: Grid -> V.Vector Grid
_handle2x2 grid =
    let (rows, cols) = Grid.gridSize grid
        to1 = (rows-2, cols-2)
        to2 = (rows-2, cols-1)
        to3 = (rows-1, cols-2)
        from1 = Grid.getPos (Grid.getOriginalTile grid to1) grid
        locked = _getLockedFromBounds grid (2, 2)
        tile1Path = _slideTo grid locked from1 to1
        tile1PathLast = if V.null tile1Path then grid else V.last tile1Path
        lockedAfterTile1 = locked VU.// [(Grid.squashIndex grid to1, True)]
        from2 = Grid.getPos (Grid.getOriginalTile tile1PathLast to2) tile1PathLast
        tile2Path = _slideTo tile1PathLast lockedAfterTile1 from2 to2
        tile2PathLast = if V.null tile2Path then tile1PathLast else V.last tile2Path
        lockedAfterTile2 = lockedAfterTile1 VU.// [(Grid.squashIndex grid to2, True)]
        from3 = Grid.getPos (Grid.getOriginalTile tile2PathLast to3) tile2PathLast
        tile3Path = _slideTo tile2PathLast lockedAfterTile2 from3 to3
    -- in  error $ show (tile1Path, tile2Path, tile3Path)
    in  V.concat [tile1Path, tile2Path, tile3Path]
        

_getLockedFromBounds :: Grid -> Ipair -> VU.Vector Bool
_getLockedFromBounds grid (rowBounds, colBounds) =
    let (rows, cols) = Grid.gridSize grid
    in VU.generate (rows * cols) (
            \i ->
                let (currRow, currCol) = Grid.expandIndex grid i
                in currRow < (rows - rowBounds) || currCol < (cols - colBounds)
        )

data Order = Row | Col deriving (Show, Eq)
-- A solving algorithm based on the fringe solving method 
-- Recursively solve from the top left to the bottom right slowly shrinking the bounds.
-- Changing the order of solve to mimic fringe solving, but then fallback to 2xN which is when fringe no longer works.
_fringeSolve :: Grid -> Ipair -> Order -> V.Vector Grid
_fringeSolve grid (2, 2) _ = _handle2x2 grid
_fringeSolve grid (2, colBounds) order =
    let (rows, cols) = Grid.gridSize grid
        to1 = (rows - 2, cols - colBounds)
        from1 = Grid.getPos (Grid.getOriginalTile grid to1) grid
        to2 = (rows - 1, cols - colBounds)
        from2 = Grid.getPos (Grid.getOriginalTile grid to2) grid
        path = _shrink2xN grid (_getLockedFromBounds grid (2, colBounds)) (from1, to1) (from2, to2)
        lastGrid = if V.null path then grid else V.last path
    in path V.++ _fringeSolve lastGrid (2, colBounds - 1) order
_fringeSolve grid (rowBounds, 2) order =
    let (rows, cols) = Grid.gridSize grid
        to1 = (rows - rowBounds, cols - 2)
        from1 = Grid.getPos (Grid.getOriginalTile grid to1) grid
        to2 = (rows - rowBounds, cols - 1)
        from2 = Grid.getPos (Grid.getOriginalTile grid to2) grid
        path = _shrink2xN grid (_getLockedFromBounds grid (rowBounds, 2)) (from1, to1) (from2, to2)
        lastGrid = if V.null path then grid else V.last path
    in path V.++ _fringeSolve lastGrid (rowBounds - 1, 2) order
_fringeSolve grid (rowBounds, colBounds) order
    | rowBounds < 2 || colBounds < 2 = error "Invalid bounds size"
    | otherwise =
        let path = _shrinkNxM grid (rowBounds, colBounds) order
            lastGrid = if V.null path then grid else V.last path
        in path V.++
            if order == Row then _fringeSolve lastGrid (rowBounds - 1, colBounds) Col
            else _fringeSolve lastGrid (rowBounds, colBounds - 1) Row

fringeSolve :: Grid -> V.Vector Grid
fringeSolve grid = V.concat [V.singleton grid, _fringeSolve grid (Grid.gridSize grid) Row]
