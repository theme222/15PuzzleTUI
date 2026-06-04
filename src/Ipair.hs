{- HLINT ignore "Use bimap" -} -- my LSP is on LSD 
module Ipair where

type Ipair = (Int, Int)

(~-) :: Ipair -> Ipair -> Ipair
p1 ~- p2 = (fst p1 - fst p2, snd p1 - snd p2)

(~+) :: Ipair -> Ipair -> Ipair
p1 ~+ p2 = (fst p1 + fst p2, snd p1 + snd p2)

nilPair :: Ipair
nilPair = (-1, -1)

zeroPair :: Ipair
zeroPair = (0, 0)

isAdjacent :: Ipair -> Ipair -> Bool
isAdjacent p1 p2 = abs (fst p1 - fst p2) + abs (snd p1 - snd p2) == 1

flipDiagonal :: Ipair -> Ipair
flipDiagonal (x, y) = (y, x) 

-- OFFSETS --
upOS :: Ipair 
upOS = (-1, 0)

leftOS :: Ipair
leftOS = (0, -1)

downOS :: Ipair
downOS = (1, 0)

rightOS :: Ipair
rightOS = (0, 1)
-- OFFSETS --

infixl 6 ~-
infixl 6 ~+