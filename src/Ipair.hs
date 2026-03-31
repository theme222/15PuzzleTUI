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

infixl 6 ~-
infixl 6 ~+