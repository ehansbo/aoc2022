import DayZero

import Control.Monad.State
import Data.Set

type Point = (Int, Int)
type Movement = (Int, Int)


type PointState = State ([Point], Set Point)

(+@) :: Num a => (a, a) -> (a, a) -> (a, a)
(+@) (x, y) (a, b) = (x+a, y+b)


main :: IO ()
main = do
    input <- concat <$> manuallyParse "day9" "\n" parseMovements
    let visited = evalState (solve input) (replicate 2 (0, 0), empty)
    print $ size visited
    let visited2 = evalState (solve input) (replicate 10 (0, 0), empty)
    print $ size visited2


getMovement :: Point -> Point -> Movement
getMovement (hx, hy) (tx, ty)
    | abs (tx - hx) <= 1 && abs (ty - hy) <= 1 = (0, 0)
    | tx == hx = (0, direction (hy - ty))
    | ty == hy = (direction (hx - tx), 0)
    | otherwise = (direction (hx - tx), direction (hy - ty))

direction :: Int -> Int
direction i = i `div` (abs i)


solve :: [Movement] -> PointState (Set Point)
solve (m:ms) = do
    (points, visited) <- get
    let movedHead = head points +@ m
    let movedTail = move movedHead (tail points)
    put (movedHead : movedTail, insert (last movedTail) visited)
    solve ms
solve [] = gets snd

-- Moves the first element in the list based on the first argument, the second element based on the moved first element, etc.
move :: Point -> [Point] -> [Point]
move h (t:ts) = 
    let movedTail = t +@ (getMovement h t)
    in  movedTail : (move movedTail ts)
move _ [] = []

parseMovements :: String -> [Movement]
parseMovements (m:' ':amount) = replicate (read amount) (charToMovement m)
    where charToMovement 'U' = (0, 1)
          charToMovement 'D' = (0, -1)
          charToMovement 'R' = (1, 0)
          charToMovement 'L' = (-1, 0)