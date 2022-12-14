import DayZero
import qualified Data.Set as S
import Data.List.Split
import Data.Maybe

type Point = (Int, Int)

sp :: Point
sp = (500, 0)

main :: IO ()
main = do
    input <- S.fromList <$> concatMap (parseLine . (splitOn " -> ")) <$> splitFile "d14" "\n"
    let voidY = maximum $ S.map ((+1) . snd) input 
    print $ solve1 voidY input
    let floorY = maximum $ S.map ((+2) . snd) input 
    print $ solve2 floorY input

solve1 :: Int -> S.Set Point -> Int
solve1 voidY occupied = 
    let maybeNewPoint = createSand voidY occupied
    in if maybeNewPoint == Nothing then 0 else 1 + solve1 voidY ((fromJust maybeNewPoint) `S.insert` occupied)

solve2 :: Int -> S.Set Point -> Int
solve2 floorY occupied = 
    let newPoint = createSand2 floorY occupied
    in if newPoint == sp then 1 else 1 + solve2 floorY (newPoint `S.insert` occupied)

createSand :: Int -> S.Set Point -> Maybe Point
createSand voidY occupied = moveSand sp
    where moveSand (x, y)
            | y >= voidY = Nothing
            | moves (x, y) == [] = Just (x, y)
            | otherwise = moveSand (head $ moves (x, y))
          moves (x, y) = filter (\p -> p `S.notMember` occupied) $ possibleMoves (x, y)
          possibleMoves (x, y) = [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]

createSand2 :: Int -> S.Set Point -> Point
createSand2 floorY occupied = moveSand sp
    where moveSand (x, y)
            | moves (x, y) == [] = (x, y)
            | otherwise = moveSand (head $ moves (x, y))
          moves (x, y) = filter (\p -> p `S.notMember` occupied && (snd p) < floorY) $ possibleMoves (x, y)
          possibleMoves (x, y) = [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]


parseLine :: [String] -> [Point]
parseLine (s1:s2:xs) = 
    let (x1:y1:[]) = map read $ splitOn "," s1
        (x2:y2:[]) = map read $ splitOn "," s2
    in [(x, y) | x <- [min x1 x2..max x1 x2], y <- [min y1 y2..max y1 y2]] ++ parseLine (s2:xs)
parseLine _ = []
