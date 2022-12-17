import DayZero
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.Maybe

data Valve = Valve {name :: String, flow :: Int, valves :: [String]}
    deriving (Show, Eq)

type ValveMap = M.Map String Valve

type ShortestPathMap = M.Map (String, String) Int

type PathSequence = Seq.Seq (Int, String)

instance Ord Valve where
    compare v1 v2 = compare (id v1) (id v2)

main = do
    valves <- manuallyParse "d16" "\n" parseValve
    let valveMap = M.fromList $ zip (map name valves) valves
    let relevantValves = map name $ filter (\v -> flow v > 0) valves
    let relevantPairs = removeOpposites $ filter (\(x, y) -> x /= y) ([("AA", y) | y <- relevantValves] ++ [(x, y) | x <- relevantValves, y <- relevantValves])
    let distances = M.fromList $ concatMap (\((a, b), x) -> if a == "AA" then [((a, b), x)] else [((a, b), x), ((b, a), x)]) $ map (\(start, stop) -> ((start, stop), bfs S.empty valveMap stop (Seq.singleton (0, start)))) relevantPairs
    print $ solve1 30 "AA" distances valveMap relevantValves
    print $ solve2 distances valveMap relevantValves
    
solve2 :: ShortestPathMap -> ValveMap -> [String] -> Int
solve2 distances valveMap relevantValves =
    let combinations = filter (\(a, b) -> length a >= length b && length b > 3) $ subsetPairs relevantValves -- assuming the elephant uses at least 4 valves.
        solve rv = solve1 26 "AA" distances valveMap rv
    in maximum $ map (\(rv1, rv2) -> solve rv1 + solve rv2) combinations

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

subsetPairs :: Eq a => [a] -> [([a], [a])]
subsetPairs xs = 
    let possibleSubsets = subsets xs
    in map (\possibleSubset -> (possibleSubset, filter (\element -> not $ element `elem` possibleSubset) xs)) possibleSubsets

solve1 :: Int -> String -> ShortestPathMap -> ValveMap -> [String] -> Int
solve1 time curr distances valveMap relevantValves =
    let solveFor next =
         let distance = distances M.! (curr, next)
             time' = time - distance - 1
         in time' * (flow $ valveMap M.! next) + solve1 time' next distances valveMap (filter (/= next) relevantValves)
        solutions = map solveFor relevantValves
    in if time <= 0 then 0 else maximum $ 0:solutions
    
bfs :: S.Set String -> ValveMap -> String -> PathSequence -> Int
bfs visited valveMap stop queue =
    let (i, curr) = fromJust $ Seq.lookup 0 queue
        neighbors = filter (\v -> not $ S.member v visited) $ valves (valveMap M.! curr)
        queue' = (Seq.drop 1 queue) Seq.>< (Seq.fromList $ zip (repeat $ i+1) neighbors)
    in if curr == stop then i else bfs (S.insert curr visited) valveMap stop queue'

removeOpposites :: Eq a => [(a, a)] -> [(a, a)]
removeOpposites ((x, y):xys) = (x, y):(removeOpposites $ remove (y, x) xys)
    where remove a (b:bs)
            | a == b = remove a bs
            | otherwise = b:(remove a bs) 
          remove _ [] = []
removeOpposites [] = []

parseValve :: String -> Valve
parseValve str = 
    let splitStr = splitOn " " str
    in Valve (splitStr !! 1) (read (filter isNumber (splitStr !! 4))) (map (filter (/= ',')) (drop 9 splitStr))