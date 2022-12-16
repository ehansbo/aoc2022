import DayZero
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

data Valve = Valve {name :: String, flow :: Int, valves :: [String]}
    deriving (Show, Eq)

type ValveMap = M.Map String Valve

data UnresolvedMove = Activate | Move Valve
    deriving Eq

instance Ord Valve where
    compare v1 v2 = compare (id v1) (id v2)

main = do
    valves <- manuallyParse "d16" "\n" parseValve
    let valveMap = M.fromList $ zip (map name valves) valves
    let totalWithFlow = length $ filter (\v -> flow v > 0) valves
    --print $ solve1 valveMap
    print $ solve2 valveMap totalWithFlow

parseValve :: String -> Valve
parseValve str = 
    let splitStr = splitOn " " str
    in Valve (splitStr !! 1) (read (filter isNumber (splitStr !! 4))) (map (filter (/= ',')) (drop 9 splitStr))

solve2 :: ValveMap -> Int -> Int
solve2 m totalWithFlow = solve2' 26 totalWithFlow (m M.! "AA") (m M.! "AA") m S.empty S.empty S.empty

solve2' :: Int -> Int -> Valve -> Valve -> ValveMap -> S.Set String -> S.Set String -> S.Set String -> Int
solve2' 0 _ _ _ _ _ _ _ = 0
solve2' remaining totalWithFlow valveMe valveElephant valveMap activated visitedMe visitedElephant = 
    -- To make moves, we need to make unresolved moves first and then resolve them in pairs, because we will change activated based on the pairing.
    let unresolvedMovesMe = getUnresolvedMoves valveMe visitedMe Nothing
        unresolvedMovesElephant = getUnresolvedMoves valveElephant visitedElephant (Just $ valveMe)
        remaining' = remaining - 1
        getUnresolvedMoves valve visited maybeDone = -- We only want to activate the valve for either elephant or me if we are in the same spot.
            if maybeDone /= (Just valve) && flow valve /= 0 &&  (not $ (name valve) `S.member` activated) then [Activate] else []
            ++ map Move (map (valveMap M.!) (filter (\v -> not $ (v `elem` visited)) $ valves valve))
        unresolvedPairs = [(x, y) | x <- unresolvedMovesMe, y <- unresolvedMovesElephant]
        unresolvedPairs' = if valveMe /= valveElephant then unresolvedPairs else removeOpposites unresolvedPairs
        resolve (Move valveMe', Move valveElephant') = solve2' remaining' totalWithFlow valveMe' valveElephant' valveMap activated (S.insert (name valveMe) visitedMe) (S.insert (name valveElephant) visitedElephant)
        resolve (Activate, Move valveElephant') = (remaining' * flow valveMe) + (solve2' remaining' totalWithFlow valveMe valveElephant' valveMap (S.insert (name valveMe) activated) S.empty (S.insert (name valveElephant) visitedElephant))
        resolve (Move valveMe', Activate) = (remaining' * flow valveElephant) + (solve2' remaining' totalWithFlow valveMe' valveElephant valveMap (S.insert (name valveElephant) activated) (S.insert (name valveMe) visitedMe) S.empty)
        resolve (Activate, Activate) = remaining' * (flow valveElephant + flow valveMe) + (solve2' remaining' totalWithFlow valveMe valveElephant valveMap ((S.insert (name valveElephant) . S.insert (name valveMe)) activated) S.empty S.empty)
    in if totalWithFlow == S.size activated then 0 else maximum $ 0 : map resolve unresolvedPairs

removeOpposites :: Eq a => [(a, a)] -> [(a, a)]
removeOpposites ((x, y):xys) = (x, y):(removeOpposites $ remove (y, x) xys)
    where remove a (b:bs)
            | a == b = remove a bs
            | otherwise = b:(remove a bs) 
          remove _ [] = []
removeOpposites [] = []


solve1 :: ValveMap -> Int
solve1 m = solve1' 30 (m M.! "AA") m S.empty []

solve1' :: Int -> Valve -> ValveMap -> S.Set String -> [String] -> Int
solve1' 0 _ _ _ _ = 0
solve1' remaining valve valveMap activated visited =
    let remaining' = remaining - 1
        nextSolve valve' activated' visited' = solve1' remaining' valve' valveMap activated' visited'
        openValve = 
            if (flow valve /= 0 && (not $ (name valve) `S.member` activated))
                then Just $ remaining' * flow valve + nextSolve valve (S.insert (name valve) activated) []
                else Nothing
        moves = map (\valve' -> nextSolve valve' activated (name valve : visited)) (map (valveMap M.!) (filter (\v -> not $ (v `elem` visited)) $ valves valve))
    in maximum $ if openValve == Nothing then 0:moves else (fromJust openValve) : 0 : moves