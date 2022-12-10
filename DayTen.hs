import DayZero
import Data.List.Split

main :: IO ()
main = do
    functions <- concat <$> manuallyParse "day10" "\n" parseFun
    --let values = getList 1 functions
    let values = scanl (flip ($)) 1 functions
    print $ solve1 values [20,60..220]
    putStrLn $ solve2 $ zip (concat $ repeat [0..39]) values

solve1 :: [Int] -> [Int] -> Int
solve1 values (i:is) = (values !! (i-1))*i + solve1 values is
solve1 _ [] = 0

solve2 :: [(Int, Int)] -> String
solve2 ((pos, value):xs) =
    let char = if abs (pos - value) <= 1 then '#' else ' '
        maybeNewline = if pos == 39 then "\n" else ""
    in [char] ++ maybeNewline ++ solve2 xs
solve2 [] = ""

parseFun :: String -> [Int -> Int]
parseFun "noop" = [id]
parseFun s = 
    let num = read $ last $ splitOn " " s
    in [id, \x -> x + num]
