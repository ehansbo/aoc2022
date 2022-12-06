import Data.List

findFirstMarker :: Int -> String -> Int
findFirstMarker i xs
    | length (nub $ take i xs) == i = i
    | otherwise = 1 + findFirstMarker i (tail xs)

main :: IO ()
main = do
    input <- readFile "day6"
    print $ findFirstMarker 4 input
    print $ findFirstMarker 14 input