import DayZero

type Grid = [[Int]]

main = do
    input <- manuallyParse "day8" "\n" parseRow
    print $ solve1 input
    print $ solve2 input

solve1 :: Grid -> Int
solve1 grid = solve1' $ allCoords grid
    where solve1' [] = 0
          solve1' ((x, y):xys) = solve1' xys + if isVisible x y grid then 1 else 0

solve2 :: Grid -> Int
solve2 grid = maximum $ map solve2' (allCoords grid)
    where solve2' (x, y) = getView x y grid

getView :: Int -> Int -> Grid -> Int
getView x y grid =
    let row = getRow y grid
        column = getColumn x grid
        beforeX = reverse $ take x row
        beforeY = reverse $ take y column
        afterX = drop (x+1) row
        afterY = drop (y+1) column
        height = (grid !! y) !! x
    in length (takeWhileInclusive (\h -> h < height) beforeX)
        * length (takeWhileInclusive (\h -> h < height) beforeY)
        * length (takeWhileInclusive (\h -> h < height) afterX)
        * length (takeWhileInclusive (\h -> h < height) afterY)

allCoords :: Grid -> [(Int, Int)]
allCoords grid = [(x, y) | x <- [0..(length (head grid) - 1)], y <- [0..(length grid - 1)]]

isVisible :: Int -> Int -> Grid -> Bool
isVisible x y grid = 
    let row = getRow y grid
        column = getColumn x grid
        beforeX = take x row
        beforeY = take y column
        afterX = drop (x+1) row
        afterY = drop (y+1) column
        height = (grid !! y) !! x
    in length beforeX == length (filter (\h -> h < height) beforeX)
        || length beforeY == length (filter (\h -> h < height) beforeY)
        || length afterX == length (filter (\h -> h < height) afterX)
        || length afterY == length (filter (\h -> h < height) afterY)

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive p xs = let (ys, zs) = span p xs in ys ++ take 1 zs

getRow :: Int -> Grid -> [Int]
getRow y grid = grid !! y

getColumn :: Int -> Grid -> [Int]
getColumn x grid = map (\r -> r !! x) grid

parseRow :: String -> [Int]
parseRow = map (\a -> read [a])