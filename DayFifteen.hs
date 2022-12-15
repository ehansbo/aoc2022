import DayZero
import Data.List.Split
import Data.List
import Data.Function (on)
import qualified Data.Set as S


main :: IO ()
main = do
    -- Part 1
    input1 <- manuallyParse "d15" "\n" (scannerForRow 2000000)
    print $ (S.size $ S.fromList $ concat input1) - 1 -- (-1) since exactly one beacon on that row. Maybe shouldn't hardcode this but it was not very fun assignment...

    -- Part 2
    -- The only way for a point to be uniquely outside all beacons is for there to exist four signals whose measuring distance is 1 to small to have overlap.
    outsideBorderPoints <- manuallyParse "d15" "\n" parseOutsideBorderPoints
    mapM (print . length) outsideBorderPoints
    withinFunctions <- manuallyParse "d15" "\n" parseToFunction
    let concated = map head $ filter ((>1) . length) $ group $ sort $ concat outsideBorderPoints
    let (x, y) = filterPoints 0 withinFunctions concated
    print $ x*4000000 + y


filterPoints :: Int -> [(Int, Int) -> Bool] -> [(Int, Int)] -> (Int, Int)
filterPoints i wf ((x, y):ps) = if not (orFs wf (x, y)) then (x, y) else filterPoints (i+1) wf ps

orFs :: [(Int, Int) -> Bool] -> (Int, Int) -> Bool
orFs (f:fs) p = f p || orFs fs p
orFs [] _ = False 
    
parseOutsideBorderPoints :: String -> [(Int, Int)]
parseOutsideBorderPoints str = 
    let strSpl = splitOn "=" str
        t i = read $ takeWhile isNumber (strSpl !! i) 
        sx = t 1
        sy = t 2
        bx = t 3
        by = t 4
        distance = abs (bx - sx) + abs (by - sy)
        p1 = (sx - distance - 1, sy)
        p2 = (sx, sy + distance + 1)
        p3 = (sx + distance + 1, sy)
        p4 = (sx, sy - distance - 1)
    in  S.toList $ S.fromList $ filter (\(x, y) -> x >= 0 && y >= 0 && x <= 4000000 && y <= 4000000) ((lineBetweenPoints p1 p2) ++ (lineBetweenPoints p2 p3) ++ (lineBetweenPoints p3 p4) ++ (lineBetweenPoints p4 p1))

parseToFunction :: String -> ((Int, Int) -> Bool)
parseToFunction str = 
    let strSpl = splitOn "=" str
        t i = read $ takeWhile isNumber (strSpl !! i) 
        sx = t 1
        sy = t 2
        bx = t 3
        by = t 4
        distance = abs (bx - sx) + abs (by - sy)
    in  \(x, y) -> abs (x - sx) + abs (y - sy) <= distance



lineBetweenPoints :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
lineBetweenPoints (x1, y1) (x2, y2)
    | x1 == x2 && y1 == y2 = [(x1, y1)]
    | otherwise =
        let signx = signum (x2 - x1)
            signy = signum (y2 - y1)
        in [(x1, y1), (x1+signx, y1), (x1, y1 + signy)] ++ lineBetweenPoints (x1+signx, y1+signy) (x2, y2)

scannerForRow :: Int -> String -> [Int]
scannerForRow row str = 
    let strSpl = splitOn "=" str
        t i = read $ takeWhile isNumber (strSpl !! i) 
        sx = t 1
        sy = t 2
        bx = t 3
        by = t 4
        distance = abs (bx - sx) + abs (by - sy)
        distanceToRow = abs (sy - row)
        in [(sx - (distance-distanceToRow))..(sx + (distance-distanceToRow))]


isNumber :: Char -> Bool
isNumber c = c `elem` (['0'..'9'] ++ "-+")