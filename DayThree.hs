import DayZero
import Data.Char
import Data.List.Split

main = do
    input <- splitFile "day3" "\n"
    print $ sum $ map point1 input
    print $ sum $ map point2 (chunksOf 3 input)

point1 :: String -> Int
point1 s = p' (take half s) (drop half s)
    where half = (length s `div` 2)
          p' (c:cs) cs' = if c `elem` cs' then priority c else p' cs cs'

point2 :: [String] -> Int
point2 ((c:cs):cs':cs'':[]) = if c `elem` cs' && c `elem` cs'' then priority c else point2 $ cs:cs':cs'':[]

priority :: Char -> Int
priority c
    | isUpper c = ord c - 38
    | otherwise = ord c - 96