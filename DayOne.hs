import DayZero
import Data.List.Split
import Data.List

main :: IO ()
main = do
    elves <- (reverse . sort) <$> map sum <$> map (map read) <$> map (filter (/= "")) <$> map (splitOn "\n")  <$> splitFile "day1" "\n\n"
    print $ elves !! 0
    print $ sum $ take 3 elves
