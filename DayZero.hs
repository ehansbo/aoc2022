module DayZero where
import Data.List.Split
import Data.List



splitAndReadFile :: Read a => String -> String -> IO [a]
splitAndReadFile name splitter = do
        input <- splitFile name splitter
        return $ map read input

manuallyParse :: String -> String -> (String -> a) -> IO [a]
manuallyParse name splitter f = do
    input <- splitFile name splitter
    return $ map f input

splitFile :: String -> String -> IO [String]
splitFile name splitter = do
    input <- readFile name
    return $ filter (\x -> x /= "") $ splitOn splitter input


uniq :: (Ord a) => [a] -> [a]
uniq = map head . group . sort