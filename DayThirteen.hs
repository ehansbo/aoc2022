import DayZero
import Data.Ord
import Data.List
import Data.List.Split

data Signal = I Int | S [Signal]
    deriving Eq

showPairs :: [(Signal, Signal)] -> String
showPairs (x:xs) = show (fst x) ++ "\n" ++ show (snd x) ++ "\n\n" ++ showPairs xs
showPairs [] = ""

sep1 = read "[[2]]"
sep2 = read "[[6]]"

main :: IO ()
main = do
    input <- manuallyParse "d13" "\n\n" parsePairs
    let zipped = zip [1..] input
    print $ sum $ map fst $ filter (\(a, (s1, s2)) -> s1 <= s2) zipped
    let sorted = sort $ sep1 : sep2 : (concatMap (\(x, y) -> [x, y]) input)
    let zipped2 = zip [1..] sorted
    print $ (product . map fst . filter ((flip elem) [sep1, sep2] . snd)) zipped2

parsePairs :: String -> (Signal, Signal)
parsePairs str = let (a:b:_) = splitOn "\n" str in (read a, read b)

instance Show Signal where
    show (I i) = show i
    show (S ss) = '[' : showS ss  ++ "]"
        where showS (x:y:xs) = show x ++ "," ++ showS (y:xs)
              showS (x:xs) = show x
              showS [] = ""

instance Ord Signal where
    compare (I i1) (I i2) = i1 `compare` i2
    compare (S s1) (S s2) = s1 `compare` s2
    compare (I i) (S s) = [I i] `compare` s
    compare (S s) (I i) = s `compare` [I i]

instance Read Signal where
  readsPrec _ s = [parseSignal s]

parseSignal :: String -> (Signal, String)
parseSignal s = case s of
  ('[':rest) -> let (signals, rest') = readList' rest in (S signals, rest')
  n -> let (i, rest) = (read (takeWhile (`elem` ['0'..'9']) n), dropWhile (`elem` ['0'..'9']) n) in (I i, rest)

readList' :: String -> ([Signal], String)
readList' s = case s of
  ']' : rest -> ([], rest)
  ',' : rest -> readList' rest
  str -> 
    let (signal, rest) = parseSignal str
        (otherSignals, rest') = readList' rest
    in (signal:otherSignals, rest')
  