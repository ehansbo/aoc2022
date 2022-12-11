import DayZero
import Data.List.Split
import Data.List
import Control.Monad.State

type Item = Int
data Monkey = Monkey {items :: [Item], operation :: Item -> Item, test :: Item -> Int, inspections :: Int}

type MonkeyState = State [Monkey]

main = do
    monkeysAndDivisors <- manuallyParse "day11" "\n\n" parseMonkey
    let monkeys = map fst monkeysAndDivisors
    let divisor = product $ map snd monkeysAndDivisors
    let inspections = reverse $ sort $ evalState (solve1 20) monkeys
    print $ head inspections * (inspections !! 1)
    let inspections2 = reverse $ sort $ evalState (solve2 10000 divisor) monkeys
    print $ head inspections2 * (inspections2 !! 1)

solve1 :: Int -> MonkeyState [Int]
solve1 0 = map inspections <$> get
solve1 rounds = do
    monkeys <- get
    mapM runRound [0..(length monkeys - 1)]
    solve1 (rounds - 1)

runRound :: Int -> MonkeyState [()]
runRound monkeyNum = do
    monkey <- gets (!! monkeyNum)
    let newItems = map (inspectItem $ operation monkey) (items monkey)
    let itemShift = map (\i -> ((test monkey) i, i)) newItems
    clearAndUpdateMonkey monkeyNum (length newItems)
    mapM addItemToMonkey itemShift

solve2 :: Int -> Int -> MonkeyState [Int]
solve2 0 _ = map inspections <$> get
solve2 rounds divisor = do
    monkeys <- get
    mapM (runRound2 divisor) [0..(length monkeys - 1)]
    solve2 (rounds - 1) divisor

runRound2 :: Int -> Int -> MonkeyState [()]
runRound2 divisor monkeyNum = do
    monkey <- gets (!! monkeyNum)
    let newItems = map (inspectItem2 divisor $ operation monkey) (items monkey)
    tests <- gets (map test)
    let itemShift = map (\i -> ((test monkey) i, i)) newItems
    clearAndUpdateMonkey monkeyNum (length newItems)
    mapM addItemToMonkey itemShift



addItemToMonkey :: (Int, Item) -> MonkeyState ()
addItemToMonkey (monkey, item) = do
    (before, including) <- splitAt monkey <$> get
    let (Monkey items op test insp) = head including
    put $ before ++ (Monkey (item:items) op test insp) : (tail including)


clearAndUpdateMonkey :: Int -> Int -> MonkeyState ()
clearAndUpdateMonkey i items = do
    (before, including) <- splitAt i <$> get
    let updateMonkey (Monkey _ op test insp) = Monkey [] op test (insp + items)
    put $ before ++ (updateMonkey (head including)) : (tail including)


inspectItem :: (Item -> Item) -> Item -> Item
inspectItem f i = (f i) `div` 3

inspectItem2 :: Int -> (Item -> Item) -> Item -> Item
inspectItem2 divisor f i = (f i) `mod` divisor


parseMonkey :: String -> (Monkey, Int)
parseMonkey str =
    let rows = splitOn "\n" str
    in ((Monkey (parseItems $ rows !! 1) (parseOperation $ rows !! 2) (parseTest (rows !! 3) (rows !! 4) (rows !! 5)) 0), (parseDivisor (rows !! 3)))

parseItems :: String -> [Item]
parseItems str = read $ "[" ++ (tail $ dropWhile (/= ':') str) ++ "]"

parseDivisor :: String -> Int
parseDivisor str = read $ last $ splitOn " " str

parseOperation :: String -> Int -> Int
parseOperation str = 
    let (_:_:op:e2:_) = splitOn " " (dropWhile (/= '=') str)
        f = if op == "+" then (+) else (*)
    in if e2 == "old" then \x -> f x x else \x -> f x (read e2)

parseTest :: String -> String -> String -> Item -> Int
parseTest testStr ifTStr ifFStr =
      let getNum = \str -> read $ last $ splitOn " " str
      in \x -> if x `mod` (getNum testStr)  == 0 then (getNum ifTStr) else (getNum ifFStr)