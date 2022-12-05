import DayZero
import Data.List.Split

type Stack = [Char]
type Amount = Int
type Index = Int
type Move = (Amount, Index, Index)


-- Stacks are hard coded because this one did not seem fun to parse.
stacks :: [Stack]
stacks = ["FLMW", "FMVZB", "QLSRVH", "JTMPQVSF", "WSL", "WJRMPVF", "FRNPCQJ", "BRWZSPHV", "WZHGCJMB"]


main = do
    moves <- map parseMove . (filter (/= "")) . (splitOn "\n") <$> (!! 1) <$> (splitFile "day5" "\n\n")
    let solvedStacks = foldl (doMove True) stacks moves
    let solvedStacks2 = foldl (doMove False) stacks moves
    print $ map head solvedStacks
    print $ map head solvedStacks2

doMove :: Bool -> [Stack] -> Move -> [Stack]
doMove doReverse stacks (amount, from, to) =
    let stuffMoved = (if doReverse then reverse else id) $ take amount (stacks !! from)
        newFrom = drop amount (stacks !! from)
        newTo = stuffMoved ++ (stacks !! to)
        replacedFrom = take from stacks ++ [newFrom] ++ tail (drop from stacks)
    in take to replacedFrom ++ [newTo] ++ tail (drop to replacedFrom)

parseMove :: String -> Move
parseMove str = parseMove' $ splitOn " " str
    where parseMove' (_:amount:_:i1:_:i2:_) = (read amount, (read i1) - 1, (read i2) -1) 