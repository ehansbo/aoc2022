import DayZero

main :: IO ()
main = do
    rounds <- splitFile "day2" "\n"
    print $ sum $ map score rounds
    print $ sum $ map score2 rounds
    return ()

score :: String -> Int
score (a:' ':b:[]) = 
    scoreWin a b + scoreMove b
        where 
              scoreWin 'A' 'Y' = 6
              scoreWin 'A' 'Z' = 0
              scoreWin 'B' 'X' = 0
              scoreWin 'B' 'Z' = 6
              scoreWin 'C' 'X' = 6
              scoreWin 'C' 'Y' = 0
              scoreWin _ _ = 3
              scoreMove 'X' = 1
              scoreMove 'Y' = 2
              scoreMove 'Z' = 3


score2 :: String -> Int
score2 (a:' ':b:[]) = score $ a:' ':[getResponse a b]

getResponse :: Char -> Char -> Char
getResponse 'A' 'X' = 'Z'
getResponse 'A' 'Y' = 'X'
getResponse 'A' 'Z' = 'Y'
getResponse 'B' 'X' = 'X'
getResponse 'B' 'Y' = 'Y'
getResponse 'B' 'Z' = 'Z'
getResponse 'C' 'X' = 'Y'
getResponse 'C' 'Y' = 'Z'
getResponse 'C' 'Z' = 'X'