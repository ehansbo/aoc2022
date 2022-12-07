import DayZero
import Data.List
import Data.List.Split

data Directory = Directory {
    name :: String,
    files :: [File],
    directories :: [Directory]
} deriving (Show)

data File = File {
    fileName :: String,
    size :: Int
} deriving (Show)


main :: IO ()
main = do
    input <- splitFile "day7" "\n"
    let (topDir, _) = parseDir (tail input) (newDir "/")
    print $ solve1 topDir
    print $ solve2 topDir

calculateSize :: Directory -> Int
calculateSize dir = (sum $ map size (files dir)) + (sum $ map calculateSize (directories dir))

getAllSizes :: Directory -> [Int]
getAllSizes dir = calculateSize dir : concatMap getAllSizes (directories dir)

solve1 :: Directory -> Int
solve1 dir = sum $ filter (<100000) (getAllSizes dir)

solve2 :: Directory -> Int
solve2 dir =
    let sizeNeeded = 30000000 - (70000000 - calculateSize dir)
    in head $ sort $ filter (>sizeNeeded) (getAllSizes dir)

newDir :: String -> Directory
newDir name = Directory name [] []

parseDir :: [String] -> Directory -> (Directory, [String])
parseDir ("$ ls":xs) current =
    let (filesAndDirs, rest) = break ("$" `isPrefixOf`) xs
        fileNames = filter (not . ("dir" `isPrefixOf`)) filesAndDirs
    in parseDir rest (addFiles (parseFiles fileNames) current)
parseDir ("$ cd ..":xs) current = (current, xs)
parseDir (('$':' ':'c':'d':' ':dir):xs) current = 
    let (parsed, rest) = parseDir xs (newDir dir)
    in parseDir rest (addDir parsed current)

parseDir [] current = (current, [])

addFiles :: [File] -> Directory -> Directory
addFiles newFiles (Directory name [] directories) = Directory name newFiles directories

addDir :: Directory -> Directory -> Directory
addDir newDir (Directory name files directories) = Directory name files (newDir:directories)

parseFiles :: [String] ->  [File]
parseFiles (x:xs) = let (sizeStr:fileName:[]) = splitOn " " x in (File fileName (read sizeStr)):(parseFiles xs)
parseFiles [] = []