import DayZero
import qualified Data.Matrix as M
import Data.Char
import Data.List
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Maybe

type Point = (Int, Int)
type PathSequence = Seq.Seq (Int, Point)


main :: IO ()
main = do
    matrix <- M.fromLists <$> splitFile "d12" "\n"
    let startingCoords = getStartingCoords matrix
    print $ fromJust $ solve1 matrix (Seq.singleton (0, startingCoords)) S.empty

    let allPossibleStartingCoords = getAllStartingCoords matrix
    let solvedList = map (\p -> solve1 matrix (Seq.singleton (0, p)) S.empty) allPossibleStartingCoords
    print $ minimum $ map fromJust $ filter (/= Nothing) solvedList

solve1 :: M.Matrix Char -> PathSequence -> S.Set Point -> Maybe Int
solve1 matrix queue visited =
    let (i, (row, col)) = fromJust $ Seq.lookup 0 queue
        neighbors = getNeighbors matrix row col
        newQueue = (Seq.drop 1 queue) Seq.>< (Seq.fromList $ zip (repeat $ i+1) neighbors)
    in  if Seq.length queue == 0 then Nothing
        else if M.getElem row col matrix == 'E' then Just i 
        else if S.member (row, col) visited then solve1 matrix (Seq.drop 1 queue) visited
        else solve1 matrix newQueue (S.insert (row, col) visited)

getNeighbors :: M.Matrix Char -> Int -> Int -> [Point]
getNeighbors matrix row col =
    let height = getHeight $ M.getElem row col matrix
        maybeNeighbors = map (\(r, c) -> ((r, c), M.safeGet r c matrix)) (zipWith (\(x, y) (a, b) -> (x + a, y + b)) (repeat (row, col)) [(0, 1), (0, -1), (1, 0), (-1, 0)])
        justNeighbors = map (\(p, n) -> (p, fromJust n)) $ filter (\(p, n) -> n /= Nothing) maybeNeighbors
    in map fst $ filter (\((nRow, nCol), char) -> getHeight char - height <= 1) justNeighbors


getStartingCoords :: M.Matrix Char -> Point
getStartingCoords m = (1 + (head $ elemIndices 'S' $ M.toList (M.colVector $ M.getCol 1 m)), 1)

getHeight :: Char -> Int
getHeight 'S' = ord 'a'
getHeight 'E' = ord 'z'
getHeight c = ord c

getAllStartingCoords :: M.Matrix Char -> [Point]
getAllStartingCoords matrix = filter (\(row, col) -> M.getElem row col matrix == 'a') [(row, col) | row <- [1..M.nrows matrix], col <- [1..M.ncols matrix]]