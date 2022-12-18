import DayZero
import qualified Data.HashSet as S
import qualified Data.Sequence as Seq
import Data.List.Split

type Cube = (Int, Int, Int)

neighbors :: Cube -> [Cube]
neighbors (x, y, z) = concat [[(x+a, y, z), (x, y+a, z), (x, y, z+a)] | a <- [-1,1]]

main :: IO ()
main = do
    cubes <- manuallyParse "d18" "\n" parseCube
    let (surface, neigh) = solve1 cubes
    let neighSet = S.fromList neigh
    print surface

    -- To do part two, we want to find all possible neighbors to the neighbor in the top left corner, contained in the block containing all neighbors.
    -- Then we filter out all blocks found from all neighbors already found. Then ones that remain are the "inner" ones, so we can check
    -- which surfaces face lava blocks, and subtract that from the original surface.
    let neighList = S.toList neighSet
    let maxXYZ = (maximum (map fst3 neighList), maximum (map snd3 neighList), maximum (map thrd3 neighList))
    let minXYZ = (minimum (map fst3 neighList), minimum (map snd3 neighList), minimum (map thrd3 neighList))
    let outsideSet = bfs maxXYZ minXYZ (Seq.singleton maxXYZ) (S.fromList cubes)


    let insideList = filter (\c -> not $ c `S.member` outsideSet) $ neighList
    print $ surface - (solve2 insideList (S.fromList cubes))

bfs :: Cube -> Cube -> Seq.Seq Cube -> S.Set Cube -> S.Set Cube
bfs maxCube minCube queue visited = 
    case queue Seq.!? 0 of
          Nothing -> visited
          (Just cube) ->
            let nbs = neighbors cube
                insideBlock = filter (\c -> c `isWithin` (maxCube, minCube)) nbs
                nonVisited = filter (\c -> not $ c `S.member` visited) insideBlock
                queue' = (Seq.drop 1 queue) Seq.>< (Seq.fromList $ nonVisited)
                visited' = S.union visited (S.fromList nonVisited)
            in bfs maxCube minCube queue' visited'

isWithin :: Cube -> (Cube, Cube) -> Bool
isWithin (x, y, z) ((xMax, yMax, zMax), (xMin, yMin, zMin)) =
    x <= xMax && x >= xMin && y <= yMax && y >= yMin && z <= zMax && z >= zMin


fst3 (x, y, z) = x
snd3 (x, y, z) = y
thrd3 (x, y, z) = z


solve1 :: [Cube] -> (Int, [Cube])
solve1 cubes = 
    let xs = solve1' cubes $ S.fromList cubes
    in (sum (map fst xs), concatMap snd xs)


solve1' :: [Cube] -> S.Set Cube -> [(Int, [Cube])]
solve1' (cube:cubes) allCubes =
    let airNeighbors = (filter (\c -> not $ c `S.member` allCubes) $ neighbors cube)
    in [(length airNeighbors, airNeighbors)] ++ solve1' cubes allCubes
solve1' _ _ = []

solve2 :: [Cube] -> S.Set Cube -> Int
solve2 (cube:cubes) allCubes =
    let cubeNeighbors = (filter (\c -> c `S.member` allCubes) $ neighbors cube)
    in (length cubeNeighbors) + solve2 cubes allCubes
solve2 _ _ = 0

parseCube :: String -> Cube
parseCube str = 
    let (s1:s2:s3:[]) = splitOn "," str
    in (read s1, read s2, read s3)