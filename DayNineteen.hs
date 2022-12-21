import DayZero
import Data.List.Split
import qualified Data.Map as M
import System.IO.Unsafe

data Blueprint = Blueprint {id :: Int, ore :: (Robot, Cost), clay :: (Robot, Cost), obsidian :: (Robot, Cost), geode :: (Robot, Cost)}
    deriving Show

data Robot = Robot Resource
    deriving (Show, Eq)

data Resource = Ore | Clay | Obsidian | Geode
    deriving (Eq, Ord, Show)

data Inventory = Inventory {robots :: [Robot], resources :: M.Map Resource Int}

data Cost = Cost [(Int, Resource)]
    deriving (Show, Eq)

initialInventory :: Inventory
initialInventory = Inventory [Robot Ore] (M.fromList [(Ore, 0), (Clay, 0), (Obsidian, 0), (Geode, 0)])

main :: IO ()
main = do
    blueprints <- manuallyParse "d19" "\n" parseBlueprint
    let solutions = map (\b -> solve1 $ b) blueprints
    print $ sum $ map (\(i, s) -> i * s) $ zip [1..] solutions
    let solutions2 = map solve2 (take 3 blueprints)
    print $ product solutions2


solve1 :: Blueprint -> Int
solve1 blueprint = solve1' 24 blueprint initialInventory

solve2 :: Blueprint -> Int
solve2 blueprint = solve1' 32 blueprint initialInventory



solve1' :: Int -> Blueprint -> Inventory -> Int
solve1' i blueprint inventory = 
    let possibleRobotPurchases = filter checkMaxNumber $ filter (\(_, _, time) -> time < i) $ getPurchases blueprint (inventory)
        checkMaxNumber (Robot resource, _, _) = resource == Geode || length (filter (== Robot resource) (robots inventory)) < maxCostOfResource resource blueprint
        maxCostOfResource r (Blueprint _ (_, Cost cOre) (_, Cost cClay) (_, Cost cObs) (_, Cost cGeode)) = 
            let costs = map fst $ filter (\(_, r') -> r == r') (cOre ++ cClay ++ cObs ++ cGeode)
            in if length costs == 0 then error (show r) else maximum costs
        possibleMore = canTheoreticallyMakeMoreGeodeBot (i-1) blueprint (length $ filter (\r -> r == Robot Obsidian) $ robots inventory) $ (resources inventory) M.! Obsidian
    in unsafePerformIO $ do
        --if possibleMore || (resources inventory) M.! Geode <= 7 then return () else print $ (resources inventory) M.! Geode + i * (length $ filter (\r -> r == Robot Geode) $ robots inventory)
        return $ 
            if i == 0 
                then (resources inventory) M.! Geode
            else if i < 0
                then 0
            else if (not possibleMore) || length possibleRobotPurchases == 0 then solve1' (i-1) blueprint (tick 1 inventory)
            else maximum $  map (\(r, c, i') -> solve1' (i - 1 - i') blueprint (makePurchase (r, c) (tick (1 + i') inventory))) possibleRobotPurchases



canTheoreticallyMakeMoreGeodeBot :: Int -> Blueprint -> Int -> Int -> Bool
canTheoreticallyMakeMoreGeodeBot i blueprint obsidianBots obsidian =
    let (Cost geodeCosts) = snd $ geode blueprint
        obsidianCost = fst $ head $ filter (\(i, r) -> r == Obsidian) geodeCosts
    in obsidian + obsidianBots*i + (i*(i+1)) `div` 2 >= obsidianCost

makePurchase :: (Robot, Cost) -> Inventory -> Inventory
makePurchase (robot, Cost costs) (Inventory robots resources) =
    let robots' = robot : robots
        resources' = updateResources costs resources
        updateResources ((i, resource):cs) r = updateResources cs (M.update (\x -> Just (x - i)) resource r)
        updateResources [] r = r
    in Inventory robots' resources'

getPurchases :: Blueprint -> Inventory -> [(Robot, Cost, Int)]
getPurchases b (Inventory robots resources) =
    let possible = map (\f -> f b) [ore, clay, obsidian, geode]
        canAffordNow = filter (\(_, Cost c) -> and $ map (\(i, resource) -> resources M.! resource >= i) c) possible
        cannotAffordNow = filter (\c -> not $ c `elem` canAffordNow) possible
        canAffordLater = filter (\(_, _, i) -> i /= 999) $ map (\(r, c) -> (r, c, timeTilAfford c robots resources)) cannotAffordNow
    in canAffordLater ++ (map (\(a,b) -> (a,b,0)) canAffordNow)

timeTilAfford :: Cost -> [Robot] -> M.Map Resource Int -> Int
timeTilAfford (Cost cs) robots resources = timeTilAfford' cs (resourcePerSec robots)
    where timeTilAfford' ((num, resource):cs) resources' = 
            if resources' M.! resource == 0 
                then 999 
                else max ((num - resources M.! resource + resources' M.! resource - 1) `div` (resources' M.! resource)) $ timeTilAfford' cs resources'
          timeTilAfford' [] _ = 0

resourcePerSec :: [Robot] -> M.Map Resource Int
resourcePerSec robots = foldl (\m (Robot resource) -> M.update (\r -> Just (r+1)) resource m) (M.fromList [(Ore, 0), (Clay, 0), (Obsidian, 0), (Geode, 0)]) robots

tick :: Int -> Inventory -> Inventory
tick 0 inventory = inventory
tick i inventory = tick (i-1) $ Inventory (robots inventory) (updateMap (resources inventory) (robots inventory))
    where updateMap resources ((Robot resource):robots) = updateMap (M.update (\r -> Just (r+1)) resource resources) robots
          updateMap resources [] = resources


parseBlueprint :: String -> Blueprint
parseBlueprint str =    
    let splitStr = splitOn " " str
    in Blueprint 
        (read $ takeWhile isNumber (splitStr !! 1)) 
        (Robot Ore, Cost [(read $ splitStr !! 6, Ore)]) 
        (Robot Clay, Cost [(read $ splitStr !! 12, Ore)])
        (Robot Obsidian, Cost [(read $ splitStr !! 18, Ore), (read $ splitStr !! 21, Clay)])
        (Robot Geode, Cost [(read $ splitStr !! 27, Ore), (read $ splitStr !! 30, Obsidian)])
