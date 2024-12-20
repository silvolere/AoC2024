import Data.List.Split (splitOn)
import Control.Monad (liftM2)

------ Part 1 ------

robotPos :: [(Int,Int)] -> [(Int,Int)] -> Int -> [(Int,Int)]
robotPos pos vel time = map (\(a,b) -> (a `rem` 101,b `rem` 103)) $
                        zipWith (\(a,b) (c,d) -> (a + c,b + d)) pos (map (\(a,b) -> (a * time,b * time)) vel)

safetyFactor :: [(Int,Int)] -> [(Int,Int)] -> Int
safetyFactor pos vel = ul * ur * bl * br
  where
    finalPos = robotPos pos vel 100
    ul = length $ filter (liftM2 (&&) (\(a,_) -> a < 50) (\(_,b) -> b < 51)) finalPos
    ur = length $ filter (liftM2 (&&) (\(a,_) -> a < 50) (\(_,b) -> b > 51)) finalPos
    bl = length $ filter (liftM2 (&&) (\(a,_) -> a > 50) (\(_,b) -> b < 51)) finalPos
    br = length $ filter (liftM2 (&&) (\(a,_) -> a > 50) (\(_,b) -> b > 51)) finalPos

------ Part 2 ------

------- Main -------

main :: IO ()
main = do
    input <- readFile "day14.txt"
    let positions = map (read . (\a -> '(' : a ++ ")") .
                    filter (liftM2 (&&) (/= 'p') (/= '=')) . head . splitOn " ") $ lines input :: [(Int,Int)]
    let velocities = map ((\(a,b) -> (a + 101, b + 103)) . read . (\a -> '(' : a ++ ")") .
                     filter (liftM2 (&&) (/= 'v') (/= '=')) . last . splitOn " ") $ lines input :: [(Int,Int)]
    print $ safetyFactor positions velocities