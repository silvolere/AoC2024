{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}
import Data.List.Split (splitOn)

------ Part 1 ------

calibrate :: Int -> [Int] -> Bool
calibrate 0 [] = True
calibrate _ [] = False
calibrate big (x:xs)
    | big `mod` x == 0 = findTrue (calibrate (big - x) xs,calibrate (big `div` x) xs)
    | big < 0          = False
    | otherwise        = calibrate (big - x) xs

findTrue :: (Bool, Bool) -> Bool
findTrue (a,b) | a = True | b = True | otherwise = False

calibrationResults :: [(Int,[Int])] -> Int
calibrationResults eqs = sum $ map fst $ filter (uncurry calibrate) eqs

------ Part 2 ------

catCalibrate :: Int -> [Int] -> Bool
catCalibrate 0 [] = True
catCalibrate _ [] = False
catCalibrate big [x] = calibrate big [x]
catCalibrate big (x:xm:xs)
    | big `mod` x == 0 = find3True (catCalibrate (big - x) (xm:xs), catCalibrate (big `div` x) (xm:xs), catCalibrate big ((x <+> xm):xs))
    | big < 0          = False
    | otherwise        = findTrue (catCalibrate (big - x) (xm:xs), catCalibrate big ((x <+> xm):xs))

(<+>) :: (Show a, Integral a, Read a) => a -> a -> a
(<+>) a b = read $ show a ++ show b

find3True :: (Bool,Bool,Bool) -> Bool
find3True (a,b,c) | a = True | b = True | c = True | otherwise = False

catCalibrationResults :: [(Int, [Int])] -> Int
catCalibrationResults eqs = sum $ map fst $ filter (uncurry catCalibrate) eqs

------- Main -------

main :: IO ()
main = do
    input <- readFile "day07.txt"
    let equations = map ((\[a, b] -> (read a,reverse $ map read $ tail $ splitOn " " b)) . splitOn ":") $ lines input :: [(Int,[Int])]
    print $ calibrationResults equations
    print $ catCalibrationResults equations