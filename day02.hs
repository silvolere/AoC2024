------ Part 1 ------

isSafe :: (Ord a, Num a) => [a] -> Bool
isSafe (x:xs)
    | x > 0 = isSafe' (x:xs)
    | x < 0 = isSafe' $ map (* (-1)) (x:xs)
    | otherwise = False
  where
    isSafe' [] = True
    isSafe' (y:ys)
        | y > 0 && y < 4 = isSafe' ys
        | otherwise = False

safeCount :: (Ord a, Num a) => [[a]] -> Int
safeCount = length . filter isSafe . map (\y -> zipWith (-) (tail y) y)

------ Part 2 ------

isDampSafe :: (Ord a, Num a) => [a] -> Bool
isDampSafe (x:xs)
    | x > 0 = isDampSafe' (x:xs) True
    | x < 0 = isDampSafe' (map (* (-1)) (x:xs)) True
    | otherwise = False
  where
    isDampSafe' [] _ = True
    isDampSafe' (y:ys) damp
        | y > 0 && y < 4 = isDampSafe' ys damp
        | damp = isDampSafe' ys False
        | otherwise = False

dampSafeCount :: (Ord a, Num a) => [[a]] -> Int
dampSafeCount = length . filter isDampSafe . map (\y -> zipWith (-) (tail y) y)

------ Main ------

test = [[7, 6, 4, 2, 1],[1, 2, 7, 8, 9],[9, 7, 6, 2, 1],[1, 3, 2, 4, 5],[8, 6, 4, 4, 1],[1, 3, 6, 7, 9]]

main :: IO ()
main = do
    input <- readFile "day02.txt"
    let reports = map (map read . words) $ lines input :: [[Int]]
    print $ safeCount reports
    print $ dampSafeCount reports