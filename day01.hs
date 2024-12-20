import Data.List (sort)

------ Part 1 ------

zipDiffSum :: [Int] -> [Int] -> Int
zipDiffSum l1 l2 = sum $ map abs $ zipWith (-) (sort l1) (sort l2)

------ Part 2 ------

similarityScore :: [Int] -> [Int] -> Int -> Int
similarityScore [] _ score = score
similarityScore (x:xs) l2 score = similarityScore xs l2 (score + sum (filter (== x) l2))

------ Main ------

main :: IO ()
main = do
    input <- readFile "day01.txt"
    let line1 = map (read . head . words) (lines input) :: [Int]
    let line2 = map (read . last . words) (lines input) :: [Int]
    print $ zipDiffSum line1 line2
    print $ similarityScore line1 line2 0