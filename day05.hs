import Data.List.Split (splitOn)
import Data.List (sortBy)

------ Part 1 ------

orderCheck :: [(Int,Int)] -> [Int] -> [Int] -> Bool
orderCheck _ _ [] = True
orderCheck rules acc (o:os)
    | null currRule                      = orderCheck rules (o:acc) os
    | afterCheck (map snd currRule) acc = False
    | otherwise                          = orderCheck rules (o:acc) os
  where
    currRule = filter (\(a,_) -> o == a) rules
    afterCheck _ [] = False
    afterCheck rule (a:as)
        | a `elem` rule = True
        | otherwise     = afterCheck rule as

validFilter :: [(Int,Int)] -> [[Int]] -> [[Int]]
validFilter rules = filter (orderCheck rules [])

getMiddle :: [a] -> a
getMiddle xs = xs !! max 0 (length xs `div` 2)

validOrderSum :: [(Int,Int)] -> [[Int]] -> Int
validOrderSum rules orders = sum $ map getMiddle $ validFilter rules orders

------ Part 2 ------

reordering :: [(Int,Int)] -> [Int] -> [Int]
reordering rules = sortBy ruleCompare
  where
    ruleCompare num1 num2
        | num1 `elem` map fst (filter (\(_,a) -> num2 == a) rules) = LT
        | num2 `elem` map fst (filter (\(_,a) -> num1 == a) rules) = GT
        | otherwise                                                = EQ

invalidFilter :: [(Int, Int)] -> [[Int]] -> [[Int]]
invalidFilter rules = filter (not . orderCheck rules [])

validReorderSum :: [(Int, Int)] -> [[Int]] -> Int
validReorderSum rules orders = sum $ map (getMiddle . reordering rules) (invalidFilter rules orders)

------ Main ------

main :: IO ()
main = do
    input <- readFile "day05.txt"
    let rules = map ((\[a,b] -> (a,b)) . map read . splitOn "|") $ take 1176 $ lines input :: [(Int,Int)]
    let orderings = map (read . (\a -> "[" ++ a ++ "]")) $ drop 1177 $ lines input :: [[Int]]
    print $ validOrderSum rules orderings
    print $ validReorderSum rules orderings