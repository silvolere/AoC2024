import Data.Array
import Data.List (singleton)

type Coord = (Int,Int)

------ Part 1 ------

trailFind :: Array Coord Int -> Coord -> Int -> Int
trailFind arr (m, n) acc
    | m == 45 && n > 45 = acc
    | n > 45            = trailFind arr (m + 1, 1) acc
    | arr ! (m, n) /= 0 = trailFind arr (m, n + 1) acc
    | otherwise         = trailFind arr (m, n + 1) (acc + length (foldl (\ac x -> if x `elem` ac then ac else ac ++ [x]) [] $ pathCount (m, n) 0 []))
      where
        pathCount (m, n) h tops
            | m < 1 || m > 45 || n < 1 || n > 45 = tops
            | arr ! (m, n) == 9 && h == 9        = (m, n) : tops
            | arr ! (m, n) == h                  = pathCount (m - 1, n) (h + 1) $
                                                    pathCount (m + 1, n) (h + 1) $
                                                    pathCount (m, n - 1) (h + 1) $
                                                    pathCount (m, n + 1) (h + 1) tops
            | otherwise                          = tops

------ Part 2 ------

trailRateFind :: Array Coord Int -> Coord -> Int -> Int
trailRateFind arr (m, n) acc
    | m == 45 && n > 45 = acc
    | n > 45            = trailRateFind arr (m + 1, 1) acc
    | arr ! (m, n) /= 0 = trailRateFind arr (m, n + 1) acc
    | otherwise         = trailRateFind arr (m, n + 1) (acc + pathCount (m, n) 0)
      where
        pathCount (m, n) h
            | m < 1 || m > 45 || n < 1 || n > 45 = 0
            | arr ! (m, n) == 9 && h == 9        = 1
            | arr ! (m, n) == h                  = sum [pathCount (m - 1, n) (h + 1),
                                                        pathCount (m + 1, n) (h + 1),
                                                        pathCount (m, n - 1) (h + 1),
                                                        pathCount (m, n + 1) (h + 1)]
            | otherwise                          = 0

------- Main -------

main :: IO ()
main = do
    input <- readFile "day10.txt"
    let topography = listArray ((1, 1), (45, 45)) $ map (read . singleton) $ filter (/= '\n') input :: Array (Int, Int) Int
    print $ trailFind topography (1, 1) 0
    print $ trailRateFind topography (1, 1) 0