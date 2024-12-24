{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

import Text.Regex.Posix ((=~), getAllTextMatches)
import Data.List (transpose, sort)
import qualified Data.Matrix as Mat

------ Part 1 ------

xmasCount :: [String] -> Int
xmasCount strs = sum (map xmasCount' strs) + sum (map xmasCount' (reverse $ transpose strs)) + xmasCountDiag
  where
    xmasCount' str = length (getAllTextMatches $ str =~ "XMAS" :: [String])
                   + length (getAllTextMatches $ str =~ "SAMX" :: [String])
    xmasCountDiag = sum (map xmasCount' $ diagonalize strs) + sum (map xmasCount' $ diagonalize $ reverse strs)

diagonalize :: [[a]] -> [[a]]
diagonalize lists = lower colLen ++ (init $ upper colLen)
  where 
    lower 0 = []
    lower n = map (head . head) (take (colLen - n + 1) (iterate (map (drop 1) . drop 1) (drop (n - 1) lists))) : (lower (n - 1))
    upper 0 = []
    upper n = map (head . head) (take (colLen - n + 1) (iterate (map (drop 1) . drop 1) (drop (n - 1) (transpose lists)))) : (upper (n - 1))
    rowLen = length $ head lists
    colLen = length lists

------ Part 2 ------

x_masCount :: [String] -> Int
x_masCount strs = x_masCount' (Mat.fromLists strs) 2 2 0
  where 
    x_masCount' mat m n acc
        | m == Mat.nrows mat = acc
        | n == Mat.ncols mat = x_masCount' mat (m + 1) 2 acc
        | Mat.getElem m n mat == 'A' = x_masCount' mat m (n + 1) (validA mat m n acc)
        | otherwise = x_masCount' mat m (n + 1) acc
    validA mat m n acc 
        | (sort [Mat.getElem (m - 1) (n - 1) mat,Mat.getElem (m - 1) (n + 1) mat,Mat.getElem (m + 1) (n - 1) mat,Mat.getElem (m + 1) (n + 1) mat] == "MMSS") && (Mat.getElem (m - 1) (n - 1) mat /= Mat.getElem (m + 1) (n + 1) mat) = acc + 1
        | otherwise = acc

------ Main ------

main :: IO ()
main = do
    input <- readFile "day04.txt"
    let wordSearch = lines input
    print $ xmasCount wordSearch
    print $ x_masCount wordSearch