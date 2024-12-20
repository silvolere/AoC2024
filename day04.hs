import Text.Regex.Posix ((=~), getAllTextMatches)
import Data.List (transpose)

------ Part 1 ------

xmasCount :: [String] -> Int
xmasCount strs = sum (map xmasCount' strs) + sum (map xmasCount' (reverse $ transpose strs))
  where
    xmasCount' str = length (getAllTextMatches $ str =~ "XMAS" :: [String])
                   + length (getAllTextMatches $ str =~ "SAMX" :: [String])
    xmasCountDiag 

------ Part 2 ------

------ Main ------

main :: IO ()
main = do
    input <- readFile "day04.txt"
    let wordSearch = lines input
    print $ xmasCount wordSearch