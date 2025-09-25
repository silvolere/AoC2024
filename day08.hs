import Data.Array
import Control.Monad (join)

------ Part 1 ------

antennaLookup :: Eq a => (Int, Int) -> Array (Int, Int) a -> [(Int, Int)]
antennaLookup ant arr = antennaLookup' (1, 1) []
  where
    antennaLookup' (50, 51) xs = xs
    antennaLookup' (m, 51) xs  = antennaLookup' (m + 1, 1) xs
    antennaLookup' (m, n) xs
        | (m, n) == ant                 = antennaLookup' (m, n + 1) xs
        | (arr ! (m, n)) == (arr ! ant) = antennaLookup' (m, n + 1) ((m, n) : xs)
        | otherwise                     = antennaLookup' (m, n + 1) xs

inBounds :: (Ord a, Ord b, Num a, Num b) => (a, b) -> Bool
inBounds (m, n) = m > 0 && m < 51 && n > 0 && n < 51

freqGen :: (Int, Int) -> [(Int, Int)]  -> [(Int, Int)]
freqGen start ants = freqGen' start ants []
  where 
    freqGen' _ [] freqs                 = filter inBounds freqs
    freqGen' (m, n) ((xm, xn):xs) freqs = freqGen' (m, n) xs ((2 * xm - m, 2 * xn - n):freqs)

antinodeFind :: Array (Int, Int) Char -> Int
antinodeFind arr = antinodeFind' (1, 1) []
  where
    antinodeFind' (50, 51) ixs = length $ foldl (\acc x -> if x `elem` acc then acc else acc ++ [x]) [] ixs
    antinodeFind' (m, 51) ixs  = antinodeFind' (m + 1, 1) ixs
    antinodeFind' (m, n) ixs
        | arr ! (m, n) == '.' = antinodeFind' (m, n + 1) ixs
        | otherwise           = antinodeFind' (m, n + 1) (freqGen (m, n) (antennaLookup (m, n) arr) ++ ixs)

------ Part 2 ------

multiFreqGen :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
multiFreqGen start ants = multiFreqGen' start ants []
  where 
    multiFreqGen' _ [] freqs                 = freqs
    multiFreqGen' (m, n) ((xm, xn):xs) freqs = multiFreqGen' (m, n) xs (antennaRepeat (m, n) (xm, xn) ++ freqs)

antennaRepeat :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
antennaRepeat (m, n) (xm, xn) = filter inBounds $ take 50 $ iterate (\(a, b) -> (a + xm - m, b + xn - n)) (xm, xn)

multiAntinodeFind :: Array (Int, Int) Char -> Int
multiAntinodeFind arr = multiAntinodeFind' (1, 1) []
  where
    multiAntinodeFind' (50, 51) ixs = length $ foldl (\acc x -> if x `elem` acc then acc else acc ++ [x]) [] ixs
    multiAntinodeFind' (m, 51) ixs  = multiAntinodeFind' (m + 1, 1) ixs
    multiAntinodeFind' (m, n) ixs
        | arr ! (m, n) == '.' = multiAntinodeFind' (m, n + 1) ixs
        | otherwise           = multiAntinodeFind' (m, n + 1) (multiFreqGen (m, n) (antennaLookup (m, n) arr) ++ ixs)

------- Main -------

main :: IO ()
main = do
    input <- readFile "day08.txt"
    let antennaMap = listArray ((1,1), (50, 50)) $ join $ lines input :: Array (Int,Int) Char
    print $ antinodeFind antennaMap
    print $ multiAntinodeFind antennaMap