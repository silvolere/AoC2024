import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Control.Monad
import Control.Monad.ST
import Data.STRef

------ Part 1 ------

blink :: [String] -> Int -> Int
blink xs 0 = length xs
blink xs i = blink (map (show . (read :: String -> Int)) $ words $ unwords $ map blinkRules xs) (i - 1)
  where
    blinkRules x
        | x == "0" = "1"
        | even (length x) = (\(a,b) -> a ++ " " ++ b) $ splitAt (length x `div` 2) x
        | otherwise = show $ 2024 * (read x :: Int)

------ Part 2 ------

memoBlink :: [String] -> Int -> Int
memoBlink xs i = runST $ do
    keyvals <- newSTRef Map.empty
    stones <- newSTRef xs
    blinkTime (blinkRules stones) i
  where
    blinkTime stns 0 = readSTRef stns 
    blinkTime stns i = blinkTime (blinkRules stns) i
    blinkRules stns = do
        stns' <- readSTRef stns
        writeSTRef stns (blinkRules' stns')
    blinkRules' [] = []
    blinkRules' (y:ys) 
        | y `inSTMap` keyvals = do
            keyvals' <- readSTRef keyvals
            (keyvals' Map.! y) : blinkRules' ys

------- Main -------

input :: [String]
input = words "4 4841539 66 5279 49207 134 609568 0"

main :: IO ()
main = do
    print $ blink input 25
    
    --print $ blink input 75