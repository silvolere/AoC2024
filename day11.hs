

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

------- Main -------

input :: [String]
input = words "4 4841539 66 5279 49207 134 609568 0"

main :: IO ()
main = do
    print $ blink input 25
    
    --print $ blink input 75