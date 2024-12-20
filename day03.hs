import Text.Regex.Posix ((=~), AllTextMatches(getAllTextMatches))

------ Part 1 ------

mulPattern :: String
mulPattern = "mul\\([0-9]{1,3},[0-9]{1,3}\\)"

mulRegex :: String -> Int
mulRegex dir = sum $ map (uncurry (*) . read . drop 3) (getAllTextMatches $ dir =~ mulPattern :: [String])

------ Part 2 ------

mulDoPattern :: String
mulDoPattern = "mul\\([0-9]{1,3},[0-9]{1,3}\\)|do\\(\\)|don't\\(\\)"

mulDoRegex :: String -> Int
mulDoRegex dir = sum $ mulSwitch [] True (getAllTextMatches $ dir =~ mulDoPattern :: [String])
  where
    mulSwitch acc _ [] = acc
    mulSwitch acc switch (m:ms)
        | m == "do()"    = mulSwitch acc True ms
        | m == "don't()" = mulSwitch acc False ms
        | not switch     = mulSwitch acc switch ms
        | otherwise      = mulSwitch ((uncurry (*) . read . drop 3) m:acc) switch ms

------ Main ------

main :: IO ()
main = do
    input <- readFile "day03.txt"
    print $ mulRegex input
    print $ mulDoRegex input