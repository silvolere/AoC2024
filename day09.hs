

------ Part 1 ------

discParse :: [Char] -> [(Maybe Int, Int)]
discParse input = discParse' input 0 []
  where
    discParse' [] _ form           = reverse form
    discParse' [x] num form        = reverse $ (Just num, read [x]) : form
    discParse' (xj:xn:xs) num form = discParse' xs (num + 1) ((Nothing, read [xn]) : (Just num, read [xj]) : form)

discCompact :: [(Maybe Int, Int)] -> [(Int, Int)]
discCompact list = discFold list 0 (reverse list) (length list - 1) []
  where
    discFold :: [(Maybe Int, Int)] -> Int -> [(Maybe Int, Int)] -> Int -> [(Int, Int)]
    discFold ((mXval, xl):xs) i ((mYval, yl):ys) j acc
        | 
        | isJust mXval = discFold xs i ((mYval, yl):ys) j (acc ++ [(fromJust mXval, xl)])

------ Part 2 ------

------- Main -------

main :: IO ()
main = do
    input <- readFile "day09.txt"
    let discData = discParse input
    print $ drop 19990 discData