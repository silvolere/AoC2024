import qualified Data.Map.Strict as Map

------ Part 1 ------

numpadMap :: Map.Map Char (Int, Int)
numpadMap = Map.fromList [('7',(1,1)),('8',(1,2)),('9',(1,3)),('4',(2,1)),('5',(2,2)),('6',(2,3)),('1',(3,1)),('2',(3,2)),('3',(3,3)),('0',(4,2)),('A',(4,3))]

dirpadMap :: Map.Map Char (Int,Int)
dirpadMap = Map.fromList [('^',(1,2)),('A',(1,3)),('<',(2,1)),('v',(2,2)),('>',(2,3))]

numpadSequence :: String -> String
numpadSequence code = numpadSequence' code (4,3)
  where
    numpadSequence' [] _ = []
    numpadSequence' (x:xs) (c1,c2)
        | leftUpFix c1 c2 x  = (replicate (abs (c2 - (n2 x)) - 1) '<') ++ "^<" ++ (replicate (abs (c1 - (n1 x)) - 1) 'v') ++ "A" ++ numpadSequence' xs (numLookup x)
        | rightUp c1 c2 x    = (replicate (abs (c2 - (n2 x))) '>') ++ (replicate (abs (c1 - (n1 x))) '^') ++ "A" ++ numpadSequence' xs (numLookup x)
        | leftUp c1 c2 x     = (replicate (abs (c2 - (n2 x))) '<') ++ (replicate (abs (c1 - (n1 x))) '^') ++ "A" ++ numpadSequence' xs (numLookup x)
        | rightDown c1 c2 x  = (replicate (abs (c2 - (n2 x))) '>') ++ (replicate (abs (c1 - (n1 x))) 'v') ++ "A" ++ numpadSequence' xs (numLookup x)
        | leftDown c1 c2 x   = (replicate (abs (c2 - (n2 x))) '<') ++ (replicate (abs (c1 - (n1 x))) 'v') ++ "A" ++ numpadSequence' xs (numLookup x)
    numLookup x = numpadMap Map.! x
    n1 x = fst $ numLookup x
    n2 x = snd $ numLookup x
    leftUpFix c1 c2 x = (c1 == 4) && (n2 x == 1)
    rightUp c1 c2 x   = (c1 >= n1 x) && (c2 < n2 x) -- right: c2 <, left: c2 >, up: c1 >, down: c1 <
    leftUp c1 c2 x    = (c1 >= n1 x) && (c2 >= n2 x)
    rightDown c1 c2 x = (c1 < n1 x) && (c2 < n2 x)
    leftDown c1 c2 x  = (c1 < n1 x) && (c2 >= n2 x)

dirpadSequence :: String -> String
dirpadSequence code = dirpadSequence' code (1,3)
  where
    dirpadSequence' [] _ = []
    dirpadSequence' (x:xs) (c1,c2)
        | leftDownFix c1 x  = (replicate (abs (c2 - (n2 x)) - 1) '<') ++ "v<A" ++ dirpadSequence' xs (dirLookup x)
        | rightUp c1 c2 x   = (replicate (abs (c2 - (n2 x))) '>') ++ (replicate (abs (c1 - (n1 x))) '^') ++ "A" ++ dirpadSequence' xs (dirLookup x)
        | leftUp c1 c2 x    = (replicate (abs (c2 - (n2 x))) '<') ++ (replicate (abs (c1 - (n1 x))) '^') ++ "A" ++ dirpadSequence' xs (dirLookup x)
        | rightDown c1 c2 x = (replicate (abs (c2 - (n2 x))) '>') ++ (replicate (abs (c1 - (n1 x))) 'v') ++ "A" ++ dirpadSequence' xs (dirLookup x)
        | leftDown c1 c2 x  = (replicate (abs (c2 - (n2 x))) '<') ++ (replicate (abs (c1 - (n1 x))) 'v') ++ "A" ++ dirpadSequence' xs (dirLookup x)
    dirLookup x = dirpadMap Map.! x
    n1 x = fst $ dirLookup x
    n2 x = snd $ dirLookup x
    leftDownFix c1 x  = (c1 == 1) && (n2 x == 1)
    rightUp c1 c2 x   = (c1 >= n1 x) && (c2 < n2 x) -- right: c2 <, left: c2 >, up: c1 >, down: c1 <
    leftUp c1 c2 x    = (c1 >= n1 x) && (c2 >= n2 x)
    rightDown c1 c2 x = (c1 < n1 x) && (c2 < n2 x)
    leftDown c1 c2 x  = (c1 < n1 x) && (c2 >= n2 x)

complexitySum :: [String] -> Int
complexitySum codes = sum $ zipWith (*) (map (read . init) codes) (map (length . dirpadSequence . dirpadSequence . numpadSequence) codes)

------ Part 2 ------

------- Main -------

input :: [String]
input = lines "340A\n149A\n582A\n780A\n463A"

test = lines "029A\n980A\n179A\n456A\n379A"

main :: IO ()
main = do
    print $ numpadSequence $ head test
    print $ complexitySum test

-- 029A: <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
--     <v<A>A<A>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
--     v<<A>>^A<A>AvA<^AA>A<vAAA>^A
--     <v<A>>^A<A>AvA<^AA>A<vAAA>^A
-- <A^A>^^AvvvA
-- <A^A>^^AvvvA