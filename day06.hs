import Data.Matrix (fromLists, (!), Matrix (ncols, nrows), safeGet)


data Dir = N | S | E | W deriving (Show, Eq)
type Pos = (Int,Int)
type Coords = [Pos]

------ Part 1 ------

getStart :: Matrix Char -> Pos
getStart mat = getStart' (1,1)
  where
    r = nrows mat
    c = ncols mat
    getStart' pos@(p1,p2)
        | mat ! pos == '^' = pos
        | p2 < c           = getStart' (p1,p2 + 1)
        | otherwise        = getStart' (p1 + 1,1)

guardTrace :: Matrix Char -> Dir -> Pos -> Coords
guardTrace mat dir' pos' = guardTrace' dir' pos' []
  where
    guardTrace' dir pos@(p1,p2) acc
        | p1 < 1 || p2 < 0 || p1 > r || p2 > c = posCheck acc pos
        | dir == N = if safeGet (p1 - 1) p2 mat == Just '#' then guardTrace' E pos acc else guardTrace' dir (p1 - 1,p2) (posCheck acc pos)
        | dir == S = if safeGet (p1 + 1) p2 mat == Just '#' then guardTrace' W pos acc else guardTrace' dir (p1 + 1,p2) (posCheck acc pos)
        | dir == E = if safeGet p1 (p2 + 1) mat == Just '#' then guardTrace' S pos acc else guardTrace' dir (p1,p2 + 1) (posCheck acc pos)
        | dir == W = if safeGet p1 (p2 - 1) mat == Just '#' then guardTrace' N pos acc else guardTrace' dir (p1,p2 - 1) (posCheck acc pos)
    r = nrows mat
    c = nrows mat
    posCheck a p = if p `elem` a then a else p:a

spaceCount :: Matrix Char -> Int
spaceCount mat = length $ guardTrace mat N (getStart mat)

------ Part 2 ------

------- Main -------

test = "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#..."

main :: IO ()
main = do
    input <- readFile "day06.txt"
    let posMap = fromLists $ lines input
    print $ spaceCount posMap