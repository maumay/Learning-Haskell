import Data.Maybe (isNothing, isJust, fromJust)
import Data.Ord (comparing)
import Data.List (sortBy, find)
import Data.Bits (shiftL, (.&.), (.|.))
import Control.Monad (join)


type GridSquare = Maybe Int

possibleEntries :: [Int]
possibleEntries = [1..9]

squares2bitset :: [GridSquare] -> Int
squares2bitset xs = foldr acc 0 xs
                    where
                        (p, f)   = (isNothing, fromJust)
                        acc gs n = if p gs then n else n .|. (1 `shiftL` (f gs))


uniqueInts :: Int -> [Int] -> Bool
uniqueInts n []     = True
uniqueInts n (x:xs) = ((xShift .&. n) == 0) && (uniqueInts (n .|. xShift) xs)
                      where
                        xShift = 1 `shiftL` x

isLegalSquareSet :: [GridSquare] -> Bool
isLegalSquareSet xs = uniqueInts 0 (map fromJust (filter isJust xs))


type Grid  = [GridSquare]


isCompletedGrid :: Grid -> Bool
isCompletedGrid g = all isJust g


type Row   = [GridSquare]
type Col   = [GridSquare]
type Block = [GridSquare]

nthRow :: Grid -> Int -> Row
nthRow g n = map (g !!) (rowIndices !! n) 

rowIndices :: [[Int]]
rowIndices = let r = [0..8] in [[9 * row + m | m <- r] | row <- r]


nthCol :: Grid -> Int -> Col
nthCol g n = map (g !!) (colIndices !! n)

colIndices :: [[Int]]
colIndices = let r = [0..8] 
             in [[9 * row + col | row <- [0..8]] | col <- [0..8]]


nthBlock :: Grid -> Int -> Block
nthBlock g n = map (g!!) (blockIndices !! n)

blockIndices :: [[Int]]
blockIndices = let gr     = [0..80]
                   br     = [0..2]
                   bstart = [27 * x + 3 * y | x <- br, y <- br]
               in [[gr !! (s + x + 9*y) | x <- br, y <- br] | s <- bstart]


type GridPoint = (Int, Int)

gridpoints :: [GridPoint]
gridpoints = [(row, col) | row <- [0..8], col <- [0..8]]


point2entry :: Grid -> GridPoint -> GridSquare
point2entry g (row, col) = g !! (9 * row + col)


p2block :: Grid -> GridPoint -> Block
p2block g (r, c) = nthBlock g (3 * (r `div` 3) + (c `div` 3))


-- Could maybe make faster still by utilising laziness I think
legalEntries :: Grid -> GridPoint -> [Int]
legalEntries g p@(row, col) = filter pred possibleEntries
                               where
                                pred n    = ((1 `shiftL` n) .&. used) == 0
                                usedRow   = squares2bitset $ nthRow g row
                                usedCol   = squares2bitset $ nthCol g col
                                usedBlock = squares2bitset $ p2block g p
                                used      = usedRow .|. usedCol .|. usedBlock


emptyPoints :: Grid -> [GridPoint]
emptyPoints g = filter (isNothing . point2entry g) gridpoints


sortEmptyGridPoints :: Grid -> [(GridPoint, [Int])]
sortEmptyGridPoints g = sortBy (comparing $ length . snd) (zip xs ys)
                        where
                          xs = emptyPoints g
                          ys = map (legalEntries g) xs


evolveGrid :: Grid -> GridPoint -> Int -> Grid
evolveGrid g (r, c) e = (take index g) ++ (Just e : drop (index + 1) g)
                         where
                          index = 9 * r + c


solveGrid :: Grid -> Maybe Grid
solveGrid g | isCompletedGrid g = Just g
            | otherwise         = join $ find isJust (map solveGrid evolved)  
            where
                evolved                = concat $ map (ev g) (sortEmptyGridPoints g)
                ev g (p, legalentries) = map (evolveGrid g p) legalentries

-- IO stuff 

getSudokuRow :: IO Row
getSudokuRow = do 
  line <- getLine
  let split = words line
      nums = map fromJust (filter isJust (map readMaybe split))
  if length nums /= 9
    then do
      putStrLn "Invalid row, try inputting it again."
      getSudokuRow
    else
      let f = \n -> if 0 < n && n < 10 then Just n else Nothing
      in return $ map f nums


getSudokuGrid :: IO Grid
getSudokuGrid = do
  extractRows <- sequence $ take 9 (repeat getSudokuRow)
  return $ concat extractRows


readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing

extractRows :: Grid -> [[Int]]
extractRows [] = []
extractRows g  = (map fromJust (take 9 g)) : (extractRows $ drop 9 g)


main :: IO ()
main = do
  putStrLn "Enter 9 line of 9 white space separated numbers."
  grid <- getSudokuGrid
  let solved = solveGrid grid
  if isNothing solved
    then do
      putStrLn "Grid not solvable!"
    else do
      let g = fromJust solved
          r = map show (extractRows g)
      sequence_ (map putStrLn r)


