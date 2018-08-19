import Data.Maybe (isNothing, isJust, fromJust)
import Data.List (find, minimumBy)
import Data.Bits (shiftL, (.&.), (.|.), finiteBitSize, xor)
import Control.Monad (join)


-- Misc stuff
possibleEntries :: [Int]
possibleEntries = [1..9]

shift :: Int -> Int
shift = (1 `shiftL`)

fullEntrySet :: Int
fullEntrySet = let shifted = map shift possibleEntries
               in foldr (.|.) 0 shifted

minimumBy' :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumBy' f [] = Nothing
minimumBy' f xs = Just (minimumBy f xs)


-- Required types
type GridSquare = Maybe Int
type Grid       = [GridSquare]
type Row        = [GridSquare]
type Col        = [GridSquare]
type Block      = [GridSquare]
type GridPoint  = (Int, Int)


-- Constants 
gridpoints :: [GridPoint]
gridpoints = [(row, col) | row <- [0..8], col <- [0..8]]

rowIndices :: [[Int]]
rowIndices = let r = [0..8] in [[9 * row + m | m <- r] | row <- r]

colIndices :: [[Int]]
colIndices = let r = [0..8] 
             in [[9 * row + col | row <- [0..8]] | col <- [0..8]]

blockIndices :: [[Int]]
blockIndices = let gr     = [0..80]
                   br     = [0..2]
                   bstart = [27 * x + 3 * y | x <- br, y <- br]
               in [[gr !! (s + x + 9*y) | x <- br, y <- br] | s <- bstart]


-- Functions
squares2bitset :: [GridSquare] -> Int
squares2bitset xs = foldr acc 0 xs
                    where
                        acc gs n | isNothing gs = n 
                                 | otherwise    = n .|. (shift $ fromJust gs)

emptyPoints :: Grid -> [GridPoint]
emptyPoints g = let point2entry (r, c) = g !! (9 * r + c)
                in filter (isNothing . point2entry) gridpoints

nthRow :: Grid -> Int -> Row
nthRow g n = map (g !!) (rowIndices !! n) 

nthCol :: Grid -> Int -> Col
nthCol g n = map (g !!) (colIndices !! n)

nthBlock :: Grid -> Int -> Block
nthBlock g n = map (g!!) (blockIndices !! n)

usedEntries :: Grid -> GridPoint -> Int
usedEntries g (row, col) = usedRow .|. usedCol .|. usedBlock
                             where
                              usedRow     = squares2bitset $ nthRow g row
                              usedCol     = squares2bitset $ nthCol g col
                              usedBlock   = squares2bitset $ p2block row col
                              p2block r c = nthBlock g (3 * (r `div` 3) + (c `div` 3))

nLegalEntries :: Grid -> GridPoint -> Int
nLegalEntries g p = finiteBitSize $ fullEntrySet `xor` (usedEntries g p)

legalEntries :: Grid -> GridPoint -> [Int]
legalEntries g p = filter pred possibleEntries
                    where
                     pred n = ((shift n) .&. used) == 0
                     used   = usedEntries g p

getTargetPoint :: Grid -> Maybe GridPoint
getTargetPoint g = minimumBy' f (emptyPoints g)
                   where
                    f p q = compare (nLegalEntries g p) (nLegalEntries g q)

evolveGrid :: Grid -> GridPoint -> Int -> Grid
evolveGrid g (r, c) e = let index = 9 * r + c 
                        in (take index g) ++ (Just e : drop (index + 1) g)                          

solveGrid :: Grid -> Maybe Grid
solveGrid g | isNothing mtarget = Just g
            | otherwise         = join $ find isJust (map solveGrid evolved)
               where
                mtarget  = getTargetPoint g
                target   = fromJust mtarget
                entries  = legalEntries g target
                evolved  = map (evolveGrid g target) entries

-- IO stuff 

uniqueInts :: Int -> [Int] -> Bool
uniqueInts n []     = True
uniqueInts n (x:xs) = let y = shift x
                      in ((y .&. n) == 0) && (uniqueInts (n .|. y) xs)

isLegalSquareSet :: [GridSquare] -> Bool
isLegalSquareSet xs = uniqueInts 0 (map fromJust (filter isJust xs))

isLegalStartGrid :: Grid -> Bool
isLegalStartGrid g | length g /= 81 = False
                   | otherwise      = all isLegalSquareSet combined
                    where
                        rows     = map (nthRow g) [0..8]
                        cols     = map (nthCol g) [0..8]
                        blocks   = map (nthBlock g) [0..8]
                        combined = rows ++ cols ++ blocks

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing

extractRows :: Grid -> [[Int]]
extractRows [] = []
extractRows g  = (map fromJust (take 9 g)) : (extractRows $ drop 9 g)


getSudokuRow :: IO Row
getSudokuRow = do 
  line <- getLine
  let split = words line
      nums = map fromJust (filter isJust (map readMaybe split))
  if length nums /= 9
    then do
      putStrLn "Invalid row, try  again."
      getSudokuRow
    else
      let f n = if 0 < n && n < 10 then Just n else Nothing
      in return $ map f nums


getSudokuGrid :: IO Grid
getSudokuGrid = do
  rows <- sequence $ take 9 (repeat getSudokuRow)
  let grid = concat rows
  if isLegalStartGrid grid
    then return grid
  else do
    putStrLn "Invalid grid, try again."
    getSudokuGrid


main :: IO ()
main = do
  putStrLn "Enter 9 rows of 9 white space separated numbers. Zero represents absence."
  grid <- getSudokuGrid
  let solved = solveGrid grid
  if isNothing solved
    then do
      putStrLn "Grid not solvable!"
    else do
      let g = fromJust solved
          r = map show (extractRows g)
      sequence_ (map putStrLn r)

