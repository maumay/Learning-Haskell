import Data.Maybe (isNothing, isJust, fromJust)
import Data.Ord (comparing)
import Data.List (sortBy, find)
import Data.Bits (shiftL, (.&.), (.|.))
import Control.Monad (join)


type GridSquare = Maybe Int

uniqueInts :: Int -> [Int] -> Bool
uniqueInts n []     = True
uniqueInts n (x:xs) = ((xShift .&. n) == 0) && (uniqueInts (n .|. xShift) xs)
                      where
                        xShift  = 1 `shiftL` x

isLegalSquareSet :: [GridSquare] -> Bool
isLegalSquareSet xs = uniqueInts 0 (map fromJust (filter isJust xs))


type Grid  = [GridSquare]
type Row   = [GridSquare]
type Col   = [GridSquare]
type Block = [GridSquare]

nthRow :: Grid -> Int -> Row
nthRow g n = map (g!!) (rowIndices!!n) 

rowIndices :: [[Int]]
rowIndices = let r = [0..8]
             in [[9 * row + m | m <- r] | row <- r]


nthCol :: Grid -> Int -> Col
nthCol g n = map (g!!) (colIndices!!n)

colIndices :: [[Int]]
colIndices = let r = [0..8]
             in [[9 * row  + col | row <- [0..8]] | col <- [0..8]]


nthBlock :: Grid -> Int -> Block
nthBlock g n = map (g!!) (blockIndices!!n)

blockIndices :: [[Int]]
blockIndices = let gr     = [0..80]
                   blen   = [0..2]
                   bstart = [27 * x + 3 * y | x <- blen, y <- blen]
               in [[gr!!(s + x + 9*y) | x <- blen, y <- blen] | s <- bstart]


isCompletedGrid :: Grid -> Bool
isCompletedGrid g = all isJust g


type GridPoint = (Int, Int)

gridpoints :: [GridPoint]
gridpoints = [(row, col) | row <- [0..8], col <- [0..8]]


point2entry :: Grid -> GridPoint -> GridSquare
point2entry g (row, col) = g !! (9 * row + col)


point2row :: Grid -> GridPoint -> Row
point2row g (row, col) = filter isJust $ nthRow g row


point2col :: Grid -> GridPoint -> Col
point2col g (row, col) = filter isJust $ nthCol g col


point2block :: Grid -> GridPoint -> Block
point2block g p = filter isJust $ nthBlock g $ f p
                  where
                   f (row, col) = (3*(row `div` 3)) + (col `div` 3)


emptyPoints :: Grid -> [GridPoint]
emptyPoints g = filter (isNothing . point2entry g) gridpoints


legalEntries :: Grid -> GridPoint -> [Int]
legalEntries g point | isJust $ point2entry g point = error ""
                     | otherwise                    = filter p [1..9]
                      where
                        (p2r, p2c, p2b) = (point2row, point2col, point2block)
                        xs              = map ($ point) [p2r g, p2c g, p2b g]
                        p n             = all isLegalSquareSet $ map (Just n :) xs


sortGridPoints :: Grid -> [(GridPoint, [Int])]
sortGridPoints g = sortBy (comparing $ length . snd ) (zip xs ys)
                   where
                    xs = emptyPoints g
                    ys = map (legalEntries g) xs


-- No checks on legality
evolveGrid :: Grid -> GridPoint -> Int -> Grid
evolveGrid g (r, c) e = (take index g) ++ (Just e : drop (index + 1) g)
                         where
                          index = 9 * r + c


multiEvolveGrid :: Grid -> (GridPoint, [Int]) -> [Grid]
multiEvolveGrid g (p, xs) = map (evolveGrid g p) xs


solveGrid :: Grid -> Maybe Grid
solveGrid g | isCompletedGrid g = Just g
            | otherwise         = join $ find isJust (map solveGrid evolved)  
            where
                evolved = concat $ map (multiEvolveGrid g) (sortGridPoints g)


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


