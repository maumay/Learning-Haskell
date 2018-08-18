import qualified Data.Set as Set
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Ord (comparing)
import Data.List (sortBy, find)
import Control.Monad (join)


type GridSquare = Maybe Int

isLegalSquareSet :: [GridSquare] -> Bool
isLegalSquareSet xs = (length ys) == (Set.size $ Set.fromList ys)
                      where
                          ys = filter isJust xs


type Grid  = [GridSquare]
type Row   = [GridSquare]
type Col   = [GridSquare]
type Block = [GridSquare]

rown :: Grid -> Int -> Row
rown g n = take 9 $ drop (9 * n) g


coln :: Grid -> Int -> Col
coln [] n  = []
coln g  n  = (head $ drop n g) : (coln (drop 9 g) n)


blockn :: Grid -> Int -> Block
blockn g n = [g !! (start + x + 9*y) | x <-[0..2], y <-[0..2]]
             where
                start = blockstarts !! n

blockstarts :: [Int]
blockstarts = [27*x + 3*y | x <-[0..2], y <-[0..2]]


isCompletedGrid :: Grid -> Bool
isCompletedGrid g = all isJust g


type GridPoint = (Int, Int)

gridpoints :: [GridPoint]
gridpoints = [(row, col) | row <- [0..8], col <- [0..8]]


point2entry :: Grid -> GridPoint -> GridSquare
point2entry g (row, col) = g !! (9 * row + col)


point2row :: Grid -> GridPoint -> Row
point2row g (row, col) = filter isJust $ rown g row


point2col :: Grid -> GridPoint -> Col
point2col g (row, col) = filter isJust $ coln g col


point2block :: Grid -> GridPoint -> Block
point2block g p = filter isJust $ blockn g $ f p
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
  rows <- sequence $ take 9 (repeat getSudokuRow)
  return $ concat rows


readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing

rows :: Grid -> [[Int]]
rows [] = []
rows g  = (map fromJust (take 9 g)) : (rows $ drop 9 g)


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
          r = map show (rows g)
      sequence_ (map putStrLn r)


