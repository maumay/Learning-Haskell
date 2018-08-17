import qualified Data.Set as Set
import Data.Maybe (isNothing, isJust)
import Data.Ord (comparing)
import Data.List (sortBy)

type GridSquare = Maybe Int

isLegalSquareSet :: [GridSquare] -> Bool
isLegalSquareSet xs = (length ys) == (Set.size $ Set.fromList ys)
                      where
                          ys = filter isJust xs



type Grid  = [GridSquare]
type Row   = [GridSquare]
type Col   = [GridSquare]
type Block = [GridSquare]

-- Function to retrieve rows in a grid.
rows :: Grid -> [Row]
rows [] = []
rows xs = (take 9 xs) : (rows $ drop 9 xs)

rown :: Grid -> Int -> Row
rown g n = take 9 $ drop (9 * n) g

-- Function to retrieve columns in a grid.
cols :: Grid -> [Col]
cols g  = map (coln g) [0..8]

coln :: Grid -> Int -> Col
coln [] n  = []
coln g  n  = (head $ drop n g) : (coln (drop 9 g) n)

-- Function to retrieve blocks in a grid.
blocks :: Grid -> [Block]
blocks g = map (blockn g) [0..8]

blockstarts :: [Int]
blockstarts = [27*x + 3*y | x <-[0..2], y <-[0..2]]

blockn :: Grid -> Int -> Block
blockn g n = [g !! (start + x + 9*y) | x <-[0..2], y <-[0..2]]
             where
                start = blockstarts !! n

-- Function which checks if a grid is in a legal state.
isLegalGrid :: Grid -> Bool
isLegalGrid g = all isLegalSquareSet $ concat [rows g, cols g, blocks g]

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

-- solveGrid :: Grid -> Maybe Grid
-- solveGrid g | isCompletedGrid g = Just g
--             | otherwise         = solveGrid 
--             where

--                 potential = map (legalEntries g) (sortGridPoints g)
