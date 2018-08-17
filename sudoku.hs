import qualified Data.Set as Set
-- import Text.Read (readMaybe)
-- import Data.List (elemIndex)
import Data.Maybe (isNothing, isJust)


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

-- Function to retrieve columns in a grid.
cols :: Grid -> [Col]
cols g  = map (coln g) [0..8]

coln :: Grid -> Int -> Col
coln [] n  = []
coln g  n  = (head $ drop n g) : (coln (drop 9 g) n)

-- Function to retrieve blocks in a grid.
blocks :: Grid -> [Block]
blocks g = map (blockn g) blockstarts

blockstarts :: [Int]
blockstarts = [27*x + 3*y | x <-[0..2], y <-[0..2]]

blockn :: Grid -> Int -> Block
blockn g start = [g !! (start + x + 9*y) | x <-[0..2], y <-[0..2]]

-- Function which checks if a grid is in a legal state.
isLegalGrid :: Grid -> Bool
isLegalGrid g = all isLegalSquareSet $ concat [rows g, cols g, blocks g]

-- blockn :: Grid -> Int -> Int -> Block
-- blockn g n m =  


-- inGridRange :: Int -> Bool
-- inGridRange x = 0 <= x && x < 81

-- indexGrid :: Grid -> Int -> GridSquare
-- indexGrid g (row, col) = g !! ((row - 1) * 9 + (col - 1)) 