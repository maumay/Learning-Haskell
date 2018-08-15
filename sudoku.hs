import qualified Data.Set as Set
import Text.Read (readMaybe)
import Data.List (elemIndex)
import Data.Maybe (fromJust, isNothing, isJust)


data Entry = E1 | E2 | E3 | E4 | E5 | E6 | E7 | E8 | E9
             deriving (Eq, Ord)

allEntries :: [Entry]
allEntries = [E1, E2, E3, E4, E5, E6, E7, E8, E9]

instance Show Entry where
    show x = show $ (fromJust $ elemIndex x allEntries) + 1

-- Don't understand the Read typeclass fully yet :)
-- utilising laziness here for things to make sense
-- would be errors in a strict world.
string2entry :: String -> Entry
string2entry x | isNothing y || outRange num = error ""
               | otherwise                   = allEntries !! (num - 1)
               where
                y        = readMaybe x :: Maybe Int
                outRange = \n -> n < 1 || n > 9
                num      = fromJust y


type GridSquare = Maybe Entry

isLegalSquareSet :: [GridSquare] -> Bool
isLegalSquareSet xs = (length ys) == (Set.size $ Set.fromList ys)
                      where
                          ys = filter isJust xs


completeSquareSet :: [GridSquare] -> Bool
completeSquareSet xs = (Set.size $ Set.fromList ys) == 9
                       where
                        ys = filter isJust xs

type Grid  = [GridSquare]
type Row   = [GridSquare]
type Col   = [GridSquare]
type Block = [GridSquare]


rows :: Grid -> [Row]
rows [] = []
rows xs = (take 9 xs) : (rows $ drop 9 xs)

cols :: Grid -> [Col]
cols g  = map (coln g) [1..9]

coln :: Grid -> Int -> Col
coln [] n  = []
coln g  n  = (head $ drop n g) : (coln (drop 9 g) n)

-- blockn :: Grid -> Int -> Int -> Block
-- blockn g n m =  


-- inGridRange :: Int -> Bool
-- inGridRange x = 0 <= x && x < 81

-- indexGrid :: Grid -> Int -> GridSquare
-- indexGrid g (row, col) = g !! ((row - 1) * 9 + (col - 1)) 