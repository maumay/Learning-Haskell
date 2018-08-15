import qualified Data.Set as Set
import Text.Read (readMaybe)
import Data.List (elemIndex)
import Data.Maybe (fromJust, isNothing)

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

-- Check if some list contains the full set of entries.
completeEntrySet :: [Entry] -> Bool
completeEntrySet xs = (Set.size $ Set.fromList xs) == 9


type GridSquare = Maybe Entry

completeSquareSet :: [GridSquare] -> Bool
completeSquareSet xs = (Set.size $ Set.fromList ys) == 9
                       where
                        ys = filter (Nothing /=) xs

