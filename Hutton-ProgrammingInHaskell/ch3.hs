-- 1/
-- :t ['a', 'b', 'c'] is [Char] (or String)
-- :t ('a', 'b', 'c') is (Char, Char, Char)
-- :t [(False, '0'), (True, '1')] is [(Bool, Char)]
-- :t ([False, True], ['0', '1']) is  ([Bool], [Char])
-- :t [tail, init, reverse]  is [[a] -> [a]]

-- 2/
bools :: [Bool]
bools = []

nums :: [[Int]]
nums = [[]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = f x

-- 3/
second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: (Num a) => a -> a
double x = x * 2

palindrome :: (Eq a) => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f $ f x

{- 4/ 
It is feasible if the type domain is small enough for
all inputs to be exhaustively checked. 
-}