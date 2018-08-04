-- Ex 1/
sumsquares :: Int
sumsquares = sum [x * x | x <- [1..100]]

-- Ex 2/
grid :: Int -> Int -> [(Int, Int)]
grid xmax ymax | xmax < 1 || ymax < 1 = error ""
               | otherwise = [(x, y) | x <- [0..xmax], y <- [0..ymax]]

-- Ex 3/
square :: Int -> [(Int, Int)]
square n | n < 1 = error ""
         | otherwise = [(x, y) | (x, y) <- grid n n, x /= y]

-- Ex 4/
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]


-- Ex 5/
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- src, y <- src, z <- src, p x y z]
          where
            src = [1..n]
            p = \x y z -> x * x + y * y == z * z


-- Ex 6/
factors :: Int -> [Int]
factors n = [m | m <- [1..upper], n `mod` m == 0]
            where
                upper = n `div` 2

perfects :: Int -> [Int]
perfects ub = [n | n <- [2..ub], (sum $ factors n) == n]


-- Ex 7/
unested = [(x, y) | x <- [1, 2], y <- [3, 4]]
nested = concat [[(x, y) | y <- [3, 4]] | x <- [1, 2]]


-- Ex 8/
find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x $ zip xs [0..]


-- Ex 9/
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
