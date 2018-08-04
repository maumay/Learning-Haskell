-- Ex 2/
sumdown :: Int -> Int
sumdown n | n < 1 = 0
          | otherwise = n + (sumdown $ n - 1)


-- Ex 3/
pow :: Int -> Int -> Int
pow a b | b < 1 = 1
        | otherwise = a * pow a (b - 1)


-- Ex 4/
euclid :: Int -> Int -> Int
euclid m n | m < 1 || n < 1 = error ""
           | m == n = m
           | otherwise = euclid small (big - small)
           where
            (small, big) = (min m n, max m n)


-- Ex 6/
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs


concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss


replicate' :: Int -> a -> [a]
replicate' n x | n == 0 = []
               | otherwise = x : replicate' (n - 1) x


(!!!) :: [a] -> Int -> a
(!!!) xs n | n == 0 = head xs
           | otherwise = (tail xs) !!! (n - 1)


elem' :: Eq a => a -> [a] -> Bool
_ `elem'` [] = False
y `elem'` (x:xs) | y == x = True
                 | otherwise = y `elem'` xs


-- Ex 7/
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge allx@(x:xs) ally@(y:ys) | x <= y = x : merge xs ally
                              | otherwise = y : merge allx ys

-- Ex 8/
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = (msort first) `merge` (msort second)
           where
            (first, second) = (take n xs, drop n xs)
            n = (length xs) `div` 2

-- ex 9/ ---> easy :p