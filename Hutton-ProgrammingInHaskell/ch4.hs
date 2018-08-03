-- 1/
halve :: [a] -> ([a], [a])
halve xs = let n = length xs `div` 2
           in (take n xs, drop n xs)

-- 2/
third :: [a] -> a
third xs = head $ tail $ tail xs

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' (x:y:z:xs) = z


-- 3/
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs | null xs = []
             | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' test@(x:xs) = xs


-- 4/ (I'm not doing it four ways)
or :: Bool -> Bool -> Bool
True `or` _ = True
_ `or` True = True
_ `or` _ = False


-- 5/
and :: Bool -> Bool -> Bool
and x y = if x then if y then True else False else False

-- 6/
and' :: Bool -> Bool -> Bool
and' x y = if x then y else False


-- 7/
mult :: Int -> Int -> Int -> Int
mult x = \y -> \z -> x * y * z


-- 8/
luhnDouble :: Int -> Int
luhnDouble x | x < 0 = error "No negative numbers"
             | x < 5 = 2 * x
             | otherwise = 2 * x - 9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z w = sum [ld x, y, ld z, w] `mod` 10 == 0
               where
                ld = luhnDouble