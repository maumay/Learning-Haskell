-- Ex 2/
all' :: (a -> Bool) -> [a] -> Bool
all' p [] = True
all' p (x:xs) = (p x) && all' p xs

any' :: (a -> Bool) -> [a] -> Bool
any' p [] = False
any' p (x:xs) = (p x) || any' p xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) = if p x then x : takeWhile' p xs else []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p z@(x:xs) = if p x then dropWhile p xs else z


-- Ex 4/
dec2int :: [Int] -> Int
dec2int [] = 0
dec2int xs = foldl (\acc (x, i) -> acc + x * 10 ^ i) 0 (zip xs indices)
             where
                indices = reverse [0..(length xs) - 1]


-- Ex 5/
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \x -> \y -> f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(x, y) -> f x y 


-- Ex 9/
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g xs = map (\(x, i) -> if even i then f x else g x) ys
                where
                    ys = zip xs [0..]


-- Ex 10/
luhnDouble :: Int -> Int
luhnDouble x | x < 0 = error "No negative numbers"
             | x < 5 = 2 * x
             | otherwise = 2 * x - 9

luhn :: [Int] -> Bool
luhn xs = (sum $ altMap id luhnDouble (reverse xs)) `mod` 10 == 0
