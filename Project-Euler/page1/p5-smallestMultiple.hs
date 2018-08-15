extract :: Int -> Int -> (Int, Int)
extract m n | (n `mod` m) == 0 = (fst xs, 1 + snd xs)
            | otherwise        = (n, 0)
            where
            xs = extract m (n `div` m)


factorise :: Int -> [(Int, Int)]
factorise n | n == 1 = []
            | null divisors = [(n, 1)]
            | otherwise = [(divisorHead, snd extractRes)] ++ (factorise $ fst extractRes)
            where 
            ub = floor $ sqrt $ fromIntegral n
            divisors = filter (\x -> n `mod` x == 0) [2..ub]
            divisorHead = head divisors
            extractRes = extract divisorHead  n


main = putStrLn $ show $ factorise 6553284428457547