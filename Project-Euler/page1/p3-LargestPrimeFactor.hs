extract :: Int -> Int -> Int
extract m n | (n `mod` m) == 0 = extract m (n `div` m)
	        | otherwise        = n

factorise :: Int -> [Int]
factorise n | n == 1 = []
            | null divisors = [n]
            | otherwise = [head divisors] ++ (factorise $ extract (head divisors) n)
            where 
            ub = floor $ sqrt $ fromIntegral n
            divisors = filter (\x -> n `mod` x == 0) [2..ub]

main :: IO()
main = putStrLn $ show $ last $ factorise 600851475143