fibNumbersToN :: Int -> [Int] -> [Int]
fibNumbersToN n xs | next < n  = fibNumbersToN n (xs ++ [next])
                   | otherwise = xs
                               where
                               	next = (\xs -> sum $ drop ((length xs) - 2) xs) xs


main = putStrLn $ show $ sum $ filter even $ fibNumbersToN 4000000 [1, 1]