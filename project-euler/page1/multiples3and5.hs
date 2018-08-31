multiples :: Int -> Int
multiples n = sum (filter (\x -> p3 x || p5 x) [1..m])
              where m = n - 1
                    p3 = \x -> x `mod` 3 == 0
                    p5 = \x -> x `mod` 5 == 0

main = putStrLn (show (multiples 1000)) 