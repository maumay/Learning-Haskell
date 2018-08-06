-- Ex 1/
putStr' :: String -> IO ()
putStr' x = sequence_ [putChar c | c <- x]


-- Ex 2/
formatIndex :: Int -> Int -> String
formatIndex reqLen n | l > reqLen = error ""
                     | otherwise  = (replicate ldiff ' ') ++ nstr ++ [':']
                     where
                        nstr  = show n
                        l     = length nstr
                        ldiff = reqLen - l

getIndices :: Int -> [String]
getIndices n | n < 1     = error ""
             | otherwise = map (formatIndex reqLen) [1..n]
             where
                reqLen = length $ show n


formatStar :: String -> Int -> String
formatStar s n | n < 1     = error ""
               | n == 1    = '*':s
               | otherwise = '*':' ':(formatStar s (n - 1))


getStars :: Int -> [String]
getStars n | n < 1     = error ""
           | otherwise = map (formatStar [])  (reverse [1..n])


printBoard :: Int -> IO ()
printBoard n | n < 1     = error ""
             | otherwise = sequence_ $ zipWith f indices stars
             where
                (indices, stars) = (getIndices n, getStars n)
                f                = \x y -> putStrLn (x ++ (' ':y))