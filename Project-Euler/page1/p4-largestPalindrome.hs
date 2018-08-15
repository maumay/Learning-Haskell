import GHC.Exts (sortWith)

products =  let nrange = reverse [900..999] in sortWith (\x -> -x) [x*y | x <- nrange, y <- nrange]
main = putStrLn $ head $ filter (\xs -> xs == (reverse xs)) $ map show products