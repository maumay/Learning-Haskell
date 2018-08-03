
{-
 Error 1: capital N
 Error 2: not backticks around div operator
 Error 3: unaligned columns in where clause.
-}
n = a `div` length xs 
    where
        a = 10
        xs = [1..5]


last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs


last'' :: [a] -> a
last'' xs = head $ drop m xs
            where
                m = length xs - 1


init' :: [a] -> [a]
init' [x] = []
init' (x:xs) = x : init xs


init'' :: [a] -> [a]
init'' xs = take m xs
           where
            m = length xs - 1
