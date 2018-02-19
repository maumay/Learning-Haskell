-- Ex1: [f x | x <- xs, p x] <==> map f $ filter p xs


-- Ex2 
myAll :: (a -> Bool) -> [a] -> Bool
myAll p [x]      = p x
myAll p (x : xs) = (p x) && (myAll p xs)


myAny :: (a -> Bool) -> [a] -> Bool
myAny p [x]      = p x
myAny p (x : xs) = (p x) || (myAny p xs)


myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p []       = []
myTakeWhile p (x : xs) = if (p x) then [x] ++ (myTakeWhile p xs) else [] 




xs = [1, 2, 3, -1, 5]
p = \x -> x > 0

main = putStrLn $ show $ myTakeWhile p xs