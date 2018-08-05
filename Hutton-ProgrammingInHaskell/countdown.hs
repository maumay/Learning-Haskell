-- Based on chapter 9 of Huttons book "Programming in Haskell".

-- First define possible operators + validity/application functions.
data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

apply :: Op -> Int -> Int -> Int
apply Add m n = m + n
apply Sub m n = m - n
apply Mul m n = m * n
apply Div m n = m `div` n

valid :: Op -> Int -> Int -> Bool
valid Div m n = m `mod` n == 0
valid Sub m n = m > n
valid _   _ _ = True


-- Now define expressions.
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n) = show n
    show (App o l r) = brak l ++ show o ++ brak r
                       where
                           brak (Val n) = show n
                           brak e       = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n)     = [n]
values (App o l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n)     = [n]
eval (App o l r) = [apply o x y | x <- xs, y <- ys, valid o x y]
                   where
                    (xs, ys) = (eval l, eval r)


-- Combinatorial functions - managed to implement them myself :)
subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = sbs ++ [x : sb | sb <- sbs]
              where
               sbs = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []       = [[x]]
interleave x z@(y:ys) = (x:z) : (map (y:) (interleave x ys))

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs


-- Solution function
solution :: Expr -> [Int] -> Int -> Bool
solution e ns targ = isChoice (values e) ns && eval e == [targ]


isChoice :: Eq a => [a] -> [a] -> Bool
isChoice []     _                = True
isChoice _      []               = False
isChoice (x:xs) ys | x `elem` ys = isChoice xs (removeFirst x ys)
                   | otherwise   = False

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst x []                 = []
removeFirst x (y:ys) | x == y    = ys
                     | otherwise = y : (removeFirst x ys)