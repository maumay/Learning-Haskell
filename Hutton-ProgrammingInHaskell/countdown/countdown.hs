-- Based on chapter 9 of Huttons book "Programming in Haskell".

-- First define possible operators + validity/application functions.
data Op = Add | Sub | Mul | Div | Pow

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Pow = "^"

apply :: Op -> Integer -> Integer -> Integer
apply Add m n = m + n
apply Sub m n = m - n
apply Mul m n = m * n
apply Div m n = m `div` n
apply Pow m n = m ^ n

valid :: Op -> Integer -> Integer -> Bool
valid Div m n = n /= 1 && m `mod` n == 0
valid Sub m n = m > n
valid Add m n = m <= n
valid Mul m n = m /= 1 && n /= 1 && m <= n
valid Pow m n = 1 < m && 1 < n && n < 10 


-- Now define expressions.
data Expr = Val Integer | App Op Expr Expr

instance Show Expr where
    show (Val n) = show n
    show (App o l r) = brak l ++ show o ++ brak r
                       where
                           brak (Val n) = show n
                           brak e       = "(" ++ show e ++ ")"

values :: Expr -> [Integer]
values (Val n)     = [n]
values (App o l r) = values l ++ values r

eval :: Expr -> [Integer]
eval (Val n)     = [n]
eval (App o l r) = [apply o x y | x <- xs, y <- ys, valid o x y]
                   where
                    (xs, ys) = (eval l, eval r)


-- List combinatorial functions
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

-- Ex 1/
choices' :: [a] -> [[a]]
choices' xs = [p | s <- subs xs, p <- perms s]


-- Solution function
solution :: Expr -> [Integer] -> Integer -> Bool
solution e ns targ = isChoice (values e) ns && eval e == [targ]

-- Ex 2/
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice []     _                = True
isChoice _      []               = False
isChoice (x:xs) ys | x `elem` ys = isChoice xs (removeFirst x ys)
                   | otherwise   = False

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst x []                 = []
removeFirst x (y:ys) | x == y    = ys
                     | otherwise = y : (removeFirst x ys)


-- Expression creation

-- Ex 3/
-- If split included empty lists then we would get an infinite recursion
-- in the results function.
split :: [a] -> [([a], [a])]
split []     = []
split [x]    = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

type Result = (Expr, Integer)

results :: [Integer] -> [Result]
results []  = []
results [n] = [(Val n, n)]
results ns  = [res | (ls, rs) <- split ns, 
                            l <- results ls, 
                            r <- results rs, 
                          res <- combine l r]

combine :: Result -> Result -> [Result]
combine (e1, v1) (e2, v2) = 
    [(App o e1 e2, apply o v1 v2) | o <- ops, valid o v1 v2]

ops :: [Op]
ops = [Add, Sub, Mul, Div, Pow]

solutions :: [Integer] -> Integer -> [Expr]
solutions ns n = [e | c <- choices ns, (e,v) <- results c, v == n]


-- Main
main :: IO ()
main = sequence_ . map putStrLn $ [show solns, "Number of solutions: " ++ nsolns]
       where
        solns  = solutions [50, 25, 2, 4, 8, 1] 241
        nsolns = show $ length solns
