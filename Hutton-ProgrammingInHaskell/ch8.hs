-- Ex 1/
data Nat = Zero | Succ Nat deriving (Eq, Show)

int2nat :: Int -> Nat
int2nat n | n < 0     = error ""
          | n == 0    = Zero
          | otherwise = Succ (int2nat $ n - 1)

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

add :: Nat -> Nat -> Nat
add Zero     m = m
add (Succ n) m = add n (Succ m)

mult :: Nat -> Nat -> Nat
mult _ Zero     = Zero
mult n (Succ m) = add n (mult n m)


-- Ex 2/
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

-- More efficient since comparison value requires only one computation.
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)                = x == y
occurs x (Node l y r) | eq == EQ = True
                      | eq == LT = occurs x l
                      | eq == GT = occurs x r
                      where
                        eq = compare x y


-- Ex 3/
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) deriving (Eq, Show)

nleaves :: Tree' a -> Int
nleaves (Leaf' x)   = 1
nleaves (Node' l r) = (nleaves l) + (nleaves r)

balanced :: Tree' a -> Bool
balanced (Leaf' x)   = True
balanced (Node' l r) = diff < 2
                      where
                        diff = abs $ (nleaves l) - (nleaves r)


-- Ex 4/
balance :: [a] -> Tree' a
balance []  = error ""
balance [x] = Leaf' x
balance xs  = Node' (balance l) (balance r)
              where
                (l, r) = (take n xs, drop n xs)
                n      = (length xs) `div` 2


-- Ex 5/
data Expr = Val Int | Add Expr Expr deriving (Eq, Show)

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x)   = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)


-- Ex 6/
eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size (Val x)   = 1
size (Add x y) = (size x) + (size y)


-- Ex 7/

data Maybe' a = Nothing' | Just' a deriving (Show)

instance Eq a => Eq (Maybe' a) where
    --(==) :: Maybe' a -> Maybe' a -> Bool
    Nothing'  == Nothing'  = True
    (Just' x) == (Just' y) = x == y
    _         == _         = False


instance Eq a => Eq [a] where
    --(==) :: [a] -> [a] -> bool
    []     == []     = True
    (x:xs) == (y:ys) = (x == y) && (xs == ys)
    _      == _      = False