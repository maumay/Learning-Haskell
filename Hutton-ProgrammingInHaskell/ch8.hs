-- Ex 1/
data Nat = Zero | Succ Nat 
           deriving (Eq, Show)

int2nat :: Int -> Nat
int2nat n | n < 0     = error ""
          | n == 0    = Zero
          | otherwise = Succ (int2nat $ n - 1)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

add :: Nat -> Nat -> Nat
add Zero     m     = m
add (Succ n) m     = add n (Succ m)

mult :: Nat -> Nat -> Nat
mult _ Zero     = Zero
mult n (Succ m) = add n (mult n m)