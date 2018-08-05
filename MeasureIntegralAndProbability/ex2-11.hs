upFactor   = 1.2 :: Double
downFactor = 0.9 :: Double

-- Double equality operator with fixed tolerance.
(<=>) :: Double -> Double -> Bool
x <=> y = (abs $ x - y) < 0.000001

-- Given some double predicate this function calculates 
-- how many paths there are to a result which satisfies
-- the predicate.
npaths :: Int -> (Double -> Bool) -> Double -> Int
npaths depth p s | depth == 0 = if p s then 1 else 0
                 | otherwise = (npaths m p s1) + (npaths m p s2)
                 where
                  m = depth - 1
                  (s1, s2) = (s * upFactor, s * downFactor)

-- Assuming equal probability for moving up/down at each step
-- this function calculates the probability of a the final result
-- satisfying a given predicate.
prob :: Int -> (Double -> Bool) -> Double -> Double
prob depth p s = (fromIntegral $ npaths depth p s) / 2^depth
