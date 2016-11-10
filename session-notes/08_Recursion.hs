
{-
the FACTORIAL function
mathematically:

4! = 4 * 3 * 2 * 1 = 24

recursive definition:
n! = n * (n-1)!
0! = 1
-}


fac :: Integer -> Integer -> Integer
fac n
    | n <= 0    = 1
    | otherwise = n * fac (n-1)

-- simulating this function:
-- fac 4
-- = 4 * fac 3
-- = 4 * (3 * fac 2)
-- = 4 * (3 * (2 * fac 1))
-- = 4 * (3 * (2 * (1 * fac 0)))
-- = 4 * (3 * (2 * (1 * 1)))
-- = 4 * (3 * (2 * 1))
-- = 4 * (3 * 2)
-- = 4 * 6
-- = 24
--
-- a triangle of expansions!


-- tail recursive version
facTail :: (Ord a, Num a) => a -> a
facTail x = go x 1
  where
    -- this helper function has an additional parameter "acc"
    -- the accumulator keeps track of our intermediate result
    go n acc
        | n <= 0    = acc
        | otherwise = go (n - 1) (n * acc)

-- simulating this function:
-- fac 4
-- = go 4 1
-- = go 3 4
-- = go 2 12
-- = go 1 24
-- = go 0 24
-- = 24
--
-- constant space!


{-
also, FIBONACCI NUMBERS
fib(0) = 0
fib(1) = 1
fib(n) = fib(n-1) + fib(n-2)
-}

