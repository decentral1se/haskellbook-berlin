import Data.Bool (bool)

-- if we want to combine the values in a list to get a result,
-- map, filter an zip won't help us.
-- fortunately, we learned about recursion!
-- some examples...

sum' :: Num a => [a] -> a
-- base case
sum' [] = 0
-- combine the first value with the result of recursion on the tail
sum' (n:ns) = n + sum' ns

length' :: Num n => [a] -> n
length' [] = 0
length' (_:xs) = 1 + length' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs


-- wait, the structure seems to be pretty similar...
-- we have a base case
-- and a function to combine a single value and an intermediate result
-- can we abstract over that? of course! (we call it foldr)

foldr' :: (a -> b -> b) -> b -> [a] -> b
-- base case:
foldr' f c [] = c
-- use f to combine x and subresult
foldr' f c (x:xs) = f x (foldr' f c xs)


-- now we can rewrite sum, length, concat, ... by plugging c and f into foldr
sum'' = foldr' (+) 0
length'' = foldr' (const (+1)) 0
concat'' = foldr' (++) []


-- we can also see that foldr is more powerful than map and filter
-- by rewriting them with foldr
map' :: (a -> b) -> [a] -> [b]
map' f = foldr' ((:) . f) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr' g []
  where g a as
            | p a       = a : as
            | otherwise = as
        -- alternatively: g a as = bool as (a:as) (p a)

