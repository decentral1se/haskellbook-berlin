import Data.Bool (bool)
import Prelude hiding (foldr, foldl)

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



-- Now, we are going to take a look at foldl
-- and the differences between foldl and foldr.
-- Thanks, Julian! :)

-- we can fold a list either in right- or left-associative fashion:


foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _  starting [] = starting
foldr fn starting (x : xs) = fn x (foldr fn starting xs)

-- foldr is RIGHT-associative:
-- foldr (+) 0 [1,2,3,4]
-- (1 + (foldr (+) 0 [2,3,4]))
-- (1 + (2 + (foldr (+) 0 [3,4])))
-- (1 + (2 + (3 + (foldr (+) 0 [4]))))
-- (1 + (2 + (3 + (4 + (foldr (+) 0 [])))))
-- (1 + (2 + (3 + (4 + 0)))))


foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _  starting [] = starting
foldl fn starting (x : xs) = foldl fn (fn starting x) xs

-- foldl is LEFT-associative:
-- foldl (+) 0 [1,2,3,4]
-- foldl (+) (0 + 1) [2,3,4]
-- foldl (+) ((0 + 1) + 2) [3,4]
-- foldl (+) (((0 + 1) + 2) + 3) [4]
-- foldl (+) ((((0 + 1) + 2) + 3) + 4) []
-- ((((0 + 1) + 2) + 3) + 4)


-- if we look at the order of evaluation, we can understand why foldr can
-- work on infinite lists:
--
-- foldl can only return something when it reaches the base case
-- (after traversing the whole list).
-- But foldr applies fn on the outermost level, so if the result does not
-- depend on the recursive call, it can already be "pulled out".
--
-- for example, foldr (:) [] folds a list by building it up again,
-- effectively acting as an identity function over lists.
-- This works even for infinite lists:
-- foldr (:) [] [1..]
-- (1 : (2 : (3 : (4 : ...
--
-- this does not work for foldl, where it acts as the reverse function.
-- foldl (flip (:)) [] [1..]
-- ((((((((((((( ...
-- we never reach the bottom.

-- The same applies for other possibly short-circuiting functions,
-- e.g. if we "or" an infinite list of Trues
-- Try:
-- foldr (||) False (repeat True)
-- foldl (||) False (repeat True)
--
-- for more information, see e.g. https://wiki.haskell.org/Foldr_Foldl_Foldl'


-- we also briefly mentioned scans, which return not just the final result,
-- but a list of the intermediate results.

-- possible implementations:
scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' _ c [] = [c]
scanr' f c (x:xs) = f x (head r) : r
  where r = scanr' f c xs

scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' _ c [] = [c]
scanl' f c (x:xs) = c : scanl' f (f c x) xs

-- we can use them for example to write the following function
iterate' :: (a -> a) -> a -> [a]
iterate' fn orig = scanl (flip ($)) orig (repeat fn)

