import Prelude hiding (map, filter, zip, zipWith)

-- LET'S IMPLEMENT SOME COMMONLY USED FUNCTIONS!


map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (first:rest) = f first : map f rest

-- evaluation:
-- map (+1) (0:1:[])
-- (+1) 0 : map (+1) (1:[])
-- (+1) 0 : (+1) 1 : map (+1) []
-- (+1) 0 : (+1) 1 : []


filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs) =
    if p x
        then x : filter p xs
        else filter p xs


zip :: [a] -> [b] -> [(a, b)]
zip (x:xs) (y:ys) = (x,y) : zip xs ys
zip _ _ = []

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith _ _ _ = []

-- now we can rewrite zip using zipWith
-- (,) is a function :: a -> b -> (a, b)
zip' = zipWith (,)

-- we could also write zipWith using zip and map
zipWith' f = map (uncurry f) .: zip

-- this operator allows readable point-free notation when we want to compose
-- a function that takes two arguments
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)
-- we could have done it this way as well (for maximum point-freeness):
-- (.:) = (.) . (.)
-- composing compose with compose!

