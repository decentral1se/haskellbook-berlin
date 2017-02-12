module SimpleCheck where

import Prelude hiding (any)

-- a quick toy implementation of QuickCheck to show the main idea

-- like Arbitrary, but this one just gives every possible value
class Any a where
    any :: [a]

instance Any Bool where
    any = [False, True]
instance Any Int where
    any = [(-100) .. 100]

-- everything we can test
class Testable a where
    test :: a -> Bool

-- Bool values are trivial to test
instance Testable Bool where
    test = id

-- here the magic happens:
-- If we need a parameter to get something testable
-- and we can get sample values for this parameter
-- then we can also test this function by testing it for all possible values of a
instance (Any a, Testable b) => Testable (a -> b) where
    -- any is of type [a] here
    test p = all (test . p) any

-- now we can do:
prop_zeroIdentity :: Int -> Bool
prop_zeroIdentity x = x + 0 == x

prop_commutativity :: Int -> Int -> Bool
prop_commutativity x y = x + y == y + x
