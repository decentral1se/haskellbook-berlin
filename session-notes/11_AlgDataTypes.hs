{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- data Maybe a = Nothing | Just { value :: a }

data Present = Pres
    { presName :: String
    , presPrice :: Double
    } deriving Show

data Android = Per Person | Rob Robot

data Person = Person { personName :: String }
    deriving Show

data Robot = Robot  { robotID :: Int }
    deriving Show

identification :: Android -> String
identification (Per p) = personName p
identification (Rob r) = show $ robotID r

-- these are just aliases, can be used interchangably
type Address = String
type Priority = Bool

-- this one is a distinct type
newtype Name = Name String deriving (Show, Eq)
-- since Name is a distinct type, it does not reuse String's typeclass instances
-- (e.g. not not have Ord etc., even though String has).
-- But we can as usual derive instances and even selectively reuse instances
-- of the wrapped type using the language extension GeneralizedNewtypeDeriving.

-- You can give any String as Address, any Bool as Priority.
-- They are only an alias and improve readability, but do not prevent mistakes.
-- Name, on the other side, cannot be a String!
f :: Name -> Name -> Address -> Int -> Priority -> String
f (Name n1) (Name n2) a x b = if b then n1 else n2



-- ALGEBRAIC?

-- no possible values (identity for sum type), is not inhabited
data Empty

-- 1 possible value (identity for product type)
data Something = Something

-- 3 possible values
data CarManufacturer = Audi | BMW | Mercedes

-- 3 * (1 + 1) possible values
data Prod = Prod CarManufacturer Bool

-- Prod can be transformed using the distributive law:
-- 3 * (1 + 1) = 3 + 3 possible values
-- Prod' it is not identical, but isomophic to Prod.
data Prod' = ProdTrue CarManufacturer
           | ProdFalse CarManufacturer

-- 3 * 2 * 1 + 1 = 7 possible values
data Mixed = Car CarManufacturer Bool Something
           | NoCar

-- 2 ^ 3 possible values, function acts as exponential
data CarCheck = Check (CarManufacturer -> Bool)

-- can be transformed as well:
-- 2 ^ 3 = 2 * 2 * 2
data CarCheck' = Check' Bool Bool Bool
-- think of it as a boolean result for each of the 3 possible CarManufacturers



-- BINARY TREES

-- reminder:
data List a = Nil | Cons a (List a)

-- [1,2,3] looks like this:
--
--   :
--  / \
-- 1   :
--    / \
--   2   :
--      / \
--     3   []


-- a binary tree looks like this (L for leaf):
--
--            2
--           / \
--          /   \
--         1     5
--        / \   / \
--       0   L L   L
--      / \
--     L   L

-- instead of one sublist, a Node has two subtrees
data Tree a = Leaf | Node (Tree a) a (Tree a)

-- if we wanted to create our own Show instance:
instance Show a => Show (Tree a) where
    show = undefined -- TODO: add implementation


exampleTree =
    Node
        (Node
            (Node
                Leaf
                0
                Leaf)
            1
            Leaf)
        2
        (Node
            Leaf
            5
            Leaf)


-- typeclasses can have default implementations for their functions:
--class Eq a where
    --(==), (/=) :: a -> a -> Bool
    --a == b = not (a /= b)
    --a /= b = not (a == b)

