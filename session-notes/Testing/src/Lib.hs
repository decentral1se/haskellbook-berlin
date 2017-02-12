module Lib where

import Prelude hiding (any)

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data List a = Empty | Cons a (List a) deriving (Eq)

--showOurList :: Show a => List a -> String
showOurList list = "[" ++ help list ++ "]"
  where
    help Empty = ""
    help (Cons x Empty) = show x
    help (Cons x rest) = show x ++ "," ++ help rest

instance Show a => Show (List a) where
    show = showOurList

fromList :: [a] -> List a
fromList [] = Empty
fromList (x:xs) = Cons x (fromList xs)
--fromList = foldr Cons Empty


safeHead :: List a -> Maybe a
safeHead Empty = Nothing
safeHead (Cons x _) = Just x



class Any a where
    any :: [a]

instance Any Bool where
    any = [False, True]
instance Any Int where
    any = [(-100) .. 100]

class Testable a where
    test :: a -> Bool

instance Testable Bool where
    test = id

instance (Any a, Testable b) => Testable (a -> b) where
    test p = all (test . p) any
