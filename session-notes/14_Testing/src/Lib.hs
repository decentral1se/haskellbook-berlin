module Lib where

-- just some code we can test later


data List a = Empty | Cons a (List a) deriving (Eq)

showOurList :: Show a => List a -> String
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

