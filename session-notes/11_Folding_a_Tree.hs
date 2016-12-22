
import Data.Function ((&))

data Zero

data One = One

data Stocking a = Toes | Segment a (Stocking a)
    deriving (Show, Eq, Ord)

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- data List a = Nil | Cons a (List a)

-- foldTree :: (b -> b -> a -> b) -> b -> Tree a -> b
data Tree b = Star | Branch b (Tree b) (Tree b)
    deriving Show

stealAllPresents :: Tree (Stocking a) -> [Stocking a]
stealAllPresents Star = []
stealAllPresents (Branch b left right) = b : (stealAllPresents left ++ stealAllPresents right)

insertTree :: Ord a => a -> Tree a -> Tree a
insertTree a Star = Branch a Star Star
insertTree a (Branch x left right) =
     if a > x
     then
         Branch x left (insertTree a right)
     else
         Branch x (insertTree a left) right

foldTree :: (b -> b -> a -> b) -> b -> Tree a -> b
foldTree f z Star = z
foldTree f z (Branch a left right) =
    f (foldTree f z left) (foldTree f z right) a

steal' :: Tree (Stocking a) -> [Stocking a]
steal' tree = foldTree
    (\presents1 presents2 p -> p : (presents1 ++ presents2)) [] tree

main :: IO ()
main = do
    print (Segment 1 Toes)
    let tree = (Branch (Segment "present" Toes) Star Star)
        biggerTree = tree
            & insertTree (Segment "socks" Toes)
            & insertTree (Segment "pillow" Toes)
            & insertTree (Segment "blanket" Toes)
            & insertTree (Segment "pillow" Toes)
    print (stealAllPresents tree)
    print (stealAllPresents biggerTree)
    print (steal' tree)
    print (steal' biggerTree)