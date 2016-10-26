import Prelude hiding ((.), ($)) -- we want to create our own version!


fun1 :: Integer -> (Integer -> Integer)
fun1 n m = n + m + 1
-- the same, using a lambda expression:
fun1' = \n m -> n + m + 1

-- a simple example of a higher order function
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
-- we can transform it in a point free style, step by step:
applyTwice' f x = (f . f) x
applyTwice'' f = f . f -- eta reduction


-- function composition, "f after g"
(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)
-- for operators, we can declare:
--      - precedence
--      - fixity (left or right associative)
infixr 9 .

-- apply a function
-- usually just to avoid parentheses
($) :: (a -> b) -> a -> b
f $ x = f x
-- the import thing is that is has the lowest possible precedence
infixr 0 $


-- example for "let" and "where"
replaceAt :: [a] -> Int -> a -> [a]
replaceAt list index value =
    let start = take index list
        end = drop (index + 1) list
    in start ++ [value] ++ end
replaceAt' list index value = start ++ [value] ++ end
  where
    start = ourTakeFunction index list
    end = drop (index + 1) list
    -- we can use other definitions from the same where block, like this one:
    ourTakeFunction = take


-- example from Chapter 5:
f :: (a,b) -> (c,d) -> ((a,c), (b,d))
f ab cd = ((fst ab, fst cd), (snd ab, snd cd))

-- now we can use pattern matching to deconstruct the tuples:
f' (a,b) (c,d) = ((a,c), (b,d))

-- there are more possible definitions using "where":
f'' :: (a, b) -> (c, d) -> ((a, c), (b, d))
f'' ab cd = ((a, c), (b, d))
  where
    (a, b) = ab
    (c, d) = cd
f''' :: (a, b) -> (c, d) -> ((a, c), (b, d))
f''' ab cd = ((a, c), (b, d))
  where
    a = fst ab
    b = snd ab
    c = fst cd
    d = snd cd


-- wrap things in newtypes for readability/documentation and safety!
newtype Name = Name (Maybe String)
newtype Surname = Surname String
newtype FullName = FullName String deriving (Show)

-- unwrap newtypes (or "regular" data types) using pattern matching
fullName :: Name -> Surname -> FullName
-- "n" refers to the whole Name argument, "name" to the unwrapped String
fullName n@(Name (Just name)) (Surname surname) =
    FullName (name ++ " " ++ surname)
fullName _ (Surname surname) =
    FullName surname -- there is no first name

fullName' _ (Surname surname)
    -- "guard"
    | length surname > 30 = error "java.lang.NameTooLongException"
fullName' (Name n) (Surname surname) = case n of -- case statement
    Nothing -> FullName surname
    Just name -> FullName (name ++ " " ++ surname)


second [] = error ":("
second (_:x:_) = x -- try to get the second element
second [x] = x -- if we only have one element in the list, return that


