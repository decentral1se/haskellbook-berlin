module Person (headMay, Person, mkPerson) where

-- to avod clash with our own definitions
import Prelude hiding (Maybe(..), Either(..))


data Maybe a = Nothing | Just a
    deriving (Eq, Show)


-- safe versions of Prelude functions
headMay :: [a] -> Maybe a
headMay (x:_) = Just x
headMay [] = Nothing


-- Our Maybe library

-- UNSAFE, don't use it!
-- fromJust (Just a) = a

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just a) = a
fromMaybe def Nothing = def

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just a) = Just (f a)

-- could be considered a fold on Maybe, compare to foldr:
-- foldr :: b -> (a -> b -> b) -> [a]     -> b
maybe    :: b -> (a -> b)      -> Maybe a -> b
maybe c _ Nothing = c
maybe _ f (Just a) = f a

-- alternatively, it could be considered to be a combination of the two
-- previously defined functions
maybe' c f = fromMaybe c . mapMaybe f



-- Either carries additional information in the error case ("Left")
data Either a b = Left a | Right b
    deriving (Eq, Show)

-- e.g. to give error messages
headEither :: [a] -> Either String a
headEither (x:_) = Right x
headEither [] = Left "empty list"


data Person = Person String Int
    deriving (Eq, Show)

-- creating your own error type is often better than using String
data PersonError = EmptyName | NegativeAge
    deriving (Eq, Show)

-- "smart constructor"
-- if we don't export the Person constructor, this is the only way to create
-- a Person, users of the module can't create invalid Persons :)
mkPerson :: String -> Int -> Either PersonError Person
mkPerson name age
    | name == "" = Left EmptyName
    | age < 0    = Left NegativeAge
    | otherwise  = Right $ Person name age



-- we can not only fold a list, but also do the opposite
-- f produces a Maybe, so it can stop the unfolding with a Nothing
unfoldr :: a -> (a -> Maybe (a,b)) -> [b]
unfoldr a f = case f a of
    Just (a', b) -> b : unfoldr a' f
    Nothing      -> []

-- experimental implementation (but without the Maybe => creates infinite list)
unfoldr' :: a -> (a -> (a,b)) -> [b]
unfoldr' a f = map (snd . f) $ iterate (fst . f) a

-- evaluation using f = \a -> (a+1, show a)
--
-- unfoldr 0 f
-- show 0 : unfoldr 1 f
-- show 0 : show 1 : unfoldr 2 f
-- ...

