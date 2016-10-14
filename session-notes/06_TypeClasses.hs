

-- the type signature is derived automatically!
-- note that a must have an Ord instance, because we used the equals sign (==)
-- isMember :: Ord a => a -> [a] -> Bool
isMember _ [] = False
isMember a (element:rest) = a == element || isMember a rest


data Mood a = Happy | Angry a | Sad
    deriving (Ord, Show) -- we can derive many typeclass instances

-- we could have derived this instance as well
instance Eq a => Eq (Mood a) where
    Happy   == Happy   = True
    Angry n == Angry m = n == m
    Sad     == Sad     = True
    _       == _       = False

-- on a side note, a common pattern: using a newtype wrapper to
-- 1) restrict usage of a type (MyType can't simply be used as a String)
-- 2) add information (documentation)
newtype MyType = MyType String
-- checkedMyType :: String -> Maybe MyType

-- we can create instances for types and typeclasses from different modules
-- "orphan instance"
-- usually not a good idea (-Wall will warn on it)
instance Bounded Integer where
    -- this example does not make sense
    -- Integers are not bounded (as opposed to Ints)
    minBound = 0
    maxBound = 11

