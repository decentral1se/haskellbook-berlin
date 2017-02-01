module Hello
    ( hello -- export our `hello` function
    , Something(..) -- the Something datatype with all its constructors
    , module L -- and reexport Data.List
    ) where

import Data.List as L -- we can still use the imports without prefix
                      -- `L` is just an additional name for it
                      -- to only allow qualified usage, use `import qualified ...`
import Data.ByteString.Char8 (pack)
import Crypto.Hash

data Something = Something | SomethingElse
    deriving Show

hello :: String -> String
hello name = "Hello, " ++ name ++ "!\n"
            ++ "Sorting you name gives " ++ sort name ++ ".\n"
            ++ "Your hash is: "
            ++ show (hash (pack name) :: Digest SHA3_512)


internalFunction = id -- not exported
