import Prelude hiding (lookup) -- we can even hide Prelude functions
import System.Environment (getArgs) -- for accessing the command line args
import System.IO
import System.Exit (exitFailure)
import Control.Exception

import qualified Hello as H -- we now can only use it qualified
import Hello (Something(SomethingElse)) -- only import one of the constructors!


addSpacing :: IO ()
addSpacing = putStrLn "\n\n\n"


main :: IO ()
main = do
    putStrLn $ "Starting..."       -- print strings
    let listToSort = [4,5,2,10,-5] -- we can use `let` in do-notation
    print $ H.sort listToSort      -- print anything we can show

    args <- getArgs -- gets a [String] of command line arguments
    case args of
        (name:_) -> putStrLn . H.hello $ name
        []       -> do
            putStrLn "Please give your name as an argument!"
            exitFailure -- stop execution
    addSpacing

    putStrLn "Give a file name to read from! (no error handling)"
    fileName <- getLine -- read user input
    -- get file handle with proper resource cleanup (even when exceptions occur)
    firstLines <- withFile fileName ReadMode $ \handle -> do
        line1 <- hGetLine handle
        line2 <- hGetLine handle
        return (line1 ++ "\n" ++ line2)
    putStrLn firstLines
    addSpacing

    -- a way of catching exceptions:
    putStrLn "Give another file name to read from! (now, with error handling)"
    fileName2 <- getLine
    content <- try $ readFile fileName2 :: IO (Either IOError String)

    putStrLn $ "We received an either:"
    putStrLn $ "\t" ++ show content
    putStrLn $ "About to handle it nicely (twice)..."
    addSpacing

    putStrLn (either show ("success: " ++) content) -- handling a either

    let outText = case content of -- pattern matching is possible as well
            Left err  -> show err
            Right str -> "success: " ++ str
    putStrLn outText

    putStrLn "Bye again"

  where


