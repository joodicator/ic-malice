-- This is not really a compiler, but is called so for the purposes of
-- submission.
--------------------------------------------------------------------------------

module Main where

import AliceParser
import AliceChecker

import Control.Monad
import System.IO
import System.Environment
import System.Exit

-- Compile the file given by each command-line argument, exiting with success
-- iff all files succeed.
main = do
    success <- mapM compile =<< getArgs
    unless (and success) $ exitWith $ ExitFailure 1

-- Compile a single file, yielding True iff it succeeds.
compile :: String -> IO Bool
compile file = do
    input <- readFile file
    case checkAlice =<< aliceParse input of
        Left message -> do
            hPutStrLn stderr $ "[" ++ file ++ "]" ++ message
            return False
        Right () -> do
            putStrLn $ file ++ " is a valid Alice program."
            return True
