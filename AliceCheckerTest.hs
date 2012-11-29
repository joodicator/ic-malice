module Main where

import System.IO
import System.Exit

import AliceChecker
import AliceParser

main = do
    input <- getContents
    case checkAlice =<< aliceParse input of
        Left message -> do
            hPutStrLn stderr message
            exitWith $ ExitFailure 1
        Right () -> do
            putStrLn "Semantic check passed."
