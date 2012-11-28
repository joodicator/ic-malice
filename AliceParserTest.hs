module Main where

import AliceLexer
import AliceParser

import System.Exit
import System.IO

main = do
    input <- getContents
    case runAlex input aliceParse of
        Left msg -> do
            putStrLn msg
            exitWith $ ExitFailure 1
        Right ast -> do
            print ast
