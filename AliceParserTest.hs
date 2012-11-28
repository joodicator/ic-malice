module Main where

import AliceParser

import System.Exit
import System.IO

main = do
    input <- getContents
    case aliceParse input of
      Left msg -> do
         hPutStrLn stderr msg
         exitWith $ ExitFailure 1
      Right ast -> do
         print ast
