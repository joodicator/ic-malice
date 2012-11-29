module Main where

import AliceParser
import AliceAST
import AliceASTShow

import Control.Monad
import System.Exit
import System.IO

main = do
    input <- getContents
    case aliceParse input of
      Left msg -> do
         hPutStrLn stderr msg
         exitWith $ ExitFailure 1
      Right ast -> do
         putStrLn $ showProgram ast
