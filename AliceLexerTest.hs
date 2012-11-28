module Main where

import Control.Monad
import System.Exit

import AliceLexer
import AliceToken

main = do
    tcs <- return . flip runAlice scanList =<< getContents
    mapM_ print tcs
    case last tcs of
      TC{ tcToken=TError _ } -> do
        exitWith $ ExitFailure 1
      _ -> do
        exitWith ExitSuccess

scanList :: Alex [TokenContext]
scanList = do
    tc <- aliceMonadScan
    case tc of
      TC{ tcToken=TEOF } -> do
        return [tc]
      TC{ tcToken=TError _ } -> do
        return [tc]
      _ -> do
        tail <- scanList
        return $ tc : tail
