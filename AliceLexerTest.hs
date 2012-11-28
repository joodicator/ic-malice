module Main where

import Control.Monad
import System.Exit
import System.IO

import AliceLexer
import AliceToken

main = scan =<< getContents

scan :: String -> IO ()
scan input
  = case runAlex input scanList of
      Left msg  -> do
        hPutStrLn stderr msg
        exitWith $ ExitFailure 1
      Right tcs -> do
        mapM_ (putStrLn . showTC) tcs

scanList :: Alex [TokenContext]
scanList = do
    tc <- aliceMonadScan
    case tc of
      TC{ tcTok=TEOF } -> do
        return [tc]
      _ -> do
        tail <- scanList
        return $ tc : tail

showTC :: TokenContext -> String
showTC TC{ tcTok=token, tcPos=(chr, row, col) }
  = show row ++ ":" ++ show col ++ "\t" ++ show token
