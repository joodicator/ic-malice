module Main where

import Control.Monad
import System.Exit

import AliceLexer
import AliceToken

main = scan =<< getContents

scan :: String -> IO ()
scan input
  = case runAlex input scanList of
      Left msg  -> do
        putStrLn msg
        exitWith $ ExitFailure 1
      Right tcs -> do
        mapM_ (putStrLn . showTC) tcs

scanList :: Alex [TokenContext]
scanList = do
    tc <- aliceMonadScan
    case tc of
      TC{ tcToken=TEOF } -> do
        return [tc]
      _ -> do
        tail <- scanList
        return $ tc : tail

showTC :: TokenContext -> String
showTC TC{ tcToken=token, tcPosn=posn }
  = showPosn posn ++ "\t" ++ show token
          
