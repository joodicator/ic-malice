module Main where

import Control.Monad

import AliceLexer
import AliceToken

main = do
    inp <- getContents
    case runAlex inp scan of
      Left err -> error err
      Right ts -> forM_ ts print

scan :: Alex [Token]    
scan = do
    tok <- alexMonadScan
    case tok of
      TEOF  -> return []
      _     -> scan >>= return . (tok :)
