module Main where

import Control.Monad

import AliceLexer
import AliceToken

main = do
    input <- getContents
    let Right output = runAlex input scanList
    mapM_ print output

scanList :: Alex [(Token, AlexPosn)]
scanList = do
    (posn, _, _, _) <- alexGetInput
    token <- scan
    let output = (token, posn)
    case token of
      TEOF      -> return []
      TError s  -> return [output]
      _         -> return . (output:) =<< scanList

scan :: Alex Token
scan = do
    input <- alexGetInput
    startCode <- alexGetStartCode
    case alexScan input startCode of
      AlexEOF ->
        alexEOF
      AlexError input' ->
        return $ TError "lexical error"
      AlexSkip input' len -> do
        alexSetInput input'
        scan
      AlexToken input' len act -> do
        alexSetInput input'
        act (ignorePendingBytes input) len

