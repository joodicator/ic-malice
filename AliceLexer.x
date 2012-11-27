-- This is an Alex (http://haskell.org/alex) lexical analyser specification for
-- the Alice programming language.
--------------------------------------------------------------------------------

{
module AliceLexer where

import Control.Monad
import System.IO

import AliceToken
}

%wrapper    "monad"

:-

$white+         ;

"and"           { con Tand }
"but"           { con Tbut }
"then"          { con Tthen }
"what"          { con Twhat }
"a"             { con Ta }
"too"           { con Ttoo }
"had"           { con Thad }
"of"            { con Tof }
"became"        { con Tbecame }
"ate"           { con Tate }
"drank"         { con Tdrank }
"spoke"         { con Tspoke }
"said"          { con Tsaid }
"found"         { con Tfound }
"either"        { con Teither }
"or"            { con Tor }
"perhaps"       { con Tperhaps }
"so"            { con Tso }
"maybe"         { con Tmaybe }
"eventually"    { con Teventually }
"enough"        { con Tenough }
"times"         { con Ttimes }
"because"       { con Tbecause }
"Alice"         { con TAlice }
"was"           { con Twas }
"unsure"        { con Tunsure }
"which"         { con Twhich }
"The"           { con TThe }
"room"          { con Troom }
"looking-glass" { con Tlooking_glass }
"contained"     { con Tcontained }
"opened"        { con Topened }
"closed"        { con Tclosed }
"spider"        { con Tspider }
"number"        { con Tnumber }
"letter"        { con Tletter }
"sentence"      { con Tsentence }

","             { con TComma }
"."             { con TDot }
"?"             { con TQuest }
"("             { con TParenO }
")"             { con TParenC }
"+"             { con TPlus }
"-"             { con TMinus }
"*"             { con TStar }
"/"             { con TSlash }
"%"             { con TPercent }
"=="            { con TEqEq }
"!="            { con TBangEq }
"<"             { con TLess }
">"             { con TGreater }
"<="            { con TLessEq }
">="            { con TGreaterEq }
"&&"            { con TAmpAmp }
"||"            { con TBarBar }
"&"             { con TAmp }
"|"             { con TBar }
"^"             { con TCaret }
"~"             { con TTilde }
"!"             { con TBang }



{
-- Outputs a token as a function of its string representation.
tok :: (String -> Token) -> AlexAction Token
tok fun input len
  = return $ fun $ take len str
  where (_, _, _, str) = input

-- Outputs a constant token.
con :: Token -> AlexAction Token
con = tok . const

alexEOF :: Alex Token
alexEOF = return TEOF
}
