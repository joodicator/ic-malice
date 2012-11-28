-- This is an Alex (http://haskell.org/alex) lexical analyser specification for
-- the Alice programming language.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- HEADER CODE
{
module AliceLexer where

import Control.Monad
import System.IO

import AliceToken
}
--------------------------------------------------------------------------------

%wrapper "monad" :-

--------------------------------------------------------------------------------
-- WHITESPACE
<0> $white+         ;
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- KEYWORDS
<0> "and"           { con TAnd }
<0> "but"           { con TBut }
<0> "then"          { con TThen }
<0> "what"          { con TWhat }
<0> "a"             { con TA }
<0> "too"           { con TToo }
<0> "had"           { con THad }
<0> "of"            { con TOf }
<0> "became"        { con TBecame }
<0> "ate"           { con TAte }
<0> "drank"         { con TDrank }
<0> "spoke"         { con TSpoke }
<0> "said"          { con TSaid }
<0> "found"         { con TFound }
<0> "either"        { con TEither }
<0> "or"            { con TOr }
<0> "perhaps"       { con TPerhaps }
<0> "so"            { con TSo }
<0> "maybe"         { con TMaybe }
<0> "eventually"    { con TEventually }
<0> "enough"        { con TEnough }
<0> "times"         { con TTimes }
<0> "because"       { con TBecause }
<0> "Alice"         { con TAlice }
<0> "was"           { con TWas }
<0> "unsure"        { con TUnsure }
<0> "which"         { con TWhich }
<0> "The"           { con TThe }
<0> "room"          { con TRoom }
<0> "looking-glass" { con TLookingGlass }
<0> "contained"     { con TContained }
<0> "opened"        { con TOpened }
<0> "closed"        { con TClosed }
<0> "spider"        { con TSpider }
<0> "number"        { con TNumber }
<0> "letter"        { con TLetter }
<0> "sentence"      { con TSentence }
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- PUNCTUATION and OPERATORS
<0> ","             { con TComma }
<0> "."             { con TDot }
<0> "?"             { con TQuest }
<0> "("             { con TParenO }
<0> ")"             { con TParenC }
<0> "+"             { con TPlus }
<0> "-"             { con TMinus }
<0> "*"             { con TStar }
<0> "/"             { con TSlash }
<0> "%"             { con TPercent }
<0> "=="            { con TEqEq }
<0> "!="            { con TBangEq }
<0> "<"             { con TLess }
<0> ">"             { con TGreater }
<0> "<="            { con TLessEq }
<0> ">="            { con TGreaterEq }
<0> "&&"            { con TAmpAmp }
<0> "||"            { con TBarBar }
<0> "&"             { con TAmp }
<0> "|"             { con TBar }
<0> "^"             { con TCaret }
<0> "~"             { con TTilde }
<0> "!"             { con TBang }
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- NUMBER LITERALS
<0> [0-9]+                  { tok $ TNumberLiteral . read }
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- IDENTIFIERS
<0> [A-Za-z_][A-Za-z0-9_]*  { tok TIdentifier }
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- QUOTED STRINGS
<0>     \'                  { con TQSingle `andBegin` qs }
<0>     \"                  { con TQDouble `andBegin` qd }
<qs>    \'                  { con TQSingle `andBegin` 0 }
<qd>    \"                  { con TQDouble `andBegin` 0 }
<qs,qd> \\[^x]              { tok TQEscape }
<qs,qd> \\x[0-9A-Fa-f]{2}   { tok TQEscape }
<qs,qd> [^\\]               { tok $ \[c] -> TQChar c }
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- FOOTER CODE
{
-- type AlexAction t = AlexInput -> Int -> Alex t
-- type AlexInput    = (AlexPosn, Char, [Byte], String)
-- data AlexPosn     = AlexPn !Int !Int !Int

-- An action for the "monad" wrapper which behaves like the "basic" wrapper.
tok :: (String -> Token) -> AlexAction Token
tok fun input len
  = return $ fun $ take len str
  where (_, _, _, str) = input

-- Like tok, except it discards the token's string representation.
con :: Token -> AlexAction Token
con = tok . const

-- The End-Of-File token that Alex's "monad" wrapper will generate.
alexEOF :: Alex Token
alexEOF = return TEOF
}
--------------------------------------------------------------------------------
