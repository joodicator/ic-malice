-- This is a Happy (http://haskell.org/happy) parser specification for the
-- Alice programming language.
--------------------------------------------------------------------------------

{
module AliceParser where

import Data.Maybe
import Data.Char
import Numeric

import AliceToken
import AliceAST
}

%name       parseAlice
%tokentype  { Token }
%error      { parseError }

%token
    AND                 { TAnd }
    BUT                 { TBut }
    THEN                { TThen }
    WHAT                { TWhat }
    A                   { TA }
    TOO                 { TToo }
    HAD                 { THad }
    OF                  { TOf }
    BECAME              { TBecame }
    ATE                 { TAte }
    DRANK               { TDrank }
    SPOKE               { TSpoke }
    SAID                { TSaid }
    FOUND               { TFound }
    EITHER              { TEither }
    OR                  { TOr }
    PERHAPS             { TPerhaps }
    SO                  { TSo }
    MAYBE               { TMaybe }
    EVENTUALLY          { TEventually }
    ENOUGH              { TEnough }
    TIMES               { TTimes }
    BECAUSE             { TBecause }
    ALICE               { TAlice }
    WAS                 { TWas }
    UNSURE              { TUnsure }
    WHICH               { TWhich }
    THE                 { TThe }
    ROOM                { TRoom }
    LOOKING_GLASS       { TLookingGlass }
    CONTAINED           { TContained }
    OPENED              { TOpened }
    CLOSED              { TClosed }
    SPIDER              { TSpider }
    NUMBER              { TNumber }
    LETTER              { TLetter }
    SENTENCE            { TSentence }
    ","                 { TComma }
    "."                 { TDot }
    "?"                 { TQuest }
    "("                 { TParenO }
    ")"                 { TParenC }
    "+"                 { TPlus }
    "-"                 { TMinus }
    "*"                 { TStar }
    "/"                 { TSlash }
    "%"                 { TPercent }
    "=="                { TEqEq }
    "!="                { TBangEq }
    "<"                 { TLess }
    ">"                 { TGreater }
    "<="                { TLessEq }
    ">="                { TGreaterEq }
    "&&"                { TAmpAmp }
    "||"                { TBarBar }
    "&"                 { TAmp }
    "|"                 { TBar }
    "^"                 { TCaret }
    "~"                 { TTilde }
    "!"                 { TBang }
    "'"                 { TQDouble }
    '"'                 { TQSingle }
    QUOTE_CHR           { TQChar $$ }
    QUOTE_ESC           { TQEscape $$ }
    LITERAL_NUMBER      { TNumberLiteral $$ }
    ID                  { TIdentifier $$ }

%left "||"
%left "&&"
%left "!"
%nonassoc "==" "!=" "<" ">" "<=" ">="
%left "+" "-"
%left "*" "/" "%"
%left "|" "^"
%left "&"
%left "-" NEGATE
%left "~"

%%

--------------------------------------------------------------------------------
body ::     { [Stmt] }
  : body_r  { reverse $1 }

body_r ::               { [Stmt] }
  : {- EMPTY -}         { [] }
  | body_r sentence_r   { $2 ++ $1 }
  | body_r struct       { $2 : $1 }

--------------------------------------------------------------------------------
sentence_r ::                   { [Stmt] }
  : init_clauses_r final_clause { $2 : $1 }

--------------------------------------------------------------------------------
init_clauses_r ::               { [Stmt] }
  : {- EMPTY -}                 { [] }
  | init_clauses_r init_clause  { $2 : $1 }

init_clause ::          { Stmt }
  : open_clause ","     { $1 }
  | open_clause AND     { $1 }
  | open_clause BUT     { $1 }
  | open_clause THEN    { $1 }

--------------------------------------------------------------------------------
final_clause ::         { Stmt }
  : open_clause "."     { $1 }
  | WHAT WAS expr "?"   { SRead{ sTarget=$3 } }

--------------------------------------------------------------------------------
open_clause ::              { Stmt }
  : ID WAS A type           { SVarDec{ sID=$1, sType=$4 } }
  | ID WAS A type TOO       { SVarDec{ sID=$1, sType=$4 } }
  | ID WAS A type OF expr   { SVarIni{ sID=$1, sType=$4, sSource=$6 } }
  | ID HAD expr type        { SArrDec{ sID=$1, sType=$4, sSize=$3 } }
  | expr BECAME expr        { SAssign{ sTarget=$1, sSource=$3 } }
  | expr ATE                { SInc{ sTarget=$1 } }
  | expr DRANK              { SDec{ sTarget=$1 } }
  | expr SPOKE              { SWrite{ sSource=$1 } }
  | expr SAID ALICE         { SWrite{ sSource=$1 } }
  | ALICE FOUND expr        { SReturn{ sSource=$3 } }
  | ID "(" call_params ")"  { SCall{ sID=$1, sCParams=$3 } }

--------------------------------------------------------------------------------
struct ::           { Stmt }
  : struct_either   { $1 }
  | struct_perhaps  { $1 }
  | struct_while    { $1 }
  | struct_fun      { $1 }
  | struct_sub      { $1 }
  | struct_block    { $1 }

--------------------------------------------------------------------------------
struct_either :: { Stmt }
  : EITHER "(" expr ")" SO body OR body BECAUSE ALICE WAS UNSURE WHICH
    { SBranch{ sBranches=[($3, $6)], sDefault=$8 } }

--------------------------------------------------------------------------------
struct_perhaps :: { Stmt }
  : branches_r branch_default BECAUSE ALICE WAS UNSURE WHICH
    { SBranch{ sBranches=$1, sDefault=$2 } }

branches_r ::                   { [(Expr, [Stmt])] }
  : PERHAPS branch              { [$2] }
  | branches_r OR MAYBE branch  { $4 : $1 }

branch ::                   { (Expr, [Stmt]) }
  : "(" expr ")" SO body    { ($2, $5) }

branch_default ::   { [Stmt] }
  : {- EMPTY -}     { [] }
  | OR body         { $2 }

--------------------------------------------------------------------------------
struct_while :: { Stmt }
  : EVENTUALLY "(" expr ")" BECAUSE body ENOUGH TIMES
    { SWhile{ sGuard=$3, sBody=$6 } }

--------------------------------------------------------------------------------
struct_fun :: { Stmt }
  : THE ROOM ID "(" decl_params ")" CONTAINED A type OPENED body CLOSED
    { SFunDec{ sID=$3, sDParams=$5, sType=$9, sBody=$11 } }

--------------------------------------------------------------------------------
struct_sub :: { Stmt }
  : THE LOOKING_GLASS ID "(" decl_params ")" OPENED body CLOSED
    { SSubDec{ sID=$3, sDParams=$5, sBody=$8 } }

--------------------------------------------------------------------------------
struct_block ::         { Stmt }
  : OPENED body CLOSED  { SBlock $2 }

--------------------------------------------------------------------------------
decl_params ::      { [Param] }
  : {- EMPTY -}     { [] }
  | decl_params_r   { reverse $1 }

decl_params_r ::                    { [Param] }
  : decl_param                      { [$1] }
  | decl_params_r "," decl_param    { $3 : $1 }

decl_param ::       { Param }
  : type ID         { ($2, $1) }
  | SPIDER type ID  { ($3, TyArray $2) }

--------------------------------------------------------------------------------
call_params ::              { [Expr] }
  : {- EMPTY -}             { [] }
  | call_params_r           { reverse $1 }

call_params_r ::            { [Expr] }
  : expr                    { [$1] }
  | call_params_r "," expr  { $3 : $1 }

--------------------------------------------------------------------------------
type ::         { Type }
  : NUMBER      { TyNumber }
  | LETTER      { TyLetter }
  | SENTENCE    { TySentence }

--------------------------------------------------------------------------------
expr ::                         { Expr }
  : expr_number                 { $1 }
  | expr_letter                 { $1 }
  | expr_sentence               { $1 }
  | ID                          { EVariable $1 }
  | ID "(" call_params ")"      { EFunCall $1 $3 }
  | "-" expr %prec NEGATE       { EUnary UNeg $2 }
  | "!" expr                    { EUnary UNot $2 }
  | "~" expr                    { EUnary UNotB $2 }
  | expr "+" expr               { EBinary BAdd $1 $3 }
  | expr "-" expr               { EBinary BSub $1 $3 }
  | expr "*" expr               { EBinary BMul $1 $3 }
  | expr "/" expr               { EBinary BDiv $1 $3 }
  | expr "%" expr               { EBinary BRem $1 $3 }
  | expr "==" expr              { EBinary BEqu $1 $3 }
  | expr "!=" expr              { EBinary BNEq $1 $3 }
  | expr "<" expr               { EBinary BLT $1 $3 }
  | expr ">" expr               { EBinary BGT $1 $3 }
  | expr "<=" expr              { EBinary BLE $1 $3 }
  | expr ">=" expr              { EBinary BGE $1 $3 }
  | expr "&&" expr              { EBinary BAnd $1 $3 }
  | expr "||" expr              { EBinary BOr $1 $3 }
  | expr "&" expr               { EBinary BAndB $1 $3 }
  | expr "|" expr               { EBinary BOrB $1 $3 }
  | expr "^" expr               { EBinary BXOrB $1 $3 }

--------------------------------------------------------------------------------
expr_number ::      { Expr }
  : LITERAL_NUMBER  { ENumber $1 }

--------------------------------------------------------------------------------
expr_letter ::          { Expr }
  : "'" QUOTE_CHR "'"   { ELetter $2 }
  | "'" QUOTE_ESC "'"   { ELetter (unescape $2) }

--------------------------------------------------------------------------------
expr_sentence ::        { Expr }
  : '"' quote_chars '"' { ESentence (reverse $2) }

quote_chars ::              { String }
  : {- EMPTY -}             { "" }
  | quote_chars QUOTE_CHR   { $2 : $1 }
  | quote_chars QUOTE_ESC   { (unescape $2) : $1 }

--------------------------------------------------------------------------------
{
unescape :: String -> Char
unescape ['\\', c]
  = fromJust $ lookup c $ zip "\"'\\abfnrtv" "\"\\'\a\b\f\n\r\t\v"
unescape ('\\' : 'x' : cs)
  = chr n where [(n, "")] = readHex cs

parseError :: [Token] -> a
parseError []
    = error "syntax error at end of input"
parseError (t : _)
    = error $ "syntax error at token " ++ show t
}
