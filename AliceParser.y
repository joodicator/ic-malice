-- This is a Happy (http://haskell.org/happy) parser specification for the
-- Alice programming language.

-- NOTES ON DESIGN:

-- The grammar is intentionally made quite loose with respect to the actual
-- semantics, to allow the semantic analyser to present more helpful error
-- messages (at the expense of greater complexity in the latter).

-- The AliceParser module is tightly coupled to the AliceLexer module, because
-- the lexical analysis and parsing both take place inside the Alex monad.
-- This need not be the case for modules depending on AliceParser, however,
-- because the function aliceParse gives its result in the Either monad.
--------------------------------------------------------------------------------

{
module AliceParser where

import AliceToken
import AliceAST
import AliceLexer
}

%name       alexParse
%monad      { Alex }
%lexer      { (>>=) aliceMonadScan } { TC{ tcTok=TEOF } }
%tokentype  { TokenContext }
%error      { syntaxError }

%token
    AND                 { TC{ tcTok=TAnd } }
    BUT                 { TC{ tcTok=TBut } }
    THEN                { TC{ tcTok=TThen } }
    WHAT                { TC{ tcTok=TWhat } }
    A                   { TC{ tcTok=TA } }
    TOO                 { TC{ tcTok=TToo } }
    HAD                 { TC{ tcTok=THad } }
    OF                  { TC{ tcTok=TOf } }
    BECAME              { TC{ tcTok=TBecame } }
    ATE                 { TC{ tcTok=TAte } }
    DRANK               { TC{ tcTok=TDrank } }
    SPOKE               { TC{ tcTok=TSpoke } }
    SAID                { TC{ tcTok=TSaid } }
    FOUND               { TC{ tcTok=TFound } }
    EITHER              { TC{ tcTok=TEither } }
    OR                  { TC{ tcTok=TOr } }
    PERHAPS             { TC{ tcTok=TPerhaps } }
    SO                  { TC{ tcTok=TSo } }
    MAYBE               { TC{ tcTok=TMaybe } }
    EVENTUALLY          { TC{ tcTok=TEventually } }
    ENOUGH              { TC{ tcTok=TEnough } }
    TIMES               { TC{ tcTok=TTimes } }
    BECAUSE             { TC{ tcTok=TBecause } }
    ALICE               { TC{ tcTok=TAlice } }
    WAS                 { TC{ tcTok=TWas } }
    UNSURE              { TC{ tcTok=TUnsure } }
    WHICH               { TC{ tcTok=TWhich } }
    THE                 { TC{ tcTok=TThe } }
    ROOM                { TC{ tcTok=TRoom } }
    LOOKING_GLASS       { TC{ tcTok=TLookingGlass } }
    CONTAINED           { TC{ tcTok=TContained } }
    OPENED              { TC{ tcTok=TOpened } }
    CLOSED              { TC{ tcTok=TClosed } }
    SPIDER              { TC{ tcTok=TSpider } }
    NUMBER              { TC{ tcTok=TNumber } }
    LETTER              { TC{ tcTok=TLetter } }
    SENTENCE            { TC{ tcTok=TSentence } }
    PIECE               { TC{ tcTok=TPiece } }
    ","                 { TC{ tcTok=TComma } }
    "."                 { TC{ tcTok=TDot } }
    "?"                 { TC{ tcTok=TQuest } }
    "("                 { TC{ tcTok=TParenO } }
    ")"                 { TC{ tcTok=TParenC } }
    "+"                 { TC{ tcTok=TPlus } }
    "-"                 { TC{ tcTok=TMinus } }
    "*"                 { TC{ tcTok=TStar } }
    "/"                 { TC{ tcTok=TSlash } }
    "%"                 { TC{ tcTok=TPercent } }
    "=="                { TC{ tcTok=TEqEq } }
    "!="                { TC{ tcTok=TBangEq } }
    "<"                 { TC{ tcTok=TLess } }
    ">"                 { TC{ tcTok=TGreater } }
    "<="                { TC{ tcTok=TLessEq } }
    ">="                { TC{ tcTok=TGreaterEq } }
    "&&"                { TC{ tcTok=TAmpAmp } }
    "||"                { TC{ tcTok=TBarBar } }
    "&"                 { TC{ tcTok=TAmp } }
    "|"                 { TC{ tcTok=TBar } }
    "^"                 { TC{ tcTok=TCaret } }
    "~"                 { TC{ tcTok=TTilde } }
    "!"                 { TC{ tcTok=TBang } }
    "'"                 { TC{ tcTok=TQSingle } }
    '"'                 { TC{ tcTok=TQDouble } }
    "'s"                { TC{ tcTok=T's } }
    QUOTE_CHARS         { TC{ tcTok=TQChars $$ } }
    LITERAL_NUMBER      { TC{ tcTok=TNumberLiteral $$ } }
    ID                  { TC{ tcTok=TIdentifier $$ } }

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
-- A complete Alice program.
program ::  { [Stmt] }
  : body    { $1 }

--------------------------------------------------------------------------------
-- A sequence of statements.
body ::         { [Stmt] }
  : body_r      { reverse $1 }

body_r ::       { [Stmt] }
  : {- EMPTY -} { [] }
  | body_r stmt { $2 : $1 }
  | body_r "."  { $1 }

--------------------------------------------------------------------------------
-- A declaration or sequential instruction.
stmt ::                     { Stmt }
  : WHAT WAS expr "?"       { SRead{ sTarget=$3 } }
  | clause clause_term      { $1 }
  | struct_block            { $1 }
  | struct_either           { $1 }
  | struct_perhaps          { $1 }
  | struct_while            { $1 }
  | struct_fun              { $1 }
  | struct_sub              { $1 }

clause ::                   { Stmt }
  : ID WAS A type           { SVarDec{ sID=$1, sType=$4 } }
  | ID WAS A type TOO       { SVarDec{ sID=$1, sType=$4 } }
  | ID WAS A type OF expr   { SVarIni{ sID=$1, sType=$4, sSource=$6 } }
  | ID HAD expr type        { SArrDec{ sID=$1, sType=$4, sSize=$3 } }
  | expr BECAME expr        { SAssign{ sTarget=$1, sSource=$3 } }
  | expr ATE                { SInc{ sTarget=$1 } }
  | expr DRANK              { SDec{ sTarget=$1 } }
  | expr SPOKE              { SWrite{ sSource=$1 } }
  | expr SAID ALICE         { SWrite{ sSource=$1 } }
  | ID "(" call_params ")"  { SCall{ sID=$1, sCParams=$3 } }
  | ALICE FOUND expr        { SReturn{ sSource=$3 } }

clause_term ::              { TokenContext }
  : "."                     { $1 }
  | ","                     { $1 }
  | AND                     { $1 }
  | BUT                     { $1 }
  | THEN                    { $1 }

--------------------------------------------------------------------------------
-- A branching structure with exactly two branches.
struct_either :: { Stmt }
  : EITHER "(" expr ")" SO body OR body BECAUSE ALICE WAS UNSURE WHICH
    { SBranch{ sBranches=[($3, $6)], sDefault=$8 } }

--------------------------------------------------------------------------------
-- A branching structure with one or more branches.
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
-- A looping structure with a boolean guard.
struct_while :: { Stmt }
  : EVENTUALLY "(" expr ")" BECAUSE body ENOUGH TIMES
    { SWhile{ sGuard=$3, sBody=$6 } }

--------------------------------------------------------------------------------
-- The declaration of a subprogram with arguments and a return type.
struct_fun :: { Stmt }
  : THE ROOM ID "(" decl_params ")" CONTAINED A type OPENED body CLOSED
    { SFunDec{ sID=$3, sDParams=$5, sType=$9, sBody=$11 } }

--------------------------------------------------------------------------------
-- The declaration of a subprogram with arguments and no return type.
struct_sub :: { Stmt }
  : THE LOOKING_GLASS ID "(" decl_params ")" OPENED body CLOSED
    { SSubDec{ sID=$3, sDParams=$5, sBody=$8 } }

--------------------------------------------------------------------------------
-- An anonymous subprogram with no arguments or return type.
struct_block ::         { Stmt }
  : OPENED body CLOSED  { SBlock $2 }

--------------------------------------------------------------------------------
-- A list of zero or more formal parameters.
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
-- A list of zero or more actual parameters.
call_params ::              { [Expr] }
  : {- EMPTY -}             { [] }
  | call_params_r           { reverse $1 }

call_params_r ::            { [Expr] }
  : expr                    { [$1] }
  | call_params_r "," expr  { $3 : $1 }

--------------------------------------------------------------------------------
-- The name of a simple (i.e. not an array) type.
type ::         { Type }
  : NUMBER      { TyNumber }
  | LETTER      { TyLetter }
  | SENTENCE    { TySentence }

--------------------------------------------------------------------------------
-- An expression.
expr ::                         { Expr }
  : expr_number                 { $1 }
  | expr_letter                 { $1 }
  | expr_sentence               { $1 }
  | ID                          { EVariable $1 }
  | ID "(" call_params ")"      { EFunCall $1 $3 }
  | ID "'s" expr PIECE          { EArrayElement $1 $3 }
  | "(" expr ")"                { $2 }
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
-- A literal expression of type "number".
expr_number ::      { Expr }
  : LITERAL_NUMBER  { ENumber $1 }

--------------------------------------------------------------------------------
-- A literal expression of type "letter".
expr_letter ::                  { Expr }
  : "'" quote_chars_single "'"  { ELetter $ head $2 }

quote_chars_single ::           { String }
  : {- EMPTY -}                 { "" }
  | quote_chars_single following_token QUOTE_CHARS
    {% case $1 ++ $3 of { _:_:_ -> syntaxError $2; cs -> return cs } }

--------------------------------------------------------------------------------
-- A literal expression of type "sentence"
expr_sentence ::                { Expr }
  : '"' quote_chars '"'         { ESentence $2 }

quote_chars ::                  { String }
  : quote_chars_r               { concat $ reverse $1 }
quote_chars_r ::                { [String] }
  : {- EMPTY -}                 { [] }
  | quote_chars_r QUOTE_CHARS   { $2 : $1 }

--------------------------------------------------------------------------------
-- A zero-width production used to obtain the token following it.
following_token ::  { TokenContext }
  : {- EMPTY -}     {%^ return }

{
--------------------------------------------------------------------------------
-- Takes the input text stream and gives either an error message or the
-- resulting list of top-level statements.
aliceParse :: String -> Either String [Stmt]
aliceParse = flip runAlex alexParse

--------------------------------------------------------------------------------
-- Generates a failure condition based on the given invalid token context.
syntaxError :: TokenContext -> Alex a
syntaxError TC{ tcTok=token, tcPos=(chr, row, col), tcStr=str }
  = alexError $ "[" ++ show row ++ ":" ++ show col  ++ "] syntax error " ++ msg
  where
    msg = case token of
      TEOF  -> "near end of input"
      _     -> "near token: " ++ show str
}
