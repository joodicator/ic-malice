-- This is a Happy parser specification for the Alice programming language.
-- For more information about Happy, see: http://haskell.org/happy
--------------------------------------------------------------------------------

{
module AliceParser where

import AliceToken
import AliceAST
}

%tokentype  { Token }
%error      { error . show }
%name       parseAlice

%%

--------------------------------------------------------------------------------
body ::       { [Stmt] }
  : body_r  { reverse $1 }

body_r ::               { [Stmt] }
  : {- EMPTY -}         { [] }
  | body_r sentence_r   { $2 ++ $1 }
  | body_r struct       { $2 : $1 }

--------------------------------------------------------------------------------
sentence_r ::                   { [Stmt] }
  : init_clauses_r final_clause { $2 ++ $1 }

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
  | ID call_params          { SCall{ sID=$1, sParams=$2 } }

--------------------------------------------------------------------------------
struct ::           { Stmt }
  : struct_either   { $1 }
  | struct_perhaps  { $1 }
  | struct_while    { $1 }
  | struct_fun      { $1 }
  | struct_sub      { $1 }

--------------------------------------------------------------------------------
struct_either :: { Stmt }
  : EITHER "(" expr ")" SO body OR body BECAUSE ALICE WAS UNSURE WHICH
    { SBranch{ sBranches=[($3, $6)], sDefault=$8 } }

--------------------------------------------------------------------------------
struct_perhaps :: { Stmt }
  : branches_r branch_default BECAUSE ALICE WAS UNSURE WHICH
    { SBranch{ sBranches=$1, sDefault=$2 } }

branches_r ::                   { [(Expr, [Stmt])] }
  : PERHAPS branch              { $2 }
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
    { SFunDec{ sID=$3, sParams=$5, sType=$9, sBody=$11 } }

--------------------------------------------------------------------------------
struct_sub :: { Stmt }
  : THE LOOKING_GLASS ID "(" decl_params ")" OPENED body CLOSED
    { SSubDec{ sID=$3, sParams=$5, sBody=$8 } }

--------------------------------------------------------------------------------
call_params

--------------------------------------------------------------------------------
decl_params

--------------------------------------------------------------------------------
expr
