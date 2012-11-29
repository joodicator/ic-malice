-- Pretty printing for Alice abstract syntax trees.
--------------------------------------------------------------------------------
module AliceASTShow where

import AliceAST

import Data.List
import Data.Maybe

showProgram :: [Stmt] -> String
showProgram 
  = indent . showStmts

showType :: Type -> String
showType TyNumber       = "number"
showType TyLetter       = "letter"
showType TySentence     = "sentence"
showType TyBoolean      = "boolean"
showType (TyArray ty)   = showType ty ++ "[]"

showStmt :: Stmt -> String
showStmt SVarDec{ sID=id, sType=ty, sInit=Nothing }
  = showParam (id, ty)
showStmt SVarDec{ sID=id, sType=ty, sInit=Just exp }
  = showParam (id, ty) ++ " := " ++ showExpr exp
showStmt SArrDec{ sID=id, sType=ty, sSize=sz }
  = showParam (id, ty) ++ "[" ++ showExpr sz ++ "];"
showStmt SFunDec{ sID=id, sDParams=ps, sType=ty, sBody=ss }
  = showParam (id, ty) ++ showParams ps ++ showStmts ss
showStmt SSubDec{ sID=id, sDParams=ps, sBody=ss }
  = id ++ " " ++ showParams ps ++ " " ++ showStmts ss
showStmt SBranch{ sBranches=bs, sDefault=ss }
  = foldr f (showStmts ss) bs
  where
    f (g, ss) cs = "if (" ++ showExpr g ++ ") " ++ showStmts ss ++ " else " ++ cs
showStmt SWhile{ sGuard=g, sBody=ss }
  = "while (" ++ showExpr g ++ ") " ++ showStmts ss
showStmt SBlock{ sBody=ss }
  = showStmts ss
showStmt SCall{ sID=id, sCParams=params }
  = id ++ showExprs params
showStmt SReturn{ sSource=expr }
  = "return " ++ showExpr expr
showStmt SAssign{ sSource=rexp, sTarget=lexp }
  = showExpr lexp ++ " := " ++ showExpr rexp
showStmt SInc{ sTarget=expr }
  = "++" ++ showExpr expr
showStmt SDec{ sTarget=expr }
  = "--" ++ showExpr expr
showStmt SRead{ sTarget=expr }
  = showExpr expr ++ " <- stdin"
showStmt SWrite{ sSource=expr }
  = showExpr expr ++ " -> stdout"

showExprs :: [Expr] -> String
showExprs exprs
  = "(" ++ (intercalate ", " $ map showExpr exprs) ++ ")"

showParams :: [Param] -> String
showParams params
  = "(" ++ (intercalate ", " $ map showParam params) ++ ")"

showParam :: Param -> String
showParam (id, ty)
  = id ++ ":" ++ showType ty

showStmts :: [Stmt] -> String
showStmts stmts
  = "{\n\1" ++ concat ["\t" ++ showStmt s ++ ";\n" | s <- stmts] ++ "\2\t}"

showExpr :: Expr -> String
showExpr (ENumber x)
  = show x
showExpr (ELetter c)
  = show c
showExpr (ESentence cs)
  = show cs
showExpr (EVariable id)
  = id
showExpr (EFunCall id ps)
  = id ++ showExprs ps
showExpr (EArrElt id el)
  = id ++ "[" ++ showExpr el ++ "]"
showExpr (EBinary op e1 e2)
  = paren e1 ++ " " ++  showBiOp op ++ " " ++ paren e2
showExpr (EUnary op ex)
  = showUnOp op ++ "(" ++ showExpr ex ++ ")"

showBiOp :: BiOp -> String
showBiOp op
  = fromJust $ lookup op (zip i o)
  where
    i = [BAdd,BSub,BMul,BDiv,BRem,BEqu,BNEq,BLT,BGT,BLE,BGE,
         BAnd,BOr,BAndB,BOrB,BXOrB]
    o = ["+","-","*","/","%","==","!=","<",">","<=",">=",
         "&&","||","&","|","^"]

showUnOp :: UnOp -> String
showUnOp op
  = fromJust $ lookup op (zip i o)
  where
    i = [UNeg,UNot,UNotB]
    o = ["-","!","~"]

indent :: String -> String
indent
  = indent' 0
  where
    indent' n []         = []
    indent' n ('\1':cs)  = indent' (n+1) cs
    indent' n ('\2':cs)  = indent' (n-1) cs
    indent' n ('\t':cs)  = take (4 * n) (repeat ' ') ++ indent' n cs
    indent' n (c:cs)     = c : indent' n cs

paren :: Expr -> String
paren ex@(EBinary _ _ _)    = "(" ++ showExpr ex ++ ")"
paren ex@_                  = showExpr ex
