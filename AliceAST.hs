module AliceAST where

type ID
  = String

type Param
  = (ID, Type)

type Branch
  = (Expr, [Stmt])

data Type
  = TNumber | TLetter | TSentence | TBool | TArray !Type

data Stmt
  = SVarDec{ sID :: !ID, sType :: !Type }
  | SVarIni{ sID :: !ID, sType :: !Type, sSource :: Expr }
  | SArrDec{ sID :: !ID, sType :: !Type, sSize :: Expr }
  | SSubDec{ sID :: !ID, sParams :: [Param], sBody :: [Stmt] }
  | SFunDec{ sID :: !ID, sParams :: [Param], sBody :: [Stmt], sType :: !Type }
  | SAssign{ sTarget :: Expr, sSource :: Expr }
  | SInc{ sTarget :: Expr }
  | SDec{ sTarget :: Expr }
  | SRead{ sTarget :: Expr }
  | SWrite{ sSource :: Expr }
  | SReturn{ sSource :: Expr }
  | SCall{ sID :: !ID, sParams :: [Expr] }
  | SBranch{ sBranches :: [Branch], sDefault :: [Stmt] }
  | SWhile{ sGuard :: Expr, sBody :: [Stmt] }

data Expr
  = Expr
