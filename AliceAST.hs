module AliceAST where

type ID
  = String

type Param
  = (ID, Type)

type Branch
  = (Expr, [Stmt])

data Type
  = TyNumber
  | TyLetter
  | TySentence
  | TyBool
  | TyArray !Type
  deriving (Eq, Show)

data Stmt
  = SFunDec{ sID :: !ID, sDParams :: [Param], sBody :: [Stmt], sType :: !Type }
  | SSubDec{ sID :: !ID, sDParams :: [Param], sBody :: [Stmt] }
  | SVarDec{ sID :: !ID, sType :: !Type }
  | SVarIni{ sID :: !ID, sType :: !Type, sSource :: Expr }
  | SArrDec{ sID :: !ID, sType :: !Type, sSize :: Expr }
  | SBranch{ sBranches :: [Branch], sDefault :: [Stmt] }
  | SWhile{ sGuard :: Expr, sBody :: [Stmt] }
  | SBlock{ sBody :: [Stmt] }
  | SCall{ sID :: !ID, sCParams :: [Expr] }
  | SReturn{ sSource :: Expr }
  | SAssign{ sTarget :: Expr, sSource :: Expr }
  | SInc{ sTarget :: Expr }
  | SDec{ sTarget :: Expr }
  | SRead{ sTarget :: Expr }
  | SWrite{ sSource :: Expr }
  deriving (Eq, Show)

data Expr
  = ENumber !Integer
  | ELetter !Char
  | ESentence !String
  | EVariable !ID
  | EFunCall !ID [Expr]
  | EBinary !BiOp Expr Expr
  | EUnary !UnOp Expr
  deriving (Eq, Show)

data BiOp
  = BAdd | BSub | BMul | BDiv | BRem
  | BEqu | BNEq | BLT | BGT | BLE | BGE
  | BAnd | BOr | BAndB | BOrB | BXOrB
  deriving (Eq, Show)

data UnOp
  = UNeg | UNot | UNotB
  deriving (Eq, Show)
