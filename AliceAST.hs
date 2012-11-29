module AliceAST where

--------------------------------------------------------------------------------
-- A variable or function or subprocedure identifier.
type ID
  = String

--------------------------------------------------------------------------------
-- A formal parameter: a component of an SFunDec or SSubDec statement.
type Param
  = (ID, Type)

--------------------------------------------------------------------------------
-- A guarded execution branch: a component of an SBranch statement.
type Branch
  = (Expr, [Stmt])

--------------------------------------------------------------------------------
-- The character, line number and column number of an input stream position.
type Posn
  = (Int, Int, Int)

--------------------------------------------------------------------------------
-- The type of a variable or expression.
data Type
  = TyNumber
  | TyLetter
  | TySentence
  | TyBoolean
  | TyArray !Type
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- A statement: a declaration or sequential instruction.

-- The field sPosn is intentionally non-strict, to allow Stmt instances to be
-- initialised without a defined sPosn
data Stmt
  = SVarDec                         { -- Variable declaration
        sPosn       :: Posn         ,
        sID         :: !ID          ,
        sType       :: !Type        ,
        sInit       :: Maybe Expr   }
  | SArrDec                         { -- Array declaration
        sPosn       :: Posn         ,
        sID         :: !ID          ,
        sType       :: !Type        ,
        sSize       :: Expr         }
  | SFunDec                         { -- Function declaration
        sPosn       :: Posn         ,
        sID         :: !ID          ,
        sDParams    :: [Param]      ,
        sBody       :: [Stmt]       ,
        sType       :: !Type        }
  | SSubDec                         { -- Subroutine declaration
        sPosn       :: Posn         ,
        sID         :: !ID          ,
        sDParams    :: [Param]      ,
        sBody       :: [Stmt]       }
  | SBranch                         { -- Branching structure
        sPosn       :: Posn         ,
        sBranches   :: [Branch]     ,
        sDefault    :: [Stmt]       }
  | SWhile                          { -- Looping structure
        sPosn       :: Posn         ,
        sGuard      :: Expr         ,
        sBody       :: [Stmt]       }
  | SBlock                          { -- Anonymous block
        sPosn       :: Posn         ,
        sBody       :: [Stmt]       }
  | SCall                           { -- Subroutine call instruction
        sPosn       :: Posn         ,
        sID         :: !ID          ,
        sCParams    :: [Expr]       }
  | SReturn                         { -- Return instruction
        sPosn       :: Posn         ,
        sSource     :: Expr         }
  | SAssign                         { -- Assignment instruction
        sPosn       :: Posn         ,
        sTarget     :: Expr         ,
        sSource     :: Expr         }
  | SInc                            { -- Increment instruction
        sPosn       :: Posn         ,
        sTarget     :: Expr         }
  | SDec                            { -- Decrement instruction
        sPosn       :: Posn         ,
        sTarget     :: Expr         }
  | SRead                           { -- Input instruction
        sPosn       :: Posn         ,
        sTarget     :: Expr         }
  | SWrite                          { -- Output instruction
        sPosn       :: Posn         ,
        sSource     :: Expr         }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- An expression.
data Expr
  = ENumber     !Integer        -- Literal number
  | ELetter     !Char           -- Literal letter
  | ESentence   !String         -- Literal sentence
  | EVariable   !ID             -- Variable
  | EFunCall    !ID [Expr]      -- Function call
  | EArrElt     !ID Expr        -- Array element
  | EBinary     !BiOp Expr Expr -- Binary operator application
  | EUnary      !UnOp Expr      -- Unary operator application
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- A binary operator symbol.
data BiOp
  = BAdd    -- Addition
  | BSub    -- Subtraction
  | BMul    -- Multiplication
  | BDiv    -- Division
  | BRem    -- Remainder
  | BEqu    -- Equal
  | BNEq    -- Not equal
  | BLT     -- Less than
  | BGT     -- Greater than
  | BLE     -- Less than or equal
  | BGE     -- Greater than or equal
  | BAnd    -- Boolean and
  | BOr     -- Boolean or
  | BAndB   -- Bitwise and
  | BOrB    -- Bitwise or
  | BXOrB   -- Bitwise exclusive or
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- A unary operator symbol.
data UnOp
  = UNeg    -- Negation
  | UNot    -- Boolean not
  | UNotB   -- Bitwise not
  deriving (Eq, Show)
