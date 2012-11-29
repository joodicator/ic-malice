module AliceChecker where

import Control.Monad
import Data.List

import AliceAST



data BindTarget
  = BStmt   !Stmt
  | BParam  !Param
  deriving (Eq, Show)

type Bind
  = (ID, BindTarget)

--------------------------------------------------------------------------------
stmtError :: Stmt -> String -> Either String a
stmtError stmt msg
  = Left $ "[" ++ show row ++ ":" ++ show col ++ "] semantic error: " ++ msg
  where (chr,row,col) = sPosn stmt

--------------------------------------------------------------------------------
aliceError :: String -> Either String a
aliceError msg
  = Left $ "semantic error: " ++ msg

--------------------------------------------------------------------------------
isDecl :: Stmt -> Bool
isDecl stmt
  = case stmt of
        SFunDec{  } -> True
        SSubDec{  } -> True
        SVarDec{  } -> True
        SArrDec{  } -> True
        _           -> False

--------------------------------------------------------------------------------
isLValue :: Expr -> Bool
isLValue expr
  = case expr of
        EVariable{ }    -> True
        EArrElt{ }      -> True
        _               -> False

--------------------------------------------------------------------------------
flatten :: [Stmt] -> [Stmt]
flatten []
  = []
flatten (SBlock{ sBody=body } : ss)
  = flatten body ++ flatten ss
flatten (s : ss)
  = s : flatten ss

--------------------------------------------------------------------------------
checkAlice :: [Stmt] -> Either String ()
checkAlice stmts = do
    (binds, execs) <- checkStmts stmts [] []
    case lookup "hatta" binds of
      Just (BStmt stmt@SSubDec{ sDParams=params }) -> do
        unless (null params) $ stmtError stmt $
            "the top-level procedure " ++ show "hatta" ++ " may not take "
            ++ "any arguments"
      _ -> do
        aliceError $
            "the top-level procedure " ++ show "hatta" ++ " is missing"
 
--------------------------------------------------------------------------------
checkStmts :: [Stmt] -> [Bind] -> [Bind] -> Either String ([Bind], [Stmt])
checkStmts [] locals globals
  = return (locals, [])
checkStmts (stmt : stmts) locals globals
  = case isDecl stmt of
      True -> do
        checkDecls (stmt : stmts) locals globals
      False -> do
        checkExecs (stmt : stmts) (locals ++ globals)
        return (locals, stmt : stmts)

--------------------------------------------------------------------------------
checkDecls :: [Stmt] -> [Bind] -> [Bind] -> Either String ([Bind], [Stmt])
checkDecls (stmt : stmts) locals globals = do
    let name = sID stmt
    checkStmt stmt (locals ++ globals)
    case lookup name locals of
      Nothing -> do
        checkStmts stmts ((name, BStmt stmt) : locals) globals
      _ -> do
        stmtError stmt $ show name ++ " was previously declared in this scope"

--------------------------------------------------------------------------------
checkExecs :: [Stmt] -> [Bind] -> Either String ()
checkExecs stmts binds = do
    when (not (null stmts) && Nothing == lookup "$" binds) $
        stmtError (head stmts) $
            "executable statements are not permitted at the top level"
    forM_ stmts $ \stmt -> do
        when (isDecl stmt) $ stmtError stmt $
            "declarations are not permitted after executable statements"
        checkStmt stmt binds

--------------------------------------------------------------------------------
checkStmt :: Stmt -> [Bind] -> Either String ()

checkStmt stmt@SVarDec{ sInit=Just init, sID=id, sType=dType } binds = do
    iType <- checkExpr init binds
    unless (iType == dType) $ stmtError stmt $
        "in declaration of variable " ++ show id ++ ", initialiser type of "
        ++ show iType ++ " does not match declared type of " ++ show dType

checkStmt stmt@SArrDec{ sID=id, sSize=size } binds = do
    sType <- checkExpr size binds
    unless (sType == TyNumber) $ stmtError stmt $
        "in declaration of array " ++ show id ++ ", size type is "
        ++ show sType ++ ", where " ++ show TyNumber ++ " is expected"

checkStmt stmt@SFunDec{ } binds = do
    checkSubFun stmt binds

checkStmt stmt@SSubDec{ } binds = do
    checkSubFun stmt binds

checkStmt stmt@SBranch{ sBranches=branches } binds = do
    forM_ branches $ \(guard, body) -> do
         checkGuarded stmt guard body binds

checkStmt stmt@SWhile{ sGuard=guard, sBody=body } binds = do
    checkGuarded stmt guard body binds

checkStmt stmt@SBlock{ sBody=body } binds = do
    void $ checkStmts body [] binds

checkStmt stmt@SCall{ sID=id, sCParams=cParams } binds = do
    case lookup id binds of
      Just (BStmt SSubDec{ sDParams=dParams }) -> do
        checkSubFunCall stmt id cParams dParams binds
      _ -> do
        stmtError stmt $
            "the name " ++ show id ++ " is not bound to a subprocedure"

checkStmt stmt@SReturn{ sSource=expr } binds = do
    case lookup "$" binds of
      Just (BStmt SFunDec{ sID=id, sType=dType }) -> do
        eType <- checkExpr expr binds
        unless (eType == dType) $ stmtError stmt $
            "the return value has type " ++ show eType ++ ", where the "
            ++ "return type of " ++ show id ++ " is " ++ show dType
      _ -> do
        stmtError stmt $
            "return statement outside of a directly enclosing function"

checkStmt stmt@SAssign{ sTarget=target, sSource=source } binds = do
    unless (isLValue target) $ stmtError stmt $
        "illegal left-hand side form in assignment statement"
    tType <- checkExpr target binds
    sType <- checkExpr source binds
    unless (tType == sType) $ stmtError stmt $
        "left-hand side of assignment statement has type " ++ show tType
        ++ ", which differs from the right-hand side's type of " ++ show sType

checkStmt stmt@SInc{ sTarget=target } binds = do
    unless (isLValue target) $ stmtError stmt $
        "illegal operand form in increment statement"
    tType <- checkExpr target binds
    unless (tType `elem` [TyNumber, TyLetter]) $ stmtError stmt $
        "operand of increment statement has illegal type " ++ show tType

checkStmt stmt@SDec{ sTarget=target } binds = do
    unless (isLValue target) $ stmtError stmt $ 
        "illegal operand form in decrement statement"
    tType <- checkExpr target binds
    unless (tType `elem` [TyNumber, TyLetter]) $ stmtError stmt $
        "operand of decrement statement has illegal type " ++ show tType

checkStmt stmt@SRead{ sTarget=target } binds = do
    unless (isLValue target) $ stmtError stmt $
        "illegal operand form in read statement"
    void $ checkExpr target binds

checkStmt stmt@SWrite{ sSource=source } binds = do
    void $ checkExpr source binds

--------------------------------------------------------------------------------
checkSubFun :: Stmt -> [Bind] -> Either String ()
checkSubFun stmt globals = do
    let params = sDParams stmt
    let names = map fst params
    let dupes = names \\ nub names
    unless (null dupes) $ stmtError stmt $
        "the formal parameter name " ++ show (head dupes) ++ "occurs multiply"
    let locals = zip names (map BParam params) ++ [("$", BStmt stmt)]
    void $ checkStmts (sBody stmt) locals globals

--------------------------------------------------------------------------------
checkGuarded :: Stmt -> Expr -> [Stmt] -> [Bind] -> Either String ()
checkGuarded stmt guard body binds = do
    gType <- checkExpr guard binds
    unless (gType == TyBoolean) $ stmtError stmt $
       "the guard expression has type " ++ show gType ++ ", where "
       ++ show TyBoolean ++ " is expected"
    checkExecs body binds

--------------------------------------------------------------------------------
checkSubFunCall :: Stmt -> ID -> [Expr] -> [Param] -> [Bind] -> Either String ()
checkSubFunCall stmt id cParams dParams binds = do
    let (cLen, dLen) = (length cParams, length dParams)
    unless (length cParams == length dParams) $ stmtError stmt $
        "in the call to " ++ show id ++ ", " ++ show cLen
        ++ " parameters are given, but " ++ show dLen ++ " are expected"
    forM_ (zip cParams dParams) $ \(cExpr, (dName, dType)) -> do
        cType <- checkExpr cExpr binds
        unless (cType == dType) $ stmtError stmt $
            "in the call to " ++ show id ++ ", the parameter " ++ show dName
            ++ " has type " ++ show cType ++ ", but " ++ show dType
            ++ " is expected"

--------------------------------------------------------------------------------
checkExpr :: Expr -> [Bind] -> Either String Type
checkExpr = undefined
