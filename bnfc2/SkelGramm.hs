module SkelGramm where

-- Haskell module generated by the BNF converter

import AbsGramm
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transPIdent :: PIdent -> Result
transPIdent x = case x of
  PIdent string -> failure x
transPFloat :: PFloat -> Result
transPFloat x = case x of
  PFloat string -> failure x
transPInteger :: PInteger -> Result
transPInteger x = case x of
  PInteger string -> failure x
transPString :: PString -> Result
transPString x = case x of
  PString string -> failure x
transPChar :: PChar -> Result
transPChar x = case x of
  PChar string -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  Prog declarations -> failure x
transTypeSpec :: TypeSpec -> Result
transTypeSpec x = case x of
  TSimple stype -> failure x
  TPointer typespec -> failure x
  TArray exp typespec -> failure x
transSType :: SType -> Result
transSType x = case x of
  SType_float -> failure x
  SType_int -> failure x
  SType_char -> failure x
  SType_string -> failure x
  SType_bool -> failure x
  SType_null -> failure x
transDeclaration :: Declaration -> Result
transDeclaration x = case x of
  DecVar pident typespec -> failure x
  DefVar pident typespec exp -> failure x
  DefArray pident exps -> failure x
  DefProc pident args body -> failure x
transBody :: Body -> Result
transBody x = case x of
  EBody exp -> failure x
  BBody block -> failure x
transArg :: Arg -> Result
transArg x = case x of
  DArg pident type_ -> failure x
transOp :: Op -> Result
transOp x = case x of
  Or -> failure x
  And -> failure x
  Less -> failure x
  LessEq -> failure x
  Greater -> failure x
  GreterEq -> failure x
  Equal -> failure x
  NotEq -> failure x
  Plus -> failure x
  Minus -> failure x
  Prod -> failure x
  Div -> failure x
  Mod -> failure x
  Pow -> failure x
transExp :: Exp -> Result
transExp x = case x of
  EArray exps -> failure x
  ENot exp -> failure x
  ENeg exp -> failure x
  EDeref exp -> failure x
  ERef exp -> failure x
  EInt pinteger -> failure x
  EFloat pfloat -> failure x
  EVar pident -> failure x
  EChar pchar -> failure x
  EString pstring -> failure x
  ETrue -> failure x
  EFalse -> failure x
  EOp exp1 op exp2 -> failure x
  ETyped exp type_ -> failure x
  EVarTyped pident type_ pinteger1 pinteger2 -> failure x
transType :: Type -> Result
transType x = case x of
  Type_float -> failure x
  Type_int -> failure x
  Type_char -> failure x
  Type_string -> failure x
  Type_bool -> failure x
  Type_null -> failure x
transStm :: Stm -> Result
transStm x = case x of
  Decla declaration -> failure x
  Expr exp -> failure x
  SBlock block -> failure x
  Assign pident exp -> failure x
  While exp stm -> failure x
  If exp stm1 stm2 -> failure x
transBlock :: Block -> Result
transBlock x = case x of
  DBlock stms -> failure x

