module SkelGramm where

-- Haskell module generated by the BNF converter

import AbsGramm
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transPTrue :: PTrue -> Result
transPTrue x = case x of
  PTrue string -> failure x
transPFalse :: PFalse -> Result
transPFalse x = case x of
  PFalse string -> failure x
transPReturn :: PReturn -> Result
transPReturn x = case x of
  PReturn string -> failure x
transPNull :: PNull -> Result
transPNull x = case x of
  PNull string -> failure x
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
  TArray typespec pinteger -> failure x
transSType :: SType -> Result
transSType x = case x of
  SType_Float -> failure x
  SType_Int -> failure x
  SType_Char -> failure x
  SType_String -> failure x
  SType_Bool -> failure x
  TypeError -> failure x
  TypeVoid -> failure x
transDeclaration :: Declaration -> Result
transDeclaration x = case x of
  DefVar pident typespec exp -> failure x
  DecVar pident typespec -> failure x
  DefFun pident paramclauses typespec block -> failure x
  DefFunInLine pident paramclauses typespec exp -> failure x
transParamClause :: ParamClause -> Result
transParamClause x = case x of
  PArg args -> failure x
transArg :: Arg -> Result
transArg x = case x of
  DArg pident typespec -> failure x
transBlock :: Block -> Result
transBlock x = case x of
  DBlock stms -> failure x
transStm :: Stm -> Result
transStm x = case x of
  SDecl declaration -> failure x
  SBlock block -> failure x
  SAssign lexp exp -> failure x
  SWhile exp stm -> failure x
  SIfElse exp stm1 stm2 -> failure x
  SReturn preturn -> failure x
  SReturnExp preturn exp -> failure x
  SProcCall pident paramss -> failure x
transParams :: Params -> Result
transParams x = case x of
  ParExp exps -> failure x
transOp :: Op -> Result
transOp x = case x of
  Or -> failure x
  And -> failure x
  Less -> failure x
  LessEq -> failure x
  Greater -> failure x
  GreaterEq -> failure x
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
  ENot exp -> failure x
  ENeg exp -> failure x
  ELExp lexp -> failure x
  EDeref lexp -> failure x
  EInt pinteger -> failure x
  EFloat pfloat -> failure x
  EChar pchar -> failure x
  EString pstring -> failure x
  ETrue ptrue -> failure x
  EFalse pfalse -> failure x
  ENull pnull -> failure x
  EArray exps -> failure x
  EFunCall pident paramss -> failure x
  EOp exp1 op exp2 -> failure x
transLExp :: LExp -> Result
transLExp x = case x of
  LRef lexp -> failure x
  LArr lexp exp -> failure x
  LIdent pident -> failure x
  LExpTyped lexp typespec loc1 loc2 -> failure x

