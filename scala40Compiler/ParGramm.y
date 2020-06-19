-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParGramm where
import AbsGramm
import LexGramm
import ErrM

}

%name pProgram Program
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  '!' { PT _ (TS _ 1) }
  '!=' { PT _ (TS _ 2) }
  '%' { PT _ (TS _ 3) }
  '%=' { PT _ (TS _ 4) }
  '&' { PT _ (TS _ 5) }
  '&&' { PT _ (TS _ 6) }
  '(' { PT _ (TS _ 7) }
  ')' { PT _ (TS _ 8) }
  '*' { PT _ (TS _ 9) }
  '*=' { PT _ (TS _ 10) }
  '+' { PT _ (TS _ 11) }
  '+=' { PT _ (TS _ 12) }
  ',' { PT _ (TS _ 13) }
  '-' { PT _ (TS _ 14) }
  '-=' { PT _ (TS _ 15) }
  '/' { PT _ (TS _ 16) }
  '/=' { PT _ (TS _ 17) }
  ':' { PT _ (TS _ 18) }
  ';' { PT _ (TS _ 19) }
  '<' { PT _ (TS _ 20) }
  '<-' { PT _ (TS _ 21) }
  '<=' { PT _ (TS _ 22) }
  '=' { PT _ (TS _ 23) }
  '==' { PT _ (TS _ 24) }
  '>' { PT _ (TS _ 25) }
  '>=' { PT _ (TS _ 26) }
  'Array' { PT _ (TS _ 27) }
  'Bool' { PT _ (TS _ 28) }
  'Char' { PT _ (TS _ 29) }
  'Float' { PT _ (TS _ 30) }
  'Int' { PT _ (TS _ 31) }
  'String' { PT _ (TS _ 32) }
  '[' { PT _ (TS _ 33) }
  ']' { PT _ (TS _ 34) }
  '^' { PT _ (TS _ 35) }
  '^=' { PT _ (TS _ 36) }
  'by' { PT _ (TS _ 37) }
  'def' { PT _ (TS _ 38) }
  'do' { PT _ (TS _ 39) }
  'else' { PT _ (TS _ 40) }
  'for' { PT _ (TS _ 41) }
  'if' { PT _ (TS _ 42) }
  'ref' { PT _ (TS _ 43) }
  'res' { PT _ (TS _ 44) }
  'until' { PT _ (TS _ 45) }
  'val' { PT _ (TS _ 46) }
  'valres' { PT _ (TS _ 47) }
  'var' { PT _ (TS _ 48) }
  'while' { PT _ (TS _ 49) }
  '{' { PT _ (TS _ 50) }
  '||' { PT _ (TS _ 51) }
  '}' { PT _ (TS _ 52) }
  L_ident  { PT _ (TV $$) }
  L_integ  { PT _ (TI $$) }
  L_PTrue { PT _ (T_PTrue _) }
  L_PFalse { PT _ (T_PFalse _) }
  L_PReturn { PT _ (T_PReturn _) }
  L_PNull { PT _ (T_PNull _) }
  L_PBreak { PT _ (T_PBreak _) }
  L_PContinue { PT _ (T_PContinue _) }
  L_PIdent { PT _ (T_PIdent _) }
  L_PFloat { PT _ (T_PFloat _) }
  L_PInteger { PT _ (T_PInteger _) }
  L_PString { PT _ (T_PString _) }
  L_PChar { PT _ (T_PChar _) }

%%

Ident   :: { Ident }
Ident    : L_ident  { Ident $1 }

Integer :: { Integer }
Integer  : L_integ  { (read ( $1)) :: Integer }

PTrue :: { PTrue}
PTrue  : L_PTrue { PTrue (mkPosToken $1)}

PFalse :: { PFalse}
PFalse  : L_PFalse { PFalse (mkPosToken $1)}

PReturn :: { PReturn}
PReturn  : L_PReturn { PReturn (mkPosToken $1)}

PNull :: { PNull}
PNull  : L_PNull { PNull (mkPosToken $1)}

PBreak :: { PBreak}
PBreak  : L_PBreak { PBreak (mkPosToken $1)}

PContinue :: { PContinue}
PContinue  : L_PContinue { PContinue (mkPosToken $1)}

PIdent :: { PIdent}
PIdent  : L_PIdent { PIdent (mkPosToken $1)}

PFloat :: { PFloat}
PFloat  : L_PFloat { PFloat (mkPosToken $1)}

PInteger :: { PInteger}
PInteger  : L_PInteger { PInteger (mkPosToken $1)}

PString :: { PString}
PString  : L_PString { PString (mkPosToken $1)}

PChar :: { PChar}
PChar  : L_PChar { PChar (mkPosToken $1)}

TypeSpec :: { TypeSpec }
TypeSpec : SType { AbsGramm.TSimple $1 }
         | '*' TypeSpec { AbsGramm.TPointer $2 }
         | 'Array' '[' TypeSpec ']' '(' PInteger ')' { AbsGramm.TArray $3 $6 }
SType :: { SType }
SType : 'Float' { AbsGramm.SType_Float }
      | 'Int' { AbsGramm.SType_Int }
      | 'Char' { AbsGramm.SType_Char }
      | 'String' { AbsGramm.SType_String }
      | 'Bool' { AbsGramm.SType_Bool }
Program :: { Program }
Program : ListDeclaration { AbsGramm.Prog (reverse $1) }
ListDeclaration :: { [Declaration] }
ListDeclaration : {- empty -} { [] }
                | ListDeclaration Declaration { flip (:) $1 $2 }
Declaration :: { Declaration }
Declaration : 'var' PIdent ':' TypeSpec '=' Exp ';' { AbsGramm.DefVar $2 $4 $6 }
            | 'var' PIdent ':' TypeSpec ';' { AbsGramm.DecVar $2 $4 }
            | 'def' PIdent ListParamClause ':' TypeSpec '=' Block { AbsGramm.DefFun $2 $3 $5 $7 }
            | 'def' PIdent ListParamClause ':' TypeSpec '=' Exp ';' { AbsGramm.DefFunInLine $2 $3 $5 $7 }
            | 'def' PIdent ListParamClause '=' Block { dproc_ $2 $3 $5 }
            | 'def' PIdent ListParamClause '=' Exp ';' { dprocinline_ $2 $3 $5 }
ListParamClause :: { [ParamClause] }
ListParamClause : ParamClause { (:[]) $1 }
                | ParamClause ListParamClause { (:) $1 $2 }
ParamClause :: { ParamClause }
ParamClause : '(' ListParam ')' { AbsGramm.PParam $2 }
ListParam :: { [Param] }
ListParam : {- empty -} { [] }
          | Param { (:[]) $1 }
          | Param ',' ListParam { (:) $1 $3 }
Param :: { Param }
Param : ParamPassMod PIdent ':' TypeSpec { AbsGramm.DParam $1 $2 $4 }
Block :: { Block }
Block : '{' ListStm '}' { AbsGramm.DBlock (reverse $2) }
ParamPassMod :: { ParamPassMod }
ParamPassMod : 'val' { AbsGramm.ParamPassMod_val }
             | 'ref' { AbsGramm.ParamPassMod_ref }
             | 'res' { AbsGramm.ParamPassMod_res }
             | 'valres' { AbsGramm.ParamPassMod_valres }
ListStm :: { [Stm] }
ListStm : {- empty -} { [] } | ListStm Stm { flip (:) $1 $2 }
Stm :: { Stm }
Stm : Declaration { AbsGramm.SDecl $1 }
    | Block { AbsGramm.SBlock $1 }
    | LExp '=' Exp ';' { AbsGramm.SAssign $1 $3 }
    | 'while' '(' Exp ')' Stm { AbsGramm.SWhile $3 $5 }
    | 'if' '(' Exp ')' Stm 'else' Stm { AbsGramm.SIfElse $3 $5 $7 }
    | 'if' '(' Exp ')' Stm { sif_ $3 $5 }
    | 'do' Stm 'while' '(' Exp ')' ';' { AbsGramm.SDoWhile $2 $5 }
    | 'for' '(' Ident '<-' Exp 'until' Exp 'by' Exp ')' Stm { AbsGramm.SFor $3 $5 $7 $9 $11 }
    | PReturn ';' { AbsGramm.SReturn $1 }
    | PReturn Exp ';' { AbsGramm.SReturnExp $1 $2 }
    | PIdent ListArgs ';' { AbsGramm.SProcCall $1 $2 }
    | LExp OpAssign Exp ';' { sugarAssign_ $1 $2 $3 }
OpAssign :: { OpAssign }
OpAssign : '*=' { AbsGramm.ProdEq }
         | '/=' { AbsGramm.DivEq }
         | '%=' { AbsGramm.ModEq }
         | '+=' { AbsGramm.PlusEq }
         | '-=' { AbsGramm.MinusEq }
         | '^=' { AbsGramm.PowEq }
Args :: { Args }
Args : '(' ListExp ')' { AbsGramm.ArgExp $2 }
ListArgs :: { [Args] }
ListArgs : Args { (:[]) $1 } | Args ListArgs { (:) $1 $2 }
ListExp :: { [Exp] }
ListExp : {- empty -} { [] }
        | Exp { (:[]) $1 }
        | Exp ',' ListExp { (:) $1 $3 }
Op :: { Op }
Op : '||' { AbsGramm.Or } | Op1 { $1 }
Op1 :: { Op }
Op1 : '&&' { AbsGramm.And } | Op2 { $1 }
Op2 :: { Op }
Op2 : '<' { AbsGramm.Less }
    | '<=' { AbsGramm.LessEq }
    | '>' { AbsGramm.Greater }
    | '>=' { AbsGramm.GreaterEq }
    | '==' { AbsGramm.Equal }
    | '!=' { AbsGramm.NotEq }
    | Op3 { $1 }
Op3 :: { Op }
Op3 : '+' { AbsGramm.Plus } | '-' { AbsGramm.Minus } | Op4 { $1 }
Op4 :: { Op }
Op4 : '*' { AbsGramm.Prod }
    | '/' { AbsGramm.Div }
    | '%' { AbsGramm.Mod }
    | Op5 { $1 }
Op5 :: { Op }
Op5 : '^' { AbsGramm.Pow } | '(' Op ')' { $2 }
Exp :: { Exp }
Exp : Exp Op Exp1 { op_ $1 $2 $3 } | Exp1 { $1 }
Exp1 :: { Exp }
Exp1 : Exp1 Op1 Exp2 { op_ $1 $2 $3 } | Exp2 { $1 }
Exp2 :: { Exp }
Exp2 : '!' Exp2 { AbsGramm.ENot $2 }
     | Exp3 Op2 Exp3 { op_ $1 $2 $3 }
     | Exp3 { $1 }
Exp3 :: { Exp }
Exp3 : Exp3 Op3 Exp4 { op_ $1 $2 $3 } | Exp4 { $1 }
Exp4 :: { Exp }
Exp4 : Exp4 Op4 Exp5 { op_ $1 $2 $3 } | Exp5 { $1 }
Exp5 :: { Exp }
Exp5 : Exp6 Op5 Exp7 { op_ $1 $2 $3 } | Exp6 { $1 }
Exp6 :: { Exp }
Exp6 : '-' Exp7 { AbsGramm.ENeg $2 } | Exp7 { $1 }
Exp7 :: { Exp }
Exp7 : LExp { AbsGramm.ELExp $1 }
     | '&' LExp { AbsGramm.EDeref $2 }
     | PInteger { AbsGramm.EInt $1 }
     | PFloat { AbsGramm.EFloat $1 }
     | PChar { AbsGramm.EChar $1 }
     | PString { AbsGramm.EString $1 }
     | PTrue { AbsGramm.ETrue $1 }
     | PFalse { AbsGramm.EFalse $1 }
     | PNull { AbsGramm.ENull $1 }
     | 'Array' '(' ListExp ')' { AbsGramm.EArray $3 }
     | PIdent ListArgs { AbsGramm.EFunCall $1 $2 }
     | 'if' '(' Exp ')' Exp 'else' Exp { AbsGramm.EIfElse $3 $5 $7 }
     | '(' Exp ')' { $2 }
LExp :: { LExp }
LExp : '*' LExp { AbsGramm.LRef $2 } | LExp1 { $1 }
LExp1 :: { LExp }
LExp1 : LExp1 '[' Exp ']' { AbsGramm.LArr $1 $3 }
      | PIdent { AbsGramm.LIdent $1 }
      | '(' LExp ')' { $2 }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens
op_ e1_ o_ e2_ = EOp e1_ o_ e2_
dproc_ id_ params_ block_ = DefFun id_ params_ (TSimple SType_Void) block_
dprocinline_ id_ params_ exp_ = DefFunInLine id_ params_ (TSimple SType_Void) exp_
sif_ exp_ stm_ = SIfElse exp_ stm_ (SBlock (DBlock []))
sugarAssign_ lexp_ ProdEq_ exp_ = SAssign lexp_ (op_ (ELExp lexp_) Prod exp_)
sugarAssign_ lexp_ MinusEq_ exp_ = SAssign lexp_ (op_ (ELExp lexp_) Minus exp_)
sugarAssign_ lexp_ PlusEq_ exp_ = SAssign lexp_ (op_ (ELExp lexp_) Plus exp_)
sugarAssign_ lexp_ DivEq_ exp_ = SAssign lexp_ (op_ (ELExp lexp_) Div exp_)
sugarAssign_ lexp_ ModEq_ exp_ = SAssign lexp_ (op_ (ELExp lexp_) Mod exp_)
sugarAssign_ lexp_ PowEq_ exp_ = SAssign lexp_ (op_ (ELExp lexp_) Pow exp_)
}

