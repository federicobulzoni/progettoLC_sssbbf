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
  '(' { PT _ (TS _ 1) }
  ')' { PT _ (TS _ 2) }
  '*' { PT _ (TS _ 3) }
  '+' { PT _ (TS _ 4) }
  ',' { PT _ (TS _ 5) }
  '-' { PT _ (TS _ 6) }
  '/' { PT _ (TS _ 7) }
  ':' { PT _ (TS _ 8) }
  ';' { PT _ (TS _ 9) }
  '=' { PT _ (TS _ 10) }
  'def' { PT _ (TS _ 11) }
  'else' { PT _ (TS _ 12) }
  'float' { PT _ (TS _ 13) }
  'if' { PT _ (TS _ 14) }
  'int' { PT _ (TS _ 15) }
  'var' { PT _ (TS _ 16) }
  'while' { PT _ (TS _ 17) }
  '{' { PT _ (TS _ 18) }
  '}' { PT _ (TS _ 19) }
  L_integ  { PT _ (TI $$) }
  L_Id { PT _ (T_Id _) }

%%

Integer :: { Integer }
Integer  : L_integ  { (read ( $1)) :: Integer }

Id :: { Id}
Id  : L_Id { Id (mkPosToken $1)}

Program :: { Program }
Program : ListDecl { AbsGramm.Prog (reverse $1) }
ListDecl :: { [Decl] }
ListDecl : {- empty -} { [] } | ListDecl Decl { flip (:) $1 $2 }
Decl :: { Decl }
Decl : 'def' Id ListArgs ':' Type '=' Exp { AbsGramm.DFunInLine $2 $3 $5 $7 }
     | 'var' Id ':' Type { AbsGramm.DecVar $2 $4 }
     | 'var' Id ':' Type '=' Exp { AbsGramm.DefVar $2 $4 $6 }
ListArgs :: { [Args] }
ListArgs : {- empty -} { [] }
         | Args ListArgs { (:) $1 $2 }
         | Args { (:[]) $1 }
         | Args ListArgs { (:) $1 $2 }
Args :: { Args }
Args : '(' ListArg ')' { AbsGramm.DArgs $2 }
ListArg :: { [Arg] }
ListArg : {- empty -} { [] }
        | Arg { (:[]) $1 }
        | Arg ',' ListArg { (:) $1 $3 }
        | {- empty -} { [] }
        | Arg ListArg { (:) $1 $2 }
Arg :: { Arg }
Arg : Id ':' Type { AbsGramm.DArg $1 $3 }
Exp :: { Exp }
Exp : Exp '+' Exp1 { AbsGramm.EAdd $1 $3 }
    | Exp '-' Exp1 { AbsGramm.ESub $1 $3 }
    | Exp1 { $1 }
Exp1 :: { Exp }
Exp1 : Exp1 '*' Exp2 { AbsGramm.EMul $1 $3 }
     | Exp1 '/' Exp2 { AbsGramm.EDiv $1 $3 }
     | Exp2 { $1 }
Exp2 :: { Exp }
Exp2 : Integer { AbsGramm.EInt $1 }
     | Id { AbsGramm.EVar $1 }
     | '(' Exp ')' { $2 }
ListStm :: { [Stm] }
ListStm : {- empty -} { [] }
        | Stm ListStm { (:) $1 $2 }
        | {- empty -} { [] }
        | Stm { (:[]) $1 }
        | Stm ';' ListStm { (:) $1 $3 }
Type :: { Type }
Type : 'float' { AbsGramm.Type_float }
     | 'int' { AbsGramm.Type_int }
Stm :: { Stm }
Stm : Decl { AbsGramm.Decla $1 }
    | Exp { AbsGramm.Expr $1 }
    | Block { AbsGramm.SBlock $1 }
    | Id '=' Exp { AbsGramm.Assign $1 $3 }
    | 'while' '(' Exp ')' Stm { AbsGramm.While $3 $5 }
    | 'if' '(' Exp ')' Stm 'else' Stm { AbsGramm.If $3 $5 $7 }
Block :: { Block }
Block : '{' ListStm '}' { AbsGramm.DBlock $2 }
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
}

