module ABSTAc where

import AbsGramm

-- Var -> ident@(r,c)
-- Temp -> t_Int

data Addr
    = Var Ident Loc
    | Temp Int
    | LitString String
    | LitFloat Float
    | LitChar Char
    | LitInt Int
    | LitBool Int
    | Null
  deriving (Eq, Ord, Show, Read)

type Label = String

data BinOp 
    = PlusInt
    | PlusFloat
    | MinusInt
    | MinusFloat
    | ProdInt
    | ProdFloat
    | DivInt
    | DivFloat
    | ModInt
    | ModFloat
    | PowInt
    | PowFloat
    | Or
    | And
    | Equal
    | NotEqual
    | Less
    | LessEq
    | Greater
    | GreaterEq
  deriving (Eq, Ord, Show, Read)

data UnOp
    = NegInt
    | NegFloat
    | Not
  deriving (Eq, Ord, Show, Read)

data TAC
    = AssignBinOp BinOp Addr Addr Addr
    | AssignUnOp UnOp Addr Addr
    | Assign Addr Addr
    -- x = y[z]
    | AssignFromArray Addr Addr Addr
    -- y[z] = x
    | AssignToArray Addr Addr Addr
    -- x = &y
    | AssignFromRef Addr Addr
    -- x = *y
    | AssignFromPointer Addr Addr
    -- *x = y
    | AssignToPointer Addr Addr

    | Goto Label
    | IfBool Addr Label
    | IfRel BinOp Addr Addr Label
    | IfFalse Addr Label

    | Label Label
  deriving (Eq, Ord, Show, Read)
