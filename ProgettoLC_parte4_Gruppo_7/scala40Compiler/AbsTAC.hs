module AbsTAC where

import AbsGramm

data Addr
    = Var Ident Loc
    | VarCopy Ident Loc
    | Temp Int
    | LitString String
    | LitFloat Float
    | LitChar Char
    | LitInt Int
    | LitBool Bool
    | LitNull
  deriving (Eq, Ord, Show, Read)

data Label 
  = LabStm Int
  | LabFun Ident Loc
  | Fall
  deriving (Eq, Ord, Show, Read)

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
    | Cast TACType
  deriving (Eq, Ord, Show, Read)

data TACType = TACInt | TACFloat | TACChar | TACBool | TACString | TACAddr
    deriving (Eq, Ord, Show, Read)

data TAC
    = AssignBinOp Addr Addr BinOp Addr TACType     -- x = y op z
    | AssignUnOp Addr UnOp Addr TACType            -- x = op y
    | Assign Addr Addr TACType                     -- x = y
    | AssignFromArray Addr Addr Addr TACType       -- x = y[z]
    | AssignToArray Addr Addr Addr TACType         -- y[z] = x
    | AssignFromRef Addr Addr TACType              -- x = &y
    | AssignFromPointer Addr Addr TACType          -- x = *y
    | AssignToPointer Addr Addr TACType            -- *x = y
    | AssignFromFunction Addr Label Int TACType    -- x = fcall f , n

    | Goto Label                                    -- goto l
    | IfBool Addr Label                             -- if x goto l
    | IfRel BinOp Addr Addr Label                   -- if x rel y goto l
    | IfFalse Addr Label                            -- ifFalse x goto l

    | Lab Label                                     -- l:

    | ReturnVoid                                    -- return
    | ReturnAddr Addr                               -- return x
    | Param Addr                                    -- param x
    | Call Label Int                                 -- call f , n

    | Comment String
    | CommentArgs [(TACType, Addr)]

  deriving (Eq, Ord, Show, Read)
