-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

module AbsGramm where

newtype PIdent = PIdent ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PFloat = PFloat ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PInteger = PInteger ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PString = PString ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PChar = PChar ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

data Program = Prog [Declaration]
  deriving (Eq, Ord, Show, Read)

data TypeSpec
    = TSimple SType | TPointer TypeSpec | TArray Exp TypeSpec
  deriving (Eq, Ord, Show, Read)

data SType
    = SType_float
    | SType_int
    | SType_char
    | SType_string
    | SType_bool
    | SType_null
  deriving (Eq, Ord, Show, Read)

data Declaration
    = DecVar PIdent TypeSpec
    | DefVar PIdent TypeSpec Exp
    | DefArray PIdent [Exp]
    | DefProc PIdent [Arg] Body
  deriving (Eq, Ord, Show, Read)

data Body = EBody Exp | BBody Block
  deriving (Eq, Ord, Show, Read)

data Arg = DArg PIdent Type
  deriving (Eq, Ord, Show, Read)

data Op
    = Or
    | And
    | Less
    | LessEq
    | Greater
    | GreterEq
    | Equal
    | NotEq
    | Plus
    | Minus
    | Prod
    | Div
    | Mod
    | Pow
  deriving (Eq, Ord, Show, Read)

data Exp
    = EArray [Exp]
    | ENot Exp
    | ENeg Exp
    | EDeref Exp
    | ERef Exp
    | EInt PInteger
    | EFloat PFloat
    | EVar PIdent
    | EChar PChar
    | EString PString
    | ETrue
    | EFalse
    | EOp Exp Op Exp
    | ETyped Exp Type
    | EVarTyped PIdent Type PInteger PInteger
  deriving (Eq, Ord, Show, Read)

data Type
    = Type_float
    | Type_int
    | Type_char
    | Type_string
    | Type_bool
    | Type_null
  deriving (Eq, Ord, Show, Read)

data Stm
    = Decla Declaration
    | Expr Exp
    | SBlock Block
    | Assign PIdent Exp
    | While Exp Stm
    | If Exp Stm Stm
  deriving (Eq, Ord, Show, Read)

data Block = DBlock [Stm]
  deriving (Eq, Ord, Show, Read)
