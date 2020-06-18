-- Haskell data types for the abstract syntax.
-- Partially generated by the BNF converter.

module AbsGramm where

type Loc = (Int, Int)
type Ident = String

newtype PTrue = PTrue (Loc,Ident)
  deriving (Eq, Ord, Show, Read)

newtype PFalse = PFalse (Loc,Ident)
  deriving (Eq, Ord, Show, Read)

newtype PReturn = PReturn (Loc,Ident)
  deriving (Eq, Ord, Show, Read)

newtype PNull = PNull (Loc,Ident)
  deriving (Eq, Ord, Show, Read)

newtype PIdent = PIdent (Loc,Ident)
  deriving (Eq, Ord, Show, Read)

newtype PFloat = PFloat (Loc,Ident)
  deriving (Eq, Ord, Show, Read)

newtype PInteger = PInteger (Loc,Ident)
  deriving (Eq, Ord, Show, Read)

newtype PString = PString (Loc,Ident)
  deriving (Eq, Ord, Show, Read)

newtype PChar = PChar (Loc,Ident)
  deriving (Eq, Ord, Show, Read)

data Program = Prog [Declaration]
  deriving (Eq, Ord, Show, Read)

data TypeSpec
    = TSimple SType | TPointer TypeSpec | TArray TypeSpec PInteger
  deriving (Show, Read)

data SType
    = SType_Bool
    | SType_Char
    | SType_Int
    | SType_Float  
    
    | SType_String
    | SType_Error
    | SType_Void
  deriving (Eq, Ord, Show, Read)

data OpAssign = ProdEq | DivEq | ModEq | PlusEq | MinusEq | PowEq
  deriving (Eq, Ord, Show, Read)

data Declaration
    = DefVar PIdent TypeSpec Exp
    | DecVar PIdent TypeSpec
    | DefFun PIdent [ParamClause] TypeSpec Block
    | DefFunInLine PIdent [ParamClause] TypeSpec Exp
  deriving (Eq, Ord, Show, Read)

data ParamClause = PArg [Arg]
  deriving (Eq, Ord, Show, Read)

data Arg = DArg PIdent TypeSpec
  deriving (Eq, Ord, Show, Read)

data Block = DBlock [Stm]
  deriving (Eq, Ord, Show, Read)

data Stm
    = SDecl Declaration
    | SBlock Block
    | SAssign LExp Exp
    | SWhile Exp Stm
    | SIfElse Exp Stm Stm
    | SReturn PReturn
    | SReturnExp PReturn Exp
    | SProcCall PIdent [Params]
  deriving (Eq, Ord, Show, Read)

data Params 
  = ParExp [Exp]
  | ParExpTyped [(Exp,TypeSpec)]
  deriving (Eq, Ord, Show, Read)

data Op
    = Or
    | And
    | Less
    | LessEq
    | Greater
    | GreaterEq
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
    = ENot Exp
    | ENeg Exp
    | ELExp LExp
    | EDeref LExp
    | EInt PInteger
    | EFloat PFloat
    | EChar PChar
    | EString PString
    | ETrue PTrue
    | EFalse PFalse
    | ENull PNull
    | EArray [Exp]
    | EFunCall PIdent [Params]
    | ETyped Exp TypeSpec Loc
    | EOp Exp Op Exp
  deriving (Eq, Ord, Show, Read)

data LExp
    = LRef LExp
    | LArr LExp Exp
    | LIdent PIdent
    | LExpTyped LExp TypeSpec Loc
  deriving (Eq, Ord, Show, Read)

instance Ord TypeSpec where
  (<=) (TSimple typ1) (TSimple typ2) = typ1 <= typ2
  (<=) (TPointer (TSimple SType_Void)) (TPointer typ2) = True
  (<=) (TPointer typ1) (TPointer typ2) = typ1 <= typ2
  (<=) (TArray typ1 (PInteger (_,dim1))) (TArray typ2 (PInteger (_,dim2))) = dim1 == dim2 && typ1 <= typ2
  (<=) _ _ = False


instance Eq TypeSpec where
  (==) (TSimple typ1) (TSimple typ2) = typ1 == typ2 
  (==) (TPointer typ1) (TPointer typ2) = typ1 == typ2
  (==) (TArray typ1 (PInteger (_,dim1))) (TArray typ2 (PInteger (_,dim2))) = dim1 == dim2 && typ1 == typ2
  (==) _ _ = False
