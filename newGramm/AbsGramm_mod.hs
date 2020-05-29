-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

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
  deriving (Ord, Show, Read)

instance Eq TypeSpec where
  (==) (TSimple typ1) (TSimple typ2) = typ1 == typ2 
  (==) (TPointer typ1) (TPointer typ2) = typ1 == typ2
  (==) (TArray typ1 (PInteger (_,ident1))) (TArray typ2 (PInteger (_,ident2))) = ident1 == ident2 && typ1 == typ2
  (==) _ _ = False

data SType
    = SType_Float
    | SType_Int
    | SType_Char
    | SType_String
    | SType_Bool
    | TypeError
    | TypeVoid
  deriving (Eq, Ord, Show, Read)

data Declaration
    = DefVar PIdent TypeSpec Exp
    | DecVar PIdent TypeSpec
    | DefFun PIdent [ParamClause] TypeSpec Block
    | DefFunInLine PIdent [ParamClause] TypeSpec Exp
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
    | EOp Exp Op Exp
    | ETyped Exp TypeSpec Loc
  deriving (Eq, Ord, Show, Read)

data ParamClause = PArg [Arg]
  deriving (Eq, Ord, Show, Read)

data Arg = DArg PIdent TypeSpec
  deriving (Eq, Ord, Show, Read)

data Block = DBlock [Stm] | BlockTyped Block TypeSpec Loc
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
    | StmTyped Stm TypeSpec Loc
  deriving (Eq, Ord, Show, Read)

data Params = ParExp [Exp]
  deriving (Eq, Ord, Show, Read)

-- cambiare NotEq in NotEqual
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

data LExp
    = LRef LExp
    | LArr LExp Exp
    | LIdent PIdent
    | LExpTyped LExp TypeSpec Loc Loc
  deriving (Eq, Ord, Show, Read)

class Typed a where
  getType :: a -> TypeSpec
  getLoc :: a -> Loc
  isTypeError :: a -> Bool


instance Typed Declaration where
  --getType (DecVar (PIdent (loc, _)) typ ) = typ
  getType (DefVar (PIdent (loc, _)) typ _) = typ
  getType (DefFun (PIdent (loc, _)) _ typ _) = typ
  getType (DefFunInLine (PIdent (loc, _)) _ typ _) = typ

  --getLoc  (DecVar (PIdent (loc, _)) typ ) = loc
  getLoc  (DefVar (PIdent (loc, _)) typ _) = loc
  getLoc  (DefFun (PIdent (loc, _)) _ typ _) = loc
  getLoc  (DefFunInLine (PIdent (loc, _)) _ typ _) = loc

  isTypeError tdecl = getType tdecl == (TSimple TypeError)

-- ETyped Exp TypeSpec Integer Integer
instance Typed Exp where
    getType (ETyped _ typ _ ) = typ
    getLoc (ETyped _ _ loc) = loc
    isTypeError texp = getType texp == (TSimple TypeError)


-- BlockTyped Block TypeSpec
instance Typed Block where
    getType (BlockTyped _ typ _) = typ
    getLoc (BlockTyped _ _ loc) = loc
    isTypeError tblock = getType tblock == (TSimple TypeError)

-- StmTyped Stm TypeSpec
instance Typed Stm where
    getType (StmTyped _ typ _) = typ
    getLoc (StmTyped _ _ loc) = loc
    isTypeError tstm = getType tstm == (TSimple TypeError)


-- LExpTyped LExp TypeSpec Integer Integer
-- LIdentTyped PIdent TypeSpec Integer Integer
instance Typed LExp where
    getType (LExpTyped _ typ _ _) = typ
    --getType (LIdentTyped _ typ _) = typ
    getLoc (LExpTyped _ _ loc _) = loc
    -- Locazione di dichiarazione in questo caso.
    --getLoc (LIdentTyped _ _ loc) = loc
    isTypeError tlexp = getType tlexp == (TSimple TypeError)
