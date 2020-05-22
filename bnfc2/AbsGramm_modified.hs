-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.
module AbsGramm where


type Loc = (Integer, Integer)
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
    = TSimple SType | TPointer TypeSpec | TArray TypeSpec Exp
  deriving (Eq, Ord, Show, Read)

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
    = DecVar PIdent TypeSpec
    | DefVar PIdent TypeSpec Exp
    | DefFun PIdent [ParamClause] TypeSpec Block
    | DefFunInLine PIdent [ParamClause] TypeSpec Exp
  deriving (Eq, Ord, Show, Read)

data ParamClause = PArg [Arg]
  deriving (Eq, Ord, Show, Read)

-- La loc serve per stampare l'errore nel caso in cui nessun return venga trovato all'interno del blocco.
data Block = DBlock [Stm] | BlockTyped Block TypeSpec Loc
  deriving (Eq, Ord, Show, Read)

data Arg = DArg PIdent TypeSpec
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
    = EArray [Exp]
    | EFunCall PIdent [Params]
    | ENot Exp
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
    | EOp Exp Op Exp
    | ETyped Exp TypeSpec Loc
  deriving (Eq, Ord, Show, Read)

data Params = ParExp [Exp]
  deriving (Eq, Ord, Show, Read)
  
data Stm
    = SDecl Declaration
    | SBlock Block
    | SAssign LExp Exp
    | SWhile Exp Stm
    | SIf Exp Stm Stm
    | SReturn PReturn
    | SReturnExp PReturn Exp
    -- Lo Stmt ha la loc per i messaggi di errore, e corrisponde alla loc dell'espressione che contiene.
    | StmTyped Stm TypeSpec Loc
  deriving (Eq, Ord, Show, Read)

data LExp
    = LRef LExp
    | LArr LExp Exp
    | LIdent PIdent
    | LExpTyped LExp TypeSpec Loc
    -- la loc è quella di dichiarazione
    | LIdentTyped PIdent TypeSpec Loc
  deriving (Eq, Ord, Show, Read)




class Typed a where
  getType :: a -> TypeSpec
  getLoc :: a -> Loc
  isTypeError :: a -> Bool


instance Typed Declaration where
  getType (DecVar (PIdent (loc, _)) typ ) = typ
  getType (DefVar (PIdent (loc, _)) typ _) = typ
  getType (DefFun (PIdent (loc, _)) _ typ _) = typ
  getType (DefFunInLine (PIdent (loc, _)) _ typ _) = typ

  getLoc  (DecVar (PIdent (loc, _)) typ ) = loc
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
    getType (LExpTyped _ typ _ ) = typ
    getType (LIdentTyped _ typ _) = typ
    getLoc (LExpTyped _ _ loc) = loc
    -- Locazione di dichiarazione in questo caso.
    getLoc (LIdentTyped _ _ loc) = loc
    isTypeError tlexp = getType tlexp == (TSimple TypeError)
