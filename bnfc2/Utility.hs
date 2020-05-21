class GetType a where
    getType :: a -> TypeSpec


-- ETyped Exp TypeSpec Integer Integer
instance GetType Exp where
    getType (EType _ typ _ _) = typ

-- BlockTyped Block TypeSpec
instance GetType Block where
    getType (BlockTyped _ typ) = typ

-- StmTyped Stm TypeSpec
instance GetType Stm where
    getType (StmTyped _ typ) = typ

-- LExpTyped LExp TypeSpec Integer Integer
-- LIdentTyped PIdent TypeSpec Integer Integer
instance GetType LExp where
    getType (LExpTyped _ typ _ _) = typ
    getType (LIdentTyped _ typ _ _) = typ


class GetLocation a where
    getType :: a -> TypeSpec


-- ETyped Exp TypeSpec Integer Integer
instance GetLocation Exp where
    getLocation (EType _ _ loc) = 

-- LExpTyped LExp TypeSpec Integer Integer
-- LIdentTyped PIdent TypeSpec Integer Integer
instance GetLocation LExp where
    getLocation (LExpTyped _ _ loc) = loc
    getLocation (LIdentTyped _ _ loc) = loc