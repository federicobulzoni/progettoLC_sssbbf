module Typed where

import AbsGramm

class Typed a where
    getType :: a -> TypeSpec
    getLoc :: a -> Loc
    isTypeError :: a -> Bool
  
instance Typed Exp where
    getType (ETyped _ typ _ ) = typ
    getLoc (ETyped _ _ loc) = loc
    isTypeError texp = getType texp == (TSimple SType_Error)

instance Typed LExp where
    getType (LExpTyped _ typ _) = typ
    getLoc (LExpTyped _ _ loc) = loc
    isTypeError tlexp = getType tlexp == (TSimple SType_Error)

