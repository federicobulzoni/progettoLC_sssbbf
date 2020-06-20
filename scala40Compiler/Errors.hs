module Errors where

import AbsGramm
import Printer
import Color
import Control.Monad (MonadPlus(..), liftM)
import Control.Applicative (Applicative(..), Alternative(..))

data ErrEnv a = Success a | Failure TCException
    deriving (Show)

instance Monad ErrEnv where
  return      = Success
  Success a  >>= f = f a
  Failure s >>= _ = Failure s

instance Applicative ErrEnv where
  pure = Success
  (Failure s) <*> _ = Failure s
  (Success f) <*> o  = liftM f o

instance Functor ErrEnv where
  fmap = liftM


data LogElement 
    = Warning Loc TCException
    | Error Loc TCException

data TCException
    = MissingReturn Ident
    | MissingMain
    | WrongExpType Exp TypeSpec TypeSpec
    | WrongExpAssignType Exp TypeSpec TypeSpec LExp
    | WrongWhileCondition Exp TypeSpec
    | WrongIfCondition Exp TypeSpec
    | WrongReturnValue TypeSpec
    | VariableUsedAsFunction Ident Loc
    | VariableUsedAsProcedure Ident Loc
    | FunctionUsedAsVariable Ident Loc
    | UnusedReturnValue Ident
    | WrongProcParams Ident [[TypeSpec]] [[TypeSpec]]
    | WrongFunctionParams Ident [[TypeSpec]] [[TypeSpec]] TypeSpec
    | WrongPointerApplication LExp TypeSpec
    | ArraySubscriptNotInt Exp TypeSpec
    | WrongArrayAccess LExp TypeSpec
    | WrongArrayAccessIndex Exp
    | ArrayInconsistency
    | WrongNotApplication Exp TypeSpec
    | WrongNegApplication Exp TypeSpec
    | WrongOpApplication Op TypeSpec TypeSpec
    | UnexpectedReturn Exp
    | UnexpectedProc Ident
    | EnvDuplicateIdent Ident Loc Bool
    | EnvNotDeclaredIdent Ident
    | InternalError
    | ExpAssignedToProcedure Ident Exp TypeSpec
    | WrongMainSignature
    | WrongIfElseExp Exp Exp
    | WrongFlowCrontrolStatement Ident
    | WrongParamMethod Ident
    | WrongParameterPassMod Exp ParamPassMod
    deriving(Show,Eq)
  

printException :: LogElement -> String
printException e = case e of
  Warning loc e -> (color Yellow Bold "Warning: ") ++ printTree loc ++ ": " ++ getExceptionMsg e 
  Error loc e   -> (color Red Bold "Error: ")   ++ printTree loc ++ ": " ++ getExceptionMsg e 

launchWarning :: Loc -> TCException -> LogElement
launchWarning loc except = Warning loc except

launchError :: Loc -> TCException -> LogElement
launchError loc except = Error loc except

isError :: LogElement -> Bool
isError (Error _ _) = True
isError _ = False

getException :: LogElement -> TCException
getException (Warning _ e)= e
getException (Error _ e)= e


isNotNull :: Exp -> Bool
isNotNull (ENull _) = False
isNotNull _ = True

isNotNullType :: TypeSpec -> Bool
isNotNullType (TPointer (TSimple SType_Void)) = False
isNotNullType _ = True

getExceptionMsg :: TCException -> String
getExceptionMsg except = case except of
    MissingReturn ident -> 
        "Not every code path returns a value in function " ++ color Default Italic (printTree ident) ++ "."

    MissingMain -> 
        "Undefined " ++ color Default Italic "main" ++ " function."

    WrongExpType exp texpTyp typ -> 
        "Expected expression of type " ++ printTree typ ++ ", but found " ++ printTree exp 
        ++ if (isNotNull exp) then " which has type " ++ printTree texpTyp ++ "." else "."

    WrongExpAssignType exp texpTyp tlexpTyp lexp -> if (isNotNull exp)
        then "Incompatible types when assigning from type " ++ printTree texpTyp ++ " to type " ++ printTree tlexpTyp ++ ":" ++ "\n\t" ++ printTree (SAssign lexp exp) 
        else color Default Italic "Null" ++ " cannot be assigned to espression of type " ++ printTree tlexpTyp ++ ":" ++ "\n\t" ++ printTree (SAssign lexp exp)

    WrongWhileCondition exp typ ->
        "Expected boolean expression, but found " ++ printTree exp
        ++ (if isNotNull exp then " of type " ++ printTree typ ++ ":" else "") ++ ":"
        ++ "\n\t" ++ "while ( " ++ printTree exp ++ " )"
    
    WrongIfCondition exp typ ->
        "Expected boolean expression, but found " ++ printTree exp
        ++ (if isNotNull exp then " of type " ++ printTree typ ++ ":" else "") ++ ":"
        ++ "\n\t" ++ "if ( " ++ printTree exp ++ " )"
    
    WrongReturnValue typ -> 
        "Unexpected return with no value in function returning " 
        ++ if isNotNullType typ then printTree typ else (color Default Italic "Null") ++ ":"
        ++ "\n\t" ++ "return ;"

    UnexpectedReturn exp -> 
        "Unexpected return with a value inside a procedure:"
        ++ "\n\t" ++ "return " ++ printTree exp ++ " ;"
    
    ExpAssignedToProcedure ident exp typ ->
        (if isNotNull exp then "Expression "++ printTree exp ++ " of type " ++ printTree typ else (color Default Italic "Null")) ++ " cannot be assigned to procedure "
        ++ color Default Italic (printTree ident) ++ ". Expected procedure call or block."

    UnexpectedProc ident -> 
        "Unexpected call of procedure " ++ color Default Italic (printTree ident) ++ "."

    FunctionUsedAsVariable ident loc ->
        "Identifier " ++ color Default Italic (printTree ident) ++ " declared in " ++ printTree loc
        ++ " does not refer to a variable."

    VariableUsedAsFunction ident loc ->
        "Identifier " ++ color Default Italic (printTree ident) ++" declared in " ++ printTree loc
        ++ " does not refer to a function."

    VariableUsedAsProcedure ident loc ->
        "Identifier " ++ color Default Italic (printTree ident) ++" declared in " ++ printTree loc
        ++ " does not refer to a procedure."

    UnusedReturnValue ident -> 
        "Unused return value of function " ++ color Default Italic (printTree ident) ++ "."
    
    WrongProcParams ident args params ->
        "Mismatch in parameter list of procedure " ++ color Default Italic (printTree ident) ++ "."
        ++ " Signature is " ++ printTree args ++ " but has been given " ++ printTree params ++ "."

    WrongFunctionParams ident args params typ -> 
        "Mismatch in parameter list of function " ++ color Default Italic (printTree ident) ++ "."
        ++ " Signature is " ++ printTree args ++ " : " ++ printTree typ 
        ++ " but has been given " ++ printTree params ++ "."

    WrongPointerApplication lexp typ ->
        "Dereference operator * cannot be applied to " ++ printTree lexp ++ " which has type "++ printTree typ ++ ":"
        ++ "\n\t * ( " ++ printTree lexp ++ " )"

    ArraySubscriptNotInt exp typ -> 
        "Array subscript must be an integer, but found " 
        ++ (if isNotNull exp then printTree exp ++ " which has type " ++ printTree typ ++ "." else (color Default Italic "Null"))
        ++ "."

    
    WrongArrayAccess lexp typ ->
        "Operator [] cannot be applied to " ++ printTree lexp ++ " which has type " ++ printTree typ ++ "."

    ArrayInconsistency -> 
        "Array elements must all have the same type."

    WrongNotApplication exp typ -> 
        "Operator ! cannot be applied to type " ++ printTree typ ++ ". Expected type Bool."
        ++ "\n\t! ( " ++ printTree exp ++ " )"

    WrongNegApplication exp typ -> 
        "Unary operator - cannot be applied to type " ++ printTree typ ++ ". Expected type Int or Float."
        ++ "\n\t- ( " ++ printTree exp ++ " )"

    WrongOpApplication op typ1 typ2 -> 
        "Operator " ++ printTree op ++ " cannot be applied " 
        ++ (if isNotNullType typ1 then "to type " ++ printTree typ1 else "to " ++ (color Default Italic "Null"))
        ++ (if isNotNullType typ2 then " and type " ++ printTree typ2 else " and" ++  (color Default Italic " Null")) ++ "."

    EnvDuplicateIdent ident dloc isVar -> 
        "Duplicate declaration of " ++ color Default Italic (printTree ident) 
        ++ ". The identifier has already been declared in " ++ printTree dloc ++ "."

    EnvNotDeclaredIdent ident -> 
        "Undeclared identifier " ++ color Default Italic (printTree ident) ++ "."
    
    InternalError -> "Internal error."

    WrongMainSignature -> "Wrong main function signature. The function should have signature " ++ color Default Italic ("void main()") 

    WrongIfElseExp texp1@(ExpTyped exp1 typ1 _) texp2@(ExpTyped exp2 typ2 _) -> "Wrong conditional expression " ++ color Default Italic (printTree exp1) ++ " has type "
        ++ printTree typ1 ++ ", but expression " ++ color Default Italic (printTree exp2) ++ " has type " ++ printTree typ2 ++ "."

    WrongFlowCrontrolStatement ident ->
        "Found " ++ ident ++ " statement not inside a While or Do-While loop statement."

    WrongParameterPassMod e m ->
        "Expected l-expression as parameter having mode " ++ color Default Italic (printTree m) ++ ", but found expression " ++ color Default Italic (printTree e) ++ "."