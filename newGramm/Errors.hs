module Errors where

import AbsGramm
import PrintGramm
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
    | WrongExpType Exp Exp TypeSpec
    | WrongExpAssignType Exp Exp LExp LExp
    | WrongWhileCondition Exp Exp
    | WrongIfCondition Exp Exp
    | WrongReturnValue TypeSpec
    | DuplicateVariable Ident Loc
    | DuplicateFunction Ident Loc
    | MissingAssignVariable Ident
    | WrongProcParams Ident [[TypeSpec]] [[TypeSpec]]
    | WrongFunctionParams Ident [[TypeSpec]] [[TypeSpec]] TypeSpec
    | WrongPointerApplication LExp TypeSpec
    | WrongArrayIndex Exp Exp
    | WrongArrayAccess LExp TypeSpec
    | WrongArrayAccessIndex Exp
    | ArrayInconsistency
    | WrongNotApplication Exp Exp
    | WrongNegApplication Exp Exp
    | WrongOpApplication Op Exp Exp
--    | ErrEnvor String
    | UnexpectedReturn
    | UnexpectedProc Ident
--    | ErrEnvorWithLoc Loc String
    | EnvDuplicateIdent Ident Loc Bool
    | EnvNotDeclaredIdent Ident
    | InternalError
    deriving(Show)
  

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

--printError :: Loc -> String -> String
--printError loc mes = "Error(" ++ printTree loc ++ "): " ++ mes

getExceptionMsg :: TCException -> String
getExceptionMsg except = case except of
    MissingReturn ident -> "Attesa un'istruzione return all'interno della funzione " 
                                    ++ printTree ident ++ ", ma non trovata."

    WrongExpType exp texp typ -> "L'espressione " ++ printTree exp ++ " ha tipo " ++ printTree (getType texp) 
                                                    ++ ", ma il tipo atteso e' " ++ printTree typ ++ "."

    WrongExpAssignType exp texp tlexp lexp -> "L'espressione " ++ printTree exp ++" ha tipo " ++ printTree (getType texp) ++ ", ma " 
                                                                ++ printTree lexp ++ " ha tipo " 
                                                                ++ printTree (getType tlexp) ++ "."

    WrongWhileCondition exp texp -> "La condizione del while deve essere di tipo booleano, invece " ++ printTree exp 
                                                            ++ " ha tipo " ++ printTree (getType texp) ++ "."
                                
    WrongIfCondition exp texp -> "La condizione dell' if deve essere di tipo booleano, invece " ++ printTree exp 
                                                        ++ " ha tipo " ++ printTree (getType texp) ++ "."
    
    WrongReturnValue typ -> "L'operazione return non ha valore di ritorno, ma la funzione ha tipo " ++ printTree typ ++ "."

    UnexpectedReturn -> "Valore di ritorno inaspettato."
    UnexpectedProc ident -> "Chiamata alla procedura " ++ printTree ident ++ " inaspettata."


    --DuplicateVariable loc ident dloc -> "L'identificatore " ++ printTree ident ++ "e' stato utilizzato in posizione " 
    --                                        ++ printTree dloc ++ "per dichiarare una variabile."

    MissingAssignVariable ident -> "Il valore di ritorno della funzione "
                                            ++ printTree ident ++ " non e' stato assegnato a nessuna variabile."
    
    WrongProcParams ident args params -> "Nella chiamata alla funzione "
                                                    ++ printTree ident ++ " la firma della funzione e': " ++ printTree args 
                                                    ++ " mentre i parametri passati sono di tipo: " ++ printTree params ++ "."
    
    WrongFunctionParams ident args params typ -> "Nella chiamata alla funzione "
                                                    ++ printTree ident ++ " la firma della funzione e': " ++ printTree args ++ " : " ++ printTree typ
                                                    ++ " mentre i parametri passati sono di tipo: " ++ printTree params ++ "."

    WrongPointerApplication lexp typ -> "Impossibile applicare operatore * a " ++ printTree lexp ++ " che ha tipo " ++ printTree typ ++ "."

    WrongArrayIndex exp texp -> "L'indice di accesso ad un'array deve avere tipo intero, ma l'espressione " 
                                                        ++ printTree exp ++ " ha tipo " ++ printTree (getType texp) ++ "."
    
    WrongArrayAccess lexp typ ->"L'accesso tramite operatore [] puo' essere effettuato solo su elementi di tipo Array, mentre "
                                                        ++ printTree lexp ++ " ha tipo " ++ printTree typ ++ "."

    --DuplicateFunction loc ident dloc -> printError loc $ "l'identificatore " ++ printTree ident
    --                                        ++ "e' stato utilizzato in posizione " ++ printTree dloc ++ "per dichiarare una funzione."

    ArrayInconsistency -> "Inconsistenza nei valori assegnati all'array."

    WrongNotApplication exp texp -> "Operatore ! applicato all'espressione " ++ printTree exp ++ ", che ha tipo "
                                                            ++ printTree (getType texp) ++ ", ma era attesa di tipo " ++ printTree (TSimple SType_Bool) ++ "."

    WrongNegApplication exp texp -> "Operatore - applicato all'espressione " ++ printTree exp ++ ", che ha tipo "
                                                            ++ printTree (getType texp) ++ ", ma era attesa di tipo numerico."

    WrongOpApplication op texpl texpr -> "L'operatore " ++ printTree op ++ " non puo' essere applicato ad un'espressione di tipo " 
                                            ++ printTree (getType texpl)
                                            ++ " e un'espressione di tipo " ++ printTree (getType texpr)  ++ "." 

    

    EnvDuplicateIdent ident dloc isVar -> "Identificatore " ++ printTree ident 
                                           ++ " usato in precedenza per una " 
                                           ++ if isVar then "variabile" else "funzione" ++ " dichiarata in posizione "
                                           ++ printTree dloc ++ "."


    EnvNotDeclaredIdent ident -> "Identificatore " ++ printTree ident ++ " utilizzato, ma non dichiarato in precedenza."
    InternalError -> "Errore interno."
    --ErrEnvor msg -> msg

    --ErrEnvorWithLoc loc msg -> printError loc $ msg
















