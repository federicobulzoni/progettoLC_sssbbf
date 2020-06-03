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
    | MainDefinedInLine
    | MissingMain
    | WrongExpType Exp TypeSpec TypeSpec
    | WrongExpAssignType Exp TypeSpec TypeSpec LExp
    | WrongWhileCondition Exp TypeSpec
    | WrongIfCondition Exp TypeSpec
    | WrongReturnValue TypeSpec
    | DuplicateVariable Ident Loc
    | DuplicateFunction Ident Loc
    | MissingAssignVariable Ident
    | WrongProcParams Ident [[TypeSpec]] [[TypeSpec]]
    | WrongFunctionParams Ident [[TypeSpec]] [[TypeSpec]] TypeSpec
    | WrongPointerApplication LExp TypeSpec
    | WrongArrayIndex Exp TypeSpec
    | WrongArrayAccess LExp TypeSpec
    | WrongArrayAccessIndex Exp
    | ArrayInconsistency
    | WrongNotApplication Exp TypeSpec
    | WrongNegApplication Exp TypeSpec
    | WrongOpApplication Op TypeSpec TypeSpec
    | UnexpectedReturn
    | UnexpectedProc Ident
    | EnvDuplicateIdent Ident Loc Bool
    | EnvNotDeclaredIdent Ident
    | InternalError
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

getExceptionMsg :: TCException -> String
getExceptionMsg except = case except of
    MissingReturn ident -> "Not every code path returns a value in function " 
                                    ++ color Default Italic (printTree ident) ++ "."
    MainDefinedInLine -> "Main definito come una funzione inline."

    MissingMain -> "Main non definito."
    WrongExpType exp texpTyp typ -> "L'espressione " ++ printTree exp ++ if (isNotNull exp) 
      then " ha tipo " ++ printTree texpTyp ++ ", ma il tipo atteso e' " ++ printTree typ ++ "."
      else " non può essere applicata. Tipo richiesto: " ++  printTree typ ++ "."

    WrongExpAssignType exp texpTyp tlexpTyp lexp -> "L'espressione " ++ printTree exp ++" ha tipo " ++ printTree texpTyp ++ ", ma " 
                                                                ++ printTree lexp ++ " ha tipo " 
                                                                ++ printTree tlexpTyp ++ "."

    WrongWhileCondition exp texpTyp -> "La condizione del while deve essere di tipo booleano, invece " ++ printTree exp 
                                                            ++ " ha tipo " ++ printTree texpTyp ++ "."
                                
    WrongIfCondition exp texpTyp -> "La condizione dell' if deve essere di tipo booleano, invece " ++ printTree exp 
                                                        ++ " ha tipo " ++ printTree texpTyp ++ "."
    
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

    WrongArrayIndex exp texpTyp -> "L'indice di accesso ad un'array deve avere tipo intero, ma l'espressione " 
                                                        ++ printTree exp ++ " ha tipo " ++ printTree texpTyp ++ "."
    
    WrongArrayAccess lexp typ ->"L'accesso tramite operatore [] puo' essere effettuato solo su elementi di tipo Array, mentre "
                                                        ++ printTree lexp ++ " ha tipo " ++ printTree typ ++ "."

    --DuplicateFunction loc ident dloc -> printError loc $ "l'identificatore " ++ printTree ident
    --                                        ++ "e' stato utilizzato in posizione " ++ printTree dloc ++ "per dichiarare una funzione."

    ArrayInconsistency -> "Inconsistenza nei valori assegnati all'array."

    WrongNotApplication exp texpTyp -> "Operatore ! applicato all'espressione " ++ printTree exp ++ ", che ha tipo "
                                                            ++ printTree texpTyp ++ ", ma era attesa di tipo " ++ printTree (TSimple SType_Bool) ++ "."

    WrongNegApplication exp texpTyp -> "Operatore - applicato all'espressione " ++ printTree exp ++ ", che ha tipo "
                                                            ++ printTree texpTyp ++ ", ma era attesa di tipo numerico."

    WrongOpApplication op texplTyp texprTyp -> "L'operatore " ++ printTree op ++ " non puo' essere applicato ad un'espressione di tipo " 
                                            ++ printTree texplTyp
                                            ++ " e un'espressione di tipo " ++ printTree texprTyp  ++ "." 

    

    EnvDuplicateIdent ident dloc isVar -> "Identificatore " ++ printTree ident 
                                           ++ " usato in precedenza per una " 
                                           ++ if isVar then "variabile" else "funzione" ++ " dichiarata in posizione "
                                           ++ printTree dloc ++ "."


    EnvNotDeclaredIdent ident -> "Identificatore " ++ printTree ident ++ " utilizzato, ma non dichiarato in precedenza."
    InternalError -> "Errore interno."
    --ErrEnvor msg -> msg


