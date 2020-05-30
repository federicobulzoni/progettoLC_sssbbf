module Errors where

import AbsGramm
import PrintGramm

data TCError
    = MissingReturn Loc Ident
    | WrongExpType Loc Exp Exp TypeSpec
    | WrongExpAssignType Loc Exp Exp LExp LExp
    | WrongWhileCondition Loc Exp Exp
    | WrongIfCondition Loc Exp Exp
    | WrongReturnValue Loc TypeSpec
    | DuplicateVariable Loc Ident Loc
    | DuplicateFunction Loc Ident Loc
    | MissingAssignVariable Loc Ident
    | WrongProcParams Loc Ident [[TypeSpec]] [[TypeSpec]]
    | WrongFunctionParams Loc Ident [[TypeSpec]] [[TypeSpec]] TypeSpec
    | WrongPointerApplication Loc LExp TypeSpec
    | WrongArrayIndex Loc Exp Exp
    | WrongArrayAccess Loc LExp TypeSpec
    | WrongArrayAccessIndex Loc Exp
    | ArrayInconsistency Loc
    | WrongNotApplication Loc Exp Exp
    | WrongNegApplication Loc Exp Exp
    | WrongOpApplication Loc Op Exp Exp
    | EnvError String
    | UnexpectedReturn Loc
    | EnvErrorWithLoc Loc String

printError :: Loc -> Ident -> Ident
printError loc mes = "Error(" ++ printTree loc ++ "): " ++ mes

getError :: TCError -> Ident
getError err = case err of
    MissingReturn loc ident -> printError loc $ "attesa un'istruzione return all'interno della funzione " 
                                    ++ printTree ident ++ ", ma non trovata."

    WrongExpType loc exp texp typ -> printError loc $ "l'espressione " ++ printTree exp ++ " ha tipo " ++ printTree (getType texp) 
                                                    ++ ", ma il tipo atteso e' " ++ printTree typ ++ "."

    WrongExpAssignType loc exp texp tlexp lexp -> printError loc $ "l'espressione " ++ printTree exp ++" ha tipo " ++ printTree (getType texp) ++ ", ma " 
                                                                ++ printTree lexp ++ " ha tipo " 
                                                                ++ printTree (getType tlexp) ++ "."

    WrongWhileCondition loc exp texp -> printError loc $ "la condizione del while deve essere di tipo booleano, invece " ++ printTree exp 
                                                            ++ " ha tipo " ++ printTree (getType texp) ++ "."
                                
    WrongIfCondition loc exp texp -> printError loc $ "la condizione dell' if deve essere di tipo booleano, invece " ++ printTree exp 
                                                        ++ " ha tipo " ++ printTree (getType texp) ++ "."
    
    WrongReturnValue loc typ -> printError loc $ "l'operazione return non ha valore di ritorno, ma la funzione ha tipo " ++ printTree typ ++ "."

    UnexpectedReturn loc -> printError loc $ "valore di ritorno inaspettato."

    DuplicateVariable loc ident dloc -> printError loc $ "l'identificatore " ++ printTree ident ++ "e' stato utilizzato in posizione " 
                                            ++ printTree dloc ++ "per dichiarare una variabile."

    MissingAssignVariable loc ident -> printError loc $ "il valore di ritorno della funzione "
                                            ++ printTree ident ++ " non e' stato assegnato a nessuna variabile."
    
    WrongProcParams loc ident args params -> printError loc $ "nella chiamata alla funzione "
                                                    ++ printTree ident ++ " la firma della funzione e': " ++ printTree args 
                                                    ++ " mentre i parametri passati sono di tipo: " ++ printTree params ++ "."
    
    WrongFunctionParams loc ident args params typ -> printError loc $ "nella chiamata alla funzione "
                                                    ++ printTree ident ++ " la firma della funzione e': " ++ printTree args ++ " : " ++ printTree typ
                                                    ++ " mentre i parametri passati sono di tipo: " ++ printTree params ++ "."

    WrongPointerApplication loc lexp typ -> printError loc $ "impossibile applicare operatore * a " ++ printTree lexp ++ " che ha tipo " ++ printTree typ ++ "."

    WrongArrayIndex loc exp texp -> printError loc $ "l'indice di accesso ad un'array deve avere tipo intero, ma l'espressione " 
                                                        ++ printTree exp ++ " ha tipo " ++ printTree (getType texp) ++ "."
    
    WrongArrayAccess loc lexp typ ->printError loc $ "l'accesso tramite operatore [] puo' essere effettuato solo su elementi di tipo Array, mentre "
                                                        ++ printTree lexp ++ " ha tipo " ++ printTree typ ++ "."

    DuplicateFunction loc ident dloc -> printError loc $ "l'identificatore " ++ printTree ident
                                            ++ "e' stato utilizzato in posizione " ++ printTree dloc ++ "per dichiarare una funzione."

    ArrayInconsistency loc -> printError loc $ "inconsistenza nei valori assegnati all'array in posizione."

    WrongNotApplication loc exp texp -> printError loc $ "operatore ! applicato all'espressione " ++ printTree exp ++ ", che ha tipo "
                                                            ++ printTree (getType texp) ++ ", ma era attesa di tipo " ++ printTree (TSimple SType_Bool) ++ "."

    WrongNegApplication loc exp texp -> printError loc $ "operatore - applicato all'espressione " ++ printTree exp ++ ", che ha tipo "
                                                            ++ printTree (getType texp) ++ ", ma era attesa di tipo numerico."

    WrongOpApplication loc op texpl texpr -> printError loc $ "l'operatore " ++ printTree op ++ " non puo' essere applicato ad un'espressione di tipo " 
                                            ++ printTree (getType texpl)
                                            ++ " e un'espressione di tipo " ++ printTree (getType texpr)  ++ "." 

    EnvError msg -> msg

    EnvErrorWithLoc loc msg -> printError loc $ msg