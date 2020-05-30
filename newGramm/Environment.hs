-- Scope.hs
-- Modulo per la gestione di un singolo environment.
module Environment where

import ErrM
import Control.Monad
import AbsGramm
import qualified Data.Map as Map
import Errors

-- PIdent invece è (Loc, Ident)
{-
type Scope = Map.Map Ident EnvEntry


--       DefProc PIdent [ParamClause] Block
--       DefFun PIdent [ParamClause] TypeSpec Body
--       data ParamClause = PArg [Arg]

-- Una procedura non è altro che una funzione con tipo type_Null.
data EnvEntry = 
    Variable Loc TypeSpec
    | Function Loc TypeSpec [ParamClause]
    deriving (Show)

-}
--type Ident = String
type Scope = Map.Map Ident Info
--type Loc = (Int, Int)

data Info = 
    VarInfo Loc TypeSpec
    | FunInfo Loc TypeSpec [ParamClause]
    deriving (Show)


type Env = [Scope]

-- Funzioni riguardanti un singolo environment.
emptyScope  :: Scope
emptyScope = Map.empty

-- Funzioni di lookup.
-- Maybe TypeSpec
lookupIdent :: Scope -> Ident -> Maybe Info
lookupIdent scope ident = Map.lookup ident scope

-- Funzioni di update.
updateEnv :: Scope -> Ident -> Info -> Err Scope
updateEnv scope ident info = case Map.lookup ident scope of
    Nothing -> return $ Map.insert ident info scope
    Just (VarInfo loc1 _) -> Bad $ "identificatore " ++ ident ++
                               " usato in precedenza per una variabile in posizione " ++ show loc1
    Just (FunInfo loc1 _ _) -> Bad $ "identificatore " ++ ident ++
                                " usato in precedenza per una funzione in posizione " ++ show loc1
{-

updateVar :: Scope -> PIdent -> TypeSpec -> Err Scope
updateVar scope (PIdent (loc, ident)) typ = case Map.lookup ident scope of
        Nothing -> return $ Map.insert ident (Variable loc typ) scope
        Just (Variable loc1 _) -> Bad $ "identificatore" ++ ident ++
                               "usato in precedenza per una variabile in posizione" ++
                                show loc1 ++ ".\n"
        Just (Function loc1 _ _) -> Bad $ "identificatore" ++ ident ++
                               "usato in precedenza per una funzione in posizione" ++
                                show loc1 ++ ".\n"
        

updateFun :: Scope -> PIdent -> TypeSpec -> [ParamClause] -> Err Scope
updateFun scope id@(PIdent (loc, ident)) typ params = 
    case Map.lookup ident scope of
        Nothing -> return $ Map.insert ident (Function loc typ params) scope
        Just (Variable loc1 _) -> Bad $ "identificatore" ++ ident ++
                               "usato in precedenza per una variabile in posizione" ++
                                show loc ++ ".\n"
        Just (Function loc1 _ _) -> Bad $ "identificatore" ++ ident ++
                               "usato in precedenza per una funzione in posizione" ++
                                show loc ++ ".\n"

-}
-- Funzioni riguardanti lo stack di environment.
emptyEnv :: Env
emptyEnv = [emptyScope]

addScope :: Env -> Env
addScope env = emptyScope : env

{-
lookup :: Env -> Ident -> Info -> Err Info
lookup [] ident (VarInfo loc _) = Bad $ "variabile " ++ ident ++ " usata in posizione " ++ show loc ++ ", ma non dichiarata in precedenza.\n"
lookup [] ident (FunInfo loc _ _) = Bad $ "funzione " ++ ident ++ " usata in posizione " ++ show loc ++ ", ma non dichiarata in precedenza.\n"
lookup (scope:stack') ident info = case lookupIdent scope ident of
    Just info -> return info
    Nothing -> lookup stack' ident info
-}
lookup :: Env -> PIdent -> Err Info
lookup [] (PIdent (loc, ident)) = Bad $ "Error(" ++ show loc ++ "): identificatore " ++ ident ++ " usato, ma non dichiarato in precedenza.\n"
lookup (scope:stack') id@(PIdent (_, ident)) = case lookupIdent scope ident of
    Just info -> return info
    Nothing -> Environment.lookup stack' id


update :: Env -> Ident -> Info -> Err Env
update [] _ _ = Bad $ "errore interno, non può essere che lo stack di environment sia vuoto."
update (scope:stack) ident info = case updateEnv scope ident info of
    Bad msg -> Bad msg
    Ok scope' -> return(scope':stack)

{-
lookupVarS :: Env -> PIdent -> Err EnvEntry
lookupVarS [] (PIdent (loc, ident)) = Bad $ "variabile " ++ ident ++ " usata in posizione " ++ show loc ++ ", ma non dichiarata in precedenza.\n"
lookupVarS scope:stack' id@(PIdent (loc,ident)) = case lookupId scope ident of
    Just varInfo -> return varInfo
    Nothing -> lookupVarS stack' id

lookupFunS :: Env -> PIdent -> Err EnvEntry
lookupFunS [] (PIdent (loc, ident)) = Bad $ "funzione " ++ ident ++ " usata in posizione " ++ show loc ++ ", ma non dichiarata in precedenza.\n"
lookupFunS scope:stack' id@(PIdent (loc, ident)) = case lookupId scope ident of
    Just varInfo -> return varInfo
    Nothing -> lookupVarS stack' id

-}
{-
updateVarS :: Env -> PIdent -> TypeSpec -> Err Env
updateVarS [] _ _ = Bad $ "errore interno, non può essere che lo stack di environment sia vuoto."
updateVarS (scope:stack) id typ = case updateVar scope id typ of
    Bad msg -> Bad msg
    Ok scope' -> return (scope':stack)

updateFunS :: Env -> PIdent  -> TypeSpec -> [ParamClause] -> Err Env
updateFunS [] _ _ _= Bad $ "errore interno, non può essere che lo stack di environment sia vuoto."
updateFunS (scope:stack) id typ params = case updateFun scope id typ params of
    Bad msg -> Bad msg
    Ok scope' -> return (scope':stack)
-}


























