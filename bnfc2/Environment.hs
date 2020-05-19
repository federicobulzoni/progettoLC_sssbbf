-- Env.hs
-- Modulo per la gestione di un singolo environment.
module Environment where

import ErrM
import Control.Monad
import AbsGramm
import qualified Data.Map as Map

-- PIdent invece è (Loc, Ident)
{-
type Env = Map.Map Ident EnvEntry


--       DefProc PIdent [ParamClause] Block
--       DefFun PIdent [ParamClause] TypeSpec Body
--       data ParamClause = PArg [Arg]

-- Una procedura non è altro che una funzione con tipo type_Null.
data EnvEntry = 
	Variable Loc TypeSpec
	| Function Loc TypeSpec [ParamClause]
	deriving (Show)

-}
type Ident = String
type Env = Map.Map Ident Info
type Loc = (Int, Int)

data Info = 
	VarInfo Loc TypeSpec
	| FunInfo Loc TypeSpec [ParamClause]
	deriving (Show)


type EnvStack = [Env]

-- Funzioni riguardanti un singolo environment.
emptyEnv  :: Env
emptyEnv = Map.empty

-- Funzioni di lookup.
-- Maybe TypeSpec
lookupIdent :: Env -> Ident -> Maybe Info
lookupIdent env ident = Map.lookup ident env

-- Funzioni di update.
updateEnv :: Env -> Ident -> Info -> Err Env
updateEnv env ident info = case Map.lookup ident env of
	Nothing -> return $ Map.insert ident info env
	Just (VarInfo loc1 _) -> Bad $ "identificatore" ++ ident ++
			                   "usato in precedenza per una variabile in posizione" ++
			                    show loc1 ++ ".\n"
	Just (FunInfo loc1 _ _) -> Bad $ "identificatore" ++ ident ++
			                   "usato in precedenza per una funzione in posizione" ++
			                    show loc1 ++ ".\n"
{-

updateVar :: Env -> PIdent -> TypeSpec -> Err Env
updateVar env (PIdent (loc, ident)) typ = case Map.lookup ident env of
		Nothing -> return $ Map.insert ident (Variable loc typ) env
		Just (Variable loc1 _) -> Bad $ "identificatore" ++ ident ++
			                   "usato in precedenza per una variabile in posizione" ++
			                    show loc1 ++ ".\n"
		Just (Function loc1 _ _) -> Bad $ "identificatore" ++ ident ++
			                   "usato in precedenza per una funzione in posizione" ++
			                    show loc1 ++ ".\n"
		

updateFun :: Env -> PIdent -> TypeSpec -> [ParamClause] -> Err Env
updateFun env id@(PIdent (loc, ident)) typ params = 
	case Map.lookup ident env of
		Nothing -> return $ Map.insert ident (Function loc typ params) env
		Just (Variable loc1 _) -> Bad $ "identificatore" ++ ident ++
			                   "usato in precedenza per una variabile in posizione" ++
			                    show loc ++ ".\n"
		Just (Function loc1 _ _) -> Bad $ "identificatore" ++ ident ++
			                   "usato in precedenza per una funzione in posizione" ++
			                    show loc ++ ".\n"

-}
-- Funzioni riguardanti lo stack di environment.
emptyStack :: EnvStack
emptyStack = [emptyEnv]

addScope :: EnvStack -> EnvStack
addScope envS = emptyEnv : envS

{-
lookup :: EnvStack -> Ident -> Info -> Err Info
lookup [] ident (VarInfo loc _) = Bad $ "variabile " ++ ident ++ " usata in posizione " ++ show loc ++ ", ma non dichiarata in precedenza.\n"
lookup [] ident (FunInfo loc _ _) = Bad $ "funzione " ++ ident ++ " usata in posizione " ++ show loc ++ ", ma non dichiarata in precedenza.\n"
lookup (env:stack') ident info = case lookupIdent env ident of
	Just info -> return info
	Nothing -> lookup stack' ident info
-}
lookup :: EnvStack -> PIdent -> Err Info
lookup [] (PIdent (loc, ident)) = Bad $ "identificatore " ++ ident ++ " usato in posizione " ++ show loc ++ ", ma non dichiarato in precedenza.\n"
lookup (env:stack') id@(PIdent (_, ident)) = case lookupIdent env ident of
	Just info -> return info
	Nothing -> Environment.lookup stack' id


update :: EnvStack -> Ident -> Info -> Err EnvStack
update [] _ _ = Bad $ "errore interno, non può essere che lo stack di environment sia vuoto."
update (env:stack) ident info = case updateEnv env ident info of
	Bad msg -> Bad msg
	Ok env' -> return(env':stack)

{-
lookupVarS :: EnvStack -> PIdent -> Err EnvEntry
lookupVarS [] (PIdent (loc, ident)) = Bad $ "variabile " ++ ident ++ " usata in posizione " ++ show loc ++ ", ma non dichiarata in precedenza.\n"
lookupVarS env:stack' id@(PIdent (loc,ident)) = case lookupId env ident of
	Just varInfo -> return varInfo
	Nothing -> lookupVarS stack' id

lookupFunS :: EnvStack -> PIdent -> Err EnvEntry
lookupFunS [] (PIdent (loc, ident)) = Bad $ "funzione " ++ ident ++ " usata in posizione " ++ show loc ++ ", ma non dichiarata in precedenza.\n"
lookupFunS env:stack' id@(PIdent (loc, ident)) = case lookupId env ident of
	Just varInfo -> return varInfo
	Nothing -> lookupVarS stack' id

-}
{-
updateVarS :: EnvStack -> PIdent -> TypeSpec -> Err EnvStack
updateVarS [] _ _ = Bad $ "errore interno, non può essere che lo stack di environment sia vuoto."
updateVarS (env:stack) id typ = case updateVar env id typ of
	Bad msg -> Bad msg
	Ok env' -> return (env':stack)

updateFunS :: EnvStack -> PIdent  -> TypeSpec -> [ParamClause] -> Err EnvStack
updateFunS [] _ _ _= Bad $ "errore interno, non può essere che lo stack di environment sia vuoto."
updateFunS (env:stack) id typ params = case updateFun env id typ params of
	Bad msg -> Bad msg
	Ok env' -> return (env':stack)
-}


























