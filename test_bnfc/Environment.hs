-- Env.hs
-- Modulo per la gestione di un singolo environment.
module Environment where

import AbsGramm
import qualified Data.Map as Map

-- Id invece è (Loc, Ident)
type Env = Map Ident EnvEntry

data EnvEntry
= Variable Loc Type
| Function Loc [Arg] Type 

-- newBlock  :: Env -> Env
-- Iniziamo a definire le funzioni.
emptyEnv  :: Env
emptyEnv = Map.empty

-- Funzioni di lookup.
lookupVar :: Env -> Id -> Err Type
lookupVar env (loc, ident) = case Map.lookup ident env of
	Nothing -> fail $ "variabile " ++ ident ++ "usata in posizione" ++
				loc ++ ", ma non dichiarata in precedenza.\n"
	Just Variable _ typ1 -> typ1

lookupFun :: Env -> Id -> Err ([Arg],Type)
lookupFun env (loc, ident) = case Map.lookup ident env of
	Nothing -> fail $ "funzione " ++ ident ++ "usata in posizione" ++
				loc ++ ", ma non dichiarata in precedenza.\n"
	Just Function _ args typ -> (args, typ)


-- Funzioni di update.
updateVar :: Env -> Id -> Type -> Err Env
updateVar env (loc, ident) typ = case Map.lookup ident env of
		Nothing -> Map.insert ident (Variable loc typ)
		Just x -> case x of
			Variable loc1 _ -> fail $ "identificatore" ++ ident ++
			                   "usato in precedenza per una variabile in posizione" ++
			                    loc ++ ".\n"
            Function loc1 _ -> fail $ "identificatore" ++ ident ++
			                   "usato in precedenza per una funzione in posizione" ++
			                    loc ++ ".\n"

updateFun :: Env -> Id -> ([Arg],Type) -> Err Env
updateFun env (loc, ident) (args, typ) = 
	case Map.lookup ident env of
		Nothing -> Map.insert ident (Function loc args typ)
		Just x -> case x of
			Variable loc1 _ -> fail $ "identificatore" ++ ident ++
			                   "usato in precedenza per una variabile in posizione" ++
			                    loc ++ ".\n"
            Function loc1 _ -> fail $ "identificatore" ++ ident ++
			                   "usato in precedenza per una funzione in posizione" ++
			                    loc ++ ".\n"


