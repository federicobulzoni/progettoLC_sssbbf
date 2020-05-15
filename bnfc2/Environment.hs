-- Env.hs
-- Modulo per la gestione di un singolo environment.
module Environment where

import ErrM
import AbsGramm
import qualified Data.Map as Map

-- Id invece è (Loc, Ident)
type Env = Map.Map Ident EnvEntry

data EnvEntry = 
	Variable Loc Type
	| Function Loc [Arg] Type 
	deriving (Show)


-- newBlock  :: Env -> Env
-- Iniziamo a definire le funzioni.
emptyEnv  :: Env
emptyEnv = Map.empty

-- Funzioni di lookup.
lookupVar :: Env -> Id -> Err Type
lookupVar env (Id (loc, ident)) = case Map.lookup ident env of
	Nothing -> Bad $ "variabile " ++ ident ++ " usata in posizione " ++
				show loc ++ ", ma non dichiarata in precedenza.\n"
	Just (Variable _ typ1) -> return typ1

lookupFun :: Env -> Id -> Err ([Arg],Type)
lookupFun env (Id (loc, ident)) = case Map.lookup ident env of
	Nothing -> Bad $ "funzione " ++ ident ++ " usata in posizione " ++
				show loc ++ ", ma non dichiarata in precedenza.\n"
	Just (Function _ args typ) -> return (args, typ)


-- Funzioni di update.
updateVar :: Env -> Id -> Type -> Err Env
updateVar env (Id (loc, ident)) typ = case Map.lookup ident env of
		Nothing -> return $ Map.insert ident (Variable loc typ) env
		Just (Variable loc1 _) -> Bad $ "identificatore" ++ ident ++
			                   "usato in precedenza per una variabile in posizione" ++
			                    show loc ++ ".\n"
		Just (Function loc1 _ _) -> Bad $ "identificatore" ++ ident ++
			                   "usato in precedenza per una funzione in posizione" ++
			                    show loc ++ ".\n"
		

updateFun :: Env -> Id -> ([Arg],Type) -> Err Env
updateFun env (Id (loc, ident)) (args, typ) = 
	case Map.lookup ident env of
		Nothing -> return $ Map.insert ident (Function loc args typ) env
		Just (Variable loc1 _) -> Bad $ "identificatore" ++ ident ++
			                   "usato in precedenza per una variabile in posizione" ++
			                    show loc ++ ".\n"
		Just (Function loc1 _ _) -> Bad $ "identificatore" ++ ident ++
			                   "usato in precedenza per una funzione in posizione" ++
			                    show loc ++ ".\n"






