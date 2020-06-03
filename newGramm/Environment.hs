-- Modulo Environment.hs
-- Il modulo offre una interfaccia semplice ed intuitiva per la gestione
-- degli environment nel TypeSystem.
-- Un environment è definito come uno stack di scope, dove ogni scope
-- mette in corrispondenza ogni identificatore di variabile/funzione precedentemente dichiarato nello scope
-- con le informazioni ad esse associate, quali la locazione di dichiarazione
-- e nel caso delle funzioni, la lista di parametri richiesti.

module Environment where

import AbsGramm
import qualified Data.Map as Map
import Errors

type LookupTable = Map.Map Ident Info

-- LookupTable, 
-- Tipo della funzione in cui è racchiuso lo scope,
-- Booleano che indica se è presente o meno un return adeguato.
type Scope = (LookupTable, TypeSpec, Bool)

data Info = 
    VarInfo Loc TypeSpec
    | FunInfo Loc TypeSpec [ParamClause]
    deriving (Show)

type Env = [Scope]

-- Funzioni riguardanti un singolo scope. --

-- Costruisce uno scope vuoto.
emptyScope  :: TypeSpec -> Scope
emptyScope ftyp = (Map.empty, ftyp, False)

-- Preso uno scope e un identificatore, cerca se tale identificatore
-- è presente e nel caso lo sia ritorna le informazioni a riguardo.
lookupIdent :: Scope -> Ident -> Maybe Info
lookupIdent (lookTable, _, _) ident = Map.lookup ident lookTable

-- Preso uno scope ed un identificatore con le proprie info associate,
-- verifica che tale identificatore non sia già presente nello scope,
-- nel caso non lo sia viene aggiunta la corrispondenza identificatore-info allo scope
-- altrimenti viene riportato un'errore dato che la variabile era già stata dichiarata
-- nello scope.
updateScope :: Scope -> Ident -> Info -> ErrEnv Scope
updateScope (lookTable, ftyp, hasReturn) ident info = case Map.lookup ident lookTable of
    Nothing -> return $ (Map.insert ident info lookTable, ftyp, hasReturn)
    Just (VarInfo dloc _) -> Failure $ EnvDuplicateIdent ident dloc True
    Just (FunInfo dloc _ _) -> Failure $ EnvDuplicateIdent ident dloc False

-- Funzioni riguardanti l'intero environment. --

-- Costruisce un environment vuoto.
emptyEnv :: Env
emptyEnv = [emptyScope (TSimple SType_Void)]

-- Aggiunge uno scope allo stack.
addScope :: Env -> TypeSpec -> Env
addScope env ftyp = (emptyScope ftyp):env

lookup :: Env -> PIdent -> ErrEnv Info
lookup [] (PIdent (_, ident)) = Failure $ EnvNotDeclaredIdent ident
lookup (scope:stack') id@(PIdent (_, ident)) = case lookupIdent scope ident of
    Just info -> return info
    Nothing -> Environment.lookup stack' id

update :: Env -> Ident -> Info -> ErrEnv Env
update [] _ _ = Failure $ InternalError
update (scope:stack) ident info = case updateScope scope ident info of
    Failure except -> Failure except
    Success scope' -> return(scope':stack)

setReturnFound :: Env -> Env
setReturnFound (scope:stack') = let (lookTable, ftyp, _) = scope in (lookTable, ftyp, True):stack'

hasReturn :: Env -> Bool
hasReturn (scope:stack') = let(_, _, hasReturn) = scope in hasReturn

getScopeType :: Env -> TypeSpec
getScopeType (scope:stack') = let(_, ftyp, _) = scope in ftyp

isGlobalScope :: Env -> Bool
isGlobalScope [scope] = True
isGlobalScope _ = False






















