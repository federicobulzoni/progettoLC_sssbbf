-- Modulo Environment.hs
-- Il modulo offre una interfaccia semplice ed intuitiva per la gestione
-- degli environment nel TypeSystem.

-- Un environment è definito come uno stack di scope, dove ogni scope
-- mette in corrispondenza ogni identificatore di variabile/funzione precedentemente dichiarato nello scope
-- con le informazioni ad esse associate, quali la locazione di dichiarazione, il tipo,
-- e nel caso delle funzioni, la lista di parametri richiesti.

-- Ogni scope ha anche due attributi personali, 
-- il primo attributo è il tipo dello Scope, che altro non è che il tipo della funzione in cui
-- è stato creato;
-- il secondo attributo è un booleano che indica se nello scope è presente o meno una istruzione return
-- con valore di ritorno compatibile con il tipo dello scope.

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

-- emptyScope
-- Costruisce uno scope vuoto assegnandoli il tipo passato come parametro.
emptyScope  :: TypeSpec -> Scope
emptyScope ftyp = (Map.empty, ftyp, False)

-- lookupIdent
-- Preso uno scope e un identificatore, cerca se tale identificatore
-- è presente nella lookuptable e nel caso lo sia ritorna le informazioni a riguardo.
lookupIdent :: Scope -> Ident -> Maybe Info
lookupIdent (lookTable, _, _) ident = Map.lookup ident lookTable

-- updateScope
-- Preso uno scope, un identificatore e le info a riguardo dell'identificatore,
-- verifica che tale identificatore non sia già presente nello scope,
-- nel caso lo sia viene lanciata una eccezione dato che si ha una doppia dichiarazione di uno
-- stesso identificatore all'interno dello scope;
-- nel caso non lo sia viene aggiunta la corrispondenza identificatore-info allo scope.
updateScope :: Scope -> Ident -> Info -> ErrEnv Scope
updateScope (lookTable, ftyp, hasReturn) ident info = case Map.lookup ident lookTable of
    Nothing -> return $ (Map.insert ident info lookTable, ftyp, hasReturn)
    Just (VarInfo dloc _) -> Failure $ EnvDuplicateIdent ident dloc True
    Just (FunInfo dloc _ _) -> Failure $ EnvDuplicateIdent ident dloc False

--------------------------------------------------------------------------------------------------------
-- Funzioni riguardanti l'intero environment. --

-- emptyEnv
-- Costruisce un environment vuoto. Il tipo dello scope "globale" è Void.
emptyEnv :: Env
emptyEnv = [emptyScope (TSimple SType_Void)]

-- addScope
-- Aggiunge uno scope di tipo ftyp allo stack.
addScope :: Env -> TypeSpec -> Env
addScope env ftyp = (emptyScope ftyp):env

-- lookup
-- preso un environment ed un identificatore verifica se tale identificatore è presente
-- nell'environment;
-- nel caso lo sia ritorna le informazioni a riguardo.
lookup :: Env -> PIdent -> ErrEnv Info
lookup [] (PIdent (_, ident)) = Failure $ EnvNotDeclaredIdent ident
lookup (scope:stack') id@(PIdent (_, ident)) = case lookupIdent scope ident of
    Just info -> return info
    Nothing -> Environment.lookup stack' id

-- update
-- Preso un environment ed un identificatore con delle informazioni ad esso associate,
-- verifica prima che non sia già presente nello scope corrente, e nel caso non lo sia 
-- inserisce la corrispondenza ident-info nello scope corrente.
update :: Env -> Ident -> Info -> ErrEnv Env
update [] _ _ = Failure $ InternalError
update (scope:stack) ident info = case updateScope scope ident info of
    Failure except -> Failure except
    Success scope' -> return(scope':stack)

-- setReturnFound
-- Preso un environment, setta il valore booleano che indica se nello scope corrente è presente
-- o meno un return compatibile con il tipo ftyp dello scope a True.
setReturnFound :: Env -> Env
setReturnFound (scope:stack') = let (lookTable, ftyp, _) = scope in (lookTable, ftyp, True):stack'

-- hasReturn
-- Preso un environment restituisce True se nello scope corrente è presente una istruzione return adeguata,
-- altrimenti restituisce False.
hasReturn :: Env -> Bool
hasReturn (scope:stack') = let(_, _, hasReturn) = scope in hasReturn

-- getScopeType
-- Triviale.
getScopeType :: Env -> TypeSpec
getScopeType (scope:stack') = let(_, ftyp, _) = scope in ftyp

-- isGlobalScope
-- Preso un environment ritorna True se lo scope corrente è quello globale,
-- ritorna False altrimenti.
isGlobalScope :: Env -> Bool
isGlobalScope [scope] = True
isGlobalScope _ = False






















