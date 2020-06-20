-- definizione del nuovo tipo TacState, attraverso la monade State
module StateManager where

import Control.Monad.State.Lazy
import AbsTAC
import AbsGramm

data FunState = FunState { funCode :: [TAC], funType :: TypeSpec }

type TacState a = State (
    Int,      -- temporanei
    Int,      -- label
    [TAC],    -- codice
    [FunState],   -- funzioni
    (Label, Label),     -- label continue, label break
    (Label, Label)  -- label out of bounds error, label end of non void error
    ) 
    a

setOutOfBounds :: Label -> TacState ()
setOutOfBounds label = do
    pushMain $ Call (LabFun "errorOutOfBounds" (0,0)) 0
    pushMain $ Lab label
    pushMain $ Comment "Out of bounds exception"
    (k, l, revcode, funs, loop, (_,n)) <- get
    put (k, l, revcode, funs, loop, (label,n))

getOutOfBoundsLabel :: TacState Label
getOutOfBoundsLabel = do
    (_, _, _, _, _,err) <- get
    return $ fst err

setEndOfNonVoid :: Label -> TacState ()
setEndOfNonVoid label = do
    pushMain $ Call (LabFun "errorEndOfNonVoidFunction" (0,0)) 0
    pushMain $ Lab label
    pushMain $ Comment "Control reached end of non-void function"
    (k, l, revcode, funs, loop, (e,_)) <- get
    put (k, l, revcode, funs, loop, (e,label))

getEndOfNonVoidLabel :: TacState Label
getEndOfNonVoidLabel = do
    (_, _, _, _, _,err) <- get
    return $ snd err

getFunType ::  TacState TypeSpec
getFunType = do
    (_, _, _, funs, _, _) <- get
    return $ funType (head funs)

setBreak :: Label -> TacState ()
setBreak label = do
    (k, l, revcode, funs, (found_continue, _), err) <- get
    put (k, l, revcode, funs, (found_continue, label), err)

setContinue :: Label -> TacState ()
setContinue label = do
    (k, l, revcode, funs, (_, found_break), err) <- get
    put (k, l, revcode, funs, (label, found_break), err)

getBreak :: TacState Label 
getBreak = do
    (_, _, _, _, loop, _) <- get
    return $ snd loop

getContinue :: TacState Label 
getContinue = do
    (_, _, _, _, loop, _) <- get
    return $ fst loop

-- funzione per l'inserimento di un'istruzione TAC in testa
-- alla lista di istruzioni della funzione in cui è utilizzata
out :: TAC -> TacState ()
out instr = do
    (k, l, revcode, funs, loop, err) <- get
    -- il codice globale rimane il medesimo, si aggiunge solo l'istruzione 
    -- in testa al suo "scope" di utilizzo
    put (k, l, revcode, (FunState (instr : funCode (head funs)) (funType (head funs))) : (tail funs), loop, err)


-- inserimento delle istruzioni di una funzione all'interno del codice principale
pushCurrentStream :: TacState ()
pushCurrentStream = do
    (k, l, revcode, funs, loop, err) <- get
    isGlobal <- isGlobalScope
    if isGlobal 
        then
            -- nel caso in cui fossimo nello scope globale le istruzioni
            -- vengono inserite in coda al codice per far sì che chiamando
            -- la funzione reverse in genTAC, esse compaiano sempre in testa
            put (k, l, revcode ++ funCode (head funs), tail funs, loop, err)
        else
            put (k, l,  funCode (head funs) ++ revcode, tail funs, loop, err)

isGlobalScope :: TacState Bool
isGlobalScope = do
    (_, _, _, funs, _, _) <- get
    return $ length funs == 1 

-- inserimento istruzione Call al main se presente
pushMain :: TAC -> TacState ()
pushMain instr = do
    (k, l, revcode, funs, loop, err) <- get
    put (k, l, revcode ++ [instr], funs, loop, err)

-- inserimento dell'etichetta in coda al codice se manca una funzione main
pushLastLabel :: Label -> TacState ()
pushLastLabel label = do
    (k, l, revcode, funs, loop, err) <- get
    put (k, l, [Lab label] ++ revcode, funs, loop, err)

-- creazione di una nuova stream per quando si crea un blocco (funzione)
-- utile per definire l'ordine delle funzioni innestate
createStream :: TypeSpec -> TacState ()
createStream typ = do
    (k, l, revcode, funs, loop, err) <- get
    put (k,l, revcode, (FunState [] typ) : funs, loop, err)


newTemp :: TacState Addr
newTemp = do
    (k, l, revcode, funs, loop, err) <- get
    put (k+1, l, revcode, funs, loop, err)
    return $ Temp k

newLabel :: TacState Label
newLabel = do
    (k, l , revcode, funs, loop, err) <- get
    put (k, l+1, revcode, funs, loop, err)
    return $ LabStm l