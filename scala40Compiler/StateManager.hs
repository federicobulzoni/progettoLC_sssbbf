-- definizione del nuovo tipo TacState, attraverso la monade State
module StateManager where

import Control.Monad.State.Lazy
import AbsTAC
import AbsGramm

type TacState a = State (
    Int,      -- temporanei
    Int,      -- label
    [TAC],    -- codice
    [[TAC]],   -- funzioni
    Label,     -- label continue
    Label,      -- label break
    (Label, Label),  -- label out of bounds error, label end of non void error
    TypeSpec   -- return type
    ) 
    a

setOutOfBounds :: Label -> TacState ()
setOutOfBounds label = do
    pushMain $ Call (LabFun "errorOutOfBounds" (0,0)) 0
    pushMain $ Lab label
    pushMain $ Comment "Out of bounds exception"
    (k, l, revcode, funs, found_continue, found_break, (_,n), t) <- get
    put (k, l, revcode, funs, found_continue, found_break, (label,n), t)

getOutOfBoundsLabel :: TacState Label
getOutOfBoundsLabel = do
    (_, _, _, _, _, _, (o,_), _) <- get
    return o

setEndOfNonVoid :: Label -> TacState ()
setEndOfNonVoid label = do
    pushMain $ Call (LabFun "errorEndOfNonVoidFunction" (0,0)) 0
    pushMain $ Lab label
    pushMain $ Comment "Control reached end of non-void function"
    (k, l, revcode, funs, found_continue, found_break, (e,_), t) <- get
    put (k, l, revcode, funs, found_continue, found_break, (e,label), t)

getEndOfNonVoidLabel :: TacState Label
getEndOfNonVoidLabel = do
    (_, _, _, _, _, _, (_,n), _) <- get
    return n

setFunType :: TypeSpec -> TacState ()
setFunType typ = do
    (k, l, revcode, funs, found_continue, found_break, o, t) <- get
    put (k, l, revcode, funs, found_continue, found_break, o, typ)

getFunType ::  TacState TypeSpec
getFunType = do
    (k, l, revcode, funs, found_continue, found_break, o, t) <- get
    return t

setBreak :: Label -> TacState ()
setBreak label = do
    (k, l, revcode, funs, found_continue, found_break, o, t) <- get
    put (k, l, revcode, funs, found_continue, label, o, t)

setContinue :: Label -> TacState ()
setContinue label = do
    (k, l, revcode, funs, found_continue, found_break, o, t) <- get
    put (k, l, revcode, funs, label, found_break, o, t)

getBreak :: TacState Label 
getBreak = do
    (k, l, revcode, funs, found_continue, found_break, o, _) <- get
    return found_break

getContinue :: TacState Label 
getContinue = do
    (k, l, revcode, funs, found_continue, found_break, o, _) <- get
    return found_continue

-- funzione per l'inserimento di un'istruzione TAC in testa
-- alla lista di istruzioni della funzione in cui è utilizzata
out :: TAC -> TacState ()
out instr = do
    (k, l, revcode, funs, found_continue, found_break, o, t) <- get
    -- il codice globale rimane il medesimo, si aggiunge solo l'istruzione 
    -- in testa al suo "scope" di utilizzo
    put (k, l, revcode, (instr : (head funs)) : (tail funs), found_continue, found_break, o, t)

-- inserimento delle istruzioni di una funzione all'interno del codice principale
pushCurrentStream :: TacState ()
pushCurrentStream = do
    (k, l, revcode, funs, found_continue, found_break, o, t) <- get
    if length funs == 1 
        then
            -- nel caso in cui fossimo nello scope globale le istruzioni
            -- vengono inserite in coda al codice per far sì che chiamando
            -- la funzione reverse in genTAC, esse compaiano sempre in testa
            put (k, l, revcode ++ (head funs), tail funs, found_continue, found_break, o, t)
        else
            put (k, l,  (head funs) ++ revcode, tail funs, found_continue, found_break, o, t)

-- inserimento istruzione Call al main se presente
pushMain :: TAC -> TacState ()
pushMain instr = do
    (k, l, revcode, funs, found_continue, found_break, o, t) <- get
    put (k, l, revcode ++ [instr], funs, found_continue, found_break, o, t)

-- inserimento dell'etichetta in coda al codice se manca una funzione main
pushLastLabel :: Label -> TacState ()
pushLastLabel label = do
    (k,l,revcode,funs, found_continue, found_break, o, t) <- get
    put (k, l, [Lab label] ++ revcode, funs, found_continue, found_break, o, t)

-- creazione di una nuova stream per quando si crea un blocco (funzione)
-- utile per definire l'ordine delle funzioni innestate
createStream :: TacState ()
createStream = do
    (k, l, revcode, funs, found_continue, found_break, o, t) <- get
    put (k,l, revcode, [] : funs, found_continue, found_break, o, t)


newTemp :: TacState Addr
newTemp = do
    (k, l, revcode, funs, found_continue, found_break, o, t) <- get
    put (k+1, l, revcode, funs, found_continue, found_break, o, t)
    return $ Temp k

newLabel :: TacState Label
newLabel = do
    (k, l , revcode, funs, found_continue, found_break, o, t) <- get
    put (k, l+1, revcode, funs, found_continue, found_break, o, t)
    return $ LabStm l