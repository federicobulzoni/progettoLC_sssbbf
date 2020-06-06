-- Modulo ThreeAddressCode.hs
-- il seguente modulo prende in input l'albero annotato prodotto durante la fase di analisi di semantica statica
-- l'albero viene percorso ed ogni istruzione viene convertita in istruzione TAC.
-- Le istruzioni TAC, come anche gli indirizzi e le etichette, sono definiti nel file AbsTAC.hs

module ThreeAddressCode where

import AbsTAC
import AbsGramm
import Control.Monad.State.Lazy
import Printer

-- definizione del nuovo tipo TacState, attraverso la monade State
type TacState a = State (
    Int,      -- temporanei
    Int,      -- label
    [TAC],    -- codice
    [[TAC]]   -- funzioni
    ) 
    a

-- funzione per l'inserimento di un'istruzione TAC in testa
-- alla lista di istruzioni della funzione in cui è utilizzata
out :: TAC -> TacState ()
out instr = do
    (k, l, revcode, funs) <- get
    -- il codice globale rimane il medesimo, si aggiunge solo l'istruzione 
    -- in testa al suo "scope" di utilizzo
    put (k, l, revcode, (instr : (head funs)) : (tail funs))



-- inserimento delle istruzioni di una funzione all'interno del codice principale
pushCurrentStream :: TacState ()
pushCurrentStream = do
    (k, l, revcode, funs) <- get
    if length funs == 1 
        then
            -- nel caso in cui fossimo nello scope globale le istruzioni
            -- vengono inserite in coda al codice per far sì che chiamando
            -- la funzione reverse in genTAC, esse compaiano sempre in testa
            put (k, l, revcode ++ (head funs), tail funs)
        else
            put (k, l,  (head funs) ++ revcode, tail funs)

-- inserimento etichetta Goto al main se presente
pushMain :: TAC -> TacState ()
pushMain label = do
    (k, l, revcode, funs) <- get
    put (k, l, revcode ++ [label], funs)

-- inserimento dell'etichetta in coda al codice se manca una funzione main
pushLastLabel :: Label -> TacState()
pushLastLabel label = do
    (k,l,revcode,funs) <- get
    put (k, l, [Lab label] ++ revcode, funs)

-- creazione di una nuova stream per quando si crea un blocco (funzione)
-- utile per definire l'ordine delle funzioni innestate
createStream :: TacState ()
createStream = do
    (k, l, revcode, funs) <- get
    put (k,l, revcode, [] : funs)


newTemp :: TacState Addr
newTemp = do
    (k, l, revcode, funs) <- get
    put (k+1, l, revcode, funs)
    return $ Temp k

newLabel :: TacState Label
newLabel = do
    (k, l , revcode, funs) <- get
    put (k, l+1, revcode, funs)
    return $ LabStm l
  

getTACCode :: (Int, Int, [TAC], [[TAC]]) -> [TAC]
getTACCode (k, l, code, _) = code

-- Entry point.
-- hasMain = True, se nell'analisi di semantica statica è stato trovato un main
genTAC :: Program -> Bool -> [TAC]
genTAC prog hasMain = reverse $ getTACCode $ execState ( genProg prog hasMain) (0, 0 ,[], [[]])


genProg :: Program -> Bool -> TacState ()
genProg (Prog decls) hasMain= do
    -- controlla la presenta di un main
    if not hasMain
        then do
            -- se non esiste si crea un'etichetta alla fine del programma con un relativo
            -- Goto dopo le dichiarazioni globali
            mainLabel <- newLabel
            pushMain $ Goto mainLabel
            pushMain $ Comment "No main found"
            genDecls decls
            pushLastLabel mainLabel
            pushCurrentStream
        else do
            genDecls decls
            pushCurrentStream

-- genera le istruzioni TAC per le dichiarazioni
genDecls :: [Declaration] -> TacState ()
genDecls [] = return ()
genDecls (decl:decls) = do
    genDecl decl
    genDecls decls

-- generazione delle singole dichiarazioni
genDecl :: Declaration -> TacState ()
genDecl decl = case decl of
    -- dichiarazione di variabile con assegnamento
    DefVar id@(PIdent (dloc, ident)) typ texp -> let addrId = buildVarAddress ident dloc in
        genExpAssign addrId texp

    -- dichiarazione di variabile: gli si assegna un valore di default
    DecVar id@(PIdent (dloc, ident)) typ ->
        genExpAssign (buildVarAddress ident dloc) (buildDefaultValue typ)

    -- definizione di funzione
    DefFun id@(PIdent (dloc, ident)) params typ block -> do
        -- creazione di un nuovo stream dove inserire le istruzioni della funzione
        createStream
        out $ (Lab (buildFunLabel ident dloc))
        -- controllo se si è in una funzione o in una procedura
        case typ of
            TSimple SType_Void -> out $ Comment "Begin procedure"
            otherwise -> out $ Comment "Begin function"
        
        -- creazione dei commenti che indicano gli argomenti richiesti dalla funzione
        let args = concat $ map (\(PArg x) -> map (\(DArg (PIdent (loc,ident)) typ) -> (convertToTACType typ,buildVarAddress ident loc)) x) params
        if (length args > 0 ) 
            then 
                out $ CommentArgs $ args
            else
                return ()
        -- controllo della presenza di un return come ultima istruzione e generazione istruzioni interne
        lastIsReturn <- genBlock block
        case (lastIsReturn,typ) of
            (False, TSimple SType_Void) -> out $ (ReturnVoid)
            (False, _ ) -> do
                addrDef <- genExp $ buildDefaultValue typ
                out $ (ReturnAddr addrDef)
            otherwise -> return ()
        case typ of
            TSimple SType_Void -> out $ Comment "End procedure"
            otherwise -> out $ Comment "End function"
        pushCurrentStream
        isGlobal <- isGlobalScope
        -- controllo per vedere se siamo in presenza del main (nello scope globale)
        if ident == "main" && isGlobal
            then do
                pushMain $ Goto (buildFunLabel ident dloc)
                return ()
            else
                return ()
    where
        isGlobalScope = do
            (k, l, revcode, funs) <- get
            return $ length funs == 1 


-- funzione ausiliaria per la generazione delle espressioni utile ad evitare
-- che vengano utilizzati temporanei superflui
genExpAssign :: Addr -> Exp -> TacState ()
genExpAssign addr texp@(ETyped exp typ _) = case exp of 
    EOp e1 op e2 -> do
        addrE1 <- genExp e1
        addrE2 <- genExp e2
        out $ AssignBinOp addr addrE1 (convertOperation op typ) addrE2 (convertToTACType typ)

    ENeg e1 -> do
        addrE1 <- genExp e1
        out $ AssignUnOp addr (if (typ == TSimple SType_Int) then NegInt else NegFloat) addrE1 (convertToTACType typ)

    ENot e1 -> do
        addrE1 <- genExp e1
        out $ AssignUnOp addr Not addrE1 (convertToTACType typ)

    EDeref lexp -> do
        addrLexp <- genLexp lexp
        out $ AssignFromRef addr addrLexp (TACAddr)

    EFunCall id@(PIdent (dloc,ident)) params -> do
        genParams params
        out $ AssignFromFunction addr (buildFunLabel ident dloc) (sum (map (\(ParExp x) -> length x) params)) (convertToTACType typ)

    ELExp lexp -> case lexp of
        (LExpTyped lexp' typ _ ) -> case lexp' of
            (LArr lexp'' exp) -> do
                addrOffset <- newTemp
                addrLexp'' <- genLexp lexp''
                addrExp <- genExp exp
                out $ AssignBinOp addrOffset addrExp AbsTAC.ProdInt (LitInt $ sizeOf typ) (convertToTACType (TSimple SType_Int))
                out $ AssignFromArray addr addrLexp'' addrOffset (convertToTACType typ)
            LRef lexp'' -> do
                addrLexp' <- genLexp lexp''
                out $ AssignFromPointer addr addrLexp' (TACAddr)
            _ -> do
                addrTExp <- genExp texp
                out $ Assign addr addrTExp (convertToTACType typ)
        LIdent (PIdent (dloc,ident)) -> out $ Assign addr (buildVarAddress ident dloc) (convertToTACType typ)
        LRef lexp' -> do
            addrLexp' <- genLexp lexp'
            out $ AssignFromPointer addr addrLexp' (TACAddr)
    
    -- se l'espressione passata ha tipo più complicato di quelli sopra elencati allora utilizziamo genExp che
    -- crea un temporaneo e lo assegna all'indirizzo addr
    _ -> do
        addrTExp <- genExp texp
        out $ Assign addr addrTExp (convertToTACType typ)


-- generazione indirizzo per le L-Expression
genLexp :: LExp -> TacState Addr
genLexp lexp = case lexp of
    (LExpTyped lexp' typ _ ) -> case lexp' of
        (LArr lexp'' exp) -> do
            addrOffset <- newTemp
            addrTemp <- newTemp
            addrLexp'' <- genLexp lexp''
            addrExp <- genExp exp
            out $ AssignBinOp addrOffset addrExp AbsTAC.ProdInt (LitInt $ sizeOf typ) (convertToTACType (TSimple SType_Int))
            out $ AssignFromArray addrTemp addrLexp'' addrOffset (convertToTACType typ)
            return addrTemp
        _ -> genLexp lexp'
    LIdent (PIdent (dloc,ident)) -> return $ buildVarAddress ident dloc
    LRef lexp' -> do
        addrTemp <- newTemp
        addrLexp' <- genLexp lexp'
        out $ AssignFromPointer addrTemp addrLexp' (TACAddr)
        return addrTemp

-- generazione indirizzi delle espressioni
genExp :: Exp -> TacState Addr
genExp texp@(ETyped exp typ loc) = case exp of
    -- i tipi base (quindi delle costanti) utilizzano come indirizzo un letterale
    EInt (PInteger (loc,ident)) -> return $ LitInt ( read ident :: Int )
    EFloat (PFloat (loc,ident)) -> return $ LitFloat ( read ident :: Float )
    EChar (PChar (loc,ident)) -> return $ LitChar ( read ident :: Char )
    EString (PString (loc, ident)) -> return $ LitString ident
    ETrue _ -> return $ LitBool True
    EFalse _ -> return $ LitBool False
    ENull _ -> return $ LitNull
    
    EDeref lexp' -> do
        addrTemp <- newTemp 
        addrLexp' <- genLexp lexp'
        out $ AssignFromRef addrTemp addrLexp' (convertToTACType typ)
        return addrTemp
    
    -- se l'avveri dovesse avere zero elementi allora il valore
    -- assegnato sarebbe Null
    EArray exps -> do
        if length exps == 0
            then
                return $ LitNull
            else do
                arrVals <- mapM (genExp) exps
                addrTemp <- newTemp
                -- allocazione dello spazio nell'array per tutti gli elementi assegnati
                zipWithM (\x i -> aux addrTemp x i (getArrayType typ)) arrVals [0..((length exps)-1)]
                return addrTemp


    ELExp lexp' -> genLexp lexp'

    _ -> do
        addrTemp <- newTemp
        genExpAssign addrTemp texp
        return addrTemp
    
    where 
        aux base x i typ' = do 
            temp <- newTemp
            out $ AssignBinOp temp (LitInt i) AbsTAC.ProdInt (LitInt $ sizeOf typ') (convertToTACType (TSimple SType_Int))
            out $ AssignToArray base temp x (convertToTACType typ')

-- ritorna true se l'ultima istruzione è un return. Per evitare ripetizioni
genBlock :: Block -> TacState Bool
genBlock (DBlock stms) = genStms stms

-- Rritorna true se l'ultima istruzione è un return. Per evitare ripetizioni.
-- genera anche le istruzioni degli statement
genStms :: [Stm] -> TacState Bool
genStms [] = return False
genStms [stm]
    | isReturnStm stm = do
        genStm stm
        return True
    | otherwise = do
        genStm stm
        return False
genStms (stm:stms) = do
    genStm stm
    genStms stms

-- gestione degli statement
genStm :: Stm -> TacState ()
genStm stm = case stm of
    SDecl decl -> genDecl decl
    SBlock block -> do
        genBlock block
        return ()

    SAssign (LExpTyped lexp typ loc) texp -> do
        case lexp of
            (LRef lexp') -> do
                addrLexp' <- genLexp lexp'
                addrExp <- genExp texp
                out $ AssignToPointer addrLexp' addrExp (convertToTACType typ)
            (LArr lexp' texp') -> do
                addrOffset <- newTemp
                addrLexp' <- genLexp lexp'
                addrExp' <- genExp texp'
                out $ AssignBinOp addrOffset addrExp' AbsTAC.ProdInt (LitInt $ sizeOf typ) (convertToTACType (TSimple SType_Int))
                addrExp <- genExp texp
                out $ AssignToArray addrLexp' addrOffset addrExp (convertToTACType typ)
            (LIdent id@(PIdent (dloc,ident))) -> 
                genExpAssign (buildVarAddress ident dloc) texp

    SWhile texp@(ETyped exp _ _) tstm -> do
        labelWhile <- newLabel
        labelFalse <- newLabel
        out $ (Lab labelWhile)
        genCondition texp Fall labelFalse
        genStm tstm
        out $ (Goto labelWhile)
        out $ (Lab labelFalse)

    SIfElse texp@(ETyped exp _ _) stm_if stm_else -> do
        labelNext <- newLabel
        labelElse <- if isBlockEmpty stm_else then return labelNext else newLabel
        genCondition texp Fall labelElse
        genStm stm_if
        if (not $ isBlockEmpty stm_else)
            then do
                out $ (Goto labelNext)
                out $ (Lab labelElse)
                genStm stm_else
            else return ()
        out $ Lab labelNext
  
    SProcCall (PIdent (loc, ident)) params -> do
        genParams params
        out $ Call (buildFunLabel ident loc) (sum (map (\(ParExp x) -> length x) params))

    SReturn preturn -> 
        out $ ReturnVoid

    SReturnExp  preturn texp -> do
        addrTexp <- genExp texp
        out $ ReturnAddr addrTexp

    where 
        isBlockEmpty bl = if (SBlock (DBlock []) == bl ) then True else False


-- FUNZIONI AUSILIARIE


-- gestione delle condizioni di un'istruzione While e If
-- lo scopo è quello di evitare controlli superflui e far saltare il flusso di controllo nel punto giusto
-- es. (a && b), se a = False non valutiamo b, il controllo passerà direttamente al caso else
genCondition :: Exp -> Label -> Label -> TacState ()
genCondition texp@(ETyped exp typ loc) lblTrue lblFalse = case exp of
    ETrue _ -> checkLabel lblTrue
    EFalse _ -> checkLabel lblFalse
    (EOp e1 AbsGramm.And e2) -> do
        case ((isFalse e1 || isFalse e2), isTrue e1, isTrue e2) of
            -- se uno dei due operandi è false allora vado direttamente all'etichetta False
            (True, _, _) -> checkLabel lblFalse
            -- altrimenti controllo se uno delle due o entrambe sono vere
            (_, True,True) -> checkLabel lblTrue
            (_, True, False) -> genCondition e2 lblTrue lblFalse
            (_, False, True) -> genCondition e1 lblTrue lblFalse
            otherwise -> do
                newLbl <- (if lblFalse == Fall then newLabel else return lblFalse)
                genCondition e1 Fall newLbl
                genCondition e2 lblTrue lblFalse
                if lblFalse == Fall 
                    then out $ (Lab newLbl)
                    else return ()
    
    (EOp e1 AbsGramm.Or e2) -> do
        case ((isTrue e1 || isTrue e2), isFalse e1, isFalse e2) of
            (True, _, _) -> checkLabel lblTrue
            (_, True,True) -> checkLabel lblFalse
            (_, True, False) -> genCondition e2 lblTrue lblFalse
            (_, False, True) -> genCondition e1 lblTrue lblFalse
            otherwise -> do
                newLbl <- (if lblTrue == Fall then newLabel else return lblTrue)
                genCondition e1 newLbl Fall
                genCondition e2 lblTrue lblFalse
                if lblTrue == Fall
                    then out $ (Lab newLbl)
                    else return ()
    
    (EOp e1 rel e2) -> do
        addrE1 <- genExp e1
        addrE2 <- genExp e2
        case (lblTrue, lblFalse) of
            (_, Fall) -> out $ (IfRel (convertToTACOp rel) addrE1 addrE2 lblTrue)
            (Fall, _) -> out $ (IfRel (convertToOppositeTACOp rel) addrE1 addrE2 lblFalse)
            (_, _) -> do
                out $ (IfRel (convertToTACOp rel) addrE1 addrE2 lblTrue)
                out $ (Goto lblFalse)

    ENot e1 -> genCondition e1 lblFalse lblTrue

    _ -> do
        addrExp <- genExp texp
        case (lblTrue, lblFalse) of
            (_,Fall) -> out $ (IfBool addrExp lblTrue)
            (Fall,_) -> out $ (IfFalse addrExp lblFalse)
            (_,_)    -> do
                out $ (IfBool addrExp lblTrue)
                out $ (Goto lblFalse)

    where
        checkLabel label = case label of
            Fall -> return ()
            _ -> out $ Goto label

-- conversione da operazione della sintassi astratta a operazione del TAC
-- scopo: differenziazione operazioni tra TS e TAC e distinguere operazioni
-- tra interi e float
convertOperation :: Op -> TypeSpec -> BinOp
convertOperation op typ = case (op,typ) of
    (Plus , (TSimple SType_Int)  )  -> PlusInt
    (Plus , (TSimple SType_Float))  -> PlusFloat
    (Minus, (TSimple SType_Int)  )  -> MinusInt
    (Minus, (TSimple SType_Float))  -> MinusFloat
    (Prod , (TSimple SType_Int)  )  -> ProdInt
    (Prod , (TSimple SType_Float))  -> ProdFloat
    (Div  , (TSimple SType_Int)  )  -> DivInt
    (Div  , (TSimple SType_Float))  -> DivFloat
    (Mod  , (TSimple SType_Int)  )  -> ModInt
    (Mod  , (TSimple SType_Float))  -> ModFloat
    (Pow  , (TSimple SType_Int)  )  -> PowInt
    (Pow  , (TSimple SType_Float))  -> PowFloat
    (AbsGramm.Or       , _)         -> AbsTAC.Or
    (AbsGramm.And      , _)         -> AbsTAC.And
    (AbsGramm.Equal    , _)         -> AbsTAC.Equal
    (AbsGramm.NotEq    , _)         -> AbsTAC.NotEqual
    (AbsGramm.Less     , _)         -> AbsTAC.Less
    (AbsGramm.LessEq   , _)         -> AbsTAC.LessEq
    (AbsGramm.Greater  , _)         -> AbsTAC.Greater
    (AbsGramm.GreaterEq, _)         -> AbsTAC.GreaterEq

-- creazione dell'istruzione Param per il passaggio dei parametri
-- in una chiamata di funzione
genParams :: [Params] -> TacState ()
genParams [] = return ()
genParams (param:params) = do
    genParamAux param
    genParams params
    where
        genParamAux (ParExp []) = return ()
        genParamAux (ParExp (exp:exps)) = do
            addrExp <- genExp exp
            out $ (Param addrExp)
            genParamAux (ParExp exps)




-- Costruzione indirizzi variabili basandosi su: identificatore, locazione --> ident@loc
buildVarAddress :: Ident -> Loc -> Addr
buildVarAddress ident dloc = Var ident dloc

-- Costruzione indirizzi funzioni
buildFunLabel :: Ident -> Loc -> Label
buildFunLabel ident dloc = LabFun ident dloc

-- dato un intero viene restituita un'espressione tipata che ha valore di default rispetto al tipo richiesto
buildDefaultValue :: TypeSpec -> Exp
buildDefaultValue etyp@(TSimple typ) = case typ of
    SType_Float     -> (ETyped (EFloat (PFloat ((0,0), "0.0")))     etyp (0,0))
    SType_Int       -> (ETyped (EInt (PInteger ((0,0), "0")))       etyp (0,0))
    SType_Char      -> (ETyped (EChar (PChar ((0,0), "'\\0'")))     etyp (0,0))
    SType_String    -> (ETyped (EString (PString ((0,0), "\"\"")))  etyp (0,0))
    SType_Bool      -> (ETyped (EFalse (PFalse ((0,0), "False")))   etyp (0,0))
    _               -> error $ "Chiamata sbagliata: " ++ show etyp

buildDefaultValue etyp@(TPointer typ) = (ETyped (ENull (PNull ((0,0),"Null"))) etyp (0,0))
buildDefaultValue etyp@(TArray typ (PInteger (_,n))) = (ETyped (EArray ( replicate (read n :: Int) (buildDefaultValue typ))) etyp (0,0))


isReturnStm :: Stm -> Bool
isReturnStm (SReturn _ ) = True
isReturnStm (SReturnExp _ _ ) = True
isReturnStm _ = False

-- preso un tipo ritorna lo spazio occupato da quel tipo
sizeOf :: TypeSpec -> Int
sizeOf (TSimple typ)  = case typ of
    SType_Float  -> 4
    SType_Int    -> 4
    SType_Char   -> 2
    SType_String -> 8
    SType_Bool   -> 1

sizeOf (TArray typ (PInteger (_,size))) = (read size :: Int)  * (sizeOf typ)
sizeOf (TPointer typ) = 4

getArrayType :: TypeSpec -> TypeSpec
getArrayType (TArray typ' _) = typ'
getArrayType typ = error $ "Errore: " ++ printTree typ


-- conversione da operatore binario a operatore binario del TAC opposto
convertToOppositeTACOp :: Op -> BinOp
convertToOppositeTACOp op = case op of
    AbsGramm.Equal     -> AbsTAC.NotEqual
    AbsGramm.NotEq     -> AbsTAC.Equal
    AbsGramm.Less      -> AbsTAC.GreaterEq
    AbsGramm.LessEq    -> AbsTAC.Greater
    AbsGramm.Greater   -> AbsTAC.LessEq
    AbsGramm.GreaterEq -> AbsTAC.Less

-- conversione da operatore binario a operatore binario del TAC
convertToTACOp :: Op -> BinOp
convertToTACOp op = case op of
    AbsGramm.Equal     -> AbsTAC.Equal
    AbsGramm.NotEq     -> AbsTAC.NotEqual
    AbsGramm.Less      -> AbsTAC.Less
    AbsGramm.LessEq    -> AbsTAC.LessEq
    AbsGramm.Greater   -> AbsTAC.Greater
    AbsGramm.GreaterEq -> AbsTAC.GreaterEq

convertToTACType :: TypeSpec -> TACType
convertToTACType typ = case typ of
    (TSimple SType_Float)  -> TACFloat
    (TSimple SType_Int)    -> TACInt
    (TSimple SType_Char)   -> TACChar 
    (TSimple SType_String) -> TACString
    (TSimple SType_Bool)   -> TACBool
    (TSimple _ )           -> error "Internal error: converting void or error to TAC type."
    _                      -> TACAddr  



isTrue :: Exp -> Bool
isTrue (ETyped (ETrue _) _ _) = True
isTrue _ = False

isFalse :: Exp -> Bool
isFalse (ETyped (EFalse _) _ _) = True
isFalse _ = False