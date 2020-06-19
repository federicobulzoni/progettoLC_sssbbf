-- Modulo ThreeAddressCode.hs
-- il seguente modulo prende in input l'albero annotato prodotto durante la fase di analisi di semantica statica.
-- L'albero viene percorso ed ogni istruzione viene convertita in istruzioni TAC.
-- Le istruzioni TAC, come anche gli indirizzi e le etichette, sono definiti nel file AbsTAC.hs.

module ThreeAddressCode (genTAC) where

import AbsTAC
import AbsGramm
import Control.Monad.State.Lazy
import Printer
import Typed

-- definizione del nuovo tipo TacState, attraverso la monade State
type TacState a = State (
    Int,      -- temporanei
    Int,      -- label
    [TAC],    -- codice
    [[TAC]],   -- funzioni
    Label,     -- label continue
    Label      -- label break
    ) 
    a

setBreak :: Label -> TacState ()
setBreak label = do
    (k, l, revcode, funs, found_continue, found_break) <- get
    put (k, l, revcode, funs, found_continue, label)

setContinue :: Label -> TacState ()
setContinue label = do
    (k, l, revcode, funs, found_continue, found_break) <- get
    put (k, l, revcode, funs, label, found_break)

foundBreak :: TacState Label 
foundBreak = do
    (k, l, revcode, funs, found_continue, found_break) <- get
    return found_break

foundContinue :: TacState Label 
foundContinue = do
    (k, l, revcode, funs, found_continue, found_break) <- get
    return found_continue

-- funzione per l'inserimento di un'istruzione TAC in testa
-- alla lista di istruzioni della funzione in cui è utilizzata
out :: TAC -> TacState ()
out instr = do
    (k, l, revcode, funs, found_continue, found_break) <- get
    -- il codice globale rimane il medesimo, si aggiunge solo l'istruzione 
    -- in testa al suo "scope" di utilizzo
    put (k, l, revcode, (instr : (head funs)) : (tail funs), found_continue, found_break)

-- inserimento delle istruzioni di una funzione all'interno del codice principale
pushCurrentStream :: TacState ()
pushCurrentStream = do
    (k, l, revcode, funs, found_continue, found_break) <- get
    if length funs == 1 
        then
            -- nel caso in cui fossimo nello scope globale le istruzioni
            -- vengono inserite in coda al codice per far sì che chiamando
            -- la funzione reverse in genTAC, esse compaiano sempre in testa
            put (k, l, revcode ++ (head funs), tail funs, found_continue, found_break)
        else
            put (k, l,  (head funs) ++ revcode, tail funs, found_continue, found_break)

-- inserimento istruzione Call al main se presente
pushMain :: TAC -> TacState ()
pushMain instr = do
    (k, l, revcode, funs, found_continue, found_break) <- get
    put (k, l, revcode ++ [instr], funs, found_continue, found_break)

-- inserimento dell'etichetta in coda al codice se manca una funzione main
pushLastLabel :: Label -> TacState ()
pushLastLabel label = do
    (k,l,revcode,funs, found_continue, found_break) <- get
    put (k, l, [Lab label] ++ revcode, funs, found_continue, found_break)

-- creazione di una nuova stream per quando si crea un blocco (funzione)
-- utile per definire l'ordine delle funzioni innestate
createStream :: TacState ()
createStream = do
    (k, l, revcode, funs, found_continue, found_break) <- get
    put (k,l, revcode, [] : funs, found_continue, found_break)


newTemp :: TacState Addr
newTemp = do
    (k, l, revcode, funs, found_continue, found_break) <- get
    put (k+1, l, revcode, funs, found_continue, found_break)
    return $ Temp k

newLabel :: TacState Label
newLabel = do
    (k, l , revcode, funs, found_continue, found_break) <- get
    put (k, l+1, revcode, funs, found_continue, found_break)
    return $ LabStm l
  

getTACCode :: (Int, Int, [TAC], [[TAC]], Label, Label) -> [TAC]
getTACCode (k, l, code, _, _, _) = code

-- Entry point. Genera il codice TAC del programma prog.
-- hasMain = True, se nell'analisi di semantica statica è stato trovato un main
genTAC :: Program -> Bool -> [TAC]
genTAC prog hasMain = reverse $ getTACCode $ execState (genProg prog hasMain) (0, 0 ,[], [[]], (LabStm $ -1), (LabStm $ -1))


genProg :: Program -> Bool -> TacState ()
genProg (Prog decls) hasMain = do
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
    DefVar id@(PIdent (dloc, ident)) typ texp -> 
        genExp (buildVarAddress ident dloc) texp typ

    -- dichiarazione di variabile: gli si assegna un valore di default
    DecVar id@(PIdent (dloc, ident)) typ ->
        genExp (buildVarAddress ident dloc) (buildDefaultValue typ) typ

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
        let args = concat $ map (\(PParam x) -> map (\(DParam passMod (PIdent (loc,ident)) typ) -> (convertToTACType typ,buildVarAddress ident loc)) x) params 
        if (length args > 0 ) 
            then 
                out $ CommentArgs $ args
            else
                return ()
        -- controllo della presenza di un return come ultima istruzione e generazione istruzioni interne.
        -- se si è in una funzione e l'ultima istruzione non è già un return, si aggiunge un return che
        -- restituisce un valore di default
        lastIsReturn <- genBlock block
        case (lastIsReturn,typ) of
            (False, TSimple SType_Void) -> out $ (ReturnVoid)
            (False, _ ) -> do
                addrDef <- newTemp
                genExp addrDef (buildDefaultValue typ) typ
                out $ (ReturnAddr addrDef)
            otherwise -> return ()
        case typ of
            TSimple SType_Void -> out $ Comment "End procedure"
            otherwise -> out $ Comment "End function"
        -- chiusura dello stream contenente le istruzioni della funzione
        pushCurrentStream
        isGlobal <- isGlobalScope
        -- controllo per vedere se siamo in presenza del main (nello scope globale)
        if ident == "main" && isGlobal
            then do
                pushMain $ Call (buildFunLabel ident dloc) 0 
                return ()
            else
                return ()
    where
        isGlobalScope = do
            (k, l, revcode, funs, found_continue, found_break) <- get
            return $ length funs == 1 

genExpAddr :: Exp -> TypeSpec -> TacState Addr
genExpAddr texp typ =
    case (getType texp == typ, isLiteral texp, isLExp texp) of
        (True, True, False) -> genLiteral texp
        (True, False, True) -> genLexp (innerLExp texp)
        (False, True, False) -> do
            addrLit <- genLiteral texp
            addrExp <- newTemp
            assignLiteral addrExp addrLit typ (getType texp)
            return addrExp
        (False, False, True) -> do
            addrLexp <- genLexp (innerLExp texp)
            addrExp <- newTemp
            out $ AssignUnOp addrExp (Cast $ convertToTACType typ) addrLexp (convertToTACType typ)
            return addrExp
        _ -> do
            addrExp <- newTemp
            genExp addrExp texp typ
            return addrExp

genExp :: Addr -> Exp -> TypeSpec -> TacState ()
genExp addr texp@(ExpTyped exp _ _) typ = case exp of
    EOp texpl op texpr -> do
        addrExpl <- genExpAddr texpl (getType texp)
        addrExpr <- genExpAddr texpr (getType texp)
       
        case areTypesDiff of
            True -> do
                addrExp <- newTemp
                out $ AssignBinOp addrExp addrExpl (convertOperation op typ) addrExpr (convertToTACType (getType texp))
                out $ AssignUnOp addr (Cast $ convertToTACType typ) addrExp (convertToTACType typ)
            False -> out $ AssignBinOp addr addrExpl (convertOperation op typ) addrExpr (convertToTACType typ)

    ENeg texp' -> do
        addrExp' <- genExpAddr texp' (getType texp)

        case areTypesDiff of
            True -> do
                addrExp <- newTemp
                out $ AssignUnOp addrExp (if (typ == TSimple SType_Int) then NegInt else NegFloat) addrExp' (convertToTACType (getType texp))
                out $ AssignUnOp addr (Cast $ convertToTACType typ) addrExp (convertToTACType typ)
            False -> out $ AssignUnOp addr (if (typ == TSimple SType_Int) then NegInt else NegFloat) addrExp' (convertToTACType typ)

    ENot texp' -> do
        addrExp' <- genExpAddr texp' (getType texp)

        case areTypesDiff of
            True -> do
                addrExp <- newTemp
                out $ AssignUnOp addrExp Not addrExp' (convertToTACType (getType texp))
                out $ AssignUnOp addr (Cast $ convertToTACType typ) addrExp (convertToTACType typ)
            False -> out $ AssignUnOp addr Not addrExp' (convertToTACType typ)

    EDeref tlexp -> do
        addrLexp <- genLexp tlexp
        out $ AssignFromRef addr addrLexp (TACAddr)

    EFunCall id@(PIdent (dloc,ident)) params -> do
        genParams params
        case areTypesDiff of
            True -> do
                addrExp <- newTemp
                out $ AssignFromFunction addrExp (buildFunLabel ident dloc) (sum (map (\(ArgExpTyped x) -> length x) params)) (convertToTACType (getType texp))
                out $ AssignUnOp addr (Cast $ convertToTACType typ) addrExp (convertToTACType typ)
            False -> out $ AssignFromFunction addr (buildFunLabel ident dloc) (sum (map (\(ArgExpTyped x) -> length x) params)) (convertToTACType typ)

    ELExp tlexp@(LExpTyped lexp _ _) -> do
        case areTypesDiff of
            True -> do
                addrLexp <- genLexp tlexp
                out $ AssignUnOp addr (Cast $ convertToTACType typ) addrLexp (convertToTACType typ)
            False -> do
                case lexp of
                    (LRef lexp') -> do
                        -- generazione del codice della left expression prima di quello della right expression
                        addrLexp' <- genLexp lexp'
                        out $ AssignFromPointer addr addrLexp' (convertToTACType typ)

                    (LArr lexp' texp') -> do
                        addrLexp' <- genLexp lexp'
                        addrOffset <- newTemp
                        addrExp' <- genExpAddr texp' (TSimple SType_Int)
                        out $ AssignBinOp addrOffset addrExp' AbsTAC.ProdInt (LitInt $ sizeOf typ) (convertToTACType (TSimple SType_Int))
                        out $ AssignFromArray addr addrLexp' addrOffset (convertToTACType typ)

                    (LIdent id@(PIdent (dloc,ident))) -> 
                        out $ Assign addr (buildVarAddress ident dloc) (convertToTACType typ)

    EArray texps -> do
        zipWithM (\e i -> assignElem addr i e (getElementType typ) ) [0 .. ( (length texps) - 1)] texps
        return ()
        where 
            assignElem base e i typ = do 
                offset <- newTemp
                out $ AssignBinOp offset (LitInt i) AbsTAC.ProdInt (LitInt $ sizeOf typ) (convertToTACType (TSimple SType_Int))
                addrE <- genExpAddr e typ
                out $ AssignToArray base offset addrE (convertToTACType typ)

            getElementType (TArray typ' _) = typ'
            getElementType _ = error $ "Fatal error."

            --SIfElse texp@(ExpTyped exp _ _) stm_if stm_else -> do
            --    labelNext <- newLabel
            --    labelElse <- if isBlockEmpty stm_else then return labelNext else newLabel
            --    genCondition texp Fall labelElse
            --    genStm stm_if
            --    -- se si tratta di uno statement if senza else, evito di generare etichette inutili
            --    if (not $ isBlockEmpty stm_else)
            --        then do
            --            out $ (Goto labelNext)
            --            out $ (Lab labelElse)
            --            genStm stm_else
            --        else return ()
            --    out $ Lab labelNext


    EIfElse texp_cond texp_if texp_else -> do
        addr_if <- genExpAddr texp_if (getType texp)
        addr_else <- genExpAddr texp_else (getType texp)
        labelNext <- newLabel
        labelElse <- newLabel
        genCondition texp_cond Fall labelElse
        if areTypesDiff 
            then
                out $ AssignUnOp addr (Cast $ convertToTACType typ) addr_if (convertToTACType typ)
            else 
                out $ Assign addr addr_if (convertToTACType $ getType texp)
        out $ Goto labelNext
        out $ Lab labelElse
        if areTypesDiff 
            then 
                out $ AssignUnOp addr (Cast $ convertToTACType typ) addr_else (convertToTACType typ)
            else
                out $ Assign addr addr_else (convertToTACType $ getType texp)

        out $ Lab labelNext

    -- i tipi base (quindi delle costanti) utilizzano come indirizzo un letterale
    _ -> do
        addrLit <- genLiteral texp
        assignLiteral addr addrLit typ (getType texp)

    where
        areTypesDiff = typ /= getType texp
        -- conversione da operazione della sintassi astratta a operazione del TAC
        -- scopo: differenziazione operazioni tra TS e TAC e distinguere operazioni
        -- tra interi e float
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


-- generazione indirizzo per le L-Expression
genLexp :: LExp -> TacState Addr
genLexp tlexp@(LExpTyped lexp _ _) = case lexp of
    LIdent (PIdent (dloc,ident)) -> return $ buildVarAddress ident dloc
    LRef tlexp' -> do
        addrRes <- newTemp
        addrLexp' <- genLexp tlexp'
        out $ AssignFromPointer addrRes addrLexp' (TACAddr)
        return addrRes
    LArr tlexp' texp -> do
        addrLexp' <- genLexp tlexp'
        addrOffset <- newTemp
        addrExp <- genExpAddr texp (TSimple SType_Int)
        addrRes <- newTemp
        out $ AssignBinOp addrOffset addrExp AbsTAC.ProdInt (LitInt $ sizeOf (getElementType tlexp')) (convertToTACType (TSimple SType_Int))
        out $ AssignFromArray addrRes addrLexp' addrOffset (convertToTACType (getElementType tlexp'))
        return addrRes

        where
            getElementType texp = case (getType texp) of
                (TArray typ _) -> typ
                _              -> error $ "Errore: " ++ printTree texp


-- ritorna true se l'ultima istruzione è un return
genBlock :: Block -> TacState Bool
genBlock (DBlock stms) = genStms stms

-- ritorna true se l'ultima istruzione è un return
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

    -- Qua abbiamo ancora problemi.
    SAssign tlexp@(LExpTyped lexp typ _) texp -> do
        -- Se lexp è del tipo array o puntatore, si vogliono utilizzare le istruzioni del tac x[y]=z e
        -- *x=y per evitare la creazione di un temporaneo inutile per la left expression.
        case lexp of
            (LRef lexp') -> do
                -- generazione del codice della left expression prima di quello della right expression
                addrLexp' <- genLexp lexp'
                addrExp <- genExpAddr texp typ
                out $ AssignToPointer addrLexp' addrExp (convertToTACType typ)

            (LArr lexp' texp') -> do
                addrLexp' <- genLexp lexp'
                addrExp' <- genExpAddr texp' (TSimple SType_Int)
                addrOffset <- newTemp
                out $ AssignBinOp addrOffset addrExp' AbsTAC.ProdInt (LitInt $ sizeOf typ) (convertToTACType (TSimple SType_Int))
                addrExp <- genExpAddr texp typ
                out $ AssignToArray addrLexp' addrOffset addrExp (convertToTACType typ)

            (LIdent id@(PIdent (dloc,ident))) -> 
                genExp (buildVarAddress ident dloc) texp typ

    SWhile texp@(ExpTyped exp _ _) tstm -> do
        labelWhile <- newLabel
        labelFalse <- newLabel
        setBreak labelFalse
        setContinue labelWhile
        out $ (Lab labelWhile)
        genCondition texp Fall labelFalse
        genStm tstm
        out $ (Goto labelWhile)
        out $ (Lab labelFalse)

    SIfElse texp@(ExpTyped exp _ _) stm_if stm_else -> do
        labelNext <- newLabel
        labelElse <- if isBlockEmpty stm_else then return labelNext else newLabel
        genCondition texp Fall labelElse
        genStm stm_if
        -- se si tratta di uno statement if senza else, evito di generare etichette inutili
        if (not $ isBlockEmpty stm_else)
            then do
                out $ (Goto labelNext)
                out $ (Lab labelElse)
                genStm stm_else
            else return ()
        out $ Lab labelNext
  
    SProcCall (PIdent (loc, ident)) params -> do
        genParams params
        out $ Call (buildFunLabel ident loc) (sum (map (\(ArgExpTyped x) -> length x) params))

    SReturn preturn -> 
        out $ ReturnVoid

    SReturnExp  preturn texp -> do
        addrExp <- genExpAddr texp (getType texp)
        out $ ReturnAddr addrExp
    
    SBreak pbreak -> do
        labelBreak <- foundBreak
        out $ Comment "Break Goto"
        out $ Goto labelBreak

    SContinue pcontinue -> do
        labelContinue <- foundContinue
        out $ Comment "Continue Goto"
        out $ Goto labelContinue
    where 
        isBlockEmpty bl = if (SBlock (DBlock []) == bl ) then True else False


-- FUNZIONI AUSILIARIE

-- gestione delle condizioni di un'istruzione While e If
-- lo scopo è quello di evitare controlli superflui e far saltare il flusso di controllo nel punto giusto
-- es. (a && b), se a = False non valutiamo b, il controllo passerà direttamente al caso else.
genCondition :: Exp -> Label -> Label -> TacState ()
genCondition texp@(ExpTyped exp _ _) lblTrue lblFalse = case exp of
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
    
    (EOp e1 rel e2) -> let typ = max (getType e1) (getType e2) in do
        addrE1 <- genExpAddr e1 typ
        addrE2 <- genExpAddr e2 typ

        case (lblTrue, lblFalse) of
            (_, Fall) -> out $ (IfRel (convertToTACOp rel) addrE1 addrE2 lblTrue)
            (Fall, _) -> out $ (IfRel (convertToOppositeTACOp rel) addrE1 addrE2 lblFalse)
            (_, _) -> do
                out $ (IfRel (convertToTACOp rel) addrE1 addrE2 lblTrue)
                out $ (Goto lblFalse)

    ENot e1 -> genCondition e1 lblFalse lblTrue

    _ -> do
        addrExp <- genExpAddr texp (getType texp)
        
        case (lblTrue, lblFalse) of
            (_,Fall) -> out $ (IfBool addrExp lblTrue)
            (Fall,_) -> out $ (IfFalse addrExp lblFalse)
            (_,_)    -> do
                out $ (IfBool addrExp lblTrue)
                out $ (Goto lblFalse)

    where
        -- se l'etichetta è FALL, non c'è bisogno di un Goto
        checkLabel label = case label of
            Fall -> return ()
            _ -> out $ Goto label

        isTrue (ExpTyped (ETrue _) _ _) = True
        isTrue _ = False

        isFalse (ExpTyped (EFalse _) _ _) = True
        isFalse _ = False

        -- conversione da operatore binario a operatore binario del TAC
        convertToTACOp op = case op of
            AbsGramm.Equal     -> AbsTAC.Equal
            AbsGramm.NotEq     -> AbsTAC.NotEqual
            AbsGramm.Less      -> AbsTAC.Less
            AbsGramm.LessEq    -> AbsTAC.LessEq
            AbsGramm.Greater   -> AbsTAC.Greater
            AbsGramm.GreaterEq -> AbsTAC.GreaterEq

        -- conversione da operatore binario a operatore binario del TAC opposto
        convertToOppositeTACOp op = case op of
            AbsGramm.Equal     -> AbsTAC.NotEqual
            AbsGramm.NotEq     -> AbsTAC.Equal
            AbsGramm.Less      -> AbsTAC.GreaterEq
            AbsGramm.LessEq    -> AbsTAC.Greater
            AbsGramm.Greater   -> AbsTAC.LessEq
            AbsGramm.GreaterEq -> AbsTAC.Less

-- creazione delle istruzioni ( param x ) per il passaggio dei parametri
-- in una chiamata di funzione
genParams :: [Args] -> TacState ()
genParams [] = return ()
genParams (param:params) = do
    genParamAux param
    genParams params
    where
        genParamAux (ArgExpTyped []) = return ()
        genParamAux (ArgExpTyped ((texp, typ):xs)) = do
            addrExp <- genExpAddr texp typ
            out $ (Param addrExp)
            genParamAux (ArgExpTyped xs)


-- Utilities 
isLiteral :: Exp -> Bool
isLiteral texp@(ExpTyped exp _ _) = 
    case exp of
        ETrue _ -> True
        EFalse _ -> True
        EChar _ -> True
        EInt _ -> True
        EFloat _ -> True
        EString _ -> True
        ENull _ -> True
        otherwise -> False

isLExp :: Exp -> Bool
isLExp texp@(ExpTyped exp _ _) = 
    case exp of
        ELExp _ -> True
        otherwise -> False

innerLExp :: Exp -> LExp
innerLExp (ExpTyped (ELExp tlexp) _ _) = tlexp

genLiteral :: Exp  -> TacState Addr
genLiteral texp@(ExpTyped exp _ _) = 
    case exp of
        ETrue _ -> return $ LitBool True
        EFalse _ -> return $ LitBool False
        EChar (PChar (loc,ident)) -> return $ LitChar (read ident :: Char)
        EInt (PInteger (loc,ident)) -> return $ LitInt (read ident :: Int)
        EFloat (PFloat (loc,ident)) -> return $ LitFloat (read ident :: Float)
        EString (PString (loc,ident)) -> return $ LitString ident
        ENull _ -> return $ LitNull


assignLiteral :: Addr -> Addr -> TypeSpec -> TypeSpec -> TacState ()
assignLiteral addr addrLit typ typLit = do
    if typ /= typLit
        then
            out $ AssignUnOp addr (Cast $ convertToTACType typ) addrLit (convertToTACType typ)
        else
            out $ Assign addr addrLit (convertToTACType typ)

-- Costruzione indirizzi variabili basandosi su: identificatore, locazione --> ident@loc
buildVarAddress :: Ident -> Loc -> Addr
buildVarAddress ident dloc = Var ident dloc

-- Costruzione indirizzi funzioni
buildFunLabel :: Ident -> Loc -> Label
buildFunLabel ident dloc = LabFun ident dloc

-- dato un intero viene restituita un'espressione tipata che ha valore di default rispetto al tipo richiesto
buildDefaultValue :: TypeSpec -> Exp
buildDefaultValue etyp@(TSimple typ) = case typ of
    SType_Float     -> (ExpTyped (EFloat (PFloat ((0,0), "0.0")))     etyp (0,0))
    SType_Int       -> (ExpTyped (EInt (PInteger ((0,0), "0")))       etyp (0,0))
    SType_Char      -> (ExpTyped (EChar (PChar ((0,0), "'\\0'")))     etyp (0,0))
    SType_String    -> (ExpTyped (EString (PString ((0,0), "\"\"")))  etyp (0,0))
    SType_Bool      -> (ExpTyped (EFalse (PFalse ((0,0), "False")))   etyp (0,0))
    _               -> error $ "Internal error: called buildDefaultValue on " ++ show etyp

buildDefaultValue etyp@(TPointer typ) = (ExpTyped (ENull (PNull ((0,0),"Null"))) etyp (0,0))
buildDefaultValue etyp@(TArray typ (PInteger (_,n))) = (ExpTyped (EArray ( replicate (read n :: Int) (buildDefaultValue typ))) etyp (0,0))


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

convertToTACType :: TypeSpec -> TACType
convertToTACType typ = case typ of
    (TSimple SType_Float)  -> TACFloat
    (TSimple SType_Int)    -> TACInt
    (TSimple SType_Char)   -> TACChar 
    (TSimple SType_String) -> TACString
    (TSimple SType_Bool)   -> TACBool
    (TSimple _ )           -> error "Internal error: converting void or error to TAC type."
    _                      -> TACAddr  

isReturnStm :: Stm -> Bool
isReturnStm (SReturn _ ) = True
isReturnStm (SReturnExp _ _ ) = True
isReturnStm _ = False