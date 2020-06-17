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

-- inserimento istruzione Call al main se presente
pushMain :: TAC -> TacState ()
pushMain instr = do
    (k, l, revcode, funs) <- get
    put (k, l, revcode ++ [instr], funs)

-- inserimento dell'etichetta in coda al codice se manca una funzione main
pushLastLabel :: Label -> TacState ()
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

-- Entry point. Genera il codice TAC del programma prog.
-- hasMain = True, se nell'analisi di semantica statica è stato trovato un main
genTAC :: Program -> Bool -> [TAC]
genTAC prog hasMain = reverse $ getTACCode $ execState (genProg prog hasMain) (0, 0 ,[], [[]])


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
        let args = concat $ map (\(PArg x) -> map (\(DArg (PIdent (loc,ident)) typ) -> (convertToTACType typ,buildVarAddress ident loc)) x) params
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
            (k, l, revcode, funs) <- get
            return $ length funs == 1 


genExp :: Addr -> Exp -> TypeSpec -> TacState ()
genExp addr texp@(ETyped exp _ _) typ = case exp of
    EOp texpl op texpr -> do
        addrExpl <- newTemp
        addrExpr <- newTemp
        genExp addrExpl texpl (getType texpl)
        genExp addrExpr texpr (getType texpr)

        if typ /= getType texp
            then do
                addrExp <- newTemp
                out $ AssignBinOp addrExp addrExpl (convertOperation op typ) addrExpr (convertToTACType (getType texp))
                out $ AssignUnOp addr (Cast $ convertToTACType typ) addrExp (convertToTACType typ)
            else
                out $ AssignBinOp addr addrExpl (convertOperation op typ) addrExpr (convertToTACType typ)
        where
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

    ENeg texp' -> do
        addrExp' <- newTemp
        genExp addrExp' texp' (getType texp')
        if typ /= getType texp
            then do
                addrExp <- newTemp
                out $ AssignUnOp addrExp (if (typ == TSimple SType_Int) then NegInt else NegFloat) addrExp' (convertToTACType (getType texp))
                out $ AssignUnOp addr (Cast $ convertToTACType typ) addrExp (convertToTACType typ)
            else
                out $ AssignUnOp addr (if (typ == TSimple SType_Int) then NegInt else NegFloat) addrExp' (convertToTACType typ)

    ENot texp' -> do
        addrExp' <- newTemp
        genExp addrExp' texp' (getType texp')
        if typ /= getType texp
            then do
                addrExp <- newTemp
                out $ AssignUnOp addrExp Not addrExp' (convertToTACType (getType texp))
                out $ AssignUnOp addr (Cast $ convertToTACType typ) addrExp (convertToTACType typ)
            else
                out $ AssignUnOp addr Not addrExp' (convertToTACType typ)

    EDeref tlexp -> do
        addrLexp <- genLexp tlexp
        -- genLexp addrLexp tlexp
        out $ AssignFromRef addr addrLexp (TACAddr)


    EFunCall id@(PIdent (dloc,ident)) params -> do
        genParams params
        if typ /= getType texp
            then do
                addrExp <- newTemp
                out $ AssignFromFunction addrExp (buildFunLabel ident dloc) (sum (map (\(ParExp x) -> length x) params)) (convertToTACType (getType texp))
                out $ AssignUnOp addr (Cast $ convertToTACType typ) addrExp (convertToTACType typ)
            else
                out $ AssignFromFunction addr (buildFunLabel ident dloc) (sum (map (\(ParExp x) -> length x) params)) (convertToTACType typ)

    ELExp tlexp -> do
        addrLexp <- genLexp tlexp
        if typ /= getType tlexp
            then
                -- genLexp addrLexp tlexp
                out $ AssignUnOp addr (Cast $ convertToTACType typ) addrLexp (convertToTACType typ)
            else
                out $ Assign addr addrLexp (convertToTACType typ)

    -- Dubbi su questo.
    EArray texps -> do
        zipWithM (\e i -> assignElem addr i e (getElementType texp) ) [0 .. ( (length texps) - 1)] texps
        return ()
        where 
            assignElem base e i typ = do 
                offset <- newTemp
                out $ AssignBinOp offset (LitInt i) AbsTAC.ProdInt (LitInt $ sizeOf typ) (convertToTACType (TSimple SType_Int))
                addrE <- newTemp
                genExp addrE e (getType e)
                if typ /= getType e
                    then do
                        castedE <- newTemp
                        out $ AssignUnOp castedE (Cast $ convertToTACType typ) addrE (convertToTACType typ)
                        out $ AssignToArray base offset castedE (convertToTACType typ)
                    else
                        out $ AssignToArray base offset addrE (convertToTACType typ)

            getElementType texp = case (getType texp) of
                (TArray typ _) -> typ
                _              -> error $ "Errore: " ++ printTree texp

    -- i tipi base (quindi delle costanti) utilizzano come indirizzo un letterale
    EInt (PInteger (loc,ident)) -> assignLiteral addr (LitInt (read ident :: Int)) typ (getType texp)
    EFloat (PFloat (loc,ident)) -> assignLiteral addr (LitFloat ( read ident :: Float )) typ (getType texp)
    EChar (PChar (loc,ident)) -> assignLiteral addr (LitChar ( read ident :: Char )) typ (getType texp)
    EString (PString (loc, ident)) -> assignLiteral addr (LitString ident) typ (getType texp)
    ETrue _ -> assignLiteral addr (LitBool True) typ (getType texp)
    EFalse _ -> assignLiteral addr (LitBool False) typ (getType texp)
    ENull _ -> assignLiteral addr (LitNull) typ (getType texp)
    
    where
        assignLiteral :: Addr -> Addr -> TypeSpec -> TypeSpec -> TacState ()
        assignLiteral addr addrLit typ typLit = do
            if typ /= typLit
                then
                    out $ AssignUnOp addr (Cast $ convertToTACType typ) addrLit (convertToTACType typ)
                else
                    out $ Assign addr addrLit (convertToTACType typ)



-- funzione ausiliaria per la generazione dell'assegnamento di un'espressione ad un
-- indirizzo utile ad evitare che vengano utilizzati temporanei superflui
-- genExpAssign :: Addr -> Exp -> TypeSpec -> TacState ()
-- genExpAssign addr texp@(ETyped exp typ' _) typ = case exp of 
--     EOp e1 op e2 -> do
--         addrE1 <- genExp e1 typ'
--         addrE2 <- genExp e2 typ'
--         if typ /= typ'
--             then do
--                 expAddr <- newTemp 
--                 out $ AssignBinOp expAddr addrE1 (convertOperation op typ) addrE2 (convertToTACType typ)
--                 out $ AssignUnOp addr (Cast $ convertToTACType typ) expAddr (convertToTACType typ)
--             else 
--                 out $ AssignBinOp addr addrE1 (convertOperation op typ) addrE2 (convertToTACType typ)

--     ENeg e1 -> do
--         addrE1 <- genExp e1 typ'
--         if typ /= typ'
--             then do
--                 expAddr <- newTemp
--                 out $ AssignUnOp expAddr (if (typ == TSimple SType_Int) then NegInt else NegFloat) addrE1 (convertToTACType typ)
--                 out $ AssignUnOp addr (Cast $ convertToTACType typ) expAddr (convertToTACType typ)
--             else
--                 out $ AssignUnOp addr (if (typ == TSimple SType_Int) then NegInt else NegFloat) addrE1 (convertToTACType typ)

--     ENot e1 -> do
--         addrE1 <- genExp e1 typ'
--         if typ /= typ'
--             then do
--                 expAddr <- newTemp
--                 out $ AssignUnOp expAddr Not addrE1 (convertToTACType typ)
--                 out $ AssignUnOp addr (Cast $ convertToTACType typ) expAddr (convertToTACType typ)
--             else
--                 out $ AssignUnOp addr Not addrE1 (convertToTACType typ)

--     -- è da gestire il casting?
--     EDeref lexp -> do
--         addrLexp <- genLexp lexp 
--         out $ AssignFromRef addr addrLexp (TACAddr)

--     EFunCall id@(PIdent (dloc,ident)) params -> do
--         genParams params
--         if typ /= typ'
--             then do
--                 expAddr <- newTemp
--                 out $ AssignFromFunction expAddr (buildFunLabel ident dloc) (sum (map (\(ParExp x) -> length x) params)) (convertToTACType typ)
--                 out $ AssignUnOp addr (Cast $ convertToTACType typ) expAddr (convertToTACType typ)
--             else
--                 out $ AssignFromFunction addr (buildFunLabel ident dloc) (sum (map (\(ParExp x) -> length x) params)) (convertToTACType typ)


--     ELExp (LExpTyped lexp' typ'' _ ) -> case lexp' of
--         (LArr lexp'' exp) -> do
--             addrOffset <- newTemp
--             addrLexp'' <- genLexp lexp''
--             addrExp <- genExp exp (TSimple SType_Int)
--             if typ /= typ'
--                 then do
--                     lexpAddr <- newTemp
--                     out $ AssignBinOp addrOffset addrExp AbsTAC.ProdInt (LitInt $ sizeOf typ) (convertToTACType (TSimple SType_Int))
--                     out $ AssignFromArray lexpAddr addrLexp'' addrOffset (convertToTACType typ)
--                     out $ AssignUnOp addr (Cast $ convertToTACType typ) lexpAddr (convertToTACType typ)
--                 else do
--                     out $ AssignBinOp addrOffset addrExp AbsTAC.ProdInt (LitInt $ sizeOf typ) (convertToTACType (TSimple SType_Int))
--                     out $ AssignFromArray addr addrLexp'' addrOffset (convertToTACType typ)
--         LRef lexp'' -> do
--             addrLexp' <- genLexp lexp''
--             if typ /= typ'
--                 then do
--                     lexpAddr <- newTemp
--                     out $ AssignFromPointer lexpAddr addrLexp' (TACAddr)
--                     out $ AssignUnOp addr (Cast $ convertToTACType typ) lexpAddr (convertToTACType typ)
--                 else
--                     out $ AssignFromPointer addr addrLexp' (TACAddr)

--         _ -> do
--             addrTExp <- genExp texp typ'
--             if typ /= typ'
--                 then 
--                     out $ AssignUnOp addr (Cast $ convertToTACType typ) addrTExp (convertToTACType typ)
--                 else
--                     out $ Assign addr addrTExp (convertToTACType typ)                
--     -- se l'espressione passata ha tipo più complicato di quelli sopra elencati allora utilizziamo genExp che
--     -- crea un temporaneo, che viene successivamente assegnato all'indirizzo addr
--     _ -> do
--         addrTExp <- genExp texp typ'
--         if typ /= typ'
--             then 
--                 out $ AssignUnOp addr (Cast $ convertToTACType typ) addrTExp (convertToTACType typ)
--             else
--                 out $ Assign addr addrTExp (convertToTACType typ)       

--     where
--         -- conversione da operazione della sintassi astratta a operazione del TAC
--         -- scopo: differenziazione operazioni tra TS e TAC e distinguere operazioni
--         -- tra interi e float
--         convertOperation op typ = case (op,typ) of
--             (Plus , (TSimple SType_Int)  )  -> PlusInt
--             (Plus , (TSimple SType_Float))  -> PlusFloat
--             (Minus, (TSimple SType_Int)  )  -> MinusInt
--             (Minus, (TSimple SType_Float))  -> MinusFloat
--             (Prod , (TSimple SType_Int)  )  -> ProdInt
--             (Prod , (TSimple SType_Float))  -> ProdFloat
--             (Div  , (TSimple SType_Int)  )  -> DivInt
--             (Div  , (TSimple SType_Float))  -> DivFloat
--             (Mod  , (TSimple SType_Int)  )  -> ModInt
--             (Mod  , (TSimple SType_Float))  -> ModFloat
--             (Pow  , (TSimple SType_Int)  )  -> PowInt
--             (Pow  , (TSimple SType_Float))  -> PowFloat
--             (AbsGramm.Or       , _)         -> AbsTAC.Or
--             (AbsGramm.And      , _)         -> AbsTAC.And
--             (AbsGramm.Equal    , _)         -> AbsTAC.Equal
--             (AbsGramm.NotEq    , _)         -> AbsTAC.NotEqual
--             (AbsGramm.Less     , _)         -> AbsTAC.Less
--             (AbsGramm.LessEq   , _)         -> AbsTAC.LessEq
--             (AbsGramm.Greater  , _)         -> AbsTAC.Greater
--             (AbsGramm.GreaterEq, _)         -> AbsTAC.GreaterEq

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
        addrOffset <- newTemp
        addrRes <- newTemp
        addrLexp' <- genLexp tlexp'
        addrExp <- newTemp
        genExp addrExp texp (TSimple SType_Int)
        out $ AssignBinOp addrOffset addrExp AbsTAC.ProdInt (LitInt $ sizeOf (getElementType tlexp')) (convertToTACType (TSimple SType_Int))
        out $ AssignFromArray addrRes addrLexp' addrOffset (convertToTACType (getElementType tlexp'))
        return addrRes

        where
            getElementType texp = case (getType texp) of
                (TArray typ _) -> typ
                _              -> error $ "Errore: " ++ printTree texp


-- genLiteral :: Addr -> TypeSpec -> TypeSpec -> TacState Addr
-- genLiteral addrLiteral typ typ' = do
--     if typ /= typ'
--         then do
--             addrRes <- newTemp
--             out $ AssignUnOp addrRes (Cast $ convertToTACType typ) addrLiteral (convertToTACType typ)
--             return addrRes
--         else
--             return addrLiteral

-- -- generazione indirizzi delle espressioni
-- genExp :: Exp -> TypeSpec -> TacState Addr
-- genExp texp@(ETyped exp typ' loc) typ = case exp of
--     -- i tipi base (quindi delle costanti) utilizzano come indirizzo un letterale
--     EInt (PInteger (loc,ident)) -> genLiteral (LitInt ( read ident :: Int )) typ typ'
--     EFloat (PFloat (loc,ident)) -> genLiteral (LitFloat ( read ident :: Float )) typ typ'
--     EChar (PChar (loc,ident)) -> genLiteral (LitChar ( read ident :: Char )) typ typ'
--     EString (PString (loc, ident)) -> genLiteral (LitString ident) typ typ'
--     ETrue _ -> genLiteral (LitBool True) typ typ'
--     EFalse _ -> genLiteral (LitBool False) typ typ'
--     ENull _ -> genLiteral (LitNull) typ typ'
    
--     EDeref lexp' -> do
--         addrRes <- newTemp 
--         addrLexp' <- genLexp lexp'
--         --if typ /= typ'
--         --    then do
--         --        addrTemp <- newTemp
--         --        out $ AssignFromRef addrTemp addrLexp' (convertToTACType typ)
--         --        out $ AssignUnOp addrRes (Cast $ convertToTACType typ) addrTemp (convertToTACType typ)
--         --        return addrRes
--         --    else do
--         out $ AssignFromRef addrRes addrLexp' (convertToTACType typ)
--         return addrRes
    
--     -- se l'array dovesse avere zero elementi allora il valore
--     -- assegnato sarebbe Null
--     EArray exps -> do
--         arrVals <- mapM (\x -> genExp x (getArrayType typ)) exps
--         addrRes <- newTemp
--         -- allocazione dello spazio nell'array per tutti gli elementi assegnati
--         zipWithM (\x i -> aux addrRes x i (getArrayType typ)) arrVals [0..((length exps)-1)]
--         return addrRes


--     ELExp lexp' -> do
--         addrLexp <- genLexp lexp'
--         if typ /= typ'
--             then do
--                 addrRes <- newTemp
--                 out $ AssignUnOp addrRes (Cast $ convertToTACType typ) addrLexp (convertToTACType typ)
--                 return addrRes
--             else
--                 return addrLexp

--     _ -> do
--         addrExp <- newTemp
--         genExpAssign addrExp texp typ'
--         if typ /= typ'
--             then do
--                 addrRes <- newTemp
--                 out $ AssignUnOp addrRes (Cast $ convertToTACType typ) addrExp (convertToTACType typ)
--                 return addrRes
--             else
--                 return addrExp
    
--     where 
--         aux base x i typ' = do 
--             offset <- newTemp
--             out $ AssignBinOp offset (LitInt i) AbsTAC.ProdInt (LitInt $ sizeOf typ') (convertToTACType (TSimple SType_Int))
--             out $ AssignToArray base offset x (convertToTACType typ')

--         getArrayType (TArray typ' _) = typ'
--         getArrayType typ = error $ "Errore: " ++ printTree typ
            

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

    SAssign tlexp@(LExpTyped lexp typ _) texp -> do
        -- Se lexp è del tipo array o puntatore, si vogliono utilizzare le istruzioni del tac x[y]=z e
        -- *x=y per evitare la creazione di un temporaneo inutile per la left expression.
        case lexp of
            (LRef lexp') -> do
                -- generazione del codice della left expression prima di quello della right expression
                addrLexp' <- genLexp lexp'
                addrExp <- newTemp
                genExp addrExp texp (getType texp)
                if typ /= getType texp
                    then do
                        castedExp <- newTemp
                        out $ AssignUnOp castedExp (Cast $ convertToTACType typ) addrExp (convertToTACType typ)
                        -- istruzione ( * addrLexp' = addrExp )
                        out $ AssignToPointer addrLexp' castedExp (convertToTACType typ)
                    else
                        out $ AssignToPointer addrLexp' addrExp (convertToTACType typ)

            (LArr lexp' texp') -> do
                addrOffset <- newTemp
                addrLexp' <- genLexp lexp'
                addrExp' <- newTemp
                genExp addrExp' texp' (TSimple SType_Int)

                out $ AssignBinOp addrOffset addrExp' AbsTAC.ProdInt (LitInt $ sizeOf typ) (convertToTACType (TSimple SType_Int))
                addrExp <- newTemp
                genExp addrExp texp (getType texp)
                if typ /= getType texp
                    then do
                        castedExp <- newTemp
                        out $ AssignUnOp castedExp (Cast $ convertToTACType typ) addrExp (convertToTACType typ)
                        out $ AssignToArray addrLexp' addrOffset castedExp (convertToTACType typ)
                    else 
                        out $ AssignToArray addrLexp' addrOffset addrExp (convertToTACType typ)

            (LIdent id@(PIdent (dloc,ident))) -> do
                addrExp <- newTemp
                genExp addrExp texp (getType texp)
                if typ /= getType texp
                    then
                        out $ AssignUnOp (buildVarAddress ident dloc) (Cast $ convertToTACType typ)  addrExp (convertToTACType typ)
                    else
                        out $ Assign (buildVarAddress ident dloc) addrExp (convertToTACType typ)

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
        out $ Call (buildFunLabel ident loc) (sum (map (\(ParExp x) -> length x) params))

    SReturn preturn -> 
        out $ ReturnVoid

    SReturnExp  preturn texp -> do
        addrExp <- newTemp
        genExp addrExp texp (getType texp)
        out $ ReturnAddr addrExp

    where 
        isBlockEmpty bl = if (SBlock (DBlock []) == bl ) then True else False


-- FUNZIONI AUSILIARIE

-- gestione delle condizioni di un'istruzione While e If
-- lo scopo è quello di evitare controlli superflui e far saltare il flusso di controllo nel punto giusto
-- es. (a && b), se a = False non valutiamo b, il controllo passerà direttamente al caso else.
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
    
    (EOp e1 rel e2) -> let typ = max (getType e1) (getType e2) in do
        tmpE1 <- newTemp
        genExp tmpE1 e1 (getType e1)
        tmpE2 <- newTemp
        genExp tmpE2 e2 (getType e2)

        -- Inizializzazione.
        addrE1 <- return tmpE1
        addrE2 <- return tmpE2
        if typ /= getType e1
            then do
                addrE1 <- newTemp
                out $ AssignUnOp addrE1 (Cast $ convertToTACType typ) tmpE1 (convertToTACType typ)
            else do
                return ()

        if typ /= getType e2
            then do
                out $ AssignUnOp addrE2 (Cast $ convertToTACType typ) tmpE2 (convertToTACType typ)
            else do 
                addrE2 <- return tmpE2
                return ()

        case (lblTrue, lblFalse) of
            (_, Fall) -> out $ (IfRel (convertToTACOp rel) addrE1 addrE2 lblTrue)
            (Fall, _) -> out $ (IfRel (convertToOppositeTACOp rel) addrE1 addrE2 lblFalse)
            (_, _) -> do
                out $ (IfRel (convertToTACOp rel) addrE1 addrE2 lblTrue)
                out $ (Goto lblFalse)

    ENot e1 -> genCondition e1 lblFalse lblTrue

    _ -> do
        addrExp <- newTemp
        genExp addrExp texp (getType texp)
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

        isTrue (ETyped (ETrue _) _ _) = True
        isTrue _ = False

        isFalse (ETyped (EFalse _) _ _) = True
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
genParams :: [Params] -> TacState ()
genParams [] = return ()
genParams (param:params) = do
    genParamAux param
    genParams params
    where
        genParamAux (ParExp []) = return ()
        genParamAux (ParExp (texp:texps)) = do
            addrExp <- newTemp
            genExp addrExp texp (getType texp)
            out $ (Param addrExp)
            genParamAux (ParExp texps)


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
    _               -> error $ "Internal error: called buildDefaultValue on " ++ show etyp

buildDefaultValue etyp@(TPointer typ) = (ETyped (ENull (PNull ((0,0),"Null"))) etyp (0,0))
buildDefaultValue etyp@(TArray typ (PInteger (_,n))) = (ETyped (EArray ( replicate (read n :: Int) (buildDefaultValue typ))) etyp (0,0))


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