module ThreeAddressCode where

import AbsTAC
import AbsGramm
import Control.Monad.State.Lazy

------------------------------------------------------------
-- TODO:
--  * aggingere statement vuoto (Stm ::= ;) per evitare brutture nell'if
------------------------------------------------------------

type MyMon a = State (
    Int,      -- temporanei
    Int,      -- label
    [TAC],    -- codice
    [[TAC]]   -- funzioni
    ) 
    a

out :: TAC -> MyMon ()
out instr = do
    (k, l, revcode, funs) <- get
    if length funs == 1 
        then put (k, l, instr : revcode, funs)
        else put (k, l, revcode, (instr : (head funs)) : (tail funs))
    
    return ()

pushCurrentStream :: MyMon ()
pushCurrentStream = do
    (k, l, revcode, funs) <- get
    put (k, l,  (head funs) ++ revcode, tail funs)
    return ()

createStream :: MyMon ()
createStream = do
    (k, l, revcode, funs) <- get
    put (k,l, revcode, [] : funs)
    return ()


newTemp :: MyMon Addr
newTemp = do
    (k, l, revcode, funs) <- get
    put (k+1, l, revcode, funs)
    return $ Temp k

newLabel :: MyMon Label
newLabel = do
    (k, l , revcode, funs) <- get
    put (k, l+1, revcode, funs)
    return $ LabStm l

getTACCode :: (Int, Int, [TAC], [[TAC]]) -> [TAC]
getTACCode (k, l, code, _) = code

genTAC :: Program -> [TAC]
genTAC prog = reverse $ getTACCode $ execState ( genProg prog ) (0, 0 ,[], [[]])

genProg :: Program -> MyMon ()
genProg (Prog decls) = do
    genDecls decls
    pushCurrentStream

genDecls :: [Declaration] -> MyMon ()
genDecls [] = return ()
genDecls (decl:decls) = do
    genDecl decl
    genDecls decls
    return ()

convertOperation :: Op -> TypeSpec -> BinOp
convertOperation Plus   (TSimple SType_Int)     = PlusInt
convertOperation Plus   (TSimple SType_Float)   = PlusFloat
convertOperation Minus  (TSimple SType_Int)     = MinusInt
convertOperation Minus  (TSimple SType_Float)   = MinusFloat
convertOperation Prod   (TSimple SType_Int)     = ProdInt
convertOperation Prod   (TSimple SType_Float)   = ProdFloat
convertOperation Div    (TSimple SType_Int)     = DivInt
convertOperation Div    (TSimple SType_Float)   = DivFloat
convertOperation Mod    (TSimple SType_Int)     = ModInt
convertOperation Mod    (TSimple SType_Float)   = ModFloat
convertOperation Pow    (TSimple SType_Int)     = PowInt
convertOperation Pow    (TSimple SType_Float)   = PowFloat
convertOperation AbsGramm.Or _        = AbsTAC.Or
convertOperation AbsGramm.And _       = AbsTAC.And
convertOperation AbsGramm.Equal _     = AbsTAC.Equal
convertOperation AbsGramm.NotEq _     = AbsTAC.NotEqual
convertOperation AbsGramm.Less _      = AbsTAC.Less
convertOperation AbsGramm.LessEq _    = AbsTAC.LessEq
convertOperation AbsGramm.Greater _   = AbsTAC.Greater
convertOperation AbsGramm.GreaterEq _ = AbsTAC.GreaterEq

genParams :: [Params] -> MyMon ()
genParams [] = return ()
genParams (param:params) = do
    genParamAux param
    genParams params

genParamAux :: Params -> MyMon ()
genParamAux (ParExp []) = return ()
genParamAux (ParExp (exp:exps)) = do
    addrExp <- genExp exp
    out $ (Param addrExp)
    genParamAux (ParExp exps)

genExpAssign :: Addr -> Exp -> MyMon ()
genExpAssign addr texp@(ETyped exp typ loc) = case exp of 
    EOp e1 op e2 -> do
        addrE1 <- genExp e1
        addrE2 <- genExp e2
        out $ (AssignBinOp (convertOperation op typ) addr addrE1 addrE2 )
        return ()
    ENeg e1 -> do
        addrE1 <- genExp e1
        out $ (AssignUnOp (if (typ == TSimple SType_Int) then NegInt else NegFloat) addr addrE1)
        return ()
    ENot e1 -> do
        addrE1 <- genExp e1
        out $ (AssignUnOp Not addr addrE1)
        return ()
    
    EFunCall id@(PIdent (_,ident)) params -> do
        genParams params
        out $ AssignFromFunction addr (getLabel ident loc) (sum (map (\(ParExp x) -> length x) params))
        return ()

    _ -> do
        addrTExp <- genExp texp
        out $ (Assign addr addrTExp)
        return ()


-- la locazione è quella di dichiarazione
getAddress :: Ident -> Loc -> Addr
getAddress ident dloc = Var ident dloc

getLabel :: Ident -> Loc -> Label
getLabel ident dloc = LabFun ident dloc

buildDefaultValue :: TypeSpec -> Exp
buildDefaultValue etyp@(TSimple typ) = case typ of
    SType_Float     -> (ETyped (EFloat (PFloat ((0,0), "0.0")))     etyp (0,0))
    SType_Int       -> (ETyped (EInt (PInteger ((0,0), "0")))       etyp (0,0))
    SType_Char      -> (ETyped (EChar (PChar ((0,0), "'\\0'")))     etyp (0,0))
    SType_String    -> (ETyped (EString (PString ((0,0), "\"\"")))  etyp (0,0))
    SType_Bool      -> (ETyped (EFalse (PFalse ((0,0), "False")))   etyp (0,0))
    _        -> error $ "Chiamata sbagliata: " ++ show etyp

buildDefaultValue etyp@(TPointer typ) = (ETyped (ENull (PNull ((0,0),"Null"))) etyp (0,0))
buildDefaultValue etyp@(TArray typ (PInteger (_,n))) = (ETyped (EArray ( replicate (read n :: Int) (buildDefaultValue typ))) etyp (0,0))
 

genDecl :: Declaration -> MyMon ()
genDecl decl = case decl of
    DefVar id@(PIdent (dloc, ident)) typ texp -> let addrId = getAddress ident dloc in
        genExpAssign addrId texp
    DecVar id@(PIdent (dloc, ident)) typ ->
        genExpAssign (getAddress ident dloc) (buildDefaultValue typ)

    DefFun id@(PIdent (dloc, ident)) _ typ block@(BlockTyped (DBlock stms) _ _) -> do
        createStream
        out $ (Lab (getLabel ident dloc))
        lastIsReturn <- genBlock block
        case (lastIsReturn,typ) of
            (False, TSimple TypeVoid) -> out $ (ReturnVoid)
            (False, _ ) -> do
                addrDef <- genExp $ buildDefaultValue typ
                out $ (ReturnAddr addrDef)
            otherwise -> return ()
        pushCurrentStream

sizeOf :: TypeSpec -> Int
sizeOf (TSimple typ)  = case typ of
    SType_Float  -> 4
    SType_Int    -> 4
    SType_Char   -> 2
    SType_String -> 8
    SType_Bool   -> 1

sizeOf (TArray typ (PInteger (_,size))) = (read size :: Int)  * (sizeOf typ)
sizeOf (TPointer typ) = 4

genLexp :: LExp -> MyMon Addr
genLexp (LExpTyped lexp _ _ dloc) = case lexp of
    LIdent (PIdent (_,ident)) -> return $ getAddress ident dloc
    LRef lexp' -> do
        addrTemp <- newTemp
        addrLexp' <- genLexp lexp'
        out $ (AssignFromPointer addrTemp addrLexp')
        return addrTemp
    LArr lexp' exp -> do
        addrTemp <- newTemp
        addrLexp' <- genLexp lexp'
        addrExp <- genExp exp
        out $ (AssignFromArray addrTemp addrLexp' addrExp)
        return addrTemp


genExp :: Exp -> MyMon Addr
genExp texp@(ETyped exp typ loc) = case exp of
    EInt (PInteger (loc,ident)) -> return $ LitInt ( read ident :: Int )
    EFloat (PFloat (loc,ident)) -> return $ LitFloat ( read ident :: Float )
    EChar (PChar (loc,ident)) -> return $ LitChar ( read ident :: Char )
    EString (PString (loc, ident)) -> return $ LitString ident
    ETrue _ -> return $ LitBool True
    EFalse _ -> return $ LitBool False
    ENull _ -> return $ LitNull
    --DummyExp -> genExp $ buildDefaultValue typ
    
    EDeref lexp' -> do
        addrTemp <- newTemp 
        addrLexp' <- genLexp lexp'
        out $ (AssignFromRef addrTemp addrLexp')
        return addrTemp
    
    EArray exps -> do
        arrVals <- mapM (genExp) exps
        addrTemp <- newTemp
        -- aggiungere la base + dimensione data dal tipo
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
            out $ (AssignBinOp AbsTAC.ProdInt temp (LitInt i) (LitInt $ sizeOf typ'))
            out $ (AssignToArray base temp x) 
            return ()
        getArrayType (TArray typ' _) = typ'

-- Returns true iff the last statement is a return. Needed for avoiding printing two consecutive returns.
genBlock :: Block -> MyMon Bool
genBlock (BlockTyped (DBlock stms) typ _) = genStms stms

-- Returns true iff the last statement is a return. Needed for avoiding printing two consecutive returns.
genStms :: [Stm] -> MyMon Bool
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

isReturnStm :: Stm -> Bool
isReturnStm (StmTyped (SReturn _ ) _ _) = True
isReturnStm (StmTyped (SReturnExp _ _ ) _ _ ) = True
isReturnStm _ = False

convertToOppositeTACOp :: Op -> BinOp
convertToOppositeTACOp AbsGramm.Equal     = AbsTAC.NotEqual
convertToOppositeTACOp AbsGramm.NotEq     = AbsTAC.Equal
convertToOppositeTACOp AbsGramm.Less      = AbsTAC.GreaterEq
convertToOppositeTACOp AbsGramm.LessEq    = AbsTAC.Greater
convertToOppositeTACOp AbsGramm.Greater   = AbsTAC.LessEq
convertToOppositeTACOp AbsGramm.GreaterEq = AbsTAC.Less

convertToTACOp :: Op -> BinOp
convertToTACOp AbsGramm.Equal     = AbsTAC.Equal
convertToTACOp AbsGramm.NotEq     = AbsTAC.NotEqual
convertToTACOp AbsGramm.Less      = AbsTAC.Less
convertToTACOp AbsGramm.LessEq    = AbsTAC.LessEq
convertToTACOp AbsGramm.Greater   = AbsTAC.Greater
convertToTACOp AbsGramm.GreaterEq = AbsTAC.GreaterEq

genStm :: Stm -> MyMon ()
genStm (StmTyped stm typ _) = case stm of
    SDecl decl -> genDecl decl
    SBlock block -> do
        genBlock block
        return ()
    SAssign lexp texp -> do
        addrLexp <- genLexp lexp
        genExpAssign addrLexp texp

    -- LABEL while
    -- ifFalse (cond) labelFalse
    -- stmts
    -- goto while
    -- LABEL labelFalse

    SWhile texp@(ETyped exp _ _) tstm -> do
        labelWhile <- newLabel
        labelFalse <- newLabel
        out $ (Lab labelWhile)
        genCondition texp Fall labelFalse
        genStm tstm
        out $ (Goto labelWhile)
        out $ (Lab labelFalse)
        return ()


    -- IfElse (conf) labelElse
    -- stmtif
    -- goto next
    -- LABEL else
    -- smtselse
    -- LABEL next
    SIfElse texp@(ETyped exp _ _) tstm_if tstm_else@(StmTyped stm_else t l) -> do
        labelNext <- newLabel
        labelElse <- (if stm_else == (SBlock (BlockTyped (DBlock []) t l)) then return labelNext else newLabel)
        genCondition texp Fall labelElse
        genStm tstm_if
        if ((SBlock (BlockTyped (DBlock []) t l)) /= stm_else )
            then do
                out $ (Goto labelNext)
                out $ (Lab labelElse)
                genStm tstm_else
            else return ()
        out $ (Lab labelNext)
        return ()

    
    SReturn preturn -> out $ (ReturnVoid)
    SReturnExp  preturn texp -> do
        addrTexp <- genExp texp
        out $ (ReturnAddr addrTexp)
        return ()

checkLabel :: Label -> MyMon ()
checkLabel label = case label of
    Fall -> return ()
    _ -> out $ Goto label

isTrue :: Exp -> Bool
isTrue (ETyped (ETrue _) _ _) = True
isTrue _ = False

isFalse :: Exp -> Bool
isFalse (ETyped (EFalse _) _ _) = True
isFalse _ = False

genCondition :: Exp -> Label -> Label -> MyMon ()
genCondition texp@(ETyped exp typ loc) lblTrue lblFalse = case exp of
    ETrue _ -> checkLabel lblTrue
    EFalse _ -> checkLabel lblFalse
    (EOp e1 AbsGramm.And e2) -> do
        case ((isFalse e1 || isFalse e2), isTrue e1, isTrue e2) of
            (True, _, _) -> checkLabel lblFalse
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