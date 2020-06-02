module ThreeAddressCode where

import AbsTAC
import AbsGramm
import Control.Monad.State.Lazy
import PrintGramm
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
    put (k, l, revcode, (instr : (head funs)) : (tail funs))


pushCurrentStream :: MyMon ()
pushCurrentStream = do
    (k, l, revcode, funs) <- get
    if length funs == 1 
        then
            put (k, l, revcode ++ (head funs), tail funs)
        else
            put (k, l,  (head funs) ++ revcode, tail funs)

pushMain :: Label -> MyMon ()
pushMain label = do
    (k, l, revcode, funs) <- get
    put (k, l, revcode ++ [Goto label], funs)

createStream :: MyMon ()
createStream = do
    (k, l, revcode, funs) <- get
    put (k,l, revcode, [] : funs)


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

-- Entry point. Called by TestGramm.
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
        out $ AssignBinOp addr addrE1 (convertOperation op typ) addrE2 (convertToTACType typ)

    ENeg e1 -> do
        addrE1 <- genExp e1
        out $ AssignUnOp addr (if (typ == TSimple SType_Int) then NegInt else NegFloat) addrE1 (convertToTACType typ)

    ENot e1 -> do
        addrE1 <- genExp e1
        out $ AssignUnOp addr Not addrE1 (convertToTACType typ)

    EFunCall id@(PIdent (_,ident)) params -> do
        genParams params
        out $ AssignFromFunction addr (buildFunLabel ident loc) (sum (map (\(ParExp x) -> length x) params)) (convertToTACType typ)

    _ -> do
        addrTExp <- genExp texp
        out $ Assign addr addrTExp (convertToTACType typ)


-- Identifier, declaration location.
buildVarAddress :: Ident -> Loc -> Addr
buildVarAddress ident dloc = Var ident dloc

buildFunLabel :: Ident -> Loc -> Label
buildFunLabel ident dloc = LabFun ident dloc

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
 

genDecl :: Declaration -> MyMon ()
genDecl decl = case decl of
    DefVar id@(PIdent (dloc, ident)) typ texp -> let addrId = buildVarAddress ident dloc in
        genExpAssign addrId texp
    DecVar id@(PIdent (dloc, ident)) typ ->
        genExpAssign (buildVarAddress ident dloc) (buildDefaultValue typ)

    DefFun id@(PIdent (dloc, ident)) _ typ block -> do
        createStream
        out $ (Lab (buildFunLabel ident dloc))
        lastIsReturn <- genBlock block
        case (lastIsReturn,typ) of
            (False, TSimple TypeVoid) -> out $ (ReturnVoid)
            (False, _ ) -> do
                addrDef <- genExp $ buildDefaultValue typ
                out $ (ReturnAddr addrDef)
            otherwise -> return ()
        pushCurrentStream
        if ident == "main"
            then do
                pushMain $ buildFunLabel ident dloc
                return ()
            else
                return ()
        

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

genLexp :: LExp -> MyMon Addr
genLexp (LExpTyped lexp typ _ dloc) = case lexp of
    LIdent (PIdent (_,ident)) -> return $ buildVarAddress ident dloc
    LRef lexp' -> do
        addrTemp <- newTemp
        addrLexp' <- genLexp lexp'
        out $ AssignFromPointer addrTemp addrLexp' (convertToTACType typ)
        return addrTemp
    LArr lexp' exp -> do
        addrOffset <- newTemp
        addrTemp <- newTemp
        addrLexp' <- genLexp lexp'
        addrExp <- genExp exp
        out $ AssignBinOp addrOffset addrExp AbsTAC.ProdInt (LitInt $ sizeOf typ) (convertToTACType (TSimple SType_Int))
        out $ AssignFromArray addrTemp addrLexp' addrOffset (convertToTACType typ)
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
    
    EDeref lexp' -> do
        addrTemp <- newTemp 
        addrLexp' <- genLexp lexp'
        out $ AssignFromRef addrTemp addrLexp' (convertToTACType typ)
        return addrTemp
    
    EArray exps -> do
        arrVals <- mapM (genExp) exps
        addrTemp <- newTemp
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


-- Returns true iff the last statement is a return. Needed for avoiding printing two consecutive returns.
genBlock :: Block -> MyMon Bool
genBlock (DBlock stms) = genStms stms

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
isReturnStm (SReturn _ ) = True
isReturnStm (SReturnExp _ _ ) = True
isReturnStm _ = False

convertToOppositeTACOp :: Op -> BinOp
convertToOppositeTACOp op = case op of
    AbsGramm.Equal     -> AbsTAC.NotEqual
    AbsGramm.NotEq     -> AbsTAC.Equal
    AbsGramm.Less      -> AbsTAC.GreaterEq
    AbsGramm.LessEq    -> AbsTAC.Greater
    AbsGramm.Greater   -> AbsTAC.LessEq
    AbsGramm.GreaterEq -> AbsTAC.Less

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

genStm :: Stm -> MyMon ()
genStm stm = case stm of
    SDecl decl -> genDecl decl
    SBlock block -> do
        genBlock block
        return ()

    --addrOffset <- newTemp
    --addrTemp <- newTemp
    --addrLexp' <- genLexp lexp'
    --addrExp <- genExp exp
    --out $ AssignBinOp addrOffset addrExp AbsTAC.ProdInt (LitInt $ sizeOf typ) (convertToTACType (TSimple SType_Int))
    --out $ AssignFromArray addrTemp addrLexp' addrOffset (convertToTACType typ)
    --return addrTemp

    SAssign (LExpTyped lexp typ loc dloc) texp -> do
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
            (LIdent id) -> do
                addrLexp <- genLexp lexp
                genExpAssign addrLexp texp

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

    where
        checkLabel label = case label of
            Fall -> return ()
            _ -> out $ Goto label