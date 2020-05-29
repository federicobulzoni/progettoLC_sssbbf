module ThreeAddressCode where

import AbsTAC
import AbsGramm
import Control.Monad.State.Lazy

------------------------------------------------------------
-- TODO:
--  
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

-- LitString String
-- LitFloat Float
-- LitChar Char
-- LitInt Int
-- LitBool Int
-- LitNull

-- SType_Float
-- SType_Int
-- SType_Char
-- SType_String
-- SType_Bool
-- TypeError
-- TypeVoid
-- 
-- TSimple SType
-- TPointer TypeSpec
-- TArray TypeSpec PInteger 

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
    DefFun id@(PIdent (dloc, ident)) _ typ block@(BlockTyped (DBlock stms) _ _) -> do
        createStream
        out $ (Lab (getLabel ident dloc))
        genBlock block
        case typ of
            (TSimple TypeVoid) -> out $ (ReturnVoid)
            _ -> do
                addrDef <- genExp $ buildDefaultValue typ
                out $ (ReturnAddr addrDef)
        pushCurrentStream
        return ()

genLexp :: LExp -> MyMon Addr
-- LRef LExp
-- LArr LExp Exp
-- LIdent PIdent
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

-- a = Array(11,12,14)
aux :: Addr -> Addr -> Int -> MyMon ()
aux base x i = do 
    out $ (AssignToArray base (LitInt i) x) 
    return ()

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
        zipWithM (\x i -> aux addrTemp x i) arrVals [0..((length exps)-1)]
        return addrTemp

    ELExp lexp' -> genLexp lexp'

    _ -> do
        addrTemp <- newTemp
        genExpAssign addrTemp texp
        return addrTemp


genBlock :: Block -> MyMon ()
genBlock (BlockTyped (DBlock stms) typ _) = genStms stms

genStms :: [Stm] -> MyMon ()
genStms [] = return ()
genStms (stm:stms) = do
    genStm stm
    genStms stms


convertToOppositeOp :: Op -> BinOp
convertToOppositeOp AbsGramm.Equal     = AbsTAC.NotEqual
convertToOppositeOp AbsGramm.NotEq     = AbsTAC.Equal
convertToOppositeOp AbsGramm.Less      = AbsTAC.GreaterEq
convertToOppositeOp AbsGramm.LessEq    = AbsTAC.Greater
convertToOppositeOp AbsGramm.Greater   = AbsTAC.LessEq
convertToOppositeOp AbsGramm.GreaterEq = AbsTAC.Less


genStm :: Stm -> MyMon ()
genStm (StmTyped stm typ _) = case stm of
    SDecl decl -> genDecl decl
    SBlock block -> genBlock block
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
        case exp of
            EOp e1 op e2 -> do
                addrE1 <- genExp e1
                addrE2 <- genExp e2
                out $ (IfRel (convertToOppositeOp op) addrE1 addrE2 labelFalse)
            _ -> do
                addrExp <- genExp texp
                out $ (IfFalse addrExp labelFalse)
        genStm tstm
        out $ (Goto labelWhile)
        out $ (Lab labelFalse)
        return ()


    -- IfElse (conf) labelElse
    -- stmtif
    -- goto next
    -- LABEL else
    -- smtselse
    -- LABEL nect
    SIfElse texp@(ETyped exp _ _) tstm_if tstm_else -> do
        labelElse <- newLabel
        labelNext <- newLabel
        case exp of
            EOp e1 op e2 -> do
                addrE1 <- genExp e1
                addrE2 <- genExp e2
                out $ (IfRel (convertToOppositeOp op) addrE1 addrE2 labelElse)
            _ -> do
                addrExp <- genExp texp
                out $ (IfFalse addrExp labelElse)
        genStm tstm_if
        out $ (Goto labelNext)
        out $ (Lab labelElse)
        genStm tstm_else
        out $ (Lab labelNext)
        return ()

    
    SReturn preturn -> out $ (ReturnVoid)
    SReturnExp  preturn texp -> do
        addrTexp <- genExp texp
        out $ (ReturnAddr addrTexp)
        return ()