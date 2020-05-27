module ThreeAddressCode where

import AbsTAC
import AbsGramm
import Control.Monad.State.Lazy

-- Copiato da "Usare una monade con stream del codice nello stato"
-- Va modificato per il problema della dichiarazione di funzioni all'interno di altre funzioni.
type MyMon a = State (Int, [TAC]) a

out :: TAC -> MyMon ()
out instr = do
    (k, revcode ) <- get
    put (k, instr : revcode )
    return ()

newTemp :: MyMon Addr
newTemp = do
    (k, revcode ) <- get
    put (k+1, revcode )
    return $ Temp k

genTAC :: Program -> [TAC]
genTAC prog = reverse $ snd $ execState ( genProg prog ) (0 ,[])

genProg :: Program -> MyMon ()
genProg (Prog decls) = genDecls decls

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

-- la locazione è quella di dichiarazione
getAddress :: Ident -> Loc -> Addr
getAddress ident dloc = Var ident dloc

genDecl :: Declaration -> MyMon ()
genDecl decl = case decl of
    DefVar id@(PIdent (dloc, ident)) typ texp@(ETyped exp etyp _) -> let addrId = getAddress ident dloc in
        case exp of
            -- 3 + ( 4 + 6 )
            EOp e1 op e2 -> do
                addrE1 <- genExp e1
                addrE2 <- genExp e2
                out $ (AssignBinOp (convertOperation op typ) addrId addrE1 addrE2 )
                return ()
            ENeg e1 -> do
                addrE1 <- genExp e1
                out $ (AssignUnOp (if (etyp == TSimple SType_Int) then NegInt else NegFloat) addrId addrE1)
                return ()
            ENot e1 -> do
                addrE1 <- genExp e1
                out $ (AssignUnOp Not addrId addrE1)
                return ()
            _ -> do
                addrTExp <- genExp texp
                out $ (Assign addrId addrTExp)
                return ()


--genDecl decl = case decl of
--    DefVar id@(PIdent (loc, ident)) typ texp@(ETyped exp _ etyp) -> genExp exp (getAddress id typ)

--genExp :: Exp -> Addr -> MyMon ()
--genExp exp addr = case exp of
--    EInt (PInteger (loc,ident)) -> LitInt $ read ident :: Int
--    EFloat (PFloat (loc,ident)) -> LitFloat $ read ident :: Float
--    EChar (PChar (loc,ident)) -> LitChar $ read ident :: Char
--    EString (PString (loc, ident)) -> LitString ident
--    ETrue _ -> LitBool 1
--    EFalse _ -> LitBool 0
--    ENull _ -> Null
--    DummyExp -> Null
--    -- 3 + 5
--    EOp e1 op e2 -> do



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

--a[i]
--lexp' = a[i] exp = j
--lexp'' = a exp = i
--a@(1,2)
--i@(4,4)
--AssignFromArray t1 a@(1,2) i@(4,4)
--AssignFromArray t2 t1 j@(4,5)

genExp :: Exp -> MyMon Addr
genExp (ETyped exp typ loc) = case exp of
    EInt (PInteger (loc,ident)) -> return $ LitInt ( read ident :: Int )
    EFloat (PFloat (loc,ident)) -> return $ LitFloat ( read ident :: Float )
    EChar (PChar (loc,ident)) -> return $ LitChar ( read ident :: Char )
    EString (PString (loc, ident)) -> return $ LitString ident
    ETrue _ -> return $ LitBool 1
    EFalse _ -> return $ LitBool 0
    ENull _ -> return $ Null
    DummyExp -> return $ Null

    EOp e1 op e2 -> do
        addrE1 <- genExp e1
        addrE2 <- genExp e2
        addrTemp <- newTemp
        -- Qua tip = tipo di exp, non e1.
        out $ (AssignBinOp (convertOperation op typ) addrTemp addrE1 addrE2)
        return addrTemp

    ENeg e1 -> do
        addrE1 <- genExp e1
        addrTemp <- newTemp
        -- Qua tip = tipo di exp, non e1.
        out $ (AssignUnOp (if (typ == TSimple SType_Int) then NegInt else NegFloat) addrTemp addrE1)
        return addrTemp

    ENot e1 -> do
        addrE1 <- genExp e1
        addrTemp <- newTemp
        out $ (AssignUnOp Not addrTemp addrE1)
        return addrTemp

    ELExp lexp' -> genLexp lexp'

    EDeref lexp' -> do
        addrTemp <- newTemp 
        addrLexp' <- genLexp lexp'
        out $ (AssignFromRef addrTemp addrLexp')
        return addrTemp

    -- a = Array(11,12,13)
    -- a[0] = 11
    -- a[1] = 12
    -- a[2] = 13
    -- andrebbe generato un indirizzo temporaneo (indirizzo base dell'array)
    -- poi bisogna fare: base + i (posizione) * (sizeofType typ) = arrVals i
    {-
    EArray exps -> do
        arrVals <- mapM (genExp) exps
        addrTemp <- newTemp
    -}
