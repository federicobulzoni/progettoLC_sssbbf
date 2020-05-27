module ThreeAddressCode where

import AbsTAC
import Control.Monad.State.Lazy

-- Questo sarà in un altro file e molto più complesso.
data TAC = String

-- Copiato da "Usare una monade con stream del codice nello stato"
-- Va modificato per il problema della dichiarazione di funzioni all'interno di altre funzioni.
type MyMon a = State (Int, [TAC]) a

out :: TAC -> MyMon ()
out instr = do
    (k, revcode ) <- get
    put (k, instr : revcode )
    return ()

newtemp :: MyMon Addr
newtemp = do
    (k, revcode ) <- get
    put (k+1, revcode )
    return $ Temp k

genTAC :: Program -> [TAC]
genTAC e = reverse $ snd $ execState ( genExp initEnv e) (0 ,[])

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
convertOperation Or _        = Or
convertOperation And _       = And
convertOperation Equal _     = Equal
convertOperation NotEq _     = NotEqual
convertOperation Less _      = Less
convertOperation LessEq _    = LessEq
convertOperation Greater _   = Greater
convertOperation GreaterEq _ = GreaterEq

-- la locazione è quella di dichiarazione
getAddress :: Ident -> Loc -> Addr
getAddress ident dloc = Var ident dloc

genDecl :: Declaration -> MyMon ()
genDecl decl = case decl of
    DefVar id@(PIdent (loc, ident)) typ texp@(ETyped exp _ etyp) -> let addrId = getAddress ident dloc in
        case exp of
            -- 3 + ( 4 + 6 )
            EOp e1 op e2 -> do
                addrE1 <- genExp e1
                addrE2 <- genExp e2
                out $ (AssignBinOp (convertOperation op) addrId addrE1 addrE2 )
                return ()
            ENeg e1 -> do
                addrE1 <- genExp e1
                out $ (AssignUnOp (if (etyp == TSimple SType_Int) then NegInt else NegFloat) addrId addrE1)
                return ()
            ENot e1 -> do
                addrE1 <- genExp e1
                out $ (AssignUnOp Not addrId addrE1)
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
    LArr lexp' exp = do
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
    EInt (PInteger (loc,ident)) -> LitInt $ read ident :: Int
    EFloat (PFloat (loc,ident)) -> LitFloat $ read ident :: Float
    EChar (PChar (loc,ident)) -> LitChar $ read ident :: Char
    EString (PString (loc, ident)) -> LitString ident
    ETrue _ -> LitBool 1
    EFalse _ -> LitBool 0
    ENull _ -> Null
    DummyExp -> Null

    EOp e1 op e2 -> do
        addrE1 <- genExp e1
        addrE2 <- genExp e2
        addrTemp <- newTemp
        out $ (AssignBinOp (convertOperation op) addrTemp addrE1 addrE2)
        return addrTemp

    ENeg e1 -> do
        addrE1 <- genExp e1
        addrTemp <- newTemp
        out $ (AssignUnOp (if (etyp == TSimple SType_Int) then NegInt else NegFloat) addrTemp addrE1)
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
    EArray exps -> do
        arrVals <- mapM (genExp) exps
        addrTemp <- newTemp
