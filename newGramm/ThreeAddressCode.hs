module ThreeAddressCode where

import AbsGramm
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
    return $ int2AddrTempName k

genCode :: Expr -> [TAC]
genCode e = reverse $ snd $ execState ( genExp initEnv e) (0 ,[])

-- Non credo serva l'environment
genExp :: Env -> Expr -> MyMon Addr
genE env expr = case expr of
    ExprAdd IntType e1 e2 -> do
        addr1 <- genE env e1
        addr2 <- genE env e2
        addr <- newtemp
        out $ TacAssignBinOp addr addr1 IntAdd addr2
        return addr
    -- altra roba