module TypeChecker where

-- Haskell module generated by the BNF converter

import AbsGramm
import ErrM
import Environment
import PrintGramm
import Control.Monad.Writer

typeCheck :: Program -> Writer [String] Program

-- data Program = Prog [Decl]

typeCheck (Prog decls) = do
  res <- checkDecls decls emptyEnv
  return (Prog res)

checkDecls :: [Decl] -> Env -> Writer [String] [Decl]
checkDecls [] env = return []
checkDecls (decl:decls) env = do
 (decl' , env') <-  checkDecl decl env
 xs <- checkDecls decls env'
 return (decl':xs)

checkDecl :: Decl -> Env -> Writer [String] (Decl, Env)
checkDecl decl env = case decl of
  --  DecVar Id Type
  DecVar id typ -> let x = updateVar env id typ in
    case x of
      Ok env' -> return (decl, env')
      Bad msg -> do
        tell [msg]
        return (decl, env)
  --  DefVar Id Type Exp
  DefVar id typ exp -> let x = updateVar env id typ in
    case x of
      Ok env' -> do
        -- Teniamo il vecchio env, perchè altrimenti si potrebbe usare nella parte destra della dichiarazione
        -- la variabile che si sta istanziando.
        texp <- checkExp exp typ env
        return ((DefVar id typ texp), env')
      Bad msg -> do
        tell [msg]
        return (decl, env)

-- OK!!!
checkExp :: Exp -> Type -> Env -> Writer [String] Exp
checkExp exp typ env = do
  typ' <- inferExp exp env
  if typ' == Type_null
    then
      return (ETyped exp Type_null)
    else 
      if (checkExpAux typ typ')
        then
          return (ETyped exp typ)
        else
          do 
            tell [printTree exp ++ " e' di tipo " ++ printTree typ' ++ ", ma il tipo aspettato e' " ++ printTree typ ++ "\n"]
            return (ETyped exp Type_null)


checkExpAux :: Type -> Type -> Bool
checkExpAux typ typ'
  | typ' == typ = True
  | otherwise = False

-- OK!!!
inferExp :: Exp -> Env -> Writer [String] Type
inferExp exp env = case exp of
  EAdd exp1 exp2 -> do 
    -- Writer [String] Type
    t1 <- inferExp exp1 env
    t2 <- inferExp exp2 env

    case (t1,t2) of
      (_, Type_null) -> return Type_null
      (Type_null, _) -> return Type_null
      (x, y) -> if x == y
        then 
          return t1
        else
          do
            tell ["operatori di tipo diverso: " ++ printTree exp1 ++ " ha tipo: " ++ show t1 ++ ", " ++ printTree exp2 ++ " ha tipo: " ++ show t2 ++ ".\n"]
            return Type_null
  EInt n -> return Type_int
  EVar id -> let t1 = lookupVar env id in
    case t1 of
      Bad msg -> do
        tell [msg]
        return Type_null
      Ok typ -> return typ

--Prog [DefVar (Id ((1,5),"x")) Type_int (EInt 3),DefVar (Id ((1,22),"y")) Type_int (EInt 3),DefVar (Id ((1,39),"z")) Type_int (EAdd (EVar (Id ((1,49),"x"))) (EVar (Id ((1,51),"y"))))]



-- Tests:
getEnv = do 
  env1 <- updateVar emptyEnv (Id ((1,5),"x")) Type_int
  updateVar env1 (Id ((1,22),"y")) Type_float


test4 :: Writer [String] (Decl, Env)
test4 = let x = getEnv
  in
    case x of
      Ok env -> checkDecl (DefVar (Id ((1,5),"z")) Type_int (EAdd (EVar (Id ((1,49),"y"))) (EVar (Id ((1,51),"x"))))) env
      Bad _ -> error $ "ciao"

test5 = let x = getEnv
  in 
    case x of
      Ok env -> checkDecl (DefVar (Id ((1,5),"x")) Type_int (EAdd (EVar (Id ((1,49),"y"))) (EVar (Id ((1,51),"x"))))) env
      Bad _ -> error $ "ciao"


test3 = let x = getEnv 
  in
    case x of
      Ok env -> checkExp (EAdd (EVar (Id ((1,49),"y"))) (EVar (Id ((1,51),"x")))) Type_float env
      Bad _ -> return (ETyped  (EVar (Id ((1,49),"crocs"))) Type_null)



test2 = let x = getEnv
  --updateVar (updateVar emptyEnv (Id ((1,5),"x")) Type_int) (Id ((1,22),"y")) Type_int
  in 
    case x of
      Ok env -> inferExp (EAdd (EVar (Id ((1,49),"x"))) (EVar (Id ((1,51),"y")))) env
      Bad msg -> return Type_int

test = let x = updateVar emptyEnv (Id ((3,5), "y")) Type_int in
  case x of
    Ok env -> inferExp (EVar (Id ((1,2), "x"))) env
    Bad msg -> return Type_int
--updateVar emptyEnv (Id ((3,5), "x")) Type_int >>= inferExp (EVar (Id ((1,2), "x")))

    


