module TypeChecker where

-- Haskell module generated by the BNF converter
-- TODO:
  -- controllare l'uso di NULL che per ora ancora non è stato utilizzato.
  -- controllare l'uso di array vuoti, e la posizione a loro assegnata di default.
  -- pensare alla compatibilita String + String. 
import AbsGramm
import ErrM
import Environment as Env
import PrintGramm
import Control.Monad.Writer

getLoc :: Exp -> Loc
--getLoc (ETyped _ _ loc) = loc
getLoc (ETyped _ _ r c) = (r,c)

getType :: a -> TypeSpec
getType (ETyped _ typ _ _) = typ
--getType (ETyped _ typ _ ) = typ
getType (StmTyped _ typ) = typ
getType (BlockTyped _ typ) = typ

isTypeError :: Exp -> Bool
isTypeError texp = getType texp == (TSimple TypeError)

isTypeVoid :: TypeSpec -> Bool
isTypeVoid typ = typ == (TSimple TypeVoid)

paramsToStms :: [ParamClause] -> [Stm]
paramsToStms params = argsToStms (concat params)  


argsToStms :: [Arg] -> [Stm]
argsToStms [] = []
argsToStms (arg@(DArg id typ):args) = (SDecl (DecVar id typ)):(argsToStms args)


initialFuns :: [(Ident, Info)]
initialFuns = [("writeInt", FunInfo (0,0) (TSimple TypeVoid) [PArg [DArg (PIdent ((0,0),"i")) (TSimple SType_Int) ]]),
  ("writeFloat", FunInfo (0,0) (TSimple TypeVoid) [PArg [DArg (PIdent ((0,0),"f")) (TSimple SType_Float) ]]),
  ("writeChar", FunInfo (0,0) (TSimple TypeVoid) [PArg [DArg (PIdent ((0,0),"c")) (TSimple SType_Char) ]]),
  ("writeString", FunInfo (0,0) (TSimple TypeVoid) [PArg [DArg (PIdent ((0,0),"s")) (TSimple SType_String) ]]),

  ("readInt", FunInfo (0,0) (TSimple SType_Int) [PArg []]),
  ("readFloat", FunInfo (0,0) (TSimple SType_Float) [PArg []]),
  ("readChar", FunInfo (0,0) (TSimple SType_Char) [PArg []]),
  ("readString", FunInfo (0,0) (TSimple SType_String) [PArg []]) ]

-- Restituisce una lista di log ed un programma annotato.
typeCheck :: Program -> Writer [String] Program
typeCheck (Prog decls) = do
  -- inizializzazione environment.
  let x = foldM (\x (ident, info) -> Env.update x ident info) (Env.emptyEnv) initialFuns in
    case x of
      Ok env -> do
        tdecls <- inferDecls decls env
        return (Prog tdecls)
      Bad msg -> do
        -- è impossibile.
        error $ msg


-- Prende una lista di dichiarazioni del programma e la ritorna annotata.
inferDecls :: [Declaration] -> Env -> Writer [String] [Declaration]
inferDecls [] env = return []
inferDecls (decl:decls) env = do
 (decl' , env') <-  inferDecl decl env
 xs <- inferDecls decls env'
 return (decl':xs)

-- Ci sono vari pattern con casi duplicati in base ad Ok env' o Bad msg che però si differenziano di poco,
-- andrebbero messi a posto.
inferDecl :: Declaration -> Env -> Writer [String] (Declaration, Env)
inferDecl decl env = case decl of
  --  DecVar PIdent TypeSpec
  DecVar (PIdent (loc, ident)) typ -> case update env ident (VarInfo loc typ) of
      Ok env' -> return (decl, env')
      Bad msg -> do
        tell [msg]
        return (decl, env)
  
  --  DefVar PIdent Type Exp
  DefVar id@(PIdent (loc, ident)) typ exp -> case update env ident (VarInfo loc typ) of
      Ok env' -> do
        -- Teniamo il vecchio env, perchè altrimenti si potrebbe usare nella parte destra della dichiarazione
        -- la variabile che si sta istanziando.
        texp <- inferExp exp env
        -- se TypeError texp, allora è inferExp che ha già scovato gli errori 
        -- e non dobbiamo ristamparli.
        if isTypeError texp || checkExp texp typ
          then
            return ((DefVar id typ texp), env')
          else do
            tell [printTree exp ++ "e' di tipo " ++ show (getType texp) ++ ", ma il tipo atteso e': " ++ show typ ++ ".\n"]
            return ((DefVar id typ texp), env')
      Bad msg -> do
        tell [msg]
        texp <- inferExp exp env 
        if isTypeError texp || checkExp texp typ
          then
            return ((DefVar id typ texp), env)
          else do
            tell [printTree exp ++ "e' di tipo " ++ show (getType texp) ++ ", ma il tipo atteso e': " ++ show typ ++ ".\n"]
            return ((DefVar id typ texp), env)

--  ---------------------------------------------------------------------------------------------------------------------------------------
--  DefVar id@(PIdent (loc, ident)) typ exp -> do
--    case update env ident (VarInfo loc typ) of
--      Ok env' -> ()
--      Bad msg -> do
--        tell [msg]
--    texp <- inferExp exp env
--    if (isTypeError texp) || (checkExp texp typ)
--      then
--        return ((DefVar id typ texp), env')
--      else
--        tell [printTree exp ++ "e' di tipo " ++ show (getType texp) ++ ", ma il tipo atteso e': " ++ show typ ++ ".\n"]
--        return ((DefVar id typ texp), env')
--  ---------------------------------------------------------------------------------------------------------------------------------------

  DefFun id@(PIdent (loc, ident)) params typ block@(DBlock stms) -> case update env ident (FunInfo loc typ params) of
    Ok env' -> do
      -- block di suo aggiunge un nuovo scope e fa le operazioni su di esso.
      -- quindi l'idea è di aggiungere i parametri come dichiarazioni in testa al blocco.
      -- lock = DBlock [Stm]
      tblock <- inferBlock (DBlock ((paramsToStms params)++stms)) typ env'
      if checkBlock tblock typ 
        then
          return ((DefFun id params typ tblock), env')
        else do
          -- manca un return giusto, se ce ne sono di sbagliati, inferBlock ce l'ha già detto.
          tell ["Attesa una istruzione return all'interno della funzione " ++ show ident ++ " dichiarata in posizione " ++ show loc ++ ", ma non trovata.\n"]
          return ((DefFun id params typ tblock), env')

    Bad msg -> do
      tell [msg]
      tblock <- inferBlock (DBlock ((paramsToStms params)++stms)) typ env
      if checkBlock tblock typ 
        then
          return ((DefFun id params typ tblock), env)
        else do
          -- manca un return giusto, se ce ne sono di sbagliati, inferBlock ce l'ha già detto.
          tell ["Attesa una istruzione return all'interno della funzione " ++ show ident ++ " dichiarata in posizione " ++ show loc ++ ", ma non trovata.\n"]
          return ((DefFun id params typ tblock), env)

  -- Zucchero sintattico manuale e via.
  DefFunInLine id@(PIdent (loc, ident)) params typ exp -> inferDecl (DefFun id params typ (DBlock [SReturnExp (PReturn (loc , "return") exp)])) env


checkBlock :: Block -> TypeSpec -> Bool
checkBlock (BlockTyped _ typ') typ = typ == typ'
checkBlock _  _ = error $ "Errore interno chiamato da checkBlock().\n"

inferBlock :: Block -> TypeSpec -> Env -> Writer [String] Block
-- typ è il tipo che ci aspettiamo che il blocco abbia.
inferBlock (DBlock stms) typ env = do
  tstms <- inferStms stms typ (Env.addScope env) 
  if any (\x -> checkStm x typ) tstms
    then
      return (BlockTyped (DBlock tstms) typ)
    else
      return (BlockTyped (DBlock tstms) (TSimple TypeVoid))


inferStms :: [Stm] -> TypeSpec -> Env -> Writer [String] [Stm]
inferStms [] typ env = return []
inferStms (stm:stms) typ env = do
  (tstm, env') <- inferStm stm env
  tstms <- inferStms stms env'
  return (tstm:tstms)

inferStm :: Stm -> TypeSpec -> Env -> Writer [String] (Stm, Env)
inferStm stm typ env = case stm of
  SWhile exp stm' -> do
    -- texp è l'espressione annotata col tipo.
    texp <- inferExp exp env
    if isTypeError texp || checkExp texp (TSimple SType_Bool)
      then 
        do
          (tstm', _) <- inferStm stm' typ env
          return ( (StmTyped (SWhile texp tstm') (getType tstm') ) , env)
      else
        do
          tell ["La condizione del while deve essere di tipo booleano, invece " 
            ++ printTree exp ++ "in posizione " ++ show (getLoc texp) 
            ++ " ha tipo: " ++ printTree (getType texp) ++ "."]
          (tstm', _) <- inferStm stm' typ env
          return ( (StmTyped (SWhile texp tstm') (getType tstm')) , env )
  SIf exp stmif stmelse -> do
    texp <- inferExp exp env
    if not (isTypeError texp || checkExp texp (TSimple SType_Bool))
      then
        tell ["La condizione dell' if deve essere di tipo booleano, invece " 
          ++ printTree exp ++ "in posizione " ++ show (getLoc texp) 
          ++ " ha tipo: " ++ printTree (getType texp) ++ "."]
      else
        return ()

    (tstmif, _) <- inferStm stmif typ env
    (tstmelse, _) <- inferStm stmelse typ env
    if checkStm tstmif typ || checkStm tstmelse typ 
      then
        return ( (StmTyped (SIf texp tstmif tstmelse) typ) , env)
      else
        return ( (StmTyped (SIf texp tstmif tstmelse) (TSimple TypeVoid)) , env)

  SDecl decl -> do
    (tdecl, env') <- inferDecl decl env
    return ( (StmTyped (SDecl tdecl) (TSimple TypeVoid)), env' )

  SBlock block -> do
    tblock <- inferBlock block typ env
    return ((StmTyped (SBlock tblock) (getType tblock)), env)

  SAssign lexp exp -> do
    tlexp <- inferLExp lexp env
    texp <- inferExp exp env
    if isTypeError tlexp || isTypeError texp || checkExp texp (getType lexp)
      then
        return ( (StmTyped (SAssign tlexp texp) (TSimple TypeVoid)), env )
      else do
        tell ["L'espressione " ++ printTree exp ++ " in posizione " ++ show (getLoc texp)
          ++ " ha tipo " ++ printTree (getType texp) ++ ", ma " ++ printTree lexp ++ " ha tipo " 
          ++ printTree (getType lexp) ++ "."]
        return ( (StmTyped (SAssign tlexp texp) (TSimple TypeVoid)), env )

  SReturnExp preturn exp -> do
    texp <- inferExp exp env
    case (isTypeError texp) of
      True -> return ((StmTyped (SReturnExp preturn texp) (TSimple TypeVoid)), env)
      False -> case ((checkExp texp typ), (isTypeVoid typ)) of

        (True,_) -> return ((StmTyped (SReturnExp preturn texp) typ), env)
        (False, True) -> do
          saveLog "Valore di ritorno inaspettato alla posizione" ++ show (getLoc texp) ++ "."
          return ((StmTyped (SReturnExp preturn texp) (TSimple TypeVoid), env))
        (False, False) -> do
          saveLog "L'espressione " ++ printTree exp ++ " in posizione " ++ show (getLoc texp)
            ++ " ha tipo " ++ printTree (getType exp) ++ ", ma il tipo di ritorno richiesto e' " ++ printTree typ ++ "."
          return ((StmTyped (SReturnExp preturn texp) (TSimple TypeVoid), env))

  SReturn preturn -> do
    if not (isTypeVoid typ)
      then 
        tell ["Il return in posizione " ++ show (getLoc preturn) ++ " non ha valore di ritorno, ma la funzione ha tipo "
          ++ printTree typ ++ "."]
      else
        return ()
    return ((StmTyped (SReturn preturn) (TSimple TypeVoid)), env)

----------------------------------------------------------------------------------------------------------------------------------------------------------
--saveLog:: String -> Writer [String] a
--saveLog log = tell [log]
---- BRUTTA
--SReturnExp preturn exp -> do
--  texp <- inferExp exp env
--  case (isTypeError texp) of
--    True -> return ((StmTyped (SReturnExp preturn texp) (TSimple TypeVoid)), env)
--    False -> case ((checkExp texp typ), (isTypeVoid typ)) of
--          (True,_) -> return ((StmTyped (SReturnExp preturn texp) typ), env)
--          (False, True) -> do
--            saveLog "Valore di ritorno inaspettato alla posizione" ++ show (getLoc texp) ++ "."
--            return ((StmTyped (SReturnExp preturn texp) (TSimple TypeVoid), env)
--          (False, False) -> do
--            saveLog "L'espressione " ++ printTree exp ++ " in posizione " ++ show (getLoc texp)
--              ++ " ha tipo " ++ printTree (getType exp) ++ ", ma il tipo di ritorno richiesto e' " ++ printTree typ ++ "."
--            return ((StmTyped (SReturnExp preturn texp) (TSimple TypeVoid), env)
----------------------------------------------------------------------------------------------------------------------------------------------------------




inferLExp :: LExp -> Env -> Writer [String] LExp
inferLExp lexp env = case lexp of
 LRef lexp' -> do
   tlexp' <- inferLExp lexp' env
   if isTypeError tlexp' 
     then
       return (LExpTyped lexp TypeError)
     else
       case tlexp' of
         LExpTyped _ (TPointer typ) -> return (LExpTyped (LRef tlexp') typ)
         LExpTyped _ typ' -> do
           tell ["Impossibile applicare operatore * a " ++ printTree lexp' ++ " in posizione " ++ show (getLoc tlexp') 
            ++ " che ha tipo " ++ printTree typ' ++ "."]
           return (LExpTyped (LRef tlexp') TypeError)


 LArr lexp exp -> do
   tlexp <- inferLExp lexp env
   texp <- inferExp exp env
   if isTypeError tlexp || isTypeError texp
     then
       return (LExpTyped (LArr tlexp texp) TypeError)
     else
       case (tlexp , checkExp texp Type_Int) of
         (LExpTyped _ (TArray typ), True) -> return (LExpTyped (LArr tlexp texp) typ)
         (LExpTyped _ (TArray typ), False) -> do
           tell ["L'indice di accesso ad un'array deve avere tipo intero, invece in posizione "
            ++ show (getLoc texp) ++ "l'espressione " ++ printTree exp ++ " ha tipo " ++ printTree (getType texp) ++ "."]

           return (LExpTyped (LArr tlexp texp) TypeError)
         (_, False) -> do
           tell ["L'accesso tramite operatore [] puo' essere effettuato solo su elementi di tipo Array, mentre "
            ++ printTree lexp ++ " ha tipo " ++ showTree (getType tlexp) ++ "."]
           tell ["L'indice di accesso ad un'array deve avere tipo intero, invece in posizione "
            ++ show (getLoc texp) ++ "l'espressione " ++ printTree exp ++ " ha tipo " ++ printTree (getType texp) ++ "."]
           
           return (LExpTyped (LArr tlexp texp) TypeError)
         (_, True) -> do
           tell ["L'accesso tramite operatore [] puo' essere effettuato solo su elementi di tipo Array, mentre "
             ++ printTree lexp ++ " ha tipo " ++ showTree (getType tlexp) ++ "."]
           
           return (LExpTyped (LArr tlexp texp) TypeError)
 LIdent id@(PIdent (loc, ident)) -> let res = Env.lookup env ident in
   case res of
     Bad msg -> do
       tell [msg]
       return (LExpTyped lexp TypeError)
     -- dloc dichiarazione loc
     Ok (VarInfo dloc typ) -> return (LIdentTyped id typ dloc)
     Ok (FunInfo dloc _ _) -> do
       -- è da mettere a posto sto errore.
       tell ["L'identificatore " ++ show ident
        ++ "e' stato utilizzato in posizione " ++ show dloc ++ "per dichiarare una funzione."]

       return (LExpTyped lexp TypeError)



-- Prende una lista di espressioni tipizzate, un tipo, e ritorna una coppia con il primo elemento
-- che dice se si è trovato almeno un elemento con tipo TypeError, ed il secondo elemento che dice
-- se tutte le espressioni hanno tipo type o meno.
inferArrayAux :: [Exp] -> TypeSpec -> (Bool, Bool)
inferArrayAux texps typ = ( (any (\x -> checkExp x (TSimple TypeError)) texps),(all (\x -> checkExp x typ) texps) )

inferExp :: Exp -> Env -> Writer [String] Exp
inferExp exp env = case exp of
  -- Punto di vista di Bulzo: gli array di dim 0 non hanno alcuna utilità per il programmatore nel nostro linguaggio
  -- in cui non ci sono liste e non ci sono allocazioni dinamiche di memoria.
                                                                              -- posizione fittizia. Spunta da qualche parte?
  EArray [] -> return (ETyped (exp) (TypeSpec (TArray (TSimple TypeVoid) 0) ) (0,0)  )
  EArray exps -> do
    texps <- mapM (\x -> inferExp x env) exps
    typ <- getType (head texps) 
    case inferArrayAux texps typ of
      (True, _) -> return (ETyped (EArray texps) (TypeSpec (TArray (TSimple TypeError) (length texps) ) ) (getLoc (head texps)) )
      (_, True) -> return (ETyped (EArray texps) (TypeSpec (TArray (typ) (length texps) ) ) (getLoc (head texps)) )
      (_ , _) -> do
        -- Da mettere un più bel messaggio di errore.
        tell ["Inconsistenza nei valori assegnati all'array in posizione " ++ show (getLoc (head texps) ) ++ "."]
        return (ETyped (EArray texps) (TypeSpec (TArray (TSimple TypeError) (length texps) ) ) (getLoc (head texps)) )

  ENot exp -> do
    texp <- inferExp exp env
    if isTypeError texp || checkExp texp (TSimple Type_Bool)
      then
        return (ETyped (ENot texp) (getType texp) (getLoc texp))
      else 
        do
          tell ["Operatore ! applicato alla espressione " ++ printTree exp ++ ", che ha tipo "
            ++ printTree (getType texp) ++ ", ma era attesa di tipo " ++ printTree (TSimple Type_Bool) "."]
          return ((ETyped (ENot texp) (TSimple TypeError) (getLoc texp)))

  ENeg exp -> do
    texp <- inferExp exp env
    if isTypeError texp || checkExp texp (TSimple Type_Int) || checkExp texp (TSimple Type_float)
      then
        return (ETyped (ENeg texp) (getType texp) (getLoc texp))
      else 
        do
          tell ["Operatore - applicato alla espressione " ++ printTree exp ++ ", che ha tipo "
            ++ printTree (getType texp) ++ ", ma era attesa di tipo numerico."]
          return ((ETyped (ENeg texp) (TSimple TypeError) (getLoc texp)))
  
  ELExp lexp -> do
    tlexp <- inferLExp lexp env
    return (ETyped (ELExp tlexp) (getType tlexp) (getLoc tlexp))

  EDeref lexp -> do
    tlexp <- inferLExp lexp env
    if isTypeError tlexp
      then
        return (ETyped (EDeref tlexp) (TSimple Type_Error) (getLoc tlexp))
      else
        return ( ETyped (EDeref tlexp) (TPointer (getType tlexp)) (getLoc tlexp) )

  EInt const@(PInteger (loc, _)) ->return (ETyped (EInt const) (TSimple Type_Int) (loc) )
  EFloat const@(PFloat (loc, _)) -> return (ETyped (EFloat const) (TSimple Type_Float) (loc) )
  EChar const@(PChar (loc, _)) -> return (ETyped (EChar const) (TSimple Type_Char) (loc) )
  EString const@(PString (loc, _)) -> return (ETyped (EString const) (TSimple Type_String) (loc) )
  ETrue const@(PTrue (loc, _)) -> return (ETyped (ETrue const) (TSimple Type_Bool) (loc) )
  EFalse const@(PFalse (loc, _)) -> return (ETyped (EFalse const) (TSimple Type_Bool) (loc) )
  -- Ma serve Null? Con i puntatori probabilmente si, nel caso non avrebbe TypeVoid.
  ENull const@(PNull (loc, _)) -> return (ETyped (ENull const) (TSimple TypeVoid) (loc) )

  EOp expl op expr -> inferBinOp expl op expr


-- Prese due espressioni ed un operatore binario ritorna la corrispondente espressione tipizzata
inferBinOp :: Exp -> Op -> Exp -> Writer [String] Exp
inferBinOp expl op expr = do 
    texpl <- inferExp expl env
    texpr <- inferExp expr env
    case ( (isConsistent (getTypeOp op) (getType texpl) (getType texpr) ) , (getType texpl) , (getType texpr) ) of
      (_ , TSimple TypeError, _ ) -> returnBinOpError texpl op texpr 
      (_ , _ , TSimple TypeError) -> returnBinOpError texpl op texpr 
      (True, typl, typr) -> return (ETyped (EOp texpl op texpr) (typl) (getLoc texpl))
      (False, typl, typr) -> do
        tell ["L'operatore " ++ printTree op ++ " in posizione " ++ show (getLoc texpl)
              ++ " non puo' essere applicato ad un'espressione di tipo " ++ printTree typl 
              ++ " e un'espressione di tipo " ++ printTree typr ++ "."]
        returnBinOpError texpl op texpr
    where returnBinOpError texpl op texpr = return (ETyped (EOp texpl op texpr) (TSimple TypeError) (getLoc texpl) )

isConsistent :: TypeOp -> TypeSpec -> TypeSpec -> Bool
isConsistent NumericOp typl typr = (typl == typr) && (checkNumericTyp typl)
isConsistent BooleanOp typl typr = (typl == typr) && (checkBooleanTyp typl)
isConsistent EqOp typl typr = (typl == typr)
isConsistent RelOp typl typr = (typl == typr) && (checkNumericTyp typl)

checkNumericTyp :: TypeSpec  -> Bool
checkNumericTyp typ = ( (typ == (TSimple SType_Int) )|| (typ == (TSimple SType_Float)) )

checkBooleanTyp :: TypeSpec -> Bool
checkBooleanTyp (TSimple SType_Bool) = True
checkBooleanTyp _ = False


data TypeOp = NumericOp | BooleanOp | EqOp | RelOp
getTypeOp :: Op -> TypeOp
getTypeOp Plus = NumericOp
getTypeOp Minus = NumericOp
getTypeOp Prod = NumericOp
getTypeOp Div = NumericOp
getTypeOp Mod = NumericOp
getTypeOp Pow = NumericOp
getTypeOp Or = BooleanOp
getTypeOp And = BooleanOp
getTypeOp Less = RelOp
getTypeOp Greater = RelOp
getTypeOp LessEq = RelOp
getTypeOp GreaterEq = RelOp
getTypeOp Equal = EqOp
getTypeOp NotEq = EqOp
 


--
---- Tests:
--getEnv = do 
--  env1 <- updateVar emptyEnv (PIdent ((1,5),"x")) Type_int
--  updateVar env1 (PIdent ((1,22),"y")) Type_float
--
--
--test4 :: Writer [String] (Declaration, Env)
--test4 = let x = getEnv
--  in
--    case x of
--      Ok env -> inferDecl (DefVar (PIdent ((1,5),"z")) Type_int (EAdd (EVar (PIdent ((1,49),"y"))) (EVar (PIdent ((1,51),"x"))))) env
--      Bad _ -> error $ "ciao"
--
--test5 = let x = getEnv
--  in 
--    case x of
--      Ok env -> inferDecl (DefVar (PIdent ((1,5),"x")) Type_int (EAdd (EVar (PIdent ((1,49),"y"))) (EVar (PIdent ((1,51),"x"))))) env
--      Bad _ -> error $ "ciao"
--
--
--test3 = let x = getEnv 
--  in
--    case x of
--      Ok env -> checkExp (EAdd (EVar (PIdent ((1,49),"y"))) (EVar (PIdent ((1,51),"x")))) Type_float env
--      Bad _ -> return (ETyped  (EVar (PIdent ((1,49),"crocs"))) Type_null)
--
--
--
--test2 = let x = getEnv
--  --updateVar (updateVar emptyEnv (PIdent ((1,5),"x")) Type_int) (PIdent ((1,22),"y")) Type_int
--  in 
--    case x of
--      Ok env -> inferExp (EAdd (EVar (PIdent ((1,49),"x"))) (EVar (PIdent ((1,51),"y")))) env
--      Bad msg -> return Type_int
--
--test = let x = updateVar emptyEnv (PIdent ((3,5), "y")) Type_int in
--  case x of
--    Ok env -> inferExp (EVar (PIdent ((1,2), "x"))) env
--    Bad msg -> return Type_int
----updateVar emptyEnv (PIdent ((3,5), "x")) Type_int >>= inferExp (EVar (PIdent ((1,2), "x")))
--
--    


