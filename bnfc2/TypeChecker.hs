module TypeChecker where

-- Haskell module generated by the BNF converter

import AbsGramm
import ErrM
import Environment as Env
import PrintGramm
import Control.Monad.Writer

getLoc :: Exp -> Loc
getLoc (ETyped _ _ loc) = loc

getType :: a -> TypeSpec
getType (ETyped _ typ _) = typ
getType (StmTyped _ typ) = typ
getType (BlockTyped _ typ) = typ

isTypeError :: Exp -> Bool
isTypeError texp = getType texp == (SType TypeError) 

isTypeVoid :: TypeSpec -> Bool
isTypeVoid typ = typ == (SType TypeVoid)

paramsToStms :: [ParamClause] -> [Stm]
paramsToStms params = argsToStms (concat params)  


argsToStms :: [Arg] -> [Stm]
argsToStms [] = []
argsToStms (arg@(DArg id typ):args) = (Decla (DecVar id typ)):(argsToStms args)


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
  let x = foldM (\x (ident, info) -> Env.update x ident info) Env.emptyStack initialFuns in
    case x of
      Ok env -> do
        tdecls <- inferDecls decls env
        return (Prog tdecls)
      Bad msg -> do
        -- è impossibile.
        error $ msg


-- Prende una lista di dichiarazioni del programma e la ritorna annotata.
inferDecls :: [Decl] -> Env -> Writer [String] [Decl]
inferDecls [] env = return []
inferDecls (decl:decls) env = do
 (decl' , env') <-  inferDecl decl env
 xs <- inferDecls decls env'
 return (decl':xs)

-- Ci sono vari pattern con casi duplicati in base ad Ok env' o Bad msg che però si differenziano di poco,
-- andrebbero messi a posto.
inferDecl :: Decl -> Env -> Writer [String] (Decl, Env)
inferDecl decl env = case decl of
  --  DecVar PIdent TypeSpec
  DecVar (Id (loc, ident)) typ -> case update env ident (VarInfo loc typ) of
      Ok env' -> return (decl, env')
      Bad msg -> do
        tell [msg]
        return (decl, env)
  --  DefVar Id Type Exp
  DefVar id@(Id (loc, ident)) typ exp -> case update env ident (VarInfo loc typ) of
      Ok env' -> do
        -- Teniamo il vecchio env, perchè altrimenti si potrebbe usare nella parte destra della dichiarazione
        -- la variabile che si sta istanziando.
        texp <- inferExp exp env
        -- se TypeError texp, allora è inferExp che ha già scovato gli errori 
        -- e non dobbiamo ristamparli.
        if isTypeError texp || checkExp texp typ
          then
            return ((DefVar id typ texp), env')
          else
            tell [printTree exp ++ "e' di tipo " ++ show (getType texp) ++ ", ma il tipo atteso e': " ++ show typ ++ ".\n"]
            return ((DefVar id typ texp), env')
      Bad msg -> do
        tell [msg]
        texp <- inferExp exp env 
        if isTypeError texp || checkExp texp typ
          then
            return ((DefVar id typ texp), env)
          else
            tell [printTree exp ++ "e' di tipo " ++ show (getType texp) ++ ", ma il tipo atteso e': " ++ show typ ++ ".\n"]
            return ((DefVar id typ texp), env)

  ---------------------------------------------------------------------------------------------------------------------------------------
  DefVar id@(Id (loc, ident)) typ exp -> case update env ident (VarInfo loc typ) of
    Ok env' -> defVarAux
    Bad msg -> do
      tell [msg]
      defVarAux
    where defVarAux = do
      texp <- inferExp exp env
      if isTypeError texp || checkExp texp typ
        then
          return ((DefVar id typ texp), env')
        else
          tell [printTree exp ++ "e' di tipo " ++ show (getType texp) ++ ", ma il tipo atteso e': " ++ show typ ++ ".\n"]
          return ((DefVar id typ texp), env')
  ---------------------------------------------------------------------------------------------------------------------------------------

  DefFun id@(Id (loc, ident)) params typ block@(DBlock stms) -> case update env ident (FunInfo loc typ params) of
    Ok env' -> do
      -- block di suo aggiunge un nuovo scope e fa le operazioni su di esso.
      -- quindi l'idea è di aggiungere i parametri come dichiarazioni in testa al blocco.
      -- lock = DBlock [Stm]
      tblock <- inferBlock (DBlock (paramsToStms params)++stms) typ env'
      if checkBlock tblock typ 
        then
          return ((DefFun id params typ tblock), env')
        else
          -- manca un return giusto, se ce ne sono di sbagliati, inferBlock ce l'ha già detto.
          tell ["Attesa una istruzione return all'interno della funzione " ++ show ident ++ " dichiarata in posizione " ++ show loc ++ ", ma non trovata.\n"]
          return ((DefFun id params typ tblock), env')

    Bad msg -> do
      tell [msg]
      tblock <- inferBlock (DBlock (paramsToStms params)++stms) typ env
      if checkBlock tblock typ 
        then
          return ((DefFun id params typ tblock), env)
        else
          -- manca un return giusto, se ce ne sono di sbagliati, inferBlock ce l'ha già detto.
          tell ["Attesa una istruzione return all'interno della funzione " ++ show ident ++ " dichiarata in posizione " ++ show loc ++ ", ma non trovata.\n"]
          return ((DefFun id params typ tblock), env)

  -- Zucchero sintattico manuale e via.
  DefFunInLine id@(Id (loc, ident)) params typ exp -> inferDecl (DefFun id params typ (DBlock [SReturnExp (PReturn (loc , "return") exp)])) env


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
      return (BlockTyped (DBlock tstms) TypeVoid)


inferStms :: [Stm] -> TypeSpec -> Env -> Writer [String] [Stm]
inferStms [] typ env = []
inferStms (stm:stms) typ env = do
  (tstm, env') <- inferStm stm env
  tstms <- inferStms stms env'
  return (tstm:tstms)

inferStm :: Stm -> TypeSpec -> Env -> Writer [String] (Stm, Env)
inferStm stm typ env = case stm of
  SWhile exp stm' -> do
    -- texp è l'espressione annotata col tipo.
    texp <- inferExp exp env
    if isTypeError texp || checkExp texp Type_Bool
      then
        (tstm', _) <- inferStm stm' typ env
        return ( (StmTyped (SWhile texp tstm') (getType tstm') ) , env)
      else
        tell ["La condizione del while deve essere di tipo booleano, invece " 
        ++ printTree exp ++ "in posizione " ++ show (getLoc texp) 
        ++ " ha tipo: " ++ printTree (getType texp) ++ ".\n"

        (tstm', _) <- inferStm stm' typ env
        return ( (StmTyped (SWhile texp tstm') (getType tstm')) , env )
  SIf exp stmif stmelse -> do
    texp <- inferExp exp env
    if !(isTypeError texp || checkExp texp Type_Bool)
      then
        tell ["La condizione dell' if deve essere di tipo booleano, invece " 
        ++ printTree exp ++ "in posizione " ++ show (getLoc texp) 
        ++ " ha tipo: " ++ printTree (getType texp) ++ ".\n"

    (tstmif, _) <- inferStm stmif typ env
    (tstmelse, _) <- inferStm stmelse typ env
    if checkStm tstmif typ || checkStm tstmelse typ 
      then
        return ( (StmTyped (SIf texp tstmif tstmelse) typ) , env)
      else
        return ( (StmTyped (SIf texp tstmif tstmelse) TypeVoid) , env)

  SDecl decl -> do
    (tdecl, env') <- inferDecl decl env
    return ( (StmTyped (SDecl tdecl) TypeVoid), env' )

  SBlock block -> do
    tblock <- inferBlock block typ env
    return ((StmTyped (SBlock tblock) (getType tblock)), env)

  SAssign lexp exp -> do
    tlexp <- inferLExp lexp env
    texp <- inferExp exp env
    if isTypeError tlexp || isTypeError texp || checkExp texp (getType lexp)
      then
        return ( (StmTyped (SAssign tlexp texp) TypeVoid), env )
      else
        tell ["L'espressione " ++ printTree exp ++ " in posizione " ++ show (getLoc texp)
        ++ " ha tipo " ++ printTree (getType texp) ++ ", ma " ++ printTree lexp ++ " ha tipo " 
        ++ printTree (getType lexp) ++ "."]

        return ( (StmTyped (SAssign tlexp texp) TypeVoid), env )

  SReturnExp preturn exp -> do
    texp <- inferExp exp env
    if isTypeError texp
      then
        return ((StmTyped (SReturnExp preturn texp) TypeVoid), env)
      else
        if checkExp texp typ
          then
            return ((StmTyped (SReturnExp preturn texp) typ), env)
          else
            if isTypeVoid typ
              then
                tell ["Valore di ritorno inaspettato alla posizione" ++ show (getLoc texp) ++ "."]
              else
                tell ["L'espressione " ++ printTree exp ++ " in posizione " ++ show (getLoc texp)
                ++ " ha tipo " ++ printTree (getType exp) ++ ", ma il tipo di ritorno richiesto e' " ++ printTree typ ++ "."]
            -- Dubbi.
            return ((StmTyped (SReturnExp preturn texp) TypeVoid, env)

----------------------------------------------------------------------------------------------------------------------------------------------------------
saveLog:: String -> m ()
saveLog log = tell [log]
-- BRUTTA
SReturnExp preturn exp -> do
  texp <- inferExp exp env
  case (isTypeError texp) of
    True -> return ((StmTyped (SReturnExp preturn texp) TypeVoid), env)
    False -> case ((checkExp texp typ), (isTypeVoid typ)) of
          (True,_) -> return ((StmTyped (SReturnExp preturn texp) typ), env)
          (False, True) -> do
            saveLog "Valore di ritorno inaspettato alla posizione" ++ show (getLoc texp) ++ "."
            return ((StmTyped (SReturnExp preturn texp) TypeVoid, env)
          (False, False) -> do
            saveLog "L'espressione " ++ printTree exp ++ " in posizione " ++ show (getLoc texp)
              ++ " ha tipo " ++ printTree (getType exp) ++ ", ma il tipo di ritorno richiesto e' " ++ printTree typ ++ "."
            return ((StmTyped (SReturnExp preturn texp) TypeVoid, env)
----------------------------------------------------------------------------------------------------------------------------------------------------------
  SReturn preturn -> do
    if !(isTypeVoid typ)
      then 
        tell ["Il return in posizione " ++ show (getLoc preturn) ++ " non ha valore di ritorno, ma la funzione ha tipo "
        ++ showTree typ ++ "."]
    return ((StmTyped (SReturn preturn) TypeVoid), env)


inferLExp :: LExp -> Env -> Writer [String] LExp
inferLExp lexp env = case lexp of
  LRef lexp' -> do
    tlexp' <- inferLExp lexp' env
    if isTypeError tlexp' 
      then
        return (LExpTyped lexp TypeError)
      else
        case tlexp' of
          LExpTyped _ (TypeSpec (TypePointer typ)) -> return (LExpTyped (LRef tlexp') typ)
          LExpTyped _ typ' -> do
            tell ["Impossibile applicare operatore * a " ++ printTree lexp' ++ " in posizione " ++ show (getLoc tlexp') 
            ++ " che ha tipo " ++ printTree typ' ++ "."]
            return (LExpTyped (LRef tlexp') TypeError)

  LArr lexp exp ->
    tlexp <- inferLExp lexp env
    texp <- inferExp exp env
    if isTypeError tlexp || isTypeError texp
      then
        return (LExpTyped (LArr tlexp texp) TypeError)
      else
        case (tlexp , checkExp texp Type_Int) of
          (LExpTyped _ (TypeSpec (TArray typ)), True) -> return (LExpTyped (LArr tlexp texp) typ)
          (LExpTyped _ (TypeSpec (TArray typ)), False) -> do
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

  














-- (Exp di tipo sconosciuto) e vuoi verificare che sia di tipo typ passato. 

    -- Type_Error
    -- Type_Int
-- while ( 3 + 5 ) ...


checkExp :: Exp -> TypeSpec -> Env -> Writer [String] Exp
checkExp exp typ env = do
  -- 
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
            -- Forse qua andrebbe typ.
            return (ETyped exp Type_null)


checkExpAux :: TypeSpec -> TypeSpec -> Bool
checkExpAux typ typ'
  | typ' == typ = True
  | otherwise = False

-- OK!!!
{-
data Exp
    = EArray [Exp]
    | ENot Exp
    | ENeg Exp
    | ELExp LExp
    | EDeref LExp
    | EInt PInteger
    | EFloat PFloat
    | EChar PChar
    | EString PString
    | ETrue PTrue
    | EFalse PFalse
    | ENull PNull
    | EOp Exp Op Exp
    | ETyped Exp TypeSpec
    | EVarTyped PIdent TypeSpec PInteger PInteger
  deriving (Eq, Ord, Show, Read)
-}


-- Ad inferExp gli passiamo un espressione di cui non conosciamo il tipo,
-- e dunque ritorna tale espressione arricchita col suo tipo.
-- la posizione viene mandata su dai terminali.
inferExp :: Exp -> EnvStack -> Writer [String] ETyped
inferExp exp env = case exp of
  -- Nel caso dell'array bisogna inferire il tipo di ogni espressione in exps
  -- e tutte devono avere lo stesso tipo.
  -- Caso con array vuoto.

  EArray [] -> return (ETyped exp TypeVoid)
  -- Caso con almeno un elemento
  --                 [ETyped]
  EArray exps -> let texps = foldM (\x -> inferExp x env) [] exps in

  EArray (exp':exps) -> do
    texp'@(ETyped _ typ) <- inferExp exp' env
    if checkExp 

  -- Il not non cambia il tipo dell'espressione a cui è applicato,
  -- il tipo di una espressione not è dunque il tipo della espressione a cui il not è applicato.
  ENot exp' -> checkExp exp' Type_Bool env



  do 
    texp <- inferExp exp' env

  checkExp exp' SType_Bool env
    
  --Stesso discorso del not.
  ENeg exp' -> inferExp exp' env

  -- Stesso discorso del not e del neg.
  ELExp exp' -> inferExp exp' env


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



-- Tests:
getEnv = do 
  env1 <- updateVar emptyEnv (Id ((1,5),"x")) Type_int
  updateVar env1 (Id ((1,22),"y")) Type_float


test4 :: Writer [String] (Decl, Env)
test4 = let x = getEnv
  in
    case x of
      Ok env -> inferDecl (DefVar (Id ((1,5),"z")) Type_int (EAdd (EVar (Id ((1,49),"y"))) (EVar (Id ((1,51),"x"))))) env
      Bad _ -> error $ "ciao"

test5 = let x = getEnv
  in 
    case x of
      Ok env -> inferDecl (DefVar (Id ((1,5),"x")) Type_int (EAdd (EVar (Id ((1,49),"y"))) (EVar (Id ((1,51),"x"))))) env
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

    


