module TypeChecker where

-- Haskell module generated by the BNF converter
-- TODO:
  -- controllare l'uso di NULL che per ora ancora non è stato utilizzato.
  -- controllare l'uso di array vuoti, e la posizione a loro assegnata di default.
  -- pensare alla compatibilita String + String. 
  -- controllare la grammatica e aggiungere le locazioni a tutti i typed che non ce l'hanno.
import AbsGramm
import ErrM
import Environment as Env
import PrintGramm
import Control.Monad.Writer

saveLog:: String -> Writer [String] ()
saveLog log = do 
  tell [log]
  return ()

isTypeVoid :: TypeSpec -> Bool
isTypeVoid typ = typ == (TSimple TypeVoid)

isDummy :: Exp -> Bool
isDummy DummyExp = True
isDummy _ = False
paramsToStms :: [ParamClause] -> [Stm]

paramsToStms params = argsToStms (concat ( map (\(PArg args) -> args) params ) )


argsToStms :: [Arg] -> [Stm]
argsToStms [] = []
argsToStms (arg@(DArg id typ):args) = (SDecl (DefVar id typ DummyExp)):(argsToStms args)


initialFuns :: [(Ident, Info)]
initialFuns = [
  ("writeInt",    FunInfo (0,0) (TSimple TypeVoid) [PArg [DArg (PIdent ((0,0),"i")) (TSimple SType_Int) ]]),
  ("writeFloat",  FunInfo (0,0) (TSimple TypeVoid) [PArg [DArg (PIdent ((0,0),"f")) (TSimple SType_Float) ]]),
  ("writeChar",   FunInfo (0,0) (TSimple TypeVoid) [PArg [DArg (PIdent ((0,0),"c")) (TSimple SType_Char) ]]),
  ("writeString", FunInfo (0,0) (TSimple TypeVoid) [PArg [DArg (PIdent ((0,0),"s")) (TSimple SType_String) ]]),

  ("readInt",     FunInfo (0,0) (TSimple SType_Int)    [PArg []]),
  ("readFloat",   FunInfo (0,0) (TSimple SType_Float)  [PArg []]),
  ("readChar",    FunInfo (0,0) (TSimple SType_Char)   [PArg []]),
  ("readString",  FunInfo (0,0) (TSimple SType_String) [PArg []]) ]

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
  --DecVar (PIdent (loc, ident)) typ -> case update env ident (VarInfo loc typ) of
  --    Ok env' -> return (decl, env')
  --    Bad msg -> do
  --      saveLog msg
  --      return (decl, env)
  
  --  DefVar PIdent Type Exp
  --DefVar id@(PIdent (loc, ident)) typ exp -> case update env ident (VarInfo loc typ) of
  --    Ok env' -> do
  --      -- Teniamo il vecchio env, perchè altrimenti si potrebbe usare nella parte destra della dichiarazione
  --      -- la variabile che si sta istanziando.
  --      texp <- inferExp exp env
  --      -- se (TSimple TypeError) texp, allora è inferExp che ha già scovato gli errori 
  --      -- e non dobbiamo ristamparli.
  --      if isTypeError texp || isDummy exp || checkExp texp typ
  --        then
  --          return ((DefVar id typ texp), env')
  --        else do
  --          saveLog ("Errore in posizione: " ++ printTree loc ++ ", " ++ printTree exp ++ "e' di tipo " ++ printTree (getType texp) ++ ", ma il tipo atteso e': " ++ printTree typ ++ ".")
  --          return ((DefVar id typ texp), env')
  --    Bad msg -> do
  --      saveLog msg
  --      texp <- inferExp exp env 
  --      if isTypeError texp || isDummy exp ||checkExp texp typ 
  --        then
  --          return ((DefVar id typ texp), env)
  --        else do
  --          saveLog ("Errore in posizione: " ++ printTree loc ++ ", " ++ printTree exp ++ "e' di tipo " ++ printTree (getType texp) ++ ", ma il tipo atteso e': " ++ printTree typ ++ ".")
  --          return ((DefVar id typ texp), env)

-------------------------------------------------------------------------------------------------------------------------------------------
  DefVar id@(PIdent (loc, ident)) typ exp -> case update env ident (VarInfo loc typ) of
    Ok env' -> defVarAux env'
    Bad msg -> do
      saveLog msg
      defVarAux env
    where
      defVarAux e = do
        texp <- inferExp exp env
        case isTypeError texp || isDummy exp ||checkExp texp typ of
          True -> return ((DefVar id typ texp), e)
          False -> do
            saveLog ("Errore (" ++ printTree loc ++ "): " ++ printTree exp ++ " e' di tipo " ++ printTree (getType texp) ++ ", ma il tipo atteso e' " ++ printTree typ ++ ".")
            return ((DefVar id typ texp), e)
-------------------------------------------------------------------------------------------------------------------------------------------

  --DefFun id@(PIdent (loc, ident)) params typ block@(DBlock stms) -> case update env ident (FunInfo loc typ params) of
  --  Ok env' -> do
  --    -- block di suo aggiunge un nuovo scope e fa le operazioni su di esso.
  --    -- quindi l'idea è di aggiungere i parametri come dichiarazioni in testa al blocco.
  --    -- lock = DBlock [Stm]
  --    tblock <- inferBlock (DBlock ((paramsToStms params)++stms)) typ env'
  --    if checkBlock tblock typ 
  --      then
  --        return ((DefFun id params typ tblock), env')
  --      else do
  --        -- manca un return giusto, se ce ne sono di sbagliati, inferBlock ce l'ha già detto.
  --        saveLog ("Attesa una istruzione return all'interno della funzione " ++ printTree ident ++ " dichiarata in posizione " ++ printTree loc ++ ", ma non trovata.")
  --        return ((DefFun id params typ tblock), env')
--
  --  Bad msg -> do
  --    saveLog msg
  --    tblock <- inferBlock (DBlock ((paramsToStms params)++stms)) typ env
  --    if checkBlock tblock typ 
  --      then
  --        return ((DefFun id params typ tblock), env)
  --      else do
  --        -- manca un return giusto, se ce ne sono di sbagliati, inferBlock ce l'ha già detto.
  --        saveLog ("Attesa una istruzione return all'interno della funzione " ++ printTree ident ++ " dichiarata in posizione " ++ printTree loc ++ ", ma non trovata.")
  --        return ((DefFun id params typ tblock), env)

-------------------------------------------------------------------------------------------------------------------------------------------
  DefFun id@(PIdent (loc, ident)) params typ block@(DBlock stms) -> case update env ident (FunInfo loc typ params) of
    Ok env' -> defFunAux env'
    Bad msg -> do
      saveLog msg
      defFunAux env
    where 
      defFunAux e = do
        tblock <- inferBlock (DBlock ((paramsToStms params)++stms)) typ e
        case (checkBlock tblock typ) of
          True -> return ((DefFun id params typ tblock), e)
          False -> do
            saveLog ("Errore (" ++ printTree loc  ++ "): attesa un'istruzione return all'interno della funzione " ++ printTree ident ++ ", ma non trovata.")
            return ((DefFun id params typ tblock), e)
-------------------------------------------------------------------------------------------------------------------------------------------


  -- Zucchero sintattico manuale e via.
  DefFunInLine id@(PIdent (loc, ident)) params typ exp -> inferDecl (DefFun id params typ (DBlock [SReturnExp (PReturn (loc , "return")) exp])) env


checkBlock :: Block -> TypeSpec -> Bool
checkBlock (BlockTyped _ typ' _) typ = typ == typ'
checkBlock _  _ = error $ "Errore interno chiamato da checkBlock().\n"

inferBlock :: Block -> TypeSpec -> Env -> Writer [String] Block
-- typ è il tipo che ci aspettiamo che il blocco abbia.
inferBlock (DBlock stms) typ env = do
  tstms <- inferStms stms typ (Env.addScope env) 
  case any (\x -> checkStm x typ) tstms of
    True ->  return (BlockTyped (DBlock tstms) typ (getLoc (head tstms)))
    False -> return (BlockTyped (DBlock tstms) (TSimple TypeVoid) (getLoc (head tstms))) 
      
inferStms :: [Stm] -> TypeSpec -> Env -> Writer [String] [Stm]
inferStms [] typ env = return []
inferStms (stm:stms) typ env = do
  (tstm, env') <- inferStm stm typ env
  tstms <- inferStms stms typ env'
  return (tstm:tstms)

inferStm :: Stm -> TypeSpec -> Env -> Writer [String] (Stm, Env)
inferStm stm typ env = case stm of
  SWhile exp stm' -> do
    -- texp è l'espressione annotata col tipo.
    texp <- inferExp exp env
    if isTypeError texp || checkExp texp (TSimple SType_Bool)
      then do
        (tstm', _) <- inferStm stm' typ env
        return ( (StmTyped (SWhile texp tstm') (getType tstm') (getLoc texp) ) , env)
      else do
        saveLog ("Errore (" ++ printTree (getLoc texp) ++ "): la condizione del while deve essere di tipo booleano, invece " ++ printTree exp 
          ++ " ha tipo " ++ printTree (getType texp) ++ ".")
        (tstm', _) <- inferStm stm' typ env
        return ( (StmTyped (SWhile texp tstm') (getType tstm') (getLoc texp)) , env )
  
  SIfElse exp stmif stmelse -> do
    texp <- inferExp exp env
    if not (isTypeError texp || checkExp texp (TSimple SType_Bool))
      then 
        saveLog ("Errore ("++ printTree (getLoc texp) ++ "): la condizione dell' if deve essere di tipo booleano, invece " ++ printTree exp 
          ++ " ha tipo " ++ printTree (getType texp) ++ ".")
      else return ()

    (tstmif, _) <- inferStm stmif typ env
    (tstmelse, _) <- inferStm stmelse typ env
    if checkStm tstmif typ || checkStm tstmelse typ 
      then return ( (StmTyped (SIfElse texp tstmif tstmelse) typ (getLoc texp)) , env)
      else return ( (StmTyped (SIfElse texp tstmif tstmelse) (TSimple TypeVoid) (getLoc texp) ) , env)

  SDecl decl -> do
    (tdecl, env') <- inferDecl decl env
    return ( (StmTyped (SDecl tdecl) (TSimple TypeVoid) (getLoc tdecl)), env' )

  SBlock block -> do
    tblock <- inferBlock block typ env
    return ((StmTyped (SBlock tblock) (getType tblock) (getLoc tblock) ), env)

  SAssign lexp exp -> do
    tlexp <- inferLExp lexp env
    texp <- inferExp exp env
    if isTypeError tlexp || isTypeError texp || checkExp texp (getType lexp)
      then return ( (StmTyped (SAssign tlexp texp) (TSimple TypeVoid) (getLoc tlexp)), env )
      else do
        saveLog ("Errore (" ++ printTree (getLoc texp) ++ "): l'espressione " ++ printTree exp ++" ha tipo " ++ printTree (getType texp) ++ ", ma " 
          ++ printTree lexp ++ " ha tipo " 
          ++ printTree (getType lexp) ++ ".")
        return ( (StmTyped (SAssign tlexp texp) (TSimple TypeVoid) (getLoc tlexp) ), env )

  SReturnExp preturn@(PReturn (loc, id)) exp -> do
    texp <- inferExp exp env
    case (isTypeError texp) of
      True -> return ((StmTyped (SReturnExp preturn texp) (TSimple TypeVoid) loc), env)
      False -> case ((checkExp texp typ), (isTypeVoid typ)) of

        (True,_) -> return ((StmTyped (SReturnExp preturn texp) typ loc), env)
        (False, True) -> do
          saveLog ("Errore (" ++ printTree (getLoc texp) ++ "): valore di ritorno inaspettato.")
          return ((StmTyped (SReturnExp preturn texp) (TSimple TypeVoid) loc, env))
        (False, False) -> do
          saveLog ("Errore (" ++ printTree (getLoc texp) ++ "): l'espressione " ++ printTree exp ++ " ha tipo " ++ printTree (getType texp) 
            ++ ", ma il tipo atteso e' " ++ printTree typ ++ ".")
          return ((StmTyped (SReturnExp preturn texp) (TSimple TypeVoid) loc, env))

  SReturn preturn@(PReturn (loc, _))-> do
    if not (isTypeVoid typ)
      then 
        saveLog ("Errore ("++ printTree loc ++"): l'operazione return non ha valore di ritorno, ma la funzione ha tipo "
          ++ printTree typ ++ ".")
      else
        return ()
    return ((StmTyped (SReturn preturn) (TSimple TypeVoid) loc), env)

                                    -- lista di liste ParExp [Exp]
  SProcCall id@(PIdent (loc, ident)) params -> do
    -- Tre possibili errori:
      -- 1. Il numero di clausole nella chiamata non corrisponde con il numero di clausole nella definizione,
      -- 2. Il numero di argomenti all'interno di una clausola non corrisponde con il numero di parametri della clausola,
      -- 3. Le dimensioni combaciano, ma almeno un'espressione passata come argomento ha tipo diverso da quello del corrispondente parametro.

    -- Possibile errore: la firma della funzione è questa, ed invece è stata chiamata con questa.
    -- questo errore è da lanciare solo al top level che è sull'uguaglianza delle liste.
    -- se any ha Type_Error allora manda su il Type_Error senza stampare che le firme non combaciano.

    -- [[TypeSpec]]
    tparams <- mapM (\(ParExp x) -> (mapM (\y -> (inferExp y env)) x)) params
    let typ_params = map (map getType) tparams in
      case Env.lookup env id of
        Ok (VarInfo dloc _) -> do
         -- è da mettere a posto sto errore.
         saveLog ("Errore (" ++ printTree loc ++ "): l'identificatore " ++ printTree ident
          ++ "e' stato utilizzato in posizione " ++ printTree dloc ++ "per dichiarare una variabile.")
         return ( (StmTyped (SProcCall id (map (\x -> (ParExp x)) tparams)) (TSimple TypeError) loc) , env )
        Bad msg -> do
          saveLog msg
          return ( (StmTyped (SProcCall id (map (\x -> (ParExp x)) tparams)) (TSimple TypeError) loc), env )
        Ok (FunInfo dloc typ paramclauses) -> 
          let typ_args = map (\(PArg x) -> (map (\(DArg ident typ) -> typ) x)) paramclauses in
            if any (isTypeError) (concat tparams)
              then return ( (StmTyped (SProcCall id (map (\x -> (ParExp x)) tparams)) (TSimple TypeError) loc) , env )
              else
                if typ_args == typ_params 
                  then
                    if isTypeVoid typ
                      then return ( (StmTyped (SProcCall id (map (\x -> (ParExp x)) tparams)) typ loc), env )
                      else do
                        saveLog ("Errore (" ++ printTree loc ++ "): il valore di ritorno della funzione "
                          ++ printTree ident ++ " non e' stato assegnato a nessuna variabile.")
                        return ( (StmTyped (SProcCall id (map (\x -> (ParExp x)) tparams)) (TSimple TypeVoid) loc), env )
                  else do
                    saveLog ("Errore (" ++ printTree loc ++ "): nella chiamata alla funzione "
                      ++ printTree ident ++ " la firma della funzione e': " ++ printTree typ_args 
                      ++ " mentre i parametri passati sono di tipo: " ++ printTree typ_params ++ ".")
                    return ( (StmTyped (SProcCall id (map (\x -> (ParExp x)) tparams)) (TSimple TypeError) loc), env )



checkStm :: Stm -> TypeSpec -> Bool
checkStm tstm typ = (getType tstm) == typ


getDLoc :: LExp -> Loc
getDLoc (LExpTyped _ _ _ dloc) = dloc

inferLExp :: LExp -> Env -> Writer [String] LExp
inferLExp lexp env = case lexp of
 LRef lexp' -> do
   tlexp' <- inferLExp lexp' env
   if isTypeError tlexp' 
     then return (LExpTyped (LRef tlexp') (TSimple TypeError) (getLoc tlexp') (getDLoc tlexp') )
     else
       case tlexp' of
         (LExpTyped _ (TPointer typ) loc dloc) -> return (LExpTyped (LRef tlexp') typ loc dloc)
         (LExpTyped _ typ' loc dloc) -> do
           saveLog ("Errore (" ++ printTree loc ++ "): impossibile applicare operatore * a " ++ printTree lexp' ++ " che ha tipo " ++ printTree typ' ++ ".")
           return (LExpTyped (LRef tlexp') (TSimple TypeError) loc dloc)

 LArr lexp exp -> do
   tlexp <- inferLExp lexp env
   texp <- inferExp exp env
   if isTypeError tlexp || isTypeError texp
     then
       return (LExpTyped (LArr tlexp texp) (TSimple TypeError) (getLoc tlexp) (getDLoc tlexp) )
     else
       case (tlexp , checkExp texp (TSimple SType_Int)) of
         (LExpTyped _ (TArray typ _) loc dloc, True) -> return (LExpTyped (LArr tlexp texp) typ loc dloc)
         (LExpTyped _ (TArray typ _) loc dloc, False) -> do
           saveLog ("Errore (" ++ printTree loc ++ "): l'indice di accesso ad un'array deve avere tipo intero, ma l'espressione " 
            ++ printTree exp ++ " ha tipo " ++ printTree (getType texp) ++ ".")
           return (LExpTyped (LArr tlexp texp) (TSimple TypeError) loc dloc)
         (_, False) -> do
           saveLog ("Errore (" ++ printTree (getLoc texp) ++ "): l'accesso tramite operatore [] puo' essere effettuato solo su elementi di tipo Array, mentre "
            ++ printTree lexp ++ " ha tipo " ++ printTree (getType tlexp) ++ ".")
           saveLog ("Errore (" ++ printTree (getLoc texp) ++ "): l'indice di accesso ad un'array deve avere tipo intero, ma l'espressione " 
            ++ printTree exp ++ " ha tipo " ++ printTree (getType texp) ++ ".")
           
           return (LExpTyped (LArr tlexp texp) (TSimple TypeError) (getLoc tlexp) (getDLoc tlexp))
         (_, True) -> do
           saveLog ("Errore (" ++ printTree (getLoc texp) ++ "): l'accesso tramite operatore [] puo' essere effettuato solo su elementi di tipo Array, mentre "
            ++ printTree lexp ++ " ha tipo " ++ printTree (getType tlexp) ++ ".")
           
           return (LExpTyped (LArr tlexp texp) (TSimple TypeError) (getLoc tlexp) (getDLoc tlexp))
 LIdent id@(PIdent (loc, ident)) -> let res = Env.lookup env id in
   case res of
     Bad msg -> do
       saveLog msg
       return (LExpTyped lexp (TSimple TypeError) loc (0,0))
     -- dloc dichiarazione loc
     Ok (VarInfo dloc typ) -> return (LExpTyped (LIdent id) typ loc dloc)
     Ok (FunInfo dloc _ _) -> do
       -- è da mettere a posto sto errore.
       saveLog ( "Errore (" ++ printTree loc ++ "): l'identificatore " ++ printTree ident
        ++ "e' stato utilizzato in posizione " ++ printTree dloc ++ "per dichiarare una funzione.")

       return (LExpTyped lexp (TSimple TypeError) loc dloc)



-- Prende una lista di espressioni tipizzate, un tipo, e ritorna una coppia con il primo elemento
-- che dice se si è trovato almeno un elemento con tipo (TSimple TypeError), ed il secondo elemento che dice
-- se tutte le espressioni hanno tipo type o meno.
inferArrayAux :: [Exp] -> TypeSpec -> (Bool, Bool)
inferArrayAux texps typ = ( (any (\x -> checkExp x (TSimple TypeError)) texps),(all (\x -> checkExp x typ) texps) )

checkExp :: Exp -> TypeSpec -> Bool
checkExp texp typ = (getType texp) == typ 

inferExp :: Exp -> Env -> Writer [String] Exp
inferExp exp env = case exp of
  DummyExp -> return (ETyped exp (TSimple TypeVoid) (0,0) )  
  -- Punto di vista di Bulzo: gli array di dim 0 non hanno alcuna utilità per il programmatore nel nostro linguaggio
  -- in cui non ci sono liste e non ci sono allocazioni dinamiche di memoria.
                                                                              -- posizione fittizia. Spunta da qualche parte?
  EArray [] -> return (ETyped (exp) (TArray (TSimple TypeVoid) (PInteger ( (0,0), show (0)  ) )) (0,0)  )
  EArray exps -> do
    texps <- mapM (\x -> inferExp x env) exps
    case inferArrayAux texps (getType (head texps) ) of
      (True, _) -> return (ETyped (EArray texps) (TArray (TSimple TypeError) (PInteger ( (getLoc (head texps)) , show (length texps)  ) )) (getLoc (head texps)) )
      (_, True) -> return (ETyped (EArray texps) (TArray (getType (head texps) )  (PInteger ( (getLoc (head texps)), show (length texps)  ))) (getLoc (head texps)) )
      (_ , _) -> do
        -- Da mettere un più bel messaggio di errore.
        -- ETyped Exp TypeSpec Integer Integer
        saveLog ("Errore(" ++ printTree (getLoc (head texps) ) ++ "): inconsistenza nei valori assegnati all'array in posizione.")
        -- bisogna far tornare l'espressione tipata, non la lunghezza della lisa texps
        return (ETyped (EArray texps) (TSimple TypeError) (getLoc (head texps)) )

                                    -- lista di liste ParExp [Exp]
  EFunCall id@(PIdent (loc, ident)) params -> do
    -- Tre possibili errori:
      -- 1. Il numero di clausole nella chiamata non corrisponde con il numero di clausole nella definizione,
      -- 2. Il numero di argomenti all'interno di una clausola non corrisponde con il numero di parametri della clausola,
      -- 3. Le dimensioni combaciano, ma almeno un'espressione passata come argomento ha tipo diverso da quello del corrispondente parametro.

    -- Possibile errore: la firma della funzione è questa, ed invece è stata chiamata con questa.
    -- questo errore è da lanciare solo al top level che è sull'uguaglianza delle liste.
    -- se any ha Type_Error allora manda su il Type_Error senza stampare che le firme non combaciano.

    -- [[TypeSpec]]
    tparams <- mapM (\(ParExp x) -> (mapM (\y -> (inferExp y env)) x)) params
    let typ_params = map (map getType) tparams in
      case Env.lookup env id of
        Ok (VarInfo dloc _) -> do
         -- è da mettere a posto sto errore.
         saveLog ("Errore (): l'identificatore " ++ printTree ident
          ++ "e' stato utilizzato in posizione " ++ printTree dloc ++ "per dichiarare una variabile.")
         return (ETyped (EFunCall id (map (\x -> (ParExp x)) tparams)) (TSimple TypeError) loc)
        Bad msg -> do
          saveLog msg
          return (ETyped (EFunCall id (map (\x -> (ParExp x)) tparams)) (TSimple TypeError) loc)
        Ok (FunInfo dloc typ paramclauses) ->
          let typ_args = map (\(PArg x) -> (map (\(DArg ident typ) -> typ) x)) paramclauses in
            if any (isTypeError) (concat tparams)
              then return (ETyped (EFunCall id (map (\x -> (ParExp x)) tparams)) (TSimple TypeError) loc)
              else
                if typ_args == typ_params 
                  then return (ETyped (EFunCall id (map (\x -> (ParExp x)) tparams)) typ loc)
                  else do
                    saveLog ("Errore (" ++ printTree loc ++ "): nella chiamata alla funzione "
                      ++ printTree ident ++ " la firma della funzione e': " ++ printTree (mapM (\x -> printTree x) typ_args) ++ ":" ++ printTree typ
                      ++ " mentre i parametri passati sono di tipo: " ++ printTree (mapM (\x -> printTree x) typ_params) ++ ".")
                    return (ETyped (EFunCall id (map (\x -> (ParExp x)) tparams)) (TSimple TypeError) loc)

  ENot exp -> do
    texp <- inferExp exp env
    if isTypeError texp || checkExp texp (TSimple SType_Bool)
      then return (ETyped (ENot texp) (getType texp) (getLoc texp))
      else do
        saveLog ("Errore (" ++ printTree (getLoc texp) ++ "): operatore ! applicato alla espressione " ++ printTree exp ++ ", che ha tipo "
          ++ printTree (getType texp) ++ ", ma era attesa di tipo " ++ printTree (TSimple SType_Bool) ++ ".")
        return ((ETyped (ENot texp) (TSimple TypeError) (getLoc texp)))

  ENeg exp -> do
    texp <- inferExp exp env
    if isTypeError texp || checkExp texp (TSimple SType_Int) || checkExp texp (TSimple SType_Float)
      then return (ETyped (ENeg texp) (getType texp) (getLoc texp))
      else do
        saveLog ("Errore (" ++ printTree (getLoc texp) ++ "): operatore - applicato alla espressione " ++ printTree exp ++ ", che ha tipo "
          ++ printTree (getType texp) ++ ", ma era attesa di tipo numerico.")
        return ((ETyped (ENeg texp) (TSimple TypeError) (getLoc texp)))
  
  ELExp lexp -> do
    tlexp <- inferLExp lexp env
    return (ETyped (ELExp tlexp) (getType tlexp) (getLoc tlexp))

  EDeref lexp -> do
    tlexp <- inferLExp lexp env
    if isTypeError tlexp
      then return (ETyped (EDeref tlexp) (TSimple TypeError) (getLoc tlexp))
      else return ( ETyped (EDeref tlexp) (TPointer (getType tlexp)) (getLoc tlexp) )

  EInt    const@(PInteger (loc, _)) -> return (ETyped (EInt const) (TSimple SType_Int) (loc) )
  EFloat  const@(PFloat (loc, _))   -> return (ETyped (EFloat const) (TSimple SType_Float) (loc) )
  EChar   const@(PChar (loc, _))    -> return (ETyped (EChar const) (TSimple SType_Char) (loc) )
  EString const@(PString (loc, _))  -> return (ETyped (EString const) (TSimple SType_String) (loc) )
  ETrue   const@(PTrue (loc, _))    -> return (ETyped (ETrue const) (TSimple SType_Bool) (loc) )
  EFalse  const@(PFalse (loc, _))   -> return (ETyped (EFalse const) (TSimple SType_Bool) (loc) )
  -- Ma serve Null? Con i puntatori probabilmente si, nel caso non avrebbe TypeVoid.
  ENull const@(PNull (loc, _)) -> return (ETyped (ENull const) (TSimple TypeVoid) loc )

  EOp expl op expr -> inferBinOp expl op expr env


-- Prese due espressioni ed un operatore binario ritorna la corrispondente espressione tipizzata
inferBinOp :: Exp -> Op -> Exp -> Env -> Writer [String] Exp
inferBinOp expl op expr env = do 
    texpl <- inferExp expl env
    texpr <- inferExp expr env

    case ((getTypeOp op), (getType texpl), (getType texpr) ) of
      (_ , TSimple TypeError, _ ) -> return (ETyped (EOp texpl op texpr) (TSimple TypeError) (getLoc texpl) )
      (_ , _ , TSimple TypeError) -> return (ETyped (EOp texpl op texpr) (TSimple TypeError) (getLoc texpl) )
      (EqOp, typl, typr) ->
        if isConsistent EqOp typl typr
          then return (ETyped (EOp texpl op texpr) (TSimple SType_Bool) (getLoc texpl))
          else returnBinOpError texpl op texpr
      (RelOp, typl, typr) ->
        if isConsistent RelOp typl typr
          then return (ETyped (EOp texpl op texpr) (TSimple SType_Bool) (getLoc texpl))
          else returnBinOpError texpl op texpr
      (_, typl, typr) ->
        if isConsistent (getTypeOp op) typl typr
          then return (ETyped (EOp texpl op texpr) typl (getLoc texpl))
          else returnBinOpError texpl op texpr

returnBinOpError :: Exp -> Op -> Exp -> Writer [String] Exp
returnBinOpError texpl op texpr = do
  saveLog ("Errore (" ++ printTree (getLoc texpl) ++ "): l'operatore " ++ printTree op ++ " non puo' essere applicato ad un'espressione di tipo " 
    ++ printTree (getType texpl) 
    ++ " e un'espressione di tipo " ++ printTree (getType texpr)  ++ ".")
  return (ETyped (EOp texpl op texpr) (TSimple TypeError) (getLoc texpl) )

isConsistent :: TypeOp -> TypeSpec -> TypeSpec -> Bool
isConsistent NumericOp typl typr = (typl == typr) && (checkNumericTyp typl)
isConsistent BooleanOp typl typr = (typl == typr) && (checkBooleanTyp typl)
isConsistent EqOp typl typr      = (typl == typr)
isConsistent RelOp typl typr     = (typl == typr) && (checkNumericTyp typl)

checkNumericTyp :: TypeSpec  -> Bool
checkNumericTyp typ = ( (typ == (TSimple SType_Int) )|| (typ == (TSimple SType_Float)) )

checkBooleanTyp :: TypeSpec -> Bool
checkBooleanTyp (TSimple SType_Bool) = True
checkBooleanTyp _ = False


data TypeOp = NumericOp | BooleanOp | EqOp | RelOp
getTypeOp :: Op -> TypeOp
getTypeOp Plus      = NumericOp
getTypeOp Minus     = NumericOp
getTypeOp Prod      = NumericOp
getTypeOp Div       = NumericOp
getTypeOp Mod       = NumericOp
getTypeOp Pow       = NumericOp
getTypeOp Or        = BooleanOp
getTypeOp And       = BooleanOp
getTypeOp Less      = RelOp
getTypeOp Greater   = RelOp
getTypeOp LessEq    = RelOp
getTypeOp GreaterEq = RelOp
getTypeOp Equal     = EqOp
getTypeOp NotEq     = EqOp
 


--
---- Tests:
--getEnv = do 
--  env1 <- updateVar emptyEnv (PIdent ((1,5),"x")) Type_int
--  updateVar env1 (PIdent ((1,22),"y")) SType_Float
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
--      Ok env -> checkExp (EAdd (EVar (PIdent ((1,49),"y"))) (EVar (PIdent ((1,51),"x")))) SType_Float env
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


