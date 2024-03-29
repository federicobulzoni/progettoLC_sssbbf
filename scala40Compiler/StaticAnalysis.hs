-- Modulo StaticAnalysis.hs
-- Il modulo partendo da un programma scritto nella sintassi astratta definita in AbsGramm.hs
-- verifica la presenza di errori statici all'interno di esso (incompatibilità di tipi, operazioni non consentite)
-- e nel frattempo si occupa di arricchire l'albero di sintassi astratta in input con informazioni aggiuntive,
-- quali ad esempio il tipo delle R-espressioni e delle L-espressioni.
-- La funzione principale del modulo è typeCheck che preso in input un programma in sintassi astratta
-- ritorna in output tale programma annotato e gli eventuali Warning ed Errori (LogElement) scovati durante l'annotazione.
module StaticAnalysis (genAnnotatedTree) where

import AbsGramm
import Environment as Env
import Control.Monad.Writer
import Errors
import Typed

type Logger a = Writer [LogElement] a

-- salva un elemento di log nella lista di elementi di log che sarà ritornata da typeCheck.
saveLog:: LogElement -> Logger ()
saveLog logelem = do 
  tell [logelem]
  return ()

-- preso un tipo ritorna True se tale tipo è Void, False altrimenti.
isTypeVoid :: TypeSpec -> Bool
isTypeVoid typ = typ == (TSimple SType_Void)

compatible :: TypeSpec -> TypeSpec -> Bool
compatible typ_exp (TSimple SType_Int)    = elem typ_exp [TSimple SType_Int, TSimple SType_Char, TSimple SType_Bool]
compatible typ_exp (TSimple SType_Float)  = elem typ_exp [TSimple SType_Float, TSimple SType_Int, TSimple SType_Char, TSimple SType_Bool]
compatible typ_exp (TSimple SType_Char)   = elem typ_exp [TSimple SType_Char, TSimple SType_Bool]
compatible typ_exp (TSimple SType_Bool)   = elem typ_exp [TSimple SType_Bool]
compatible typ_exp (TSimple SType_String) = elem typ_exp [TSimple SType_String]
compatible typ_exp (TSimple _)            = False

compatible typ_exp (TPointer typ) = case typ_exp of
  TPointer (TSimple SType_Void) -> True
  (TPointer typ')               -> compatible typ' typ
  _                             -> False

compatible typ_exp (TArray typ2 (PInteger (_,dim2))) = case typ_exp of
  (TArray typ1 (PInteger (_,dim1))) -> dim1 == dim2 && (compatible typ1 typ2)
  _                                 -> False


checkExpsMod :: [Exp] -> [(TypeSpec, ParamPassMod)] -> Logger Bool
checkExpsMod [] [] = return True
checkExpsMod [] _ = return False
checkExpsMod _ [] = return False
checkExpsMod (e:params') ((t,m):args') = do
  sameMod <- checkExpMod e m
  res <- checkExpsMod params' args'
  if sameMod == True
    then return $ res && True
    else  do
      saveLog $ launchError (getLoc e) (WrongParameterPassMod e m)
      return $ res && False
  where
    checkExpMod :: Exp -> ParamPassMod -> Logger Bool
    checkExpMod texp m = case (texp,m) of
      (ExpTyped (ELExp _) _ _, m) -> return True
      (_, ParamPassMod_val)      -> return True
      (_, _)                     -> return False
    



-- Presa una espressione tipata texp, ed un tipo richiesto typ, ritorna True se la espressione tipata
-- è compatibile con il tipo richiesto; altrimenti ritorna False.
-- All'interno di questa funzione è possibile differenziare le compatibilità per i diversi tipi presenti
-- nella grammatica astratta.
isCompatible :: Exp -> TypeSpec -> Bool
isCompatible texp typ = getType texp <= typ

allCompatible :: [[Exp]] -> [[TypeSpec]] -> Bool
allCompatible [] [] = True
allCompatible [] _ = False
allCompatible _ [] = False
allCompatible (x:xs) (y:ys) = length x == length y && (all (==True) (zipWith (compatible) (map (getType) x) y)) && (allCompatible xs ys)
            

-- Definizione dell'environment iniziale di un programma. Contiene le informazioni a riguardo delle
-- funzioni native presenti nel linguaggio.
startingEnv :: Env
startingEnv = 
  let (Success env) = foldM (\x (ident, info) -> Env.update x ident info) (Env.emptyEnv) initialFuns
  in
    env
  where
    initialFuns = [
      ("writeInt",    FunInfo (0,0) (TSimple SType_Void) [PParam [DParam (ParamPassMod_val) (PIdent ((0,0),"i")) (TSimple SType_Int) ]]),
      ("writeFloat",  FunInfo (0,0) (TSimple SType_Void) [PParam [DParam (ParamPassMod_val) (PIdent ((0,0),"f")) (TSimple SType_Float) ]]),
      ("writeChar",   FunInfo (0,0) (TSimple SType_Void) [PParam [DParam (ParamPassMod_val) (PIdent ((0,0),"c")) (TSimple SType_Char) ]]),
      ("writeString", FunInfo (0,0) (TSimple SType_Void) [PParam [DParam (ParamPassMod_val) (PIdent ((0,0),"s")) (TSimple SType_String) ]]),

      ("readInt",     FunInfo (0,0) (TSimple SType_Int)    [PParam []]),
      ("readFloat",   FunInfo (0,0) (TSimple SType_Float)  [PParam []]),
      ("readChar",    FunInfo (0,0) (TSimple SType_Char)   [PParam []]),
      ("readString",  FunInfo (0,0) (TSimple SType_String) [PParam []]) ]

-- Si occupa di inizializzare l'environment prima dell'inferenza degli statement contenuti nel suo body.
startFunScope :: Env -> PIdent -> [ParamClause] -> TypeSpec -> Logger Env
startFunScope env id@(PIdent (loc, ident)) params typ = do
  case foldM (\x (ident', info) -> Env.update x ident' info) (Env.addScope env typ) funInfo of
    Success env' -> return env'
    Failure except -> do
      saveLog $ launchError loc except
      return env
  where
    argsInfo = map (\(DParam passMod argId@(PIdent (argLoc, argIdent)) argTyp) -> (argIdent, VarInfo argLoc argTyp passMod)) (concat ( map (\(PParam args) -> args) params ))
    funInfo = (ident, FunInfo loc typ params):argsInfo

------------------------------------------------------------------------------------------------------------------------

genAnnotatedTree :: Program -> (Program, [LogElement])
genAnnotatedTree prog = runWriter $ typeCheck prog

-- Dato un programma scritto in sintassi astratta restituisce una lista di log ed un programma annotato.
typeCheck :: Program -> Logger Program
typeCheck (Prog decls) = do
  (tdecls, env) <- inferDecls decls startingEnv
  -- L'environment globale ha il campo booleano a True se e solo se è stato trovato un
  -- main tra le dichiarazioni globali.
  if not $ Env.hasReturn env then do
    saveLog $ launchWarning (0,0) MissingMain
    return $ Prog tdecls
  else
    return $ Prog tdecls


inferDecls :: [Declaration] -> Env -> Logger ([Declaration], Env)
inferDecls [] env = return ([], env)
inferDecls (decl:decls) env = do
  (tdecl , env') <-  inferDecl decl env
  (tdecls, env'') <- inferDecls decls env'
  return ((tdecl:tdecls), env'')

inferDecl :: Declaration -> Env -> Logger (Declaration, Env)
inferDecl decl env = case decl of
  DefVar id@(PIdent (loc, ident)) typ exp -> 
    -- Si prova ad inserire i dati della variabile nell'environment ...
    case (Env.update env ident (VarInfo loc typ NoParam)) of
      Success env' -> do
        -- ... se è stato possibile, ...
        texp <- inferExp exp env
        -- ... allora si verifica che la espressione che si sta cercando di assegnare
        -- è compatibile con il tipo della variabile. Se il tipo della espressione è un typeError
        -- non abbiamo bisogno di lanciare nuovi errori (questa è una costante lungo tutto il codice).
        if isTypeError texp || compatible (getType texp) typ
          then
            return $ (DefVar id typ texp, env')
          else do
            saveLog $ launchError (getLoc texp) (WrongExpType exp (getType texp) typ)
            return $ (DefVar id typ texp, env')
      Failure except -> do
        -- Se qualcosa è andato storto si lancia l'errore.
        saveLog $ launchError loc except
        texp <- inferExp exp env
        return $ (DefVar id typ texp, env)
-------------------------------------------------------------------------------------------------------------------------------------------
  DecVar id@(PIdent (loc, ident)) typ -> 
    case Env.update env ident (VarInfo loc typ NoParam) of
      Success env' -> return $ (DecVar id typ, env')
      Failure except -> do
        saveLog $ launchError loc except
        return $ (DecVar id typ, env)
----------------------------------------------------------------------------------------------------------------------------
  DefFun id@(PIdent (loc, ident)) params typ block@(DBlock stms) -> 
    case update env ident (FunInfo loc typ params) of
      Success env' -> do
        -- Se la dichiarazione che abbiamo appena inserito nello scope è la funzione main
        -- e se lo scope corrente è quello globale, allora abbiamo trovato il main del programma,
        -- lo notifichiamo grazie alla funzione Env.setReturnFound.
        if ident == "main" && Env.isGlobalScope env' then
          if (not (isTypeVoid typ)) || (notEmptyParams params)
            then do
              saveLog $ launchError loc WrongMainSignature
              functionHandler env'
            else
              functionHandler (Env.setReturnFound env')
        else
          functionHandler env'
      Failure except -> do
        saveLog $ launchError loc except
        functionHandler env
      where 
        functionHandler e = do
          -- Creare nuovo scope avviato con i parametri e il nome della fun.
          newScope <- startFunScope e id params typ
          (tstms, e') <- inferStms stms newScope False

          -- Nel caso in cui stiam trattando una funzione, ma non è presente alcun
          -- return nel suo scope lo notifichiamo.
          if Env.hasReturn e' || isTypeVoid typ
            then return $ (DefFun id params typ (DBlock tstms), e)
            else do
              saveLog $ launchWarning loc (MissingReturn ident)
              return $ (DefFun id params typ (DBlock tstms), e)

        notEmptyParams [PParam []] = False
        notEmptyParams par = True
-------------------------------------------------------------------------------------------------------------------------------------------
  DefFunInLine id@(PIdent (loc, ident)) params typ exp -> 
    case update env ident (FunInfo loc typ params) of
      Success env' -> do
        if ident == "main" && Env.isGlobalScope env' then
          if (not (isTypeVoid typ)) || (notEmptyParams params)
            then do
              saveLog $ launchError loc WrongMainSignature
              functionHandler env'
            else
              functionHandler (Env.setReturnFound env')
        else
          functionHandler env'

      Failure except -> do
        saveLog $ launchError loc except
        functionHandler env
      where 
        functionHandler e = do
          case (exp, isTypeVoid typ) of
            -- PROCEDURA con assegnata FUNZIONE
            (EFunCall id' params', True) -> do
              (stmt, e') <- inferStm (SProcCall id' params') e False
              return (DefFun id params typ (DBlock [stmt]), e')
            -- PROCEDURA con assegnata EXP
            (_,True) -> do
              e' <-  startFunScope e id params typ
              texp <- inferExp exp e'
              saveLog $ launchError loc (ExpAssignedToProcedure ident exp (getType texp))
              return (DefFun id params typ (DBlock [SReturnExp (PReturn (loc , "return")) texp]), e)
            -- FUNZIONE
            (_,False) -> do
              e' <- startFunScope e id params typ
              texp <- inferExp exp e'
              if isTypeError texp || compatible (getType texp) typ 
                then
                  return (DefFun id params typ (DBlock [SReturnExp (PReturn (loc , "return")) texp]), e)
                else do
                  saveLog $ launchError loc (WrongExpType exp (getType texp) typ)
                  return (DefFun id params typ (DBlock [SReturnExp (PReturn (loc , "return")) texp]), e)

        notEmptyParams [PParam []] = False
        notEmptyParams par = True

inferStms :: [Stm] -> Env -> Bool -> Logger ([Stm], Env)
inferStms [] env _ = return ([], env)
inferStms (stm:stms) env inLoop = do
  (tstm, env') <- inferStm stm env inLoop
  (tstms, env'') <- inferStms stms env' inLoop
  return $ (tstm:tstms, env'') 


inferStm :: Stm  -> Env -> Bool -> Logger (Stm, Env)
inferStm stm env inLoop = case stm of
  SWhile exp stm' -> do
    -- texp è l'espressione annotata col tipo.
    texp <- inferExp exp env
    -- Se la condizione non ha tipo compatibile con Bool, allora si lancia un errore.
    if not (isTypeError texp || compatible (getType texp) (TSimple SType_Bool))
      then
        saveLog $ launchError (getLoc texp) (WrongWhileCondition exp (getType texp))
      else
        return ()
    -- viene inferito lo stm del while.
    (tstm', env') <- inferStm stm' env True
    -- e nel caso in cui un return sia stato trovato all'interno del corpo del while
    -- si segnala che è stato trovato anche nello scope in cui il while è contenuto.
    if Env.hasReturn env' 
      then
        return $ (SWhile texp tstm', Env.setReturnFound env)
      else
        return $ (SWhile texp tstm', env)
 
  -------------------------------------------------------------------------------------------------------------------------------------------
  SIfElse exp stmif stmelse -> do
    texp <- inferExp exp env
    if not (isTypeError texp || compatible (getType texp) (TSimple SType_Bool))
      then 
        saveLog $ launchError (getLoc texp) (WrongIfCondition exp (getType texp))
      else return ()

    (tstmif, envif) <- inferStm stmif env inLoop
    (tstmelse, envelse) <- inferStm stmelse env inLoop
    -- Solo nel caso in cui sia il corpo dell'if che quello dell'else contengono
    -- una istruzione return viene segnalata la presenza di un return nello scope in 
    -- cui la istruzione if...else è contenuta.
    if Env.hasReturn envif && Env.hasReturn envelse then
      return $ (SIfElse texp tstmif tstmelse, Env.setReturnFound env)
    else 
      return $ (SIfElse texp tstmif tstmelse, env)

-------------------------------------------------------------------------------------------------------------------------------------------
  SDecl decl -> do
    (tdecl, env') <- inferDecl decl env
    return $ (SDecl tdecl, env')

-------------------------------------------------------------------------------------------------------------------------------------------
  SBlock block -> do
    (tblock, env') <- inferBlock block (Env.getScopeType env) env
    if Env.hasReturn env'
      then
        return $ (SBlock tblock, Env.setReturnFound env)
      else
        return $ (SBlock tblock, env)
    where
      inferBlock (DBlock []) _ env = return $ (DBlock [], env)
      inferBlock (DBlock stms) ftyp env = do
        (tstms, env') <- inferStms stms (Env.addScope env ftyp) inLoop
        return $ (DBlock tstms, env')

-------------------------------------------------------------------------------------------------------------------------------------------
  SAssign lexp exp -> do
    tlexp <- inferLExp lexp env
    texp <- inferExp exp env
    if not (isTypeError tlexp || isTypeError texp || compatible (getType texp) (getType tlexp))
      then 
        saveLog $ launchError (getLoc texp) (WrongExpAssignType exp (getType texp) (getType tlexp) lexp)
      else
        return ()
    return $ (SAssign tlexp texp, env)

-------------------------------------------------------------------------------------------------------------------------------------------
  SReturnExp preturn@(PReturn (loc, id)) exp ->
    do
      texp <- inferExp exp env
      checkError texp env

      where
        checkError texp env = case isTypeError texp of
          True -> return $ (SReturnExp preturn texp, env)
          False -> checkTypeVoid texp (Env.getScopeType env) env
        
        checkTypeVoid texp ftyp env = case isTypeVoid ftyp of
          True ->  do
            -- Se il tipo dello scope è Void, allora ci troviamo all'interno di una procedura
            -- in questo caso viene lanciata un eccezione dato che stiamo usando un return con valore.
            saveLog $ launchError loc (UnexpectedReturn exp)
            return $ (SReturnExp preturn texp, env)
          False -> checkCompatible texp ftyp env
        
        checkCompatible texp ftyp env = case compatible (getType texp) ftyp of
          True -> return $ (SReturnExp preturn texp, Env.setReturnFound env)
          False -> do
            -- se il tipo dello scope non è void, ma è incompatibile con quello dell'espressione
            -- allora si lancia un'errore.
            saveLog $ launchError loc (WrongExpType exp (getType texp) ftyp)
            return $ (SReturnExp preturn texp, env)

-------------------------------------------------------------------------------------------------------------------------------------------
  SReturn preturn@(PReturn (loc, _))-> let ftyp = Env.getScopeType env in
    if not (isTypeVoid ftyp)
      -- Se un return senza valore di ritorno viene utilizzato in una funzione lo si segnala come errore.
      then do
        saveLog $ launchError loc (WrongReturnValue ftyp)
        return $ (SReturn preturn, env)
      else
        return $ (SReturn preturn, env)

-------------------------------------------------------------------------------------------------------------------------------------------
  SBreak pbreak@(PBreak (loc,ident)) -> do
    if inLoop
      then return (SBreak pbreak, env)
      else do
        saveLog $ launchError loc (WrongFlowCrontrolStatement ident)
        return (SBreak pbreak, env)

  SContinue pcontinue@(PContinue (loc,ident)) -> do
    if inLoop
      then return (SContinue pcontinue, env)
      else do
        saveLog $ launchError loc (WrongFlowCrontrolStatement ident)
        return (SContinue pcontinue, env)
-------------------------------------------------------------------------------------------------------------------------------------------
  SProcCall id@(PIdent (loc, ident)) params -> do
    -- Tre possibili errori:
      -- 1. Il numero di clausole nella chiamata non corrisponde con il numero di clausole nella definizione,
      -- 2. Il numero di argomenti all'interno di una clausola non corrisponde con il numero di parametri della clausola,
      -- 3. Le dimensioni combaciano, ma almeno un'espressione passata come argomento ha tipo diverso da quello del corrispondente parametro.

    tparams <- mapM (\(ArgExp x) -> (mapM (\y -> (inferExp y env)) x)) params
    case Env.lookup env id of
      -- Caso in cui l'identificatore usato nella chiamata di procedura sia assegnato ad una variabile.
      Success (VarInfo dloc _ _) -> do
        saveLog $ launchError loc (VariableUsedAsProcedure ident dloc)
        -- Notare come l'identificatore che viene ritornato non sia lo stesso che ci arriva in input,
        -- bensì la locazione dell'identificatore viene sostituita diventando quella di dichiarazione
        -- dell'identificatore.
        return (SProcCall (PIdent (dloc, ident)) (map (\x -> (ArgExp x)) tparams) , env)
      Failure except -> do
        saveLog $ launchError loc except
        return (SProcCall (PIdent ((0,0), ident)) (map (\x -> (ArgExp x)) tparams) , env)
      
      Success (FunInfo dloc typ paramclauses) ->
        -- typ_args è il corrispettivo di typ_params, i due devono combaciare per poter affermare
        -- che la chiamata di procedura è valida.
        let 
            typ_mod_args = map (\(PParam x) -> (map (\(DParam passMod ident typ) -> (typ,passMod)) x)) paramclauses
            typ_args = map (\x -> ( map (\(t,m) ->t) x ) ) typ_mod_args
        in do
          case (any (isTypeError) (concat tparams), not $ allCompatible tparams typ_args, not (isTypeVoid typ)) of
            (True,_,_) -> return ()
            (_,True,False) -> saveLog $ launchError loc (WrongProcParams ident typ_args (map (map getType) tparams))
            (_,True,True) -> saveLog $ launchError loc (WrongFunctionParams ident typ_args (map (map getType) tparams) typ)
            (_,False,True) -> saveLog $ launchWarning loc (UnusedReturnValue ident)
            otherwise -> return ()

          checkModPassCorrect <- checkExpsMod (concat tparams) (concat typ_mod_args)
          if checkModPassCorrect
            then return (SProcCall (PIdent (dloc, ident)) (zipWith (\x y-> (ArgExpTyped (zipWith (\e (t,m)->(e,t,m)) x y))) tparams typ_mod_args) , env)
            else return (SProcCall id params,env)

-------------------------------------------------------------------------------------------------------------------------------------------

  SFor id@(PIdent (loc, ident)) exp_init exp_end exp_step stm -> do
    texp_init <- inferExp exp_init env
    texp_end  <- inferExp exp_end env
    texp_step <- inferExp exp_step env
    -- per ogni expression viene eseguito il controllo che siano di tipo compatibile con il tipo intero
    mapM (checkCompatible) [texp_init, texp_end, texp_step]


    (tstm, env') <- inferStm (addIteratorDec stm) env False

    -- controllo per vedere se nel ciclo ci fosse un return
    if Env.hasReturn env'
      then return ( SFor id texp_init texp_end texp_step (getForStm stm tstm), Env.setReturnFound env)
      else return ( SFor id texp_init texp_end texp_step (getForStm stm tstm), env)

    where
      -- funzione per estrarre la prima istruzione dal blocco inferito (la dichiarazione dell'iteratore
      -- inserito in testa in precedenza) e ritornare il resto degli statement: il blocco se lo statement 
      -- era un blocco, il singolo statement altrimenti
      getForStm (SBlock (DBlock stms)) (SBlock (DBlock (s:tstms))) = SBlock $ DBlock tstms
      getForStm _ (SBlock (DBlock (s1:[s2]))) = s2
      -- viene generato il blocco di statement che verrà analizzato da inferStm:
      --  - se lo stm è un blocco aggiungo in testa la dichiarazione dell'iteratore (per evitare che venga ri-dichiarato nel blocco)
      --  - se non è un blocco, si crea un blocco con in testa la dichiarazione della variabile iteratore
      addIteratorDec (SBlock (DBlock stms)) = SBlock $ DBlock $ (SDecl $ DecVar id (TSimple SType_Int)):stms
      addIteratorDec _                      = SBlock $ DBlock $ [(SDecl $ DecVar id (TSimple SType_Int)), stm]

      -- funzione per controllare la compatibilità dei tipi con il tipo intero
      checkCompatible texp@(ExpTyped exp _ _) = do
        if not $ compatible (getType texp) (TSimple SType_Int)
          then saveLog $ launchError loc (WrongExpType exp (getType texp) (TSimple SType_Int) )
          else return ()

-------------------------------------------------------------------------------------------------------------------------------------------

  SDoWhile stm exp -> do
    texp <- inferExp exp env
    if not (isTypeError texp || compatible (getType texp) (TSimple SType_Bool))
      then saveLog $ launchError (getLoc texp) (WrongWhileCondition exp (getType texp))
      else return ()
    (tstm, env') <- inferStm stm env True
    if Env.hasReturn env'
      then return $ (SDoWhile tstm texp, Env.setReturnFound env)
      else return $ (SDoWhile tstm texp, env)
      
    
inferLExp :: LExp -> Env -> Logger LExp
inferLExp lexp env = case lexp of
  LRef lexp' -> do
    tlexp' <- inferLExp lexp' env
    if isTypeError tlexp' 
      then 
        return $ LExpTyped (LRef tlexp') (TSimple SType_Error) (getLoc tlexp')
      else
        case tlexp' of
          -- Quando l'operatore di referenziazione * è applicato ad x di tipo puntatore a typ, il tipo di 
          -- *x è typ.
          -- Si noti che in questo caso la LExpTyped viene costruita senza passare la lexp annidata tipata,
          -- questo viene fatto per mantenere leggera la rappresentazione evitando inutile ridondanza.
          (LExpTyped _ (TPointer typ) loc) -> return $ LExpTyped (LRef tlexp') typ loc
          -- Se invece viene applicato ad un qualcosa che non è di tipo puntatore allora si lancia un errore.
          (LExpTyped _ typ' loc) -> do
            saveLog $ launchError loc (WrongPointerApplication lexp' typ')
            return $ LExpTyped (LRef tlexp') (TSimple SType_Error) loc

-------------------------------------------------------------------------------------------------------------------------------------------
  LArr lexp exp -> do
     tlexp <- inferLExp lexp env
     -- Espressione contenente la cella a cui si vuole accedere.
     texp <- inferExp exp env
     if isTypeError tlexp || isTypeError texp
       then
          return $ LExpTyped (LArr tlexp texp) (TSimple SType_Error) (getLoc tlexp)
       else
         -- se l'accesso viene effettuato su di un array di tipo "Array typ dim", l'elemento
         -- acceduto ha tipo typ.
         -- Nel caso l'accesso venga effettuato su di una variabile che non ha tipo typ bisogna notificare l'errore.
         -- Un ulteriore vincolo è dato dal tipo dell'espressione contenente l'indice di accesso, che deve avere tipo
         -- compatibile con SType_Int.
         case (tlexp , compatible (getType texp) (TSimple SType_Int)) of
           (LExpTyped _ (TArray typ _) loc, True) -> return $ LExpTyped (LArr tlexp texp) typ loc
           (LExpTyped _ (TArray typ _) loc, False) -> do
             saveLog $ launchError loc (ArraySubscriptNotInt exp (getType texp))
             return $ LExpTyped (LArr tlexp texp) (TSimple SType_Error) loc
           (_, False) -> do
             saveLog $ launchError (getLoc texp) (WrongArrayAccess  lexp (getType tlexp))
             saveLog $ launchError (getLoc texp) (ArraySubscriptNotInt exp (getType texp))
             return  $ LExpTyped (LArr tlexp texp) (TSimple SType_Error) (getLoc tlexp)
           (_, True) -> do
             saveLog $ launchError (getLoc texp) (WrongArrayAccess lexp (getType tlexp))
             return  $ LExpTyped (LArr tlexp texp) (TSimple SType_Error) (getLoc tlexp)

-------------------------------------------------------------------------------------------------------------------------------------------
  LIdent id@(PIdent (loc, ident)) -> let res = Env.lookup env id in
    -- Come prima cosa si verifica che l'identificatore sia effettivamente presente nell'environment.
    case res of
      Failure except -> do
        saveLog $ launchError loc except
        return $ LExpTyped (LIdentMod (PIdent ((0,0), ident)) NoParam) (TSimple SType_Error) loc
      -- Si noti che nella LExpTyped, la locazione dell'identificatore, che quando arriva dal parser
      -- corrisponde a quella relativa all'utilizzo corrente dell'identificatore nella LExp considerata,
      -- viene sostituito con la locazione in cui tale identificatore è stato dichiarato.
      Success (VarInfo dloc typ mode) -> return $ LExpTyped (LIdentMod (PIdent (dloc, ident)) mode) typ loc
      Success (FunInfo dloc _ _) -> do
        saveLog $ launchError loc (FunctionUsedAsVariable ident dloc)
        return $ LExpTyped (LIdentMod (PIdent (dloc, ident)) NoParam) (TSimple SType_Error) loc

inferExp :: Exp -> Env -> Logger Exp
inferExp exp env = case exp of                                                                       
  EArray exps -> do
    -- Vengono tipizzate tutte le espressioni contenute nell'espressione Array(exp1, ..., expn)
    texps <- mapM (\x -> inferExp x env) exps
    -- Se non è presente alcuna espressione viene ritornato un array vuoto a cui si assegna tipo SType_Void.
    if length texps == 0 
      then
        return $ ExpTyped (exp) (TArray (TSimple SType_Void) (PInteger ( (0,0), show (0)  ) )) (0,0) 
      else 
        -- Altrimenti se nessuna espressione ha errori al proprio interno,
        -- e se tutte le espressioni hanno tipo compatibile tra loro, allora l'espressione Array(exp1, ..., expn)
        -- ha il tipo delle sotto-espressioni exp1, ..., expn e dimensione <lunghezza exps>.
        let (anyError, allCompatible) = inferArrayAux texps in
          case (anyError, allCompatible) of
            (True, _) -> return $ ExpTyped (EArray texps) (TArray (TSimple SType_Error) (PInteger ( (getLoc (head texps)) , show (length texps)  ) )) (getLoc (head texps)) 
            (_, True) -> return $ ExpTyped (EArray texps) (TArray (maximum (map getType texps))  (PInteger ( (getLoc (head texps)), show (length texps)  ))) (getLoc (head texps))
            (_ , _) -> do
              saveLog $ launchError (getLoc (head texps)) ArrayInconsistency
              return $ ExpTyped (EArray texps) (TSimple SType_Error) (getLoc (head texps))
        where
          -- Prende una lista di espressioni tipate, un tipo, e ritorna una coppia con il primo elemento
          -- che dice se si è trovato almeno un elemento con tipo (TSimple SType_Error), ed il secondo elemento che dice
          -- se tutte le espressioni hanno tipo type o meno.
          inferArrayAux texps = ( any (\x -> getType x == (TSimple SType_Error)) texps, all (\x -> compatible (getType x) (maximum (map getType texps))) texps)


-------------------------------------------------------------------------------------------------------------------------------------------

  -- Molto simile a SProcCall, per dubbi riferirsi ai commenti di quest'ultima.
  EFunCall id@(PIdent (loc, ident)) params -> do
    -- Tre possibili errori:
      -- 1. Il numero di clausole nella chiamata non corrisponde con il numero di clausole nella definizione,
      -- 2. Il numero di argomenti all'interno di una clausola non corrisponde con il numero di parametri della clausola,
      -- 3. Le dimensioni combaciano, ma almeno un'espressione passata come argomento ha tipo diverso da quello del corrispondente parametro.

    -- [[TypeSpec]]
    -- Parametri che passiamo alla funzione.
    tparams <- mapM (\(ArgExp x) -> (mapM (\y -> (inferExp y env)) x)) params
    case Env.lookup env id of
      Success (VarInfo dloc _ _) -> do
        saveLog $ launchError loc (VariableUsedAsFunction ident dloc)
        return $ ExpTyped (EFunCall (PIdent (dloc, ident)) (map (\x -> (ArgExp x)) tparams)) (TSimple SType_Error) loc
      Failure except -> do
        saveLog $ launchError loc except
        return $ ExpTyped (EFunCall (PIdent ((0,0), ident)) (map (\x -> (ArgExp x)) tparams)) (TSimple SType_Error) loc
      Success (FunInfo dloc typ paramclauses) -> do
        checkModPassCorrect <- checkExpsMod (concat tparams) (concat (map (\(PParam x) -> (map (\(DParam passMod ident typ) -> (typ,passMod)) x)) paramclauses))
        let
          typ_mod_args = map (\(PParam x) -> (map (\(DParam passMod ident typ) -> (typ,passMod)) x)) paramclauses
          typ_args = map (\x -> ( map (\(t,m) ->t) x ) ) typ_mod_args
          paramsCorrect = checkModPassCorrect && (allCompatible tparams typ_args)
          isProc = isTypeVoid typ
        -- controllo che i parametri abbiano il tipo corretto corretto 
        -- e siano di numero giusto
        if not paramsCorrect
          then
            saveLog $ launchError loc (WrongFunctionParams ident typ_args (map (map getType) tparams) typ)
          else
            return ()
        -- controllo che sia una funzione e non una procedura
        if isProc
          then
            saveLog $ launchError loc (UnexpectedProc ident)
          else
            return ()
        -- se è stato restituito un messaggio di errore si ritorna la funzione tipata con l'errore
        if not paramsCorrect || isProc || errorFound tparams
          then
            returnFunCallWithType (TSimple SType_Error) typ_mod_args
          else
            returnFunCallWithType typ typ_mod_args
        where 
          returnFunCallWithType typ' typ_mod_args = return $ ExpTyped (EFunCall (PIdent (dloc, ident)) (zipWith (\x y-> (ArgExpTyped (zipWith (\e (t,m)->(e,t,m)) x y))) tparams typ_mod_args)) typ' loc
          errorFound params' = any (isTypeError) (concat params')
 -------------------------------------------------------------------------------------------------------------------------------------------
  ENot exp -> do
   texp <- inferExp exp env
   if isTypeError texp || compatible (getType texp) (TSimple SType_Bool)
     then return (ExpTyped (ENot texp) (getType texp) (getLoc texp))
     else do
       saveLog $ launchError (getLoc texp) (WrongNotApplication exp (getType texp))
       return $ ExpTyped (ENot texp) (TSimple SType_Error) (getLoc texp)

-------------------------------------------------------------------------------------------------------------------------------------------
  ENeg exp -> do
    texp <- inferExp exp env
    if isTypeError texp
      then
        return $ ExpTyped (ENeg texp) (TSimple SType_Error) (getLoc texp)
      else
        if compatible (getType texp) (TSimple SType_Float)
          then
            if getType texp == (TSimple SType_Float)
              then
                return $ ExpTyped (ENeg texp) (TSimple SType_Float) (getLoc texp)
              else
                return $ ExpTyped (ENeg texp) (TSimple SType_Int) (getLoc texp)
          else do
            saveLog $ launchError (getLoc texp) (WrongNegApplication exp (getType texp))
            return $ (ExpTyped (ENeg texp) (TSimple SType_Error) (getLoc texp))

-------------------------------------------------------------------------------------------------------------------------------------------
  ELExp lexp -> do
    tlexp <- inferLExp lexp env
    return $ ExpTyped (ELExp tlexp) (getType tlexp) (getLoc tlexp)

-------------------------------------------------------------------------------------------------------------------------------------------
  EDeref lexp -> do
    tlexp <- inferLExp lexp env
    if isTypeError tlexp
      then return $ ExpTyped (EDeref tlexp) (TSimple SType_Error) (getLoc tlexp)
      else return $ ExpTyped (EDeref tlexp) (TPointer (getType tlexp)) (getLoc tlexp)

-------------------------------------------------------------------------------------------------------------------------------------------
  EInt    const@(PInteger (loc, _)) -> return $ ExpTyped (EInt const) (TSimple SType_Int) (loc) 
  EFloat  const@(PFloat (loc, _))   -> return $ ExpTyped (EFloat const) (TSimple SType_Float) (loc) 
  EChar   const@(PChar (loc, _))    -> return $ ExpTyped (EChar const) (TSimple SType_Char) (loc) 
  EString const@(PString (loc, _))  -> return $ ExpTyped (EString const) (TSimple SType_String) (loc)
  ETrue   const@(PTrue (loc, _))    -> return $ ExpTyped (ETrue const) (TSimple SType_Bool) (loc)
  EFalse  const@(PFalse (loc, _))   -> return $ ExpTyped (EFalse const) (TSimple SType_Bool) (loc)
  ENull   const@(PNull (loc, _))    -> return $ ExpTyped (ENull const) (TPointer (TSimple SType_Void)) loc 
  EOp expl op expr -> inferBinOp expl op expr env
-------------------------------------------------------------------------------------------------------------------------------------------

  EIfElse exp_cond exp_if exp_else -> do
    texp_cond <- inferExp exp_cond env
    texp_if <- inferExp exp_if env
    texp_else <- inferExp exp_else env
    if not (isTypeError texp_cond || compatible (getType texp_cond) (TSimple SType_Bool))
      then do
        saveLog $ launchError (getLoc texp_cond) (WrongIfCondition exp_cond (getType texp_cond))
        return $ ExpTyped (EIfElse texp_cond texp_if texp_else) (TSimple SType_Error) (getLoc texp_cond)
      else
        let maxTyp = max (getType texp_if) (getType texp_else) in
          case (isTypeError texp_if || isTypeError texp_else, (compatible (getType texp_if) maxTyp  && compatible (getType texp_else) maxTyp)) of
            (True,_) -> return $ ExpTyped (EIfElse texp_cond texp_if texp_else) (TSimple SType_Error) (getLoc texp_cond)
            (False, True) -> return $ ExpTyped (EIfElse texp_cond texp_if texp_else) maxTyp (getLoc texp_cond)
            (False, False) -> do
              saveLog $ launchError (getLoc texp_cond) (WrongIfElseExp texp_if texp_else)
              return $ ExpTyped (EIfElse texp_cond texp_if texp_else) (TSimple SType_Error) (getLoc texp_cond)

-- Prese due espressioni ed un operatore binario ritorna la corrispondente espressione tipizzata
inferBinOp :: Exp -> Op -> Exp -> Env -> Logger Exp
inferBinOp expl op expr env = do 
  texpl <- inferExp expl env
  texpr <- inferExp expr env

  -- Viene recuperato il tipo dell'operazione (es. Numerica, Relazionale, etc.) e i tipi delle due sotto-espressioni
  -- coinvolte, poi si verifica che i tipi di tali espressioni siano consistenti con il tipo dell'operazione.
  case ((getTypeOp op), (getType texpl), (getType texpr) ) of
    (_ , TSimple SType_Error, _ ) -> return $ ExpTyped (EOp texpl op texpr) (TSimple SType_Error) (getLoc texpl) 
    (_ , _ , TSimple SType_Error) -> return $ ExpTyped (EOp texpl op texpr) (TSimple SType_Error) (getLoc texpl) 
    (BooleanOp, typl, typr) ->
      if compatible typl (TSimple SType_Bool) && compatible typr (TSimple SType_Bool)
        then return $ ExpTyped (EOp texpl op texpr) (TSimple SType_Bool) (getLoc texpl)
        else returnBinOpError texpl op texpr
    (NumericOp, typl, typr) ->
      case (max typl typr) of
        (TSimple SType_String) -> returnBinOpError texpl op texpr
        (TSimple SType_Float) -> return $ ExpTyped (EOp texpl op texpr) (TSimple SType_Float) (getLoc texpl)
        _ -> return $ ExpTyped (EOp texpl op texpr) (TSimple SType_Int) (getLoc texpl)
    (RelOp, typl, typr) ->
      if compatible (min typl typr) (max typl typr) 
        then return $ ExpTyped (EOp texpl op texpr) (TSimple SType_Bool) (getLoc texpl)
        else returnBinOpError texpl op texpr
    
  where
    returnBinOpError texpl op texpr = do
      saveLog $ launchError (getLoc texpl) (WrongOpApplication op (getType texpl) (getType texpr))
      return $ ExpTyped (EOp texpl op texpr) (TSimple SType_Error) (getLoc texpl) 

    -- preso un operatore binario ritorna il suo tipo.
    getTypeOp :: Op -> TypeOp
    getTypeOp op = case op of
      Plus      -> NumericOp
      Minus     -> NumericOp
      Prod      -> NumericOp
      Div       -> NumericOp
      Mod       -> NumericOp
      Pow       -> NumericOp
      Or        -> BooleanOp
      And       -> BooleanOp
      Less      -> RelOp
      Greater   -> RelOp
      LessEq    -> RelOp
      GreaterEq -> RelOp
      Equal     -> RelOp
      NotEq     -> RelOp


-- Definizione dei tipi di operazione.
data TypeOp = NumericOp | BooleanOp | RelOp
