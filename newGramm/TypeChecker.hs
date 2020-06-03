-- Modulo SemanticAnalysis.hs
-- Il modulo partendo da un programma scritto nella sintassi astratta definita in AbsGramm.hs
-- verifica la presenza di errori statici all'interno di esso (incompatibilità di tipi, operazioni non consentite)
-- e nel frattempo si occupa di arricchire l'albero di sintassi astratta in input con informazioni aggiuntive,
-- quali ad esempio il tipo delle R-espressioni e delle L-espressioni.
-- La funzione principale del modulo è typeCheck che preso in input un programma in sintassi astratta
-- ritorna in output tale programma annotato e gli eventuali Warning ed Errori (LogElement) scovati durante l'annotazione.

module TypeChecker where

import AbsGramm
import Environment as Env
import Control.Monad.Writer
import Errors
import Typed

-- Utilities 
-- saveLog
-- salva un elemento di log nella lista di elementi di log che sarà ritornata da typeCheck.
saveLog:: LogElement -> Writer [LogElement] ()
saveLog logelem = do 
  tell [logelem]
  return ()

-- isTypeVoid
-- preso un tipo ritorna True se tale tipo è Void, False altrimenti.
isTypeVoid :: TypeSpec -> Bool
isTypeVoid typ = typ == (TSimple SType_Void)


-- isCompatible
-- Presa una espressione tipata texp, ed un tipo richiesto typ, ritorna True se la espressione tipata
-- è compatibile con il tipo richiesto; altrimenti ritorna False.
-- All'interno di questa funzione è possibile differenziare le compatibilità per i diversi tipi presenti
-- nella grammatica astratta.
isCompatible :: Exp -> TypeSpec -> Bool
isCompatible texp typ = case typ of
  (TPointer typ') -> getType texp == (TPointer (TSimple SType_Void)) || getType texp == typ
  (TArray typ' _) -> let typExp = getType texp in
    case typExp of
      (TArray (TSimple SType_Void) _) -> True
      _ -> typExp == typ 
  otherwise       -> getType texp == typ

-- startingEnv
-- Definizione dell'environment iniziale di un programma. Contiene le informazioni a riguardo delle
-- funzioni native presenti nel linguaggio.
startingEnv :: Env
startingEnv = 
  let (Success env) = foldM (\x (ident, info) -> Env.update x ident info) (Env.emptyEnv) initialFuns
  in
    env
  where
    initialFuns = [
      ("writeInt",    FunInfo (0,0) (TSimple SType_Void) [PArg [DArg (PIdent ((0,0),"i")) (TSimple SType_Int) ]]),
      ("writeFloat",  FunInfo (0,0) (TSimple SType_Void) [PArg [DArg (PIdent ((0,0),"f")) (TSimple SType_Float) ]]),
      ("writeChar",   FunInfo (0,0) (TSimple SType_Void) [PArg [DArg (PIdent ((0,0),"c")) (TSimple SType_Char) ]]),
      ("writeString", FunInfo (0,0) (TSimple SType_Void) [PArg [DArg (PIdent ((0,0),"s")) (TSimple SType_String) ]]),

      ("readInt",     FunInfo (0,0) (TSimple SType_Int)    [PArg []]),
      ("readFloat",   FunInfo (0,0) (TSimple SType_Float)  [PArg []]),
      ("readChar",    FunInfo (0,0) (TSimple SType_Char)   [PArg []]),
      ("readString",  FunInfo (0,0) (TSimple SType_String) [PArg []]) ]

-- startFunScope
-- Si occupa di inizializzare l'environment prima dell'inferenza degli statement contenuti nel suo body.
startFunScope :: Env -> PIdent -> [ParamClause] -> TypeSpec -> Env
startFunScope env id@(PIdent (loc, ident)) params typ = 
  let 
    (Success env') = foldM (\x (ident', info) -> Env.update x ident' info) (Env.addScope env typ) funInfo
  in
    env'
  where
    argsInfo = map (\(DArg argId@(PIdent (argLoc, argIdent)) argTyp) -> (argIdent, VarInfo argLoc argTyp)) (concat ( map (\(PArg args) -> args) params ))
    funInfo = (ident, FunInfo loc typ params):argsInfo
------------------------------------------------------------------------------------------------------------------------

-- typeCheck
-- Dato un programma scritto in sintassi astratta restituisce una lista di log ed un programma annotato.
typeCheck :: Program -> Writer [LogElement] Program
typeCheck (Prog decls) = do
  (tdecls, env) <- inferDecls decls startingEnv
  -- L'environment globale ha il campo booleano a True se e solo se è stato trovato un
  -- main tra le dichiarazioni globali.
  if not $ Env.hasReturn env then do
    saveLog $ launchWarning (0,0) MissingMain
    return $ Prog tdecls
  else
    return $ Prog tdecls


inferDecls :: [Declaration] -> Env -> Writer [LogElement] ([Declaration], Env)
inferDecls [] env = return ([], env)
inferDecls (decl:decls) env = do
  (tdecl , env') <-  inferDecl decl env
  (tdecls, env'') <- inferDecls decls env'
  return ((tdecl:tdecls), env'')

inferDecl :: Declaration -> Env -> Writer [LogElement] (Declaration, Env)
inferDecl decl env = case decl of
  DefVar id@(PIdent (loc, ident)) typ exp -> 
    -- Si prova ad inserire i dati della variabile nell'environment ...
    case (Env.update env ident (VarInfo loc typ)) of
      Success env' -> do
        -- ... se è stato possibile, ...
        texp <- inferExp exp env
        -- ... allora si verifica che la espressione che si sta cercando di assegnare
        -- è compatibile con il tipo della variabile. Se il tipo della espressione è un typeError
        -- non abbiamo bisogno di lanciare nuovi errori (questa è una costante lungo tutto il codice).
        if isTypeError texp || isCompatible texp typ
          then
            return $ (DefVar id typ texp, env')
          else do
            saveLog $ launchError (getLoc texp) (WrongExpType exp (getType texp) typ)
            return $ (DefVar id typ texp, env)
      Failure except -> do
        -- Se qualcosa è andato storto si lancia l'errore.
        saveLog $ launchError loc except
        texp <- inferExp exp env
        return $ (DefVar id typ texp, env)
-------------------------------------------------------------------------------------------------------------------------------------------
  DecVar id@(PIdent (loc, ident)) typ -> 
    case Env.update env ident (VarInfo loc typ) of
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
          functionHandler (Env.setReturnFound env')
        else
          functionHandler env'
      Failure except -> do
        saveLog $ launchError loc except
        functionHandler env
      where 
        functionHandler e = do
          -- Creare nuovo scope avviato con i parametri e il nome della fun.
          (tstms, e') <- inferStms stms (startFunScope e id params typ)
          -- Nel caso in cui stiam trattando una funzione, ma non è presente alcun
          -- return nel suo scope lo notifichiamo.
          if Env.hasReturn e' || isTypeVoid typ
            then
              return $ (DefFun id params typ (DBlock tstms), e)
            else do
              saveLog $ launchWarning loc (MissingReturn ident)
              return $ (DefFun id params typ (DBlock tstms), e)
-------------------------------------------------------------------------------------------------------------------------------------------
  DefFunInLine id@(PIdent (loc, ident)) params typ exp -> 
    case update env ident (FunInfo loc typ params) of
      Success env' -> do
        if ident == "main" && Env.isGlobalScope env' then do
          saveLog $ launchWarning loc MainDefinedInLine
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
              (stmt, e') <- inferStm (SProcCall id' params') e
              return (DefFun id params typ (DBlock [stmt]), e')
            -- PROCEDURA con assegnata EXP
            (_,True) -> do
              texp <- inferExp exp (startFunScope e id params typ)
              saveLog $ launchError loc UnexpectedReturn
              return (DefFun id params typ (DBlock [SReturnExp (PReturn (loc , "return")) texp]), e)
            -- FUNZIONE
            (_,False) -> do
              texp <- inferExp exp (startFunScope e id params typ)
              if isTypeError texp || isCompatible texp typ 
                then
                  return (DefFun id params typ (DBlock [SReturnExp (PReturn (loc , "return")) texp]), e)
                else do
                  saveLog $ launchError loc (WrongExpType exp (getType texp) typ)
                  return (DefFun id params typ (DBlock [SReturnExp (PReturn (loc , "return")) texp]), e)


  
{-
inferBlock :: Block -> TypeSpec -> Env -> Writer [LogElement] (Block, Env)
inferBlock (DBlock []) _ env = return $ (DBlock [], env)
inferBlock (DBlock stms) ftyp env = do
  (tstms, env') <- inferStms stms (Env.addScope env ftyp) 
  return $ (DBlock tstms, env')
-}

inferStms :: [Stm] -> Env -> Writer [LogElement] ([Stm], Env)
inferStms [] env = return ([], env)
inferStms (stm:stms) env = do
  (tstm, env') <- inferStm stm env
  (tstms, env'') <- inferStms stms env'
  return $ (tstm:tstms, env'') 


inferStm :: Stm  -> Env -> Writer [LogElement] (Stm, Env)
inferStm stm env = case stm of
  SWhile exp stm' -> do
    -- texp è l'espressione annotata col tipo.
    texp <- inferExp exp env
    -- Se la condizione non ha tipo compatibile con Bool, allora si lancia un errore.
    if not (isTypeError texp || isCompatible texp (TSimple SType_Bool))
      then
        saveLog $ launchError (getLoc texp) (WrongWhileCondition exp (getType texp))
      else
        return ()
    -- viene inferito lo stm del while.
    (tstm', env') <- inferStm stm' env
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
    if not (isTypeError texp || isCompatible texp (TSimple SType_Bool))
      then 
        saveLog $ launchError (getLoc texp) (WrongIfCondition exp (getType texp))
      else return ()

    (tstmif, envif) <- inferStm stmif env
    (tstmelse, envelse) <- inferStm stmelse env
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
  SBlock block -> let ftyp = Env.getScopeType env in do
    (tblock, env') <- inferBlock block ftyp env
    if Env.hasReturn env'
      then
        return $ (SBlock tblock, Env.setReturnFound env)
      else
        return $ (SBlock tblock, env)
    where
      inferBlock (DBlock []) _ env = return $ (DBlock [], env)
      inferBlock (DBlock stms) ftyp env = do
        (tstms, env') <- inferStms stms (Env.addScope env ftyp) 
        return $ (DBlock tstms, env')

-------------------------------------------------------------------------------------------------------------------------------------------
  SAssign lexp exp -> do
    tlexp <- inferLExp lexp env
    texp <- inferExp exp env
    if not (isTypeError tlexp || isTypeError texp || isCompatible texp (getType tlexp))
      then 
        saveLog $ launchError (getLoc texp) (WrongExpAssignType exp (getType texp) (getType tlexp) lexp)
      else
        return ()
    return $ (SAssign tlexp texp, env)

-------------------------------------------------------------------------------------------------------------------------------------------
  SReturnExp preturn@(PReturn (loc, id)) exp -> let ftyp = Env.getScopeType env in
    do
      texp <- inferExp exp env
      if isTypeError texp
        then
          return $ (SReturnExp preturn texp, env)
        else
          -- Nel caso in cui l'espressione ritornata sia compatibile con il tipo
          -- dello scope viene segnalato che un return adeguato nello scope è stato trovato.
          if isCompatible texp ftyp
            then
              return $ (SReturnExp preturn texp, Env.setReturnFound env)
            else
              -- Se il tipo dello scope è Void, allora ci troviamo all'interno di una procedura
              -- in questo caso viene lanciata un eccezione dato che stiamo usando un return con valore.
              if isTypeVoid ftyp
                then do
                  saveLog $ launchError loc UnexpectedReturn
                  return $ (SReturnExp preturn texp, env)
                -- se il tipo dello scope non è void, ma è incompatibile con quello dell'espressione
                -- allora si lancia un'errore.
                else do
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
                                    -- lista di liste ParExp [Exp]
  SProcCall id@(PIdent (loc, ident)) params -> do
    -- Tre possibili errori:
      -- 1. Il numero di clausole nella chiamata non corrisponde con il numero di clausole nella definizione,
      -- 2. Il numero di argomenti all'interno di una clausola non corrisponde con il numero di parametri della clausola,
      -- 3. Le dimensioni combaciano, ma almeno un'espressione passata come argomento ha tipo diverso da quello del corrispondente parametro.

    -- [[TypeSpec]]

    tparams <- mapM (\(ParExp x) -> (mapM (\y -> (inferExp y env)) x)) params
    -- typ_params è una lista di liste di TypeSpec contenente il tipo di ogni parametro.
    let typ_params = map (map getType) tparams in
      case Env.lookup env id of
        -- Caso in cui l'identificatore usato nella chiamata di procedura sia assegnato ad una variabile.
        Success (VarInfo dloc _) -> do
         saveLog $ launchError loc (DuplicateVariable ident dloc)
         -- Notare come l'identificatore che viene ritornato non sia lo stesso che ci arriva in input,
         -- bensì la locazione dell'identificatore viene sostituita diventando quella di dichiarazione
         -- dell'identificatore.
         return (SProcCall (PIdent (dloc, ident)) (map (\x -> (ParExp x)) tparams) , env)
        Failure except -> do
          saveLog $ launchError loc except
          return (SProcCall (PIdent ((0,0), ident)) (map (\x -> (ParExp x)) tparams) , env)
        Success (FunInfo dloc typ paramclauses) -> 
          -- typ_args è il corrispettivo di typ_params, i due devono combaciare per poter affermare
          -- che la chiamata di procedura è valida.
          let typ_args = map (\(PArg x) -> (map (\(DArg ident typ) -> typ) x)) paramclauses in
          do
            if any (isTypeError) (concat tparams)
              then
                return (SProcCall (PIdent (dloc, ident)) (map (\x -> (ParExp x)) tparams) , env)
              else
                -- Nel caso in cui quella considerata non sia una procedura viene lanciato un warning
                --  dato che non viene utilizzato il valore di ritorno.
                if not (isTypeVoid typ)
                  then do
                    saveLog $ launchWarning loc (MissingAssignVariable ident)
                    return (SProcCall (PIdent (dloc, ident)) (map (\x -> (ParExp x)) tparams) , env)
                  else
                    if not (typ_args == typ_params)
                      -- Se i tipi dei parametri non combaciano con quelli degli argomenti viene lanciato
                      -- un errore.
                      then do
                        saveLog $ launchError loc (WrongProcParams ident typ_args typ_params)
                        return (SProcCall (PIdent (dloc, ident)) (map (\x -> (ParExp x)) tparams) , env)
                      else
                        return (SProcCall (PIdent (dloc, ident)) (map (\x -> (ParExp x)) tparams) , env)


inferLExp :: LExp -> Env -> Writer [LogElement] LExp
inferLExp lexp env = case lexp of
 LRef lexp' -> do
   tlexp' <- inferLExp lexp' env
   if isTypeError tlexp' 
     then return $ LExpTyped (LRef tlexp') (TSimple SType_Error) (getLoc tlexp')
     else
       case tlexp' of
         (LExpTyped _ (TPointer typ) loc) -> return $ LExpTyped (LRef tlexp') typ loc
         (LExpTyped _ typ' loc) -> do
           saveLog $ launchError loc (WrongPointerApplication lexp' typ')
           return $ LExpTyped (LRef tlexp') (TSimple SType_Error) loc

-------------------------------------------------------------------------------------------------------------------------------------------
 LArr lexp exp -> do
   tlexp <- inferLExp lexp env
   texp <- inferExp exp env
   if isTypeError tlexp || isTypeError texp
     then
       return $ LExpTyped (LArr tlexp texp) (TSimple SType_Error) (getLoc tlexp)
     else
       case (tlexp , isCompatible texp (TSimple SType_Int)) of
         (LExpTyped _ (TArray typ _) loc, True) -> return $ LExpTyped (LArr tlexp texp) typ loc
         (LExpTyped _ (TArray typ _) loc, False) -> do
           saveLog $ launchError loc (WrongArrayIndex exp (getType texp))
           return $ LExpTyped (LArr tlexp texp) (TSimple SType_Error) loc
         (_, False) -> do
           saveLog $ launchError (getLoc texp) (WrongArrayAccess  lexp (getType tlexp))
           saveLog $ launchError (getLoc texp) (WrongArrayIndex exp (getType texp))
           return  $ LExpTyped (LArr tlexp texp) (TSimple SType_Error) (getLoc tlexp)
         (_, True) -> do
           saveLog $ launchError (getLoc texp) (WrongArrayAccess lexp (getType tlexp))
           return  $ LExpTyped (LArr tlexp texp) (TSimple SType_Error) (getLoc tlexp)

-------------------------------------------------------------------------------------------------------------------------------------------
 LIdent id@(PIdent (loc, ident)) -> let res = Env.lookup env id in
   case res of
     Failure except -> do
       saveLog $ launchError loc except
       return $ LExpTyped (LIdent (PIdent ((0,0), ident))) (TSimple SType_Error) loc
     Success (VarInfo dloc typ) -> return $ LExpTyped (LIdent (PIdent (dloc, ident))) typ loc
     Success (FunInfo dloc _ _) -> do
       saveLog $ launchError loc (DuplicateFunction ident dloc)
       return $ LExpTyped (LIdent (PIdent (dloc, ident))) (TSimple SType_Error) loc



-- Prende una lista di espressioni tipate, un tipo, e ritorna una coppia con il primo elemento
-- che dice se si è trovato almeno un elemento con tipo (TSimple SType_Error), ed il secondo elemento che dice
-- se tutte le espressioni hanno tipo type o meno.
inferArrayAux :: [Exp] -> TypeSpec -> (Bool, Bool)
inferArrayAux texps typ = ( (any (\x -> isCompatible x (TSimple SType_Error)) texps),(all (\x -> isCompatible x typ) texps) )

inferExp :: Exp -> Env -> Writer [LogElement] Exp
inferExp exp env = case exp of                                                                       
  EArray exps -> do
    texps <- mapM (\x -> inferExp x env) exps
    if length texps == 0 
      then
        -- serve?
        return $ ETyped (exp) (TArray (TSimple SType_Void) (PInteger ( (0,0), show (0)  ) )) (0,0) 
      else 
        let (anyError, allCompatible) = inferArrayAux texps (getType (head texps) ) in
          case (anyError, allCompatible) of
            (True, _) -> return $ ETyped (EArray texps) (TArray (TSimple SType_Error) (PInteger ( (getLoc (head texps)) , show (length texps)  ) )) (getLoc (head texps)) 
            (_, True) -> return $ ETyped (EArray texps) (TArray (getType (head texps) )  (PInteger ( (getLoc (head texps)), show (length texps)  ))) (getLoc (head texps))
            (_ , _) -> do
              saveLog $ launchError (getLoc (head texps)) ArrayInconsistency
              return $ ETyped (EArray texps) (TSimple SType_Error) (getLoc (head texps))

-------------------------------------------------------------------------------------------------------------------------------------------
                                    -- lista di liste ParExp [Exp]
  EFunCall id@(PIdent (loc, ident)) params -> do
    -- Tre possibili errori:
      -- 1. Il numero di clausole nella chiamata non corrisponde con il numero di clausole nella definizione,
      -- 2. Il numero di argomenti all'interno di una clausola non corrisponde con il numero di parametri della clausola,
      -- 3. Le dimensioni combaciano, ma almeno un'espressione passata come argomento ha tipo diverso da quello del corrispondente parametro.

    -- [[TypeSpec]]
    tparams <- mapM (\(ParExp x) -> (mapM (\y -> (inferExp y env)) x)) params
    let typ_params = map (map getType) tparams in
      case Env.lookup env id of
        Success (VarInfo dloc _) -> do
         saveLog $ launchError loc (DuplicateVariable ident dloc)
         return $ ETyped (EFunCall (PIdent (dloc, ident)) (map (\x -> (ParExp x)) tparams)) (TSimple SType_Error) loc
        Failure except -> do
          saveLog $ launchError loc except
          return $ ETyped (EFunCall (PIdent ((0,0), ident)) (map (\x -> (ParExp x)) tparams)) (TSimple SType_Error) loc
        Success (FunInfo dloc typ paramclauses) ->
          let typ_args = map (\(PArg x) -> (map (\(DArg ident typ) -> typ) x)) paramclauses in
            if any (isTypeError) (concat tparams)
              then 
                return $ ETyped (EFunCall (PIdent (dloc, ident)) (map (\x -> (ParExp x)) tparams)) (TSimple SType_Error) loc
              else
                if typ_args == typ_params 
                  then 
                    if isTypeVoid typ
                      then do
                        saveLog $ launchError loc (UnexpectedProc ident)
                        return $ ETyped (EFunCall (PIdent (dloc, ident)) (map (\x -> (ParExp x)) tparams)) (TSimple SType_Error) loc
                      else
                        return $ ETyped (EFunCall (PIdent (dloc, ident)) (map (\x -> (ParExp x)) tparams)) typ loc
                  else do
                    saveLog $ launchError loc (WrongFunctionParams ident typ_args typ_params typ)
                    return $ ETyped (EFunCall (PIdent (dloc, ident)) (map (\x -> (ParExp x)) tparams)) (TSimple SType_Error) loc

-------------------------------------------------------------------------------------------------------------------------------------------
  ENot exp -> do
    texp <- inferExp exp env
    if isTypeError texp || isCompatible texp (TSimple SType_Bool)
      then return (ETyped (ENot texp) (getType texp) (getLoc texp))
      else do
        saveLog $ launchError (getLoc texp) (WrongNotApplication exp (getType texp))
        return $ ETyped (ENot texp) (TSimple SType_Error) (getLoc texp)

-------------------------------------------------------------------------------------------------------------------------------------------
  ENeg exp -> do
    texp <- inferExp exp env
    if isTypeError texp || isCompatible texp (TSimple SType_Int) || isCompatible texp (TSimple SType_Float)
      then return $ ETyped (ENeg texp) (getType texp) (getLoc texp)
      else do
        saveLog $ launchError (getLoc texp) (WrongNegApplication exp (getType texp))
        return $ (ETyped (ENeg texp) (TSimple SType_Error) (getLoc texp))

-------------------------------------------------------------------------------------------------------------------------------------------
  ELExp lexp -> do
    tlexp <- inferLExp lexp env
    return $ ETyped (ELExp tlexp) (getType tlexp) (getLoc tlexp)

-------------------------------------------------------------------------------------------------------------------------------------------
  EDeref lexp -> do
    tlexp <- inferLExp lexp env
    if isTypeError tlexp
      then return $ ETyped (EDeref tlexp) (TSimple SType_Error) (getLoc tlexp)
      else return $ ETyped (EDeref tlexp) (TPointer (getType tlexp)) (getLoc tlexp)

-------------------------------------------------------------------------------------------------------------------------------------------
  EInt    const@(PInteger (loc, _)) -> return $ ETyped (EInt const) (TSimple SType_Int) (loc) 
  EFloat  const@(PFloat (loc, _))   -> return $ ETyped (EFloat const) (TSimple SType_Float) (loc) 
  EChar   const@(PChar (loc, _))    -> return $ ETyped (EChar const) (TSimple SType_Char) (loc) 
  EString const@(PString (loc, _))  -> return $ ETyped (EString const) (TSimple SType_String) (loc)
  ETrue   const@(PTrue (loc, _))    -> return $ ETyped (ETrue const) (TSimple SType_Bool) (loc)
  EFalse  const@(PFalse (loc, _))   -> return $ ETyped (EFalse const) (TSimple SType_Bool) (loc)
  ENull const@(PNull (loc, _)) -> return $ ETyped (ENull const) (TPointer (TSimple SType_Void)) loc 
  EOp expl op expr -> inferBinOp expl op expr env

-- Prese due espressioni ed un operatore binario ritorna la corrispondente espressione tipizzata
inferBinOp :: Exp -> Op -> Exp -> Env -> Writer [LogElement] Exp
inferBinOp expl op expr env = do 
    texpl <- inferExp expl env
    texpr <- inferExp expr env

    case ((getTypeOp op), (getType texpl), (getType texpr) ) of
      (_ , TSimple SType_Error, _ ) -> return $ ETyped (EOp texpl op texpr) (TSimple SType_Error) (getLoc texpl) 
      (_ , _ , TSimple SType_Error) -> return $ ETyped (EOp texpl op texpr) (TSimple SType_Error) (getLoc texpl) 
      (EqOp, typl, typr) ->
        if isConsistent EqOp typl typr
          then return $ ETyped (EOp texpl op texpr) (TSimple SType_Bool) (getLoc texpl)
          else returnBinOpError texpl op texpr
      (RelOp, typl, typr) ->
        if isConsistent RelOp typl typr
          then return $ ETyped (EOp texpl op texpr) (TSimple SType_Bool) (getLoc texpl)
          else returnBinOpError texpl op texpr
      (_, typl, typr) ->
        if isConsistent (getTypeOp op) typl typr
          then return $ ETyped (EOp texpl op texpr) typl (getLoc texpl)
          else returnBinOpError texpl op texpr

returnBinOpError :: Exp -> Op -> Exp -> Writer [LogElement] Exp
returnBinOpError texpl op texpr = do
  saveLog $ launchError (getLoc texpl) (WrongOpApplication op (getType texpl) (getType texpr))
  return $ ETyped (EOp texpl op texpr) (TSimple SType_Error) (getLoc texpl) 

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