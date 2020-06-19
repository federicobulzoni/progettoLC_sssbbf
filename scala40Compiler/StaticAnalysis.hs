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

-- Utilities 
-- saveLog
-- salva un elemento di log nella lista di elementi di log che sarà ritornata da typeCheck.
saveLog:: LogElement -> Logger ()
saveLog logelem = do 
  tell [logelem]
  return ()

-- isTypeVoid
-- preso un tipo ritorna True se tale tipo è Void, False altrimenti.
isTypeVoid :: TypeSpec -> Bool
isTypeVoid typ = typ == (TSimple SType_Void)

------------------------------------------------------------------------------------------------------------------------

-- PROPOSTA VELOCE PER RISOLVERE COMPATIBILITA' TIPI

--compatible :: TypeSpec -> Exp -> Bool
--compatible (TSimple SType_Int) texp = elem (getType texp) [TSimple SType_Int, TSimple SType_Char, TSimple SType_Bool]
--compatible (TSimple SType_Float) texp = elem (getType texp) [SType_Float, TSimple SType_Int, TSimple SType_Char, TSimple SType_Bool]
--compatible (TSimple SType_Char) texp = elem (getType texp) [TSimple SType_Char, TSimple SType_Bool]
--compatible (TSimple SType_Bool) texp = elem (getType texp) [TSimple SType_Bool]
--compatible (TSimple SType_String) texp = elem (getType texp) [TSimple SType_String]
--compatible _ = False

------------------------------------------------------------------------------------------------------------------------


-- isCompatible
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
allCompatible (x:xs) (y:ys) = length x == length y && (all (==True) (zipWith (isCompatible) x y)) && (allCompatible xs ys)
            
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
      ("writeInt",    FunInfo (0,0) (TSimple SType_Void) [PParam [DParam (ParamPassMod_val) (PIdent ((0,0),"i")) (TSimple SType_Int) ]]),
      ("writeFloat",  FunInfo (0,0) (TSimple SType_Void) [PParam [DParam (ParamPassMod_val) (PIdent ((0,0),"f")) (TSimple SType_Float) ]]),
      ("writeChar",   FunInfo (0,0) (TSimple SType_Void) [PParam [DParam (ParamPassMod_val) (PIdent ((0,0),"c")) (TSimple SType_Char) ]]),
      ("writeString", FunInfo (0,0) (TSimple SType_Void) [PParam [DParam (ParamPassMod_val) (PIdent ((0,0),"s")) (TSimple SType_String) ]]),

      ("readInt",     FunInfo (0,0) (TSimple SType_Int)    [PParam []]),
      ("readFloat",   FunInfo (0,0) (TSimple SType_Float)  [PParam []]),
      ("readChar",    FunInfo (0,0) (TSimple SType_Char)   [PParam []]),
      ("readString",  FunInfo (0,0) (TSimple SType_String) [PParam []]) ]

-- startFunScope
-- Si occupa di inizializzare l'environment prima dell'inferenza degli statement contenuti nel suo body.
startFunScope :: Env -> PIdent -> [ParamClause] -> TypeSpec -> Logger Env
startFunScope env id@(PIdent (loc, ident)) params typ = do
  case foldM (\x (ident', info) -> Env.update x ident' info) (Env.addScope env typ) funInfo of
    Success env' -> return env'
    Failure except -> do
      saveLog $ launchError loc except
      return env
  where
    argsInfo = map (\(DParam passMod argId@(PIdent (argLoc, argIdent)) argTyp) -> (argIdent, VarInfo argLoc argTyp)) (concat ( map (\(PParam args) -> args) params ))
    funInfo = (ident, FunInfo loc typ params):argsInfo

-- Definizione dei tipi di operazione.
data TypeOp = NumericOp | BooleanOp | EqOp | RelOp

-- getTypeOp
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
  Equal     -> EqOp
  NotEq     -> EqOp

------------------------------------------------------------------------------------------------------------------------

genAnnotatedTree :: Program -> (Program, [LogElement])
genAnnotatedTree prog = runWriter $ typeCheck prog

-- typeCheck
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
            return $ (DefVar id typ texp, env')
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
          if (not (isTypeVoid typ)) || (notEmptyParams params)
            then do
              saveLog $ launchError loc WrongMainSignature
              functionHandler (Env.setReturnFound env)
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
          (tstms, e') <- inferStms stms newScope

          -- Nel caso in cui stiam trattando una funzione, ma non è presente alcun
          -- return nel suo scope lo notifichiamo.
          if Env.hasReturn e' || isTypeVoid typ
            then
              return $ (DefFun id params typ (DBlock tstms), e)
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
              functionHandler (Env.setReturnFound env)
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
              (stmt, e') <- inferStm (SProcCall id' params') e
              return (DefFun id params typ (DBlock [stmt]), e')
            -- PROCEDURA con assegnata EXP
            (_,True) -> do
              newScope <-  startFunScope e id params typ
              texp <- inferExp exp newScope
              if isTypeError texp then
                return (DefFun id params typ (DBlock [SReturnExp (PReturn (loc , "return")) texp]), e)
              else do
                saveLog $ launchError loc (ExpAssignedToProcedure ident exp (getType texp))
                return (DefFun id params typ (DBlock [SReturnExp (PReturn (loc , "return")) texp]), e)
            -- FUNZIONE
            (_,False) -> do
              newScope <- startFunScope e id params typ
              texp <- inferExp exp newScope
              if isTypeError texp || isCompatible texp typ 
                then
                  return (DefFun id params typ (DBlock [SReturnExp (PReturn (loc , "return")) texp]), e)
                else do
                  saveLog $ launchError loc (WrongExpType exp (getType texp) typ)
                  return (DefFun id params typ (DBlock [SReturnExp (PReturn (loc , "return")) texp]), e)

        notEmptyParams [PParam []] = False
        notEmptyParams par = True

  
{-
inferBlock :: Block -> TypeSpec -> Env -> Logger (Block, Env)
inferBlock (DBlock []) _ env = return $ (DBlock [], env)
inferBlock (DBlock stms) ftyp env = do
  (tstms, env') <- inferStms stms (Env.addScope env ftyp) 
  return $ (DBlock tstms, env')
-}

inferStms :: [Stm] -> Env -> Logger ([Stm], Env)
inferStms [] env = return ([], env)
inferStms (stm:stms) env = do
  (tstm, env') <- inferStm stm env
  (tstms, env'') <- inferStms stms env'
  return $ (tstm:tstms, env'') 


inferStm :: Stm  -> Env -> Logger (Stm, Env)
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
          if isTypeVoid ftyp
            then do
              -- Se il tipo dello scope è Void, allora ci troviamo all'interno di una procedura
              -- in questo caso viene lanciata un eccezione dato che stiamo usando un return con valore.
              saveLog $ launchError loc (UnexpectedReturn exp)
              return $ (SReturnExp preturn texp, env)
            else 
              -- Nel caso in cui l'espressione ritornata sia compatibile con il tipo
              -- dello scope viene segnalato che un return adeguato nello scope è stato trovato.
              if isCompatible texp ftyp
                then
                  return $ (SReturnExp preturn texp, Env.setReturnFound env)
                else do
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
  -- DA GESTIRE - PROPOSTE SOTTO
  SBreak pbreak@(PBreak (loc,ident)) -> return (SBreak pbreak, env)
  SContinue pcontinue@(PContinue (loc,ident)) -> return (SContinue pcontinue, env)
  --SBreak pbreak@(PBreak (loc,ident)) -> do
  --  if inLoop
  --    then return (SBreak pbreak, env)
  --    else do
  --      saveLog $ launchError loc (WrongFlowCrontrolStatement ident)
  --      return (SBreak pbreak, env)
  --SContinue pcontinue@(PContinue (loc,ident)) -> do
  --  if inLoop
  --    then return (SContinue pcontinue, env)
  --    else do
  --      saveLog $ launchError loc (WrongFlowCrontrolStatement ident)
  --      return (SContinue pcontinue, env)
-------------------------------------------------------------------------------------------------------------------------------------------
                                    -- lista di liste ArgExp [Exp]
  SProcCall id@(PIdent (loc, ident)) params -> do
    -- Tre possibili errori:
      -- 1. Il numero di clausole nella chiamata non corrisponde con il numero di clausole nella definizione,
      -- 2. Il numero di argomenti all'interno di una clausola non corrisponde con il numero di parametri della clausola,
      -- 3. Le dimensioni combaciano, ma almeno un'espressione passata come argomento ha tipo diverso da quello del corrispondente parametro.

    -- [[TypeSpec]]
    tparams <- mapM (\(ArgExp x) -> (mapM (\y -> (inferExp y env)) x)) params
    -- typ_params è una lista di liste di TypeSpec contenente il tipo di ogni parametro.
    case Env.lookup env id of
      -- Caso in cui l'identificatore usato nella chiamata di procedura sia assegnato ad una variabile.
      Success (VarInfo dloc _) -> do
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
        let typ_args = map (\(PParam x) -> (map (\(DParam passMod ident typ) -> typ) x)) paramclauses in
          do
            case (any (isTypeError) (concat tparams), not $ allCompatible tparams typ_args, not (isTypeVoid typ)) of
              (True,_,_) -> return ()
              (_,True,False) -> saveLog $ launchError loc (WrongProcParams ident typ_args (map (map getType) tparams))
              (_,True,True) -> saveLog $ launchError loc (WrongFunctionParams ident typ_args (map (map getType) tparams) typ)
              (_,False,True) -> saveLog $ launchWarning loc (UnusedReturnValue ident)
              otherwise -> return ()
            return (SProcCall (PIdent (dloc, ident)) (zipWith (\x y-> (ArgExpTyped (zip x y))) tparams typ_args) , env)


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
         case (tlexp , isCompatible texp (TSimple SType_Int)) of
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
        return $ LExpTyped (LIdent (PIdent ((0,0), ident))) (TSimple SType_Error) loc
      -- Si noti che nella LExpTyped, la locazione dell'identificatore, che quando arriva dal parser
      -- corrisponde a quella relativa all'utilizzo corrente dell'identificatore nella LExp considerata,
      -- viene sostituito con la locazione in cui tale identificatore è stato dichiarato.
      Success (VarInfo dloc typ) -> return $ LExpTyped (LIdent (PIdent (dloc, ident))) typ loc
      Success (FunInfo dloc _ _) -> do
        saveLog $ launchError loc (FunctionUsedAsVariable ident dloc)
        return $ LExpTyped (LIdent (PIdent (dloc, ident))) (TSimple SType_Error) loc

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
          inferArrayAux texps = ( any (\x -> getType x == (TSimple SType_Error)) texps, all (\x -> isCompatible x (maximum (map getType texps))) texps)


-------------------------------------------------------------------------------------------------------------------------------------------
                                    -- lista di liste ArgExp [Exp]
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
      Success (VarInfo dloc _) -> do
        saveLog $ launchError loc (VariableUsedAsFunction ident dloc)
        return $ ExpTyped (EFunCall (PIdent (dloc, ident)) (map (\x -> (ArgExp x)) tparams)) (TSimple SType_Error) loc
      Failure except -> do
        saveLog $ launchError loc except
        return $ ExpTyped (EFunCall (PIdent ((0,0), ident)) (map (\x -> (ArgExp x)) tparams)) (TSimple SType_Error) loc
      Success (FunInfo dloc typ paramclauses) ->
        -- Argomenti di quando lo dichiati
        -- Lista di liste di tipi degli argomenti.
        let typ_args = map (\(PParam x) -> (map (\(DParam passMod ident typ) -> typ) x)) paramclauses in
          if any (isTypeError) (concat tparams)
            then 
                return $ ExpTyped (EFunCall (PIdent (dloc, ident)) (zipWith (\x y-> (ArgExpTyped (zip x y))) tparams typ_args)) (TSimple SType_Error) loc                    
            else 
              if allCompatible tparams typ_args
                then 
                  if isTypeVoid typ
                    then do
                      saveLog $ launchError loc (UnexpectedProc ident)
                      return $ ExpTyped (EFunCall (PIdent (dloc, ident)) (zipWith (\x y-> (ArgExpTyped (zip x y))) tparams typ_args)) (TSimple SType_Error) loc                    
                    else 
                      return $ ExpTyped (EFunCall (PIdent (dloc, ident)) (zipWith (\x y-> (ArgExpTyped (zip x y))) tparams typ_args)) typ loc                    
                else do
                  saveLog $ launchError loc (WrongFunctionParams ident typ_args (map (map getType) tparams) typ)
                  return $ ExpTyped (EFunCall (PIdent (dloc, ident)) (zipWith (\x y-> (ArgExpTyped (zip x y))) tparams typ_args)) (TSimple SType_Error) loc                    
 -------------------------------------------------------------------------------------------------------------------------------------------
  ENot exp -> do
    texp <- inferExp exp env
    if isTypeError texp || isCompatible texp (TSimple SType_Bool)
      then return (ExpTyped (ENot texp) (getType texp) (getLoc texp))
      else do
        saveLog $ launchError (getLoc texp) (WrongNotApplication exp (getType texp))
        return $ ExpTyped (ENot texp) (TSimple SType_Error) (getLoc texp)

-------------------------------------------------------------------------------------------------------------------------------------------
  ENeg exp -> do
    texp <- inferExp exp env
    if isTypeError texp || isCompatible texp (TSimple SType_Float)
      then return $ ExpTyped (ENeg texp) (getType texp) (getLoc texp)
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
    if not (isTypeError texp_cond || isCompatible texp_cond (TSimple SType_Bool))
      then do
        saveLog $ launchError (getLoc texp_cond) (WrongIfCondition exp_cond (getType texp_cond))
        return $ ExpTyped (EIfElse texp_cond texp_if texp_else) (TSimple SType_Error) (getLoc texp_cond)
      else case (isTypeError texp_if, isTypeError texp_else) of
        (True, False) -> do
          saveLog $ launchError (getLoc texp_if) (WrongIfElseExp exp_if)
          return $ ExpTyped (EIfElse texp_cond texp_if texp_else) (TSimple SType_Error) (getLoc texp_if)
        (False, True) -> do
          saveLog $ launchError (getLoc texp_else) (WrongIfElseExp exp_else)
          return $ ExpTyped (EIfElse texp_cond texp_if texp_else) (TSimple SType_Error) (getLoc texp_else)
        (True, True) -> do
          saveLog $ launchError (getLoc texp_if) (WrongIfElseExp exp_if)
          saveLog $ launchError (getLoc texp_else) (WrongIfElseExp exp_else)
          return $ ExpTyped (EIfElse texp_cond texp_if texp_else) (TSimple SType_Error) (getLoc texp_if)
        (False, False) -> return $ ExpTyped (EIfElse texp_cond texp_if texp_else) (max (getType texp_if) (getType texp_else)) (getCorrectLocation texp_if texp_else)
  where
    getCorrectLocation te1 te2 = if((getType te1) <= (getType te2)) then getLoc te2 else getLoc te1

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
    
    (EqOp, typl, typr) ->
      if typl == typr || (max typl typr) < (TSimple SType_String)
      -- if isConsistent EqOp typl typr
        then return $ ExpTyped (EOp texpl op texpr) (TSimple SType_Bool) (getLoc texpl)
        else returnBinOpError texpl op texpr
    (RelOp, typl, typr) ->
      if (max typl typr) < (TSimple SType_String)
      -- if isConsistent RelOp typl typr
        then return $ ExpTyped (EOp texpl op texpr) (TSimple SType_Bool) (getLoc texpl)
        else returnBinOpError texpl op texpr
    (BooleanOp, typl, typr) ->
      if typl == (TSimple SType_Bool) && typr == (TSimple SType_Bool)
        then return $ ExpTyped (EOp texpl op texpr) (TSimple SType_Bool) (getLoc texpl)
        else returnBinOpError texpl op texpr
    (NumericOp, typl, typr) ->
      case (max typl typr) of
        (TSimple SType_String) -> returnBinOpError texpl op texpr
        (TSimple SType_Float) -> return $ ExpTyped (EOp texpl op texpr) (TSimple SType_Float) (getLoc texpl)
        _ -> return $ ExpTyped (EOp texpl op texpr) (TSimple SType_Int) (getLoc texpl)
  where
    returnBinOpError texpl op texpr = do
      saveLog $ launchError (getLoc texpl) (WrongOpApplication op (getType texpl) (getType texpr))
      return $ ExpTyped (EOp texpl op texpr) (TSimple SType_Error) (getLoc texpl) 