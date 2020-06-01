module TypeChecker where

-- TODO:
  -- controllare l'uso di NULL che per ora ancora non è stato utilizzato.
  -- controllare l'uso di array vuoti, e la posizione a loro assegnata di default.
  -- pensare alla compatibilita String + String. 
  -- controllare la grammatica e aggiungere le locazioni a tutti i typed che non ce l'hanno.
  -- controllare che la locazione negli statement serva veramente (sembra servire solo nella chiamata di funzione)
  -- ci serve la posizione di dichiarazione di tutto ciò che viene dichiarato (identificatori: var, fun) --> serve nel tac per identificare gli id
  -- l'unica cosa che necessita della locazione sono le espressioni --> stampa errori

import AbsGramm
--import ErrM
import Environment as Env
import PrintGramm
import Control.Monad.Writer
import Errors


saveLog:: LogElement -> Writer [LogElement] ()
saveLog logelem = do 
  tell [logelem]
  return ()

isTypeVoid :: TypeSpec -> Bool
isTypeVoid typ = typ == (TSimple TypeVoid)

-- Questa funzione ora è banale, ma nel caso sia richiesta la compatibilità tra tipi
-- diventerà utile. Sostituisce checkExp.
isCompatible :: Exp -> TypeSpec -> Bool
isCompatible texp typ = getType texp == typ

paramsToStms :: [ParamClause] -> [Stm]
paramsToStms params = argsToStms (concat ( map (\(PArg args) -> args) params ) )
  where
    argsToStms [] = []  
    argsToStms (arg@(DArg id typ):args) = (SDecl (DecVar id typ)):(argsToStms args)


startingEnv :: Env
startingEnv = 
  -- bruttino da vedere, come si può fare altrimenti?
  let (Ok env) = foldM (\x (ident, info) -> Env.update x ident info) (Env.emptyEnv) initialFuns
  in
    env
  where
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
typeCheck :: Program -> Writer [LogElement] Program
typeCheck (Prog decls) = do
  tdecls <- inferDecls decls startingEnv
  return $ Prog tdecls


-- Prende una lista di dichiarazioni del programma e la ritorna annotata.
inferDecls :: [Declaration] -> Env -> Writer [LogElement] [Declaration]
inferDecls [] env = return []
inferDecls (decl:decls) env = do
 (tdecl , env') <-  inferDecl decl env
 tdecls <- inferDecls decls env'
 return (tdecl:tdecls)


-- Ci sono vari pattern con casi duplicati in base ad Ok env' o Bad msg che però si differenziano di poco,
-- andrebbero messi a posto.
inferDecl :: Declaration -> Env -> Writer [LogElement] (Declaration, Env)
inferDecl decl env = case decl of
  DefVar id@(PIdent (loc, ident)) typ exp -> 
    case (Env.update env ident (VarInfo loc typ)) of
      Ok env' -> do
        texp <- inferExp exp env
        if isTypeError texp || isCompatible texp typ
          then
            return $ (DefVar id typ texp, env')
          else do
            saveLog $ launchError loc (WrongExpType exp texp typ)
            return $ (DefVar id typ texp, env)
      Bad except -> do
        saveLog $ launchError loc except
        texp <- inferExp exp env
        return $ (DefVar id typ texp, env)
-------------------------------------------------------------------------------------------------------------------------------------------
  DecVar id@(PIdent (loc, ident)) typ -> 
    case Env.update env ident (VarInfo loc typ) of
      Ok env' -> return $ (DecVar id typ, env')
      Bad except -> do
        saveLog $ launchError loc except
        return $ (DecVar id typ, env)
----------------------------------------------------------------------------------------------------------------------------
  DefFun id@(PIdent (loc, ident)) params typ block@(DBlock stms) -> 
    case update env ident (FunInfo loc typ params) of
      Ok env' -> do
        functionHandler env'
      Bad except -> do
        saveLog $ launchError loc except
        functionHandler env
      where 
        functionHandler e = do
          -- Creare nuovo scope avviato con i parametri e il nome della fun.
          (tstms, e') <- inferStms stms (startFunScope e id params typ)
          -- (tblock, e') <- inferBlock (DBlock ((paramsToStms params)++stms)) typ e
          if Env.hasReturn e'
            then
              return $ (DefFun id params typ (DBlock tstms), e)
            else do
              saveLog $ launchWarning loc (MissingReturn ident)
              return $ (DefFun id params typ (DBlock tstms), e)
-------------------------------------------------------------------------------------------------------------------------------------------
  -- Zucchero sintattico manuale e via.
  DefFunInLine id@(PIdent (loc, ident)) params typ exp -> 
    inferDecl (DefFun id params typ (DBlock [SReturnExp (PReturn (loc , "return")) exp])) env


startFunScope :: Env -> PIdent -> [ParamClause] -> TypeSpec -> Env
startFunScope env id@(PIdent (loc, ident)) params typ = 
  let 
    (Ok env') = foldM (\x (ident', info) -> Env.update x ident' info) (Env.addScope env typ) funInfo
  in
    env'
  where
    argsInfo = map (\(DArg argId@(PIdent (argLoc, argIdent)) argTyp) -> (argIdent, VarInfo argLoc argTyp)) (concat ( map (\(PArg args) -> args) params ))
    funInfo = (ident, FunInfo loc typ params):argsInfo
  

inferBlock :: Block -> TypeSpec -> Env -> Writer [LogElement] (Block, Env)
inferBlock (DBlock []) _ env = return $ (DBlock [], env)
inferBlock (DBlock stms) ftyp env = do
  (tstms, env') <- inferStms stms (Env.addScope env ftyp) 
  return $ (DBlock tstms, env')
      
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
    if not (isTypeError texp || isCompatible texp (TSimple SType_Bool))
      then
        saveLog $ launchError (getLoc texp) (WrongWhileCondition exp texp)
      else
        return ()

    (tstm', env') <- inferStm stm' env
    if Env.hasReturn env' 
      then
        return $ (SWhile texp tstm', Env.setReturnFound env)
      else
        return $ (SWhile texp tstm', env)
  
  SIfElse exp stmif stmelse -> do
    texp <- inferExp exp env
    if not (isTypeError texp || isCompatible texp (TSimple SType_Bool))
      then 
        saveLog $ launchError (getLoc texp) (WrongIfCondition exp texp)
      else return ()

    (tstmif, envif) <- inferStm stmif env
    (tstmelse, envelse) <- inferStm stmelse env
    if Env.hasReturn envif && Env.hasReturn envelse then
      return $ (SIfElse texp tstmif tstmelse, Env.setReturnFound env)
    else 
      return $ (SIfElse texp tstmif tstmelse, env)

  SDecl decl -> do
    (tdecl, env') <- inferDecl decl env
    return $ (SDecl tdecl, env')

  SBlock block -> let ftyp = Env.getScopeType env in do
    (tblock, env') <- inferBlock block ftyp env
    if Env.hasReturn env'
      then
        return $ (SBlock tblock, Env.setReturnFound env)
      else
        return $ (SBlock tblock, env)
  
  SAssign lexp exp -> do
    tlexp <- inferLExp lexp env
    texp <- inferExp exp env
    if not (isTypeError tlexp || isTypeError texp || isCompatible texp (getType tlexp))
      then 
        saveLog $ launchError (getLoc texp) (WrongExpAssignType exp texp tlexp lexp)
      else
        return ()
    return $ (SAssign tlexp texp, env)

  SReturnExp preturn@(PReturn (loc, id)) exp -> let ftyp = Env.getScopeType env in
    do
      texp <- inferExp exp env
      if isTypeError texp
        then
          return $ (SReturnExp preturn texp, env)
        else
          case ((isCompatible texp ftyp), (isTypeVoid ftyp)) of
            (True,_) -> 
              return $ (SReturnExp preturn texp, Env.setReturnFound env)
            (False, True) -> do
              saveLog $ launchError loc UnexpectedReturn
              return $ (SReturnExp preturn texp, env)
            (False, False) -> do
              saveLog $ launchError loc (WrongExpType exp texp ftyp)
              return $ (SReturnExp preturn texp, env)

  SReturn preturn@(PReturn (loc, _))-> let ftyp = Env.getScopeType env in
    if not (isTypeVoid ftyp)
      then do
        saveLog $ launchError loc (WrongReturnValue ftyp)
        return $ (SReturn preturn, env)
      else
        return $ (SReturn preturn, env)


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
        Ok (VarInfo dloc _) ->
         -- è da mettere a posto sto errore.
         saveLog $ launchError loc (DuplicateVariable ident dloc)
        Bad except ->
          saveLog $ launchError loc except
        Ok (FunInfo dloc typ paramclauses) -> 
          let typ_args = map (\(PArg x) -> (map (\(DArg ident typ) -> typ) x)) paramclauses in
            if not (any (isTypeError) (concat tparams))
              then
                if typ_args == typ_params 
                  then
                    if not (isTypeVoid typ)
                      then
                        saveLog $ launchError loc (MissingAssignVariable ident)
                      else
                        return ()
                  else 
                    saveLog $ launchError loc (WrongProcParams ident typ_args typ_params)
              else
                return ()
    return (SProcCall id (map (\x -> (ParExp x)) tparams) , env)



getDLoc :: LExp -> Loc
getDLoc (LExpTyped _ _ _ dloc) = dloc

inferLExp :: LExp -> Env -> Writer [LogElement] LExp
inferLExp lexp env = case lexp of
 LRef lexp' -> do
   tlexp' <- inferLExp lexp' env
   if isTypeError tlexp' 
     then return $ LExpTyped (LRef tlexp') (TSimple TypeError) (getLoc tlexp') (getDLoc tlexp') 
     else
       case tlexp' of
         (LExpTyped _ (TPointer typ) loc dloc) -> return $ LExpTyped (LRef tlexp') typ loc dloc
         (LExpTyped _ typ' loc dloc) -> do
           saveLog $ launchError loc (WrongPointerApplication lexp' typ')
           return $ LExpTyped (LRef tlexp') (TSimple TypeError) loc dloc

 LArr lexp exp -> do
   tlexp <- inferLExp lexp env
   texp <- inferExp exp env
   if isTypeError tlexp || isTypeError texp
     then
       return $ LExpTyped (LArr tlexp texp) (TSimple TypeError) (getLoc tlexp) (getDLoc tlexp) 
     else
       case (tlexp , isCompatible texp (TSimple SType_Int)) of
         (LExpTyped _ (TArray typ _) loc dloc, True) -> return $ LExpTyped (LArr tlexp texp) typ loc dloc
         (LExpTyped _ (TArray typ _) loc dloc, False) -> do
           saveLog $ launchError loc (WrongArrayIndex exp texp)
           return $ LExpTyped (LArr tlexp texp) (TSimple TypeError) loc dloc
         (_, False) -> do
           saveLog $ launchError (getLoc texp) (WrongArrayAccess  lexp (getType tlexp))
           saveLog $ launchError (getLoc texp) (WrongArrayIndex exp texp)
           return  $ LExpTyped (LArr tlexp texp) (TSimple TypeError) (getLoc tlexp) (getDLoc tlexp)
         (_, True) -> do
           saveLog $ launchError (getLoc texp) (WrongArrayAccess lexp (getType tlexp))
           return  $ LExpTyped (LArr tlexp texp) (TSimple TypeError) (getLoc tlexp) (getDLoc tlexp)
 LIdent id@(PIdent (loc, ident)) -> let res = Env.lookup env id in
   case res of
     Bad except -> do
       saveLog $ launchError loc except
       return $ LExpTyped lexp (TSimple TypeError) loc (0,0)
     -- dloc dichiarazione loc
     Ok (VarInfo dloc typ) -> return $ LExpTyped (LIdent id) typ loc dloc
     Ok (FunInfo dloc _ _) -> do
       -- è da mettere a posto sto errore.
       saveLog $ launchError loc (DuplicateFunction ident dloc)
       return $ LExpTyped lexp (TSimple TypeError) loc dloc



-- Prende una lista di espressioni tipizzate, un tipo, e ritorna una coppia con il primo elemento
-- che dice se si è trovato almeno un elemento con tipo (TSimple TypeError), ed il secondo elemento che dice
-- se tutte le espressioni hanno tipo type o meno.
inferArrayAux :: [Exp] -> TypeSpec -> (Bool, Bool)
inferArrayAux texps typ = ( (any (\x -> isCompatible x (TSimple TypeError)) texps),(all (\x -> isCompatible x typ) texps) )

inferExp :: Exp -> Env -> Writer [LogElement] Exp
inferExp exp env = case exp of
  --DummyExp -> return (ETyped exp (TSimple TypeVoid) (0,0) )  
  -- Punto di vista di Bulzo: gli array di dim 0 non hanno alcuna utilità per il programmatore nel nostro linguaggio
  -- in cui non ci sono liste e non ci sono allocazioni dinamiche di memoria.
                                                                              -- posizione fittizia. Spunta da qualche parte?
  EArray [] -> return $ ETyped (exp) (TArray (TSimple TypeVoid) (PInteger ( (0,0), show (0)  ) )) (0,0) 
  EArray exps -> do
    texps <- mapM (\x -> inferExp x env) exps
    case inferArrayAux texps (getType (head texps) ) of
      (True, _) -> return $ ETyped (EArray texps) (TArray (TSimple TypeError) (PInteger ( (getLoc (head texps)) , show (length texps)  ) )) (getLoc (head texps)) 
      (_, True) -> return $ ETyped (EArray texps) (TArray (getType (head texps) )  (PInteger ( (getLoc (head texps)), show (length texps)  ))) (getLoc (head texps))
      (_ , _) -> do
        -- Da mettere un più bel messaggio di errore.
        -- ETyped Exp TypeSpec Integer Integer
        saveLog $ launchError (getLoc (head texps)) ArrayInconsistency
        -- bisogna far tornare l'espressione tipata, non la lunghezza della lisa texps
        return $ ETyped (EArray texps) (TSimple TypeError) (getLoc (head texps))

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
         saveLog $ launchError loc (DuplicateVariable ident dloc)
         return $ ETyped (EFunCall id (map (\x -> (ParExp x)) tparams)) (TSimple TypeError) dloc
        Bad except -> do
          saveLog $ launchError loc except
          return $ ETyped (EFunCall id (map (\x -> (ParExp x)) tparams)) (TSimple TypeError) loc
        Ok (FunInfo dloc typ paramclauses) ->
          let typ_args = map (\(PArg x) -> (map (\(DArg ident typ) -> typ) x)) paramclauses in
            if any (isTypeError) (concat tparams)
              then return $ ETyped (EFunCall id (map (\x -> (ParExp x)) tparams)) (TSimple TypeError) dloc
              else
                if typ_args == typ_params 
                  then return $ ETyped (EFunCall id (map (\x -> (ParExp x)) tparams)) typ dloc
                  else do
                    saveLog $ launchError loc (WrongFunctionParams ident typ_args typ_params typ)
                    return $ ETyped (EFunCall id (map (\x -> (ParExp x)) tparams)) (TSimple TypeError) dloc

  ENot exp -> do
    texp <- inferExp exp env
    if isTypeError texp || isCompatible texp (TSimple SType_Bool)
      then return (ETyped (ENot texp) (getType texp) (getLoc texp))
      else do
        saveLog $ launchError (getLoc texp) (WrongNotApplication exp texp)
        return $ ETyped (ENot texp) (TSimple TypeError) (getLoc texp)

  ENeg exp -> do
    texp <- inferExp exp env
    if isTypeError texp || isCompatible texp (TSimple SType_Int) || isCompatible texp (TSimple SType_Float)
      then return $ ETyped (ENeg texp) (getType texp) (getLoc texp)
      else do
        saveLog $ launchError (getLoc texp) (WrongNegApplication exp texp)
        return $ (ETyped (ENeg texp) (TSimple TypeError) (getLoc texp))
  
  ELExp lexp -> do
    tlexp <- inferLExp lexp env
    return $ ETyped (ELExp tlexp) (getType tlexp) (getLoc tlexp)

  EDeref lexp -> do
    tlexp <- inferLExp lexp env
    if isTypeError tlexp
      then return $ ETyped (EDeref tlexp) (TSimple TypeError) (getLoc tlexp)
      else return $ ETyped (EDeref tlexp) (TPointer (getType tlexp)) (getLoc tlexp)

  EInt    const@(PInteger (loc, _)) -> return $ ETyped (EInt const) (TSimple SType_Int) (loc) 
  EFloat  const@(PFloat (loc, _))   -> return $ ETyped (EFloat const) (TSimple SType_Float) (loc) 
  EChar   const@(PChar (loc, _))    -> return $ ETyped (EChar const) (TSimple SType_Char) (loc) 
  EString const@(PString (loc, _))  -> return $ ETyped (EString const) (TSimple SType_String) (loc)
  ETrue   const@(PTrue (loc, _))    -> return $ ETyped (ETrue const) (TSimple SType_Bool) (loc)
  EFalse  const@(PFalse (loc, _))   -> return $ ETyped (EFalse const) (TSimple SType_Bool) (loc)
  -- Ma serve Null? Con i puntatori probabilmente si, nel caso non avrebbe TypeVoid.
  ENull const@(PNull (loc, _)) -> return $ ETyped (ENull const) (TSimple TypeVoid) loc 
  EOp expl op expr -> inferBinOp expl op expr env


-- Prese due espressioni ed un operatore binario ritorna la corrispondente espressione tipizzata
inferBinOp :: Exp -> Op -> Exp -> Env -> Writer [LogElement] Exp
inferBinOp expl op expr env = do 
    texpl <- inferExp expl env
    texpr <- inferExp expr env

    case ((getTypeOp op), (getType texpl), (getType texpr) ) of
      (_ , TSimple TypeError, _ ) -> return $ ETyped (EOp texpl op texpr) (TSimple TypeError) (getLoc texpl) 
      (_ , _ , TSimple TypeError) -> return $ ETyped (EOp texpl op texpr) (TSimple TypeError) (getLoc texpl) 
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
  saveLog $ launchError (getLoc texpl) (WrongOpApplication op texpl texpr)
  return $ ETyped (EOp texpl op texpr) (TSimple TypeError) (getLoc texpl) 

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

-- controllare che operazione mod di possa fare tra Float
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