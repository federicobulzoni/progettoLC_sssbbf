module PrintTAC where

import AbsTAC

-- (Minimum) width of the column containing labels.
columnWidth :: Int
columnWidth = 20

-- Entrypoint. Called by TestGramm.
printTAC :: [TAC] -> IO ()
printTAC [] = return ()
printTAC [Lab label] = putStrLn $ buildLabel label
printTAC ((Lab label1):(Lab label2):xs) = do
    putStrLn $ buildInstrLabel label1
    printTAC $ (Lab label2):xs
printTAC ((Lab label):x:xs)
    | length stringLabel < columnWidth = do 
        putStr $ padStringLabel stringLabel
        putStrLn $ buildTACInstruction x
        printTAC xs
    | otherwise = do
        putStrLn $ stringLabel
        printTAC (x:xs)
    where stringLabel = buildInstrLabel label
printTAC (x:xs) = do
    putStr $ padStringLabel ""
    putStrLn $ buildTACInstruction x
    printTAC xs

-- Convert TAC instruction to string. Does not receive labels.
buildTACInstruction :: TAC -> String
buildTACInstruction istr = case istr of
    -- TODO: Type?
    AssignBinOp op addr1 addr2 addr3 -> 
        buildAddr addr1 ++ " = " ++ buildAddr addr2 ++ " " ++ buildBinOpr op ++ " " ++ buildAddr addr3

    -- TODO: Type?
    AssignUnOp op addr1 addr2 -> 
        buildAddr addr1 ++ " = " ++ buildUnOpr op ++ " " ++ buildAddr addr2

    -- TODO: Type?
    Assign addr1 addr2 -> 
        buildAddr addr1 ++ " = " ++ buildAddr addr2

    AssignFromArray addr1 addr2 addr3 -> 
        buildAddr addr1 ++ " = " ++  buildAddr addr2 ++ "[" ++ buildAddr addr3 ++ "]"

    AssignToArray addr1 addr2 addr3 -> 
        buildAddr addr1 ++ "[" ++ buildAddr addr2 ++ "]" ++ " = " ++ buildAddr addr3 

    AssignFromRef addr1 addr2 -> 
        buildAddr addr1 ++ " = " ++ "&" ++ buildAddr addr2

    AssignFromPointer addr1 addr2 -> 
        buildAddr addr1 ++ " = " ++ "*" ++ buildAddr addr2

    AssignToPointer addr1 addr2 -> 
        "*" ++ buildAddr addr1 ++ " = " ++ buildAddr addr2

    -- TODO: Type?
    AssignFromFunction addr1 label  n -> 
        buildAddr addr1 ++ " = " ++ "call " ++ buildLabel label ++ " , n = " ++ show n

    Goto label -> 
        "goto " ++ buildLabel label
    
    IfBool addr1 label -> 
        "if " ++ buildAddr addr1 ++ " goto " ++  buildLabel label

    IfRel op addr1 addr2 label -> 
        "if " ++ buildAddr addr1 ++ " " ++ buildBinOpr op ++ " " ++ buildAddr addr2 ++ " goto " ++ buildLabel label

    IfFalse addr1 label -> 
        "ifFalse " ++ buildAddr addr1 ++ " goto " ++ buildLabel label
    
    ReturnVoid -> 
        "return"

    ReturnAddr addr1 -> 
        "return " ++ buildAddr addr1

    Param addr1 -> 
        "param " ++ buildAddr addr1




buildBinOpr :: BinOp -> String
buildBinOpr op = case op of
    AbsTAC.PlusInt       -> "+"
    AbsTAC.PlusFloat     -> "+"
    AbsTAC.MinusInt      -> "-"
    AbsTAC.MinusFloat    -> "-"
    AbsTAC.ProdInt       -> "*"
    AbsTAC.ProdFloat     -> "*"
    AbsTAC.DivInt        -> "/"
    AbsTAC.DivFloat      -> "/"
    AbsTAC.ModInt        -> "%"
    AbsTAC.ModFloat      -> "%"
    AbsTAC.PowInt        -> "^"
    AbsTAC.PowFloat      -> "^"
    AbsTAC.Or            -> "||"
    AbsTAC.And           -> "&&"
    AbsTAC.Equal         -> "=="
    AbsTAC.NotEqual      -> "!="
    AbsTAC.Less          -> "<"
    AbsTAC.LessEq        -> "<="
    AbsTAC.Greater       -> ">"
    AbsTAC.GreaterEq     -> ">="

buildUnOpr :: UnOp -> String
buildUnOpr op = case op of
    AbsTAC.NegInt    -> "!"
    AbsTAC.NegFloat  -> "!"
    AbsTAC.Not       -> "-"

buildAddr :: Addr -> String
buildAddr addr = case addr of
    Var ident (r,c)   -> id ident ++ "@(" ++ show r ++ "," ++ show c ++ ")"
    Temp n            -> id "t" ++ show n
    LitString n       -> n
    LitFloat n        -> show n
    LitChar n         -> show n
    LitInt n          -> show n
    LitBool n         -> show n
    LitNull           -> "Null"

buildLabel :: Label -> String
buildLabel label = case label of
    LabStm n              -> id "l" ++ show n
    LabFun ident (r,c)       ->  ident ++ "@(" ++ show r ++ "," ++ show c ++ ")"

buildInstrLabel :: Label -> String
buildInstrLabel label = (buildLabel label) ++ ":" 

padStringLabel :: String -> String
padStringLabel x = x ++ concat ( replicate (max 1 (columnWidth - length x)) " " )


-- TODO: qua sotto non serve.
printTAC2 :: [TAC] -> IO ()
printTAC2 [] = putStrLn "\n"
printTAC2 (istr:istrs) = do
    printTACInstruction istr
    printTAC2 istrs


printTACInstruction :: TAC -> IO ()
printTACInstruction istr = case istr of
    AssignBinOp op addr1 addr2 addr3 -> putStrLn $ buildAddr addr1 ++ "\t=\t" ++ buildAddr addr2 ++ " " ++ buildBinOpr op ++ " " ++ buildAddr addr3
    
    AssignUnOp op addr1 addr2 -> putStrLn $ buildAddr addr1 ++ "\t=\t" ++ buildUnOpr op ++ " " ++ buildAddr addr2
    
    Assign addr1 addr2 -> putStrLn $ buildAddr addr1 ++ "\t=\t" ++ buildAddr addr2

    AssignFromArray addr1 addr2 addr3 -> putStrLn $ buildAddr addr1 ++ "\t=\t" ++  buildAddr addr2 ++ "[" ++ buildAddr addr3 ++ "]"

    AssignToArray addr1 addr2 addr3 -> putStrLn $ buildAddr addr1 ++ "[" ++ buildAddr addr2 ++ "]" ++ "\t=\t" ++ buildAddr addr3 

    AssignFromRef addr1 addr2 -> putStrLn $ buildAddr addr1 ++ "\t=\t" ++ "&" ++ buildAddr addr2

    AssignFromPointer addr1 addr2 -> putStrLn $ buildAddr addr1 ++ "\t=\t" ++ "*" ++ buildAddr addr2

    AssignToPointer addr1 addr2 -> putStrLn $ "*" ++ buildAddr addr1 ++ "\t=\t" ++ buildAddr addr2

    AssignFromFunction addr1 label  n-> putStrLn $  buildAddr addr1 ++ "\t=\t" ++ "call " ++ buildLabel label ++ ", n = " ++ show n

    Goto label -> putStrLn $ "goto " ++ buildLabel label
    
    IfBool addr1 label -> putStrLn $ "IfBool " ++ buildAddr addr1 ++ " goto " ++  buildLabel label

    IfRel op addr1 addr2 label -> putStrLn $ "IfRel " ++ buildAddr addr1 ++ buildBinOpr op ++ buildAddr addr2 ++ " goto " ++ buildLabel label

    IfFalse addr1 label -> putStrLn $ "IfFalse " ++ buildAddr addr1 ++ " goto " ++ buildLabel label

    Lab label -> putStrLn $ "\nlabel " ++  buildLabel label
    
    ReturnVoid -> putStrLn $ "ReturnVoid" ++ "\n---------"

    ReturnAddr addr1 -> putStrLn $ "Return " ++ buildAddr addr1

    Param addr1 -> putStrLn $ "Param " ++ buildAddr addr1