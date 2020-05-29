module PrintTAC where

import AbsTAC
import AbsGramm
import Control.Monad.State.Lazy

printTAC2 :: [TAC] -> IO ()
printTAC2 (istr:istrs) = do
    printTACIstruction istr
    printTAC2 istrs

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

printTACIstruction :: TAC -> IO ()
printTACIstruction istr = case istr of
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
    
    IfBool addr1 label -> putStrLn $ "IfFalse " ++ buildAddr addr1 ++ " goto " ++  buildLabel label

    IfRel op addr1 addr2 label -> putStrLn $ "IfFalse " ++ buildAddr addr1 ++ buildBinOpr op ++ buildAddr addr2 ++ " goto " ++ buildLabel label

    IfBool addr1 label -> putStrLn $ "IfFalse " ++ buildAddr addr1 ++ " goto " ++ buildLabel label

    Lab label -> putStrLn $ "\n---------\n" ++ "label " ++  buildLabel label
    
    ReturnVoid -> putStrLn $ "ReturnVoid" ++ "\n---------"

    ReturnAddr addr1 -> putStrLn $ "Return " ++ buildAddr addr1

    Param addr1 -> putStrLn $ "Param " ++ buildAddr addr1