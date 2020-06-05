module PrintTAC where

import AbsTAC
import Data.List
import Color

-- (Minimum) width of the column containing labels.
columnWidth :: Int
columnWidth = 16

-- Entrypoint. Called by TestGramm.
printTAC :: [TAC] -> String
printTAC code = intercalate "\n" (printTACAux code)

printTACAux :: [TAC] -> [String]
printTACAux [] = return []
printTACAux [Lab label] = [buildLabel label]
printTACAux ((Lab label1):(Lab label2):xs) = (buildInstrLabel label1):(printTACAux ((Lab label2):xs))
printTACAux ((Lab label):x:xs)
    | length stringLabel < columnWidth = (padStringLabel stringLabel ++ buildTACInstruction x):(printTACAux xs)
    | otherwise = stringLabel:(printTACAux (x:xs))
    where stringLabel = buildInstrLabel label
printTACAux (x:xs) = (padStringLabel "" ++ buildTACInstruction x):(printTACAux xs)

-- Convert TAC instruction to string. Does not receive labels.
buildTACInstruction :: TAC -> String
buildTACInstruction instr = case instr of
    AssignBinOp addr1 addr2 op addr3 typ -> 
        intercalate " " [buildAddr addr1,"=",buildColAssignType typ,buildAddr addr2,buildBinOpr op,buildAddr addr3]

    AssignUnOp addr1 op addr2 typ ->
        intercalate " " [buildAddr addr1,"=",buildColAssignType typ,buildUnOpr op,buildAddr addr2]

    Assign addr1 addr2 typ ->
        intercalate " " [buildAddr addr1,"=",buildColAssignType typ,buildAddr addr2]
        
    AssignFromArray addr1 addr2 addr3 typ ->
        intercalate " " [buildAddr addr1,"=",buildColAssignType typ,buildAddr addr2,"[",buildAddr addr3,"]"]

    AssignToArray addr1 addr2 addr3 typ ->
        intercalate " " [buildAddr addr1,"[",buildAddr addr2,"]","=",buildColAssignType typ,buildAddr addr3]

    AssignFromRef addr1 addr2 typ ->
        intercalate " " [buildAddr addr1,"=",buildColAssignType typ,"&",buildAddr addr2]
        
    AssignFromPointer addr1 addr2 typ ->
        intercalate " " [buildAddr addr1,"=",buildColAssignType typ,"*",buildAddr addr2] 
        
    AssignToPointer addr1 addr2 typ ->
        intercalate " " ["*",buildAddr addr1,"=",buildColAssignType typ,buildAddr addr2] 
        
    AssignFromFunction addr1 label n typ ->
        intercalate " " [buildAddr addr1,"=",buildColAssignType typ,"fcall",buildLabel label,",",show n] 
        
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

    Call label n ->
        intercalate " " ["pcall",buildLabel label,",",show n]

    Comment comment ->
        color Default Italic $ color Default Faint $ "// " ++ comment

    CommentArgs tactyp_addr -> 
        color Default Italic $ color Default Faint $ "// Args: " ++ if (length tactyp_addr == 0)
            then "None"
            else intercalate ", " (map (\(t,a) -> (buildAssignType t) ++ " " ++ (buildAddr a)) tactyp_addr)


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

buildAssignType :: TACType -> String
buildAssignType TACInt = "int"
buildAssignType TACFloat = "float"
buildAssignType TACChar = "char"  
buildAssignType TACString = "string"
buildAssignType TACBool = "bool"
buildAssignType TACAddr = "addr"

buildColAssignType :: TACType -> String
buildColAssignType t = color Magenta Bold $ buildAssignType t 

buildLabel :: Label -> String
buildLabel label = case label of
    LabStm n              -> id "l" ++ show n
    LabFun ident (r,c)       ->  case (r,c) of
        (0,0) -> ident ++ "@default"
        otherwise -> ident ++ "@(" ++ show r ++ "," ++ show c ++ ")"

buildInstrLabel :: Label -> String
buildInstrLabel label = (buildLabel label) ++ ":" 

padStringLabel :: String -> String
padStringLabel x = x ++ concat ( replicate (max 1 (columnWidth - length x)) " " )
