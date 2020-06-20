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
        intercalate " " [buildAddr addr1 True,"=",buildColAssignType typ,buildAddr addr2 True,buildBinOpr op,buildAddr addr3 True, opComment op]

    AssignUnOp addr1 op addr2 typ ->
        intercalate " " [buildAddr addr1 True,"=",buildColAssignType typ,buildUnOpr op,buildAddr addr2 True]

    Assign addr1 addr2 typ ->
        intercalate " " [buildAddr addr1 True,"=",buildColAssignType typ,buildAddr addr2 True]
        
    AssignFromArray addr1 addr2 addr3 typ ->
        intercalate " " [buildAddr addr1 True,"=",buildColAssignType typ,buildAddr addr2 True,"[",buildAddr addr3 True,"]"]

    AssignToArray addr1 addr2 addr3 typ ->
        intercalate " " [buildAddr addr1 True,"[",buildAddr addr2 True,"]","=",buildColAssignType typ,buildAddr addr3 True]

    AssignFromRef addr1 addr2 typ ->
        intercalate " " [buildAddr addr1 True,"=",buildColAssignType typ,"&",buildAddr addr2 True]
        
    AssignFromPointer addr1 addr2 typ ->
        intercalate " " [buildAddr addr1 True,"=",buildColAssignType typ,"*",buildAddr addr2 True] 
        
    AssignToPointer addr1 addr2 typ ->
        intercalate " " ["*",buildAddr addr1 True,"=",buildColAssignType typ,buildAddr addr2 True] 
        
    AssignFromFunction addr1 label n typ ->
        intercalate " " [buildAddr addr1 True,"=",buildColAssignType typ, (color Blue Bold "fcall"),buildLabel label,",",show n] 
        
    Goto label -> 
        (color Blue Bold "goto ") ++ buildLabel label
    
    IfBool addr1 label -> 
        (color Blue Bold "if ") ++ buildAddr addr1 True ++ (color Blue Bold " goto ") ++  buildLabel label

    IfRel op addr1 addr2 label -> 
        (color Blue Bold "if ") ++ buildAddr addr1 True ++ " " ++ buildBinOpr op ++ " " ++ buildAddr addr2 True ++ (color Blue Bold " goto ") ++ buildLabel label

    IfFalse addr1 label -> 
        (color Blue Bold "ifFalse ") ++ buildAddr addr1 True ++ (color Blue Bold " goto ") ++ buildLabel label
    
    ReturnVoid -> 
        (color Blue Bold "return")

    ReturnAddr addr1 -> 
        (color Blue Bold "return ") ++ buildAddr addr1 True

    Param addr1 -> 
        (color Blue Bold "param ") ++ buildAddr addr1 True

    Call label n ->
        intercalate " " [(color Blue Bold "pcall"),buildLabel label,",",show n]

    Comment comment ->
        color Default Italic $ color Default Faint $ "// " ++ comment

    CommentArgs tactyp_addr -> 
        color Default Italic $ color Default Faint $ "// Args: " ++ if (length tactyp_addr == 0)
            then "None"
            else intercalate ", " (map (\(t,a) ->(buildAddr a False)) tactyp_addr)


opComment :: BinOp -> String
opComment op =  color Default Italic $ color Default Faint $ "\t // " ++ show op

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
    AbsTAC.NegInt    -> "-"
    AbsTAC.NegFloat  -> "-"
    AbsTAC.Not       -> "!"
    AbsTAC.Cast typ  -> "(" ++ buildAssignType typ ++ ")"

buildAddr :: Addr -> Bool -> String
buildAddr addr colored = case addr of
    Var ident (r,c)  -> if colored
        then id (color Green Bold ident) ++ (color Green Italic ("@(" ++ show r ++ "," ++ show c ++ ")"))
        else  id ident ++ "@(" ++ show r ++ "," ++ show c ++ ")"
    VarCopy ident (r,c) -> if colored
        then id (color Green Bold ident) ++ (color Green Italic ("@copy@(" ++ show r ++ "," ++ show c ++ ")"))
        else id ident ++ "@copy@(" ++ show r ++ "," ++ show c ++ ")"
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