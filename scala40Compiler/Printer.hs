{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for PrintGramm.
--   Generated by the BNF converter.

module Printer where

import qualified AbsGramm
import Data.Char
import Color
-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else ' ':s)

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prt2 :: Int -> a -> Doc
  prt2 _ x = doc (shows "")
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)
instance Print Int where
  prt _ x = doc (shows x)
    
    
instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsGramm.PTrue where
  prt _ (AbsGramm.PTrue (_,i)) = doc (showString i)

instance Print AbsGramm.PFalse where
  prt _ (AbsGramm.PFalse (_,i)) = doc (showString i)

instance Print AbsGramm.PReturn where
  prt _ (AbsGramm.PReturn (_,i)) = doc (showString i)

instance Print AbsGramm.PNull where
  prt _ (AbsGramm.PNull (_,i)) = doc (showString i)

instance Print AbsGramm.PBreak where
  prt _ (AbsGramm.PBreak (_,i)) = doc (showString i)

instance Print AbsGramm.PContinue where
  prt _ (AbsGramm.PContinue (_,i)) = doc (showString i)

instance Print AbsGramm.PIdent where
  prt _ (AbsGramm.PIdent (_,i)) = doc (showString i)

instance Print AbsGramm.PFloat where
  prt _ (AbsGramm.PFloat (_,i)) = doc (showString i)

instance Print AbsGramm.PInteger where
  prt _ (AbsGramm.PInteger (_,i)) = doc (showString i)
  prt2 _ (AbsGramm.PInteger (_,i)) = doc (showString $ t i)


instance Print AbsGramm.PString where
  prt _ (AbsGramm.PString (_,i)) = doc (showString i)

instance Print AbsGramm.PChar where
  prt _ (AbsGramm.PChar (_,i)) = doc (showString i)

instance Print AbsGramm.Loc where
  prt i (line, column) =  (concatD [doc (showString "(line "), prt 0 line, doc (showString ", column "), prt 0 column, doc (showString ")")])
  
instance Print AbsGramm.TypeSpec where
  prt i e = case e of
    AbsGramm.TSimple stype -> prPrec i 0 (concatD [prt 0 stype])
    AbsGramm.TPointer typespec -> prPrec i 0 (concatD [doc (showString "*"), prt 0 typespec])
    AbsGramm.TArray typespec pinteger -> prPrec i 0 (concatD [doc (showString "Array"), doc (showString "["), prt 0 typespec, doc (showString "]"), doc (showString "("), prt 0 pinteger, doc (showString ")")])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

  prt2 i e = case e of
    AbsGramm.TSimple stype -> prPrec i 0 (concatD [prt2 0 stype])
    AbsGramm.TPointer typespec -> prPrec i 0 (concatD [doc (showString $ t "*"), prt2 0 typespec])
    AbsGramm.TArray typespec pinteger -> prPrec i 0 (concatD [doc (showString $ t "Array"), doc (showString $ t "["), prt2 0 typespec, doc (showString $ t "]"), doc (showString $ t "("), prt2 0 pinteger, doc (showString $ t ")")]) 


instance Print [AbsGramm.TypeSpec] where
  prt = prtList
  
instance Print [[AbsGramm.TypeSpec]] where
  prt i e = concatD (map (\x -> concatD [doc (showString "("), prt 0 x, doc (showString ")") ] ) e )

instance Print AbsGramm.SType where
  prt i e = case e of
    AbsGramm.SType_Float -> prPrec i 0 (concatD [doc (showString "Float")])
    AbsGramm.SType_Int -> prPrec i 0 (concatD [doc (showString "Int")])
    AbsGramm.SType_Char -> prPrec i 0 (concatD [doc (showString "Char")])
    AbsGramm.SType_String -> prPrec i 0 (concatD [doc (showString "String")])
    AbsGramm.SType_Bool -> prPrec i 0 (concatD [doc (showString "Bool")])
    AbsGramm.SType_Error -> prPrec i 0 (concatD [doc (showString "Error")])
    AbsGramm.SType_Void -> prPrec i 0 (concatD [doc (showString "Void")])

  prt2 i e = case e of
    AbsGramm.SType_Float  -> prPrec i 0 (concatD [doc (showString $ t "Float")])
    AbsGramm.SType_Int    -> prPrec i 0 (concatD [doc (showString $ t "Int")])
    AbsGramm.SType_Char   -> prPrec i 0 (concatD [doc (showString $ t "Char")])
    AbsGramm.SType_String -> prPrec i 0 (concatD [doc (showString $ t "String")])
    AbsGramm.SType_Bool   -> prPrec i 0 (concatD [doc (showString $ t "Bool")])
    AbsGramm.SType_Error  -> prPrec i 0 (concatD [doc (showString $ t "Error")])
    AbsGramm.SType_Void   -> prPrec i 0 (concatD [doc (showString $ t "Void")])

instance Print AbsGramm.Program where
  prt i e = case e of
    AbsGramm.Prog declarations -> prPrec i 0 (concatD [prt 0 declarations])

instance Print [AbsGramm.Declaration] where
  prt = prtList

instance Print AbsGramm.Declaration where
  prt i e = case e of
    AbsGramm.DefVar pident typespec exp -> prPrec i 0 (concatD [doc (showString "var"), prt 0 pident, doc (showString ":"), prt 0 typespec, doc (showString "="), prt 0 exp, doc (showString ";")])
    AbsGramm.DecVar pident typespec -> prPrec i 0 (concatD [doc (showString "var"), prt 0 pident, doc (showString ":"), prt 0 typespec, doc (showString ";")])
    AbsGramm.DefFun pident paramclauses typespec block -> prPrec i 0 (concatD [doc (showString "def"), prt 0 pident, prt 0 paramclauses, doc (showString ":"), prt 0 typespec, doc (showString "="), prt 0 block])
    AbsGramm.DefFunInLine pident paramclauses typespec exp -> prPrec i 0 (concatD [doc (showString "def"), prt 0 pident, prt 0 paramclauses, doc (showString ":"), prt 0 typespec, doc (showString "="), prt 0 exp, doc (showString ";")])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [AbsGramm.ParamClause] where
  prt = prtList

instance Print AbsGramm.ParamClause where
  prt i e = case e of
    AbsGramm.PParam params -> prPrec i 0 (concatD [doc (showString "("), prt 0 params, doc (showString ")")])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [AbsGramm.Param] where
  prt = prtList

instance Print AbsGramm.Param where
  prt i e = case e of
    AbsGramm.DParam parampassmod pident typespec -> prPrec i 0 (concatD [prt 0 parampassmod, prt 0 pident, doc (showString ":"), prt 0 typespec])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsGramm.Block where
  prt i e = case e of
    AbsGramm.DBlock stms -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stms, doc (showString "}")])

instance Print AbsGramm.ParamPassMod where
  prt i e = case e of
    AbsGramm.ParamPassMod_val -> prPrec i 0 (concatD [doc (showString "val")])
    AbsGramm.ParamPassMod_ref -> prPrec i 0 (concatD [doc (showString "ref")])
    AbsGramm.ParamPassMod_res -> prPrec i 0 (concatD [doc (showString "res")])
    AbsGramm.ParamPassMod_valres -> prPrec i 0 (concatD [doc (showString "valres")])

instance Print [AbsGramm.Stm] where
  prt = prtList

instance Print AbsGramm.Stm where
  prt i e = case e of
    AbsGramm.SDecl declaration -> prPrec i 0 (concatD [prt 0 declaration])
    AbsGramm.SBlock block -> prPrec i 0 (concatD [prt 0 block])
    AbsGramm.SAssign lexp exp -> prPrec i 0 (concatD [prt 0 lexp, doc (showString "="), prt 0 exp, doc (showString ";")])
    AbsGramm.SWhile exp stm -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 exp, doc (showString ")"), prt 0 stm])
    AbsGramm.SIfElse exp stm1 stm2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 exp, doc (showString ")"), prt 0 stm1, doc (showString "else"), prt 0 stm2])
    AbsGramm.SDoWhile stm exp -> prPrec i 0 (concatD [doc (showString "do"), prt 0 stm, doc (showString "while"), doc (showString "("), prt 0 exp, doc (showString ")"), doc (showString ";")])
    AbsGramm.SFor id exp1 exp2 exp3 stm -> prPrec i 0 (concatD [doc (showString "for"), doc (showString "("), prt 0 id, doc (showString "<-"), prt 0 exp1, doc (showString "until"), prt 0 exp2, doc (showString "by"), prt 0 exp3, doc (showString ")"), prt 0 stm])
    AbsGramm.SReturn preturn -> prPrec i 0 (concatD [prt 0 preturn, doc (showString ";")])
    AbsGramm.SContinue pcontinue -> prPrec i 0 (concatD [prt 0 pcontinue, doc (showString ";")])
    AbsGramm.SBreak pbreak -> prPrec i 0 (concatD [prt 0 pbreak, doc (showString ";")])
    AbsGramm.SReturnExp preturn exp -> prPrec i 0 (concatD [prt 0 preturn, prt 0 exp, doc (showString ";")])
    AbsGramm.SProcCall pident argss -> prPrec i 0 (concatD [prt 0 pident, prt 0 argss, doc (showString ";")])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print AbsGramm.OpAssign where
  prt i e = case e of
    AbsGramm.ProdEq -> prPrec i 0 (concatD [doc (showString "*=")])
    AbsGramm.DivEq -> prPrec i 0 (concatD [doc (showString "/=")])
    AbsGramm.ModEq -> prPrec i 0 (concatD [doc (showString "%=")])
    AbsGramm.PlusEq -> prPrec i 0 (concatD [doc (showString "+=")])
    AbsGramm.MinusEq -> prPrec i 0 (concatD [doc (showString "-=")])
    AbsGramm.PowEq -> prPrec i 0 (concatD [doc (showString "^=")])

instance Print AbsGramm.Args where
  prt i e = case e of
    AbsGramm.ArgExp exps -> prPrec i 0 (concatD [doc (showString "("), prt 0 exps, doc (showString ")")])
    AbsGramm.ArgExpTyped exps -> prPrec i 0 (concatD [doc (showString "("), prt 0 (map (\(x,y,z) -> (x,y)) exps), doc (showString ")")])

  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [(AbsGramm.Exp, AbsGramm.TypeSpec)] where
  prt = prtList

instance Print (AbsGramm.Exp, AbsGramm.TypeSpec) where
  prt i (exp, typ) = prPrec i 0 (concatD [doc (showString $ t "("), prt 0 exp, doc (showString $ t ")"), doc (showString $ t "<="), prt2 0 typ])

instance Print [AbsGramm.Args] where
  prt = prtList

instance Print [AbsGramm.Exp] where
  prt = prtList

instance Print AbsGramm.Op where
  prt i e = case e of
    AbsGramm.Or -> prPrec i 0 (concatD [doc (showString "||")])
    AbsGramm.And -> prPrec i 1 (concatD [doc (showString "&&")])
    AbsGramm.Less -> prPrec i 2 (concatD [doc (showString "<")])
    AbsGramm.LessEq -> prPrec i 2 (concatD [doc (showString "<=")])
    AbsGramm.Greater -> prPrec i 2 (concatD [doc (showString ">")])
    AbsGramm.GreaterEq -> prPrec i 2 (concatD [doc (showString ">=")])
    AbsGramm.Equal -> prPrec i 2 (concatD [doc (showString "==")])
    AbsGramm.NotEq -> prPrec i 2 (concatD [doc (showString "!=")])
    AbsGramm.Plus -> prPrec i 3 (concatD [doc (showString "+")])
    AbsGramm.Minus -> prPrec i 3 (concatD [doc (showString "-")])
    AbsGramm.Prod -> prPrec i 4 (concatD [doc (showString "*")])
    AbsGramm.Div -> prPrec i 4 (concatD [doc (showString "/")])
    AbsGramm.Mod -> prPrec i 4 (concatD [doc (showString "%")])
    AbsGramm.Pow -> prPrec i 5 (concatD [doc (showString "^")])

instance Print AbsGramm.Exp where
  prt i e = case e of
    AbsGramm.ENot exp -> prPrec i 2 (concatD [doc (showString "!"), prt 2 exp])
    AbsGramm.ENeg exp -> prPrec i 6 (concatD [doc (showString "-"), prt 7 exp])
    AbsGramm.ELExp lexp -> prPrec i 7 (concatD [prt 0 lexp])
    AbsGramm.EDeref lexp -> prPrec i 7 (concatD [doc (showString "&"), prt 0 lexp])
    AbsGramm.EInt pinteger -> prPrec i 7 (concatD [prt 0 pinteger])
    AbsGramm.EFloat pfloat -> prPrec i 7 (concatD [prt 0 pfloat])
    AbsGramm.EChar pchar -> prPrec i 7 (concatD [prt 0 pchar])
    AbsGramm.EString pstring -> prPrec i 7 (concatD [prt 0 pstring])
    AbsGramm.ETrue ptrue -> prPrec i 7 (concatD [prt 0 ptrue])
    AbsGramm.EFalse pfalse -> prPrec i 7 (concatD [prt 0 pfalse])
    AbsGramm.ENull pnull -> prPrec i 7 (concatD [prt 0 pnull])
    AbsGramm.EArray exps -> prPrec i 7 (concatD [doc (showString "Array"), doc (showString "("), prt 0 exps, doc (showString ")")])
    AbsGramm.EFunCall pident argss -> prPrec i 7 (concatD [prt 0 pident, prt 0 argss])
    AbsGramm.EIfElse exp1 exp2 exp3 -> prPrec i 7 (concatD [doc (showString "if"), doc (showString "("), prt 0 exp1, doc (showString ")"), prt 0 exp2, doc (showString "else"), prt 0 exp3])
    AbsGramm.ExpTyped exp typespec loc -> prPrec i 0 (concatD [doc (showString $ t "("), prt 0 exp, doc (showString $ t ")") , doc (showString $ t ":"), prt2 0 typespec])
    AbsGramm.EOp exp1 op exp2 -> prPrec i 0 (concatD [prt 7 exp1, prt 0 op, prt 7 exp2])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsGramm.LExp where
  prt i e = case e of
    AbsGramm.LRef lexp -> prPrec i 0 (concatD [doc (showString "*"), prt 1 lexp])
    AbsGramm.LArr lexp exp -> prPrec i 1 (concatD [prt 1 lexp, doc (showString "["), prt 0 exp, doc (showString "]")])
    AbsGramm.LIdent pident -> prPrec i 1 (concatD [prt 0 pident])
    AbsGramm.LIdentMod pident _ -> prPrec i 1 (concatD [prt 0 pident])
    AbsGramm.LExpTyped lexp typespec loc -> prPrec i 0 (concatD [prt 0 lexp, doc (showString $ t ":"), prt2 0 typespec])

t :: String -> String
t s = color Default Faint s