module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)

import Lexer
import Parser
import Printer
import AbsGramm
import AbsTAC
import ThreeAddressCode
import Errors
import PrintTAC
import Color
import ErrM
import StaticAnalysis

type ParseFun a = [Token] -> Err Program

myLLexer = myLexer

type Verbosity = Int

separator :: String
separator = "----------------------------------------------------------------"

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Verbosity -> ParseFun Program -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s  -> do 
                          putStrLn $ color Red Bold "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok tree -> do 
                          putStrLn $ color Green Bold "\nParse Successful!\n"
                          showTree v tree
                          let (annotatedTree, logs) = genAnnotatedTree tree
                          case (logs) of
                            [] -> do
                              showAnnotatedTree v annotatedTree
                              let code = genTAC annotatedTree True
                              showTAC v code
                            _ -> do
                              showAnnotatedTree v annotatedTree
                              errors <- showErrors logs
                              if errors == True
                                then do
                                  let code = genTAC annotatedTree (hasMain logs)
                                  showTAC v code
                                else 
                                  return ()
                              
                          exitSuccess


hasMain :: [LogElement] -> Bool
hasMain [log] = not (getException log == MissingMain)
hasMain logs = not (getException (last logs) == MissingMain)

colorSectionTitle :: String -> String
colorSectionTitle s = color Cyan Bold s

showErrors :: [LogElement] -> IO Bool
showErrors logs = do
  putStrLn $ separator ++ "\n"
  putStrLn $ colorSectionTitle "\n[Type checker error list]\n"
  errorFound <- printTypeCheckErrors logs 0
  putStrLn $ "\n"
  return errorFound

showTAC :: Int -> [TAC] -> IO ()
showTAC v code = do
  putStrV v $ separator ++ "\n"
  putStrV v $ (colorSectionTitle "\n[Three Address Code]\n\n") ++ printTAC code
  putStrV v $ ""

showTree :: Int -> Program -> IO ()
showTree v tree
 = do
      putStrV v $ separator ++ "\n"
      putStrV v $ (colorSectionTitle "\n[Abstract Syntax]\n\n") ++ show tree
      putStrV v $ (colorSectionTitle "\n[Linearized tree]\n\n") ++ printTree tree
      putStrV v $ ""

showAnnotatedTree :: Int -> Program -> IO ()
showAnnotatedTree v tree
  = do
      putStrV v $ separator ++ "\n"
      putStrV v $ (colorSectionTitle "\n[Annotated Tree - Abstract Syntax]\n\n") ++ show tree
      putStrV v $ (colorSectionTitle "\n[Annotated Tree - Linearized tree]\n\n") ++ printTree tree
      putStrV v $ ""

printTypeCheckErrors :: [LogElement] -> Int -> IO Bool
printTypeCheckErrors [] index = return True
printTypeCheckErrors (log:logs) index = do
  putStrLn $ (printIndex index) ++  " " ++ printException log
  res <- printTypeCheckErrors logs (index+1)
  if isError log
    then
      return $ res && False 
    else
      return $ res && True
    
  where
    printIndex n = show index ++ ")"

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run 2 pProgram
    "-s":fs -> mapM_ (runFile 0 pProgram) fs
    fs -> mapM_ (runFile 2 pProgram) fs





