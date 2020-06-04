module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)

import LexGramm
import ParGramm
import PrintGramm
import AbsGramm
import AbsTAC
import ThreeAddressCode
import TypeChecker
import Errors
import PrintTAC
import Color
import ErrM

type ParseFun a = [Token] -> Err Program

myLLexer = myLexer

type Verbosity = Int

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
                          putStrLn $ color Green Bold "\nParse Successful!"
                          showTree v tree
                          putStrLn $ "\n" ++ separator ++ "\n\n"
                          let (annotatedTree, logs) = genAnnotatedTree tree
                          case (logs) of
                            [] -> do
                              showAnnotatedTree v annotatedTree
                              let code = genTAC annotatedTree True
                              showTAC code
                            _ -> do
                              showAnnotatedTree v annotatedTree
                              errors <- showErrors logs
                              if errors == True
                                then do
                                  let code = genTAC annotatedTree (hasMain logs)
                                  showTAC code
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
  putStrLn $ "\n" ++ separator
  putStrLn $ colorSectionTitle "[Lista errori type checker]"
  putStrLn separator
  errorFound <- printTypeCheckErrors logs 0
  putStrLn separator
  return errorFound

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

--printTypeCheckSuccess :: Program -> IO()
--printTypeCheckSuccess prog = do
--  putStrLn $ "\n" ++ separator
--  putStrLn "[Albero tipato]"
--  putStrLn separator
--  putStrV 2 $ show prog
--  putStrLn separator

separator :: String
separator = "----------------------------------------------------------------"

showTAC :: [TAC] -> IO ()
showTAC code = do
  putStrLn $ "\n" ++ separator
  putStrLn "[Three Address Code]"
  putStrLn separator
  printTAC code
  putStrLn $ separator ++ "\n"

showTree :: Int -> Program -> IO ()
showTree v tree
 = do
      putStrV v $ (colorSectionTitle "\n[Abstract Syntax]\n\n") ++ show tree
      putStrV v $ (colorSectionTitle "\n[Linearized tree]\n\n") ++ printTree tree

showAnnotatedTree :: Int -> Program -> IO ()
showAnnotatedTree v tree
  = do
      --putStrLn "\n" ++ separator ++ "\n\n"
      putStrV v $ (colorSectionTitle "\n[Annotated Tree - Abstract Syntax]\n\n") ++ show tree
      putStrV v $ (colorSectionTitle "\n[Annotated Tree - Linearized tree]\n\n") ++ printTree tree

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





