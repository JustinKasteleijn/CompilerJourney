module Main where

import           BackEnd.CPlusPlus
import           Generic.DebugShow
import           Parser.ExprParser            (parseProgram)
import           Parser.ParserBase            (runParser, runParserDebug)
import           SemanticAnalysis.TypeChecker (infer, inferProgram,
                                               runTypeChecker)

main :: IO ()
main = do
  program <- readFile "app/program.spl"
  runProgram program

runProgram :: String -> IO ()
runProgram program = do
  let parsed = runParser parseProgram program
      typed  = runTypeChecker $ inferProgram parsed
  _ <- writeCPlusPlusFile  "/Users/justi/Documents/Projects/CompilerJourney/output/c.cpp" typed
  debugPrint typed
