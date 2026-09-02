module Main where

import           BackEnd.CPlusPlus
import           Data.Either                  (fromLeft, fromRight)
import           Generic.DebugShow
import           Generic.ProgramShow          (prettyPrint)
import           Generic.Triple               (thd3)
import           Parser.ExprParser            (exprParser, parseStatement)
import           Parser.ParserBase            (runParser, runParserDebug)
import           SemanticAnalysis.TypeChecker (evalTI, infer, runTypeChecker)

main :: IO ()
main =
  let lam   = "var x = |x, y| { return (x, y) } ('c', 5)"
      while = "while (false) {var v = 5;};"
   in runProgram lam

runProgram :: String -> IO ()
runProgram program = do
  let parsed = runParser parseStatement program
      typed  = runTypeChecker $ infer parsed
  _ <- writeCPlusPlusFile  "/Users/justi/Documents/Projects/CompilerJourney/output/c.cpp" typed
  debugPrint typed
