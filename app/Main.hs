module Main where

import           BackEnd.CPlusPlus
import           Data.Either                  (fromLeft, fromRight)
import           Generic.DebugShow
import           Generic.ProgramShow          (prettyPrint)
import           Generic.Triple               (thd3)
import           Parser.ExprParser            (exprParser, parseFunction,
                                               parseStatement)
import           Parser.ParserBase            (runParser, runParserDebug)
import           SemanticAnalysis.TypeChecker (evalTI, infer, runTypeChecker)

main :: IO ()
main =
  let lam   = "var make_tuple = |x, y| { return (x + 5, y - 2) }"
      fn    = unlines ["fn add(x, y) {", "  return x + y", "}"]
      while = unlines ["fn repeat(n: int) {", "var b: bool = true;", "var res = 0;", "while (n > 0) {", "n = n - 1;};", " return n;", "};"]
   in runProgram while

runProgram :: String -> IO ()
runProgram program = do
  let parsed = runParser parseFunction program
      typed  = runTypeChecker $ infer parsed
  _ <- writeCPlusPlusFile  "/Users/justi/Documents/Projects/CompilerJourney/output/c.cpp" typed
  debugPrint typed
