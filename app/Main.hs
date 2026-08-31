module Main where

import           BackEnd.CPlusPlus
import           Data.Either                  (fromLeft, fromRight)
import           Generic.DebugShow
import           Generic.ProgramShow          (prettyPrint)
import           Generic.Triple               (thd3)
import           Parser.ExprParser            (exprParser)
import           Parser.ParserBase            (runParser, runParserDebug)
import           Parser.StatementParser       (parseStatement)
import           SemanticAnalysis.TypeChecker (evalTI, infer, runTypeChecker)

main :: IO ()
main = runProgram "var x: |Char, Int| -> (Char, Int) = |x, y| { (x, y) } ('c', 5)"

runProgram :: String -> IO ()
runProgram program = do
  let parsed = runParser parseStatement program
     -- typed  = runTypeChecker $ infer parsed
  --_ <- writeCPlusPlusFile  "/Users/justi/Documents/Projects/CompilerJourney/output/c.cpp" typed
  debugPrint parsed
