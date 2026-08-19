module Main where

import           BackEnd.CPlusPlus
import           Data.Either                  (fromLeft, fromRight)
import           Generic.DebugShow
import           Generic.ProgramShow          (prettyPrint)
import           Generic.Triple               (thd3)
import           Parser.ExprParser            (exprParser)
import           Parser.ParserBase            (runParser, runParserDebug)
import           SemanticAnalysis.TypeChecker (evalTI, infer, runTypeChecker)

main :: IO ()
main = runProgram "|x, y| { (x, y) } ('c', 5)"

runProgram :: String -> IO ()
runProgram program = do
  let parsed = runParser exprParser program
      typed  = runTypeChecker $ infer parsed
  _ <- writeCPlusPlusFile  "/Users/justi/Documents/Projects/CompilerJourney/output/c.cpp" typed
  debugPrint typed
  pure ()
