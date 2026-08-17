module Main where

import           BackEnd.CPlusPlus
import           Data.Either                  (fromRight)
import           Generic.DebugShow
import           Generic.ProgramShow          (prettyPrint)
import           Generic.Triple               (thd3)
import           Parser.ExprParser            (exprParser)
import           Parser.ParserBase            (runParser, runParserDebug)
import           SemanticAnalysis.TypeChecker (evalTI, infer)

main :: IO ()
main = debugPrint $ thd3 $ fromRight undefined $ evalTI $ infer $ runParser exprParser "|x| { x } ('c')"
