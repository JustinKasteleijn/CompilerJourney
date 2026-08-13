module Main where

import           BackEnd.CPlusPlus
import           Data.Either                  (fromRight)
import           Generic.DebugShow
import           Generic.ProgramShow          (prettyPrint)
import           Parser.ExprParser            (exprParser)
import           Parser.ParserBase            (runParser, runParserDebug)
import           SemanticAnalysis.TypeChecker (evalTI, infer)

main :: IO ()
main = prettyPrint $ runParser exprParser "|x| { x } (-5)"
