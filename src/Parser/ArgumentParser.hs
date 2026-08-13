{-# LANGUAGE DataKinds #-}

module Parser.ArgumentParser where

import           AST.Annotation                  (Phase (Parsed))
import           AST.Argument                    (FuncArg (FuncArg))
import           Control.Applicative.Combinators (sepBy)
import           Parser.ParserBase               (Parser, identifierString,
                                                  symbol, withSpan)
import           Text.Megaparsec                 (MonadParsec (label))

funcArg :: Parser (FuncArg 'Parsed)
funcArg = withSpan FuncArg identifierString

funcArgs :: Parser [FuncArg 'Parsed]
funcArgs = label "function argument" $
  sepBy funcArg (symbol ",")
