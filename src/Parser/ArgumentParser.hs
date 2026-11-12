{-# LANGUAGE DataKinds #-}

module Parser.ArgumentParser where

import           AST.Annotation                  (Phase (Parsed))
import           AST.Syntax                      (FuncArg (FuncArg))
import           Control.Applicative.Combinators (optional, sepBy)
import           Parser.ParserBase               (Parser, identifier, symbol,
                                                  withSpan)
import           Parser.TypeParser               (annotatedType)
import           Text.Megaparsec                 (MonadParsec (label))

funcArg :: Parser (FuncArg 'Parsed)
funcArg = withSpan FuncArg identifier
  <*> optional (symbol ":" *> annotatedType)

funcArgs :: Parser [FuncArg 'Parsed]
funcArgs = label "function argument" $
  sepBy funcArg (symbol ",")
