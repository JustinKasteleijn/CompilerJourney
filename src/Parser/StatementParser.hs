{-# LANGUAGE DataKinds #-}

module Parser.StatementParser where

import           AST.Annotation      (Phase (Parsed))
import           AST.Syntax          (AnnotatedType, Expr, Ident,
                                      Statement (Assign, While))
import           Control.Applicative (optional, (<|>))
import           Generic.Triple      (uncurry3)
import           Parser.ExprParser   (exprParser)
import           Parser.ParserBase   (Parser, identifier, keyword, symbol,
                                      withSpan)
import           Parser.TypeParser   (annotatedType)
import           Text.Megaparsec     (between, sepEndBy)

parseStatements :: Parser [Statement 'Parsed]
parseStatements = sepEndBy parseStatement (symbol ";")

parseStatement :: Parser (Statement 'Parsed)
parseStatement = parseAssign
  <|> parseWhile

parseAssign :: Parser (Statement 'Parsed)
parseAssign =
  (uncurry3 . Assign) `withSpan` assign
 where
   assign :: Parser (Ident 'Parsed, Maybe AnnotatedType, Expr 'Parsed)
   assign = (,,)
     <$  keyword "var"
     <*> identifier
     <*> optional (symbol ":" *> annotatedType)
     <*  symbol "="
     <*> exprParser

parseWhile :: Parser (Statement 'Parsed)
parseWhile = (uncurry . While) `withSpan`
  (do _ <- keyword "while"
      c <- between (symbol "(") (symbol ")") exprParser
      b <- between (symbol "{") (symbol "}") parseStatements
      pure (c, b))
