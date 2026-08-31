{-# LANGUAGE DataKinds #-}

module Parser.StatementParser where

import           AST.Annotation      (Phase (Parsed))
import           AST.Syntax          (AnnotatedType, Expr, Ident,
                                      Statement (Assign))
import           Control.Applicative (optional)
import           Generic.Triple      (uncurry3)
import           Parser.ExprParser   (exprParser)
import           Parser.ParserBase   (Parser, identifier, keyword, symbol,
                                      withSpan)
import           Parser.TypeParser   (annotatedType)

parseStatement :: Parser (Statement 'Parsed)
parseStatement = parseAssign

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
