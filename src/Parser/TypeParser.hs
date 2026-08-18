{-# LANGUAGE DataKinds #-}

module Parser.TypeParser where

import           AST.Syntax          (AnnotatedType (..))
import           Control.Applicative (Alternative ((<|>)))
import           Data.Char           (isAlpha)
import           Parser.ParserBase   (Parser, keyword, symbol, withSpan)
import           Text.Megaparsec     (between, satisfy, sepBy1, some)

annotatedType :: Parser AnnotatedType
annotatedType = annotatedTuple
            <|> annotatedFunc
            <|> annotatedInt
            <|> annotatedBool
            <|> annotatedChar
            <|> annotatedTVar

annotatedInt :: Parser AnnotatedType
annotatedInt = withSpan (\sp _ -> ATInt sp) (keyword "int")

annotatedBool :: Parser AnnotatedType
annotatedBool = withSpan (\sp _ -> ATBool sp) (keyword "bool")

annotatedChar :: Parser AnnotatedType
annotatedChar = withSpan (\sp _ -> ATChar sp) (keyword "char")

annotatedTVar :: Parser AnnotatedType
annotatedTVar = withSpan ATTVar typeVar
  where typeVar = some $ satisfy isAlpha

annotatedTuple :: Parser AnnotatedType
annotatedTuple = withSpan ATTuple (between (symbol "(") (symbol ")") (some annotatedType))

annotatedFunc :: Parser AnnotatedType
annotatedFunc = withSpan ATFun (between (symbol "|") (symbol "|") (sepBy1 annotatedType (symbol " "))) <*> annotatedType
