{-# LANGUAGE DataKinds #-}

module Parser.TypeParser where

import           AST.Annotation      (Phase (Parsed))
import           AST.Syntax          (AnnotatedType (..))
import           Control.Applicative (Alternative ((<|>)))
import           Data.Char           (isAlpha)
import           Parser.ParserBase   (Parser, keyword, withSpan)
import           Text.Megaparsec     (satisfy, some)

annotatedType :: Parser (AnnotatedType 'Parsed)
annotatedType = annotatedInt
            <|> annotatedBool
            <|> annotatedChar
            <|> annotatedTVar
            <|> annotatedTuple
            <|> annotatedFunc

annotatedInt :: Parser (AnnotatedType 'Parsed)
annotatedInt = withSpan (\sp _ -> ATInt sp) (keyword "int")

annotatedBool :: Parser (AnnotatedType 'Parsed)
annotatedBool = withSpan (\sp _ -> ATBool sp) (keyword "bool")

annotatedChar :: Parser (AnnotatedType 'Parsed)
annotatedChar = withSpan (\sp _ -> ATChar sp) (keyword "char")

annotatedTVar :: Parser (AnnotatedType 'Parsed)
annotatedTVar = withSpan ATTVar typeVar
  where typeVar = some $ satisfy isAlpha

annotatedTuple :: Parser (AnnotatedType 'Parsed)
annotatedTuple = withSpan ATTuple (some annotatedType)

annotatedFunc :: Parser (AnnotatedType 'Parsed)
annotatedFunc = withSpan ATFun (some annotatedType) <*> annotatedType
