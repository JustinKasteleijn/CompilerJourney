{-# LANGUAGE DataKinds #-}

module Parser.ParserBase
  (
    Parser,
    runParser,
    runParserDebug,
    lexeme,
    symbol,
    keyword,
    identifierString,
    withSpan
  ) where

import           Data.Void                  (Void)
import           Generic.DebugShow          (DebugShow (debugShow))
import           Generic.Span               (Span (Span))
import           Text.Megaparsec            hiding (runParser)
import           Text.Megaparsec.Char       (alphaNumChar, char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

runParserInternal :: (a -> b) -> Parser a -> String -> Either String b
runParserInternal f p input
  = case parse p "filename" input of
      Left err  -> Left $ errorBundlePretty err
      Right res -> Right $ f res

runParser :: Parser a -> String -> a
runParser p input =
  case runParserInternal id p input of
    Left err -> error err
    Right x  -> x

runParserDebug :: DebugShow a => Parser a -> String -> IO ()
runParserDebug p input =
  case runParserInternal debugShow p input of
    Left err -> putStrLn err
    Right s  -> putStrLn s

spaceAndComments :: Parser ()
spaceAndComments
  = L.space space1 (L.skipLineComment "//") (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceAndComments

symbol :: String -> Parser String
symbol = L.symbol spaceAndComments

keyword :: String -> Parser ()
keyword w = try
  $ string w
    *> notFollowedBy alphaNumChar

identifierString :: Parser String
identifierString = lexeme (some (alphaNumChar <|> char '_'))

withSpan :: (Span -> a -> b) -> Parser a -> Parser b
withSpan constructor p = do
  start <- getSourcePos
  value <- p
  end   <- getSourcePos
  return $ constructor (Span start end) value
