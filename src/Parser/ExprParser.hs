{-# LANGUAGE DataKinds #-}

module Parser.ExprParser where

import           AST.Annotation                  (Phase (Parsed))
import           AST.Syntax                      (BinaryOperator (..),
                                                  Expr (..), UnaryOperator (..))
import           Parser.ParserBase

import           Control.Applicative             (Alternative (..), optional)

import           Control.Applicative.Combinators (between, sepBy)
import           Control.Monad.Combinators.Expr  (Operator (InfixL, InfixN, InfixR, Prefix),
                                                  makeExprParser)
import           Data.Char                       (isAlpha)
import           Generic.Span                    (Span (Span))
import           Parser.ArgumentParser           (funcArgs)
import           Text.Megaparsec                 (MonadParsec (label),
                                                  getSourcePos, satisfy)
import qualified Text.Megaparsec.Char.Lexer      as L

operatorTable :: [[Operator Parser (Expr 'Parsed)]]
operatorTable = [
    [   -- 1. Highest precedence for unary operations
        Prefix (unary Neg "-"),
        Prefix (unary Not "!")
    ],
    [  -- 2. Exponentiation
       InfixR (binary Pow "^")
    ],
    [  -- 3. Multiplication, Division, Modulo
       InfixL (binary Mul "*"),
       InfixL (binary Div "/"),
       InfixL (binary Mod "%")
    ],
    [  -- 4. Addition, Substraction
       InfixL (binary Add "+"),
       InfixL (binary Sub "-")
    ],
    [  -- 5. Comparison
       InfixN (binary Gte ">="),
       InfixN (binary Lte "<="),
       InfixN (binary Gt  ">"),
       InfixN (binary Lt  "<")
    ],
    [  -- 6. Equality, Inequality
       InfixL (binary Eq "=="),
       InfixL (binary Neq "!=")
    ],
    [
       -- 7. Logical and
       InfixL (binary And "&&")
    ],
    [  -- 8. Logical or
       InfixL (binary Or "||")
    ]
  ]
  where
    unary :: UnaryOperator -> String  -> Parser (Expr 'Parsed -> Expr 'Parsed)
    unary op s = UnaryOperation `withSpan` (op <$ symbol s)

    binary :: BinaryOperator -> String -> Parser (Expr 'Parsed -> Expr 'Parsed -> Expr 'Parsed)
    binary op s = (\sp _ lhs rhs -> BinaryOperation sp lhs op rhs)
                  `withSpan`
                  symbol s

exprParser :: Parser (Expr 'Parsed)
exprParser = label "expression" $
  makeExprParser application operatorTable

application :: Parser (Expr 'Parsed)
application = do
  base  <- atom
  calls <- many call
  pure $ foldl (\e f -> f e) base calls
 where
   call :: Parser (Expr 'Parsed -> Expr 'Parsed)
   call = do
     start <- getSourcePos
     args  <- between (symbol "(")
                      (symbol ")")
                      (exprParser `sepBy` symbol ",")
     end <- getSourcePos
     pure $ \f -> Application (Span start end) f args

atom :: Parser (Expr 'Parsed)
atom = integer
   <|> boolean
   <|> char
   <|> identifier
   <|> tupleOrExpr
   <|> lambda

integer :: Parser (Expr 'Parsed)
integer = label "integer" $
  LInt `withSpan` lexeme L.decimal

boolean :: Parser (Expr 'Parsed)
boolean = label "boolean" $
  LBool `withSpan` lexeme (True <$ keyword "true" <|> False <$ keyword "false")

char :: Parser (Expr 'Parsed)
char = LChar `withSpan` between (symbol "'") (symbol "'") (satisfy isAlpha)

identifier :: Parser (Expr 'Parsed)
identifier = label "identifier"
  $ LIdent `withSpan` identifierString

tupleOrExpr :: Parser (Expr 'Parsed)
tupleOrExpr = label "parenthesized expression" $ do
  start <- getSourcePos
  _     <- symbol "("
  lhs   <- exprParser
  mrest <- optional (symbol "," *> exprParser `sepBy` symbol ",")
  _     <- symbol ")"
  end   <- getSourcePos
  pure $ case mrest of
    Nothing   -> lhs
    Just rest -> Tuple (Span start end) (lhs : rest)

lambda :: Parser (Expr 'Parsed)
lambda = label "lambda expression" $ do
  start <- getSourcePos
  args  <- between (symbol "|") (symbol "|") funcArgs
  expr  <- between (symbol "{") (symbol "}") (label "function argument" exprParser)
  end   <- getSourcePos
  return $ Lambda (Span start end) args expr
