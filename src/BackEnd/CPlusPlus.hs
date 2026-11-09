
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}

module BackEnd.CPlusPlus where

import           AST.Annotation              (Phase (..))
import           AST.Syntax                  (AnnotatedType (..), Expr (..),
                                              FuncArg (FuncArg), Ident (..),
                                              Statement (Assign, Return, VarDecl, While),
                                              annotatedTypeToType)
import           AST.Type                    (Type (TBool, TChar, TFun, TInt, TTuple, TUnit, TVar))
import           Control.Monad.State.Strict  (MonadState (get), StateT,
                                              evalStateT, modify)
import           Control.Monad.Writer.Strict (MonadWriter (tell), Writer,
                                              execWriter)
import           Data.Char                   (toLower)
import           Data.List                   (intersperse)
import           Generic.StringUtils         (getBetween, indent)

type Cpp = StateT Int (Writer String)
type CppGen = Cpp ()

render :: CppGen -> String
render = execWriter . (`evalStateT` 0)

emit :: String -> CppGen
emit = tell

line :: CppGen -> CppGen
line action = do
  n <- get
  emit (indent n)
  action
  emit "\n"

statement :: CppGen -> CppGen
statement action =
  line $ action >> emit ";"

withIndent :: CppGen -> CppGen
withIndent action = do
  modify (+1)
  action
  modify (subtract 1)

block :: CppGen -> CppGen -> CppGen
block header body = do
  line (header >> emit "{")
  withIndent body
  line (emit "}")

commaSep :: [CppGen] -> CppGen
commaSep = sequence_ . intersperse (emit ", ")

inBetween :: String -> String -> CppGen -> CppGen
inBetween s e action = do
  emit s
  action
  emit e

parens :: CppGen -> CppGen
parens = inBetween "(" ")"

class CPlusPlus a where
  genCPlusPlus :: a -> CppGen

instance CPlusPlus a => CPlusPlus [a] where
  genCPlusPlus :: [a] -> CppGen
  genCPlusPlus = sequence_
    . intersperse (emit "\n")
    . map genCPlusPlus

instance CPlusPlus (Expr Typed) where
  genCPlusPlus :: Expr Typed -> CppGen
  genCPlusPlus (LInt _ n)                      = emit (show n)
  genCPlusPlus (LBool _ b)                     = emit (map toLower (show b))
  genCPlusPlus (LChar _ c)                     = emit (show c)
  genCPlusPlus (LIdent _ n)                    = emit n
  genCPlusPlus (Tuple (_, t) xs)               = do
    emit "std::tuple<"
    emit (getBetween (show t))
    emit ">{"
    commaSep (map genCPlusPlus xs)
    emit "}"
  genCPlusPlus (BinaryOperation _ lhs op rhs)  = do
    genCPlusPlus lhs
    emit (" " ++ show op ++ " ")
    genCPlusPlus rhs
  genCPlusPlus (UnaryOperation _ op expr)       = do
    emit (show op)
    genCPlusPlus expr
  genCPlusPlus (Lambda _ args body)             = do
    emit "[] ("
    commaSep (map genCPlusPlus args)
    line (emit ") {")
    withIndent $
      sequence_ (intersperse (emit "\n") (map genCPlusPlus body))
    line (emit "}")
  genCPlusPlus (Application _ lam args)         = do
    genCPlusPlus lam
    parens $
      commaSep (map genCPlusPlus args)

instance CPlusPlus AnnotatedType where
  genCPlusPlus :: AnnotatedType -> CppGen
  genCPlusPlus = genCPlusPlus . annotatedTypeToType

instance CPlusPlus (Ident 'Typed) where
  genCPlusPlus :: Ident 'Typed -> CppGen
  genCPlusPlus (Ident _ v) = emit v

instance CPlusPlus Type where
  genCPlusPlus :: Type -> CppGen
  genCPlusPlus TInt           = emit "int"
  genCPlusPlus TBool          = emit "bool"
  genCPlusPlus TChar          = emit "char"
  genCPlusPlus TUnit          = emit "void"
  genCPlusPlus (TVar n)       = emit n
  genCPlusPlus (TTuple types) = do
    emit "std::tuple<"
    commaSep
     $ map genCPlusPlus types
    emit ">"
  genCPlusPlus (TFun _ o)     = genCPlusPlus o

instance CPlusPlus (Statement 'Typed) where
  genCPlusPlus :: Statement 'Typed -> CppGen
  genCPlusPlus = \case
    While _ c b ->
      block
        (do
            emit "while("
            genCPlusPlus c
            emit ")")
        (mapM_ genCPlusPlus b)
    VarDecl (_, t) v _ (Lambda _ args body) ->
      block
        (do
          genCPlusPlus t
          emit " "
          genCPlusPlus v
          parens $
            commaSep (map genCPlusPlus args))
        (mapM_ genCPlusPlus body)
    Assign _ v _ e ->
      statement $ do
        genCPlusPlus v
        emit " = "
        genCPlusPlus e
    VarDecl (_, t) v mt e ->
      statement $ do
        genCPlusPlus $ maybe t annotatedTypeToType mt
        emit " "
        genCPlusPlus v
        emit " = "
        genCPlusPlus e
    Return (_, _) e ->
      statement $ do
        emit "return "
        genCPlusPlus e

instance CPlusPlus (FuncArg 'Typed) where
  genCPlusPlus :: FuncArg 'Typed -> CppGen
  genCPlusPlus (FuncArg (_, t) v mt) = do
    genCPlusPlus (maybe t annotatedTypeToType mt)
    emit $ " " ++ v

buildCPlusPlusFile :: (CPlusPlus p) => p -> String
buildCPlusPlusFile linez = unlines
  [ "#include <iostream>"
  , "#include <tuple>"
  , ""
  ] ++ render (genCPlusPlus linez)

writeCPlusPlusFile :: (CPlusPlus p) => String -> p -> IO String
writeCPlusPlusFile path linez = do
  let cpp = buildCPlusPlusFile linez
  writeFile path cpp
  pure cpp
