
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}

module BackEnd.CPlusPlus where

import           AST.Annotation      (Phase (..))
import           AST.Syntax          (AnnotatedType (..), Expr (..),
                                      FuncArg (FuncArg), Ident (..),
                                      Statement (Assign, Return, VarDecl, While),
                                      annotatedTypeToType)
import           AST.Type            (Type (TBool, TChar, TFun, TInt, TTuple, TUnit, TVar))
import           Data.Char           (toLower)
import           Data.List           (intercalate)
import           Generic.StringUtils (getBetween, indent, indented)

class CPlusPlus a where
  genCPlusPlus :: Int -> a -> String

instance CPlusPlus (Expr Typed) where
  genCPlusPlus :: Int -> Expr Typed -> String
  genCPlusPlus _ (LInt _ n)                      = show n
  genCPlusPlus _ (LBool _ b)                     = map toLower (show b)
  genCPlusPlus _ (LChar _ c)                     = show c
  genCPlusPlus _ (LIdent _ n)                    = n
  genCPlusPlus n (Tuple (_, t) xs)               = "std::tuple<" ++ getBetween (show t) ++ ">{" ++ intercalate ", " (map (genCPlusPlus n) xs) ++ "}"
  genCPlusPlus n (BinaryOperation _ lhs op rhs)  = genCPlusPlus n lhs ++ " " ++ show op ++ " " ++ genCPlusPlus n rhs
  genCPlusPlus n (UnaryOperation _ op expr)      = show op ++ genCPlusPlus n expr
  genCPlusPlus n (Lambda _ args body)             =
    "[] (" ++ intercalate ", " (map (genCPlusPlus n) args) ++ ") {\n"
      ++ intercalate "\n" (map (genCPlusPlus (n + 1)) body)
      ++ "\n" ++ indent n ++ "}"
  genCPlusPlus n (Application _ lam args)         =
    genCPlusPlus n lam ++ "(" ++ intercalate ", " (map (genCPlusPlus n) args) ++ ")"

instance CPlusPlus AnnotatedType where
  genCPlusPlus :: Int -> AnnotatedType -> String
  genCPlusPlus n = genCPlusPlus n . annotatedTypeToType

instance CPlusPlus (Ident 'Typed) where
  genCPlusPlus :: Int -> Ident 'Typed -> String
  genCPlusPlus _ (Ident _ v) = v

instance CPlusPlus Type where
  genCPlusPlus :: Int -> Type -> String
  genCPlusPlus _ TInt           = "int"
  genCPlusPlus _ TBool          = "bool"
  genCPlusPlus _ TChar          = "char"
  genCPlusPlus _ TUnit          = "void"
  genCPlusPlus _ (TVar n)       = n
  genCPlusPlus n (TTuple types) = "std::tuple<" ++ intercalate ", " (map (genCPlusPlus n) types) ++ ">"
  genCPlusPlus n (TFun _ o)     = genCPlusPlus n o

instance CPlusPlus (Statement 'Typed) where
  genCPlusPlus :: Int -> Statement 'Typed -> String
  genCPlusPlus n = \case
    While _ c b ->
      indented n $
        "while (" ++ genCPlusPlus n c ++ ") {\n"
          ++ intercalate "\n" (map (genCPlusPlus (n + 1)) b)
          ++ "\n" ++ indent n ++ "}"

    VarDecl (_, t) v _ (Lambda _ args body) ->
      indented n $
        genCPlusPlus n t ++ " "
          ++ genCPlusPlus n v
          ++ "(" ++ intercalate ", " (map (genCPlusPlus n) args) ++ ") {\n"
          ++ intercalate "\n" (map (genCPlusPlus (n + 1)) body)
          ++ "\n" ++ indent n ++ "}"

    Assign _ v _ e ->
      indented n $ genCPlusPlus n v ++ " = " ++ genCPlusPlus n e ++ ";"

    VarDecl (_, t) v _ e ->
      indented n $
        genCPlusPlus n t ++ " " ++ genCPlusPlus n v
          ++ " = " ++ genCPlusPlus n e ++ ";"

    Return (_, _) e ->
      indented n $ "return " ++ genCPlusPlus n e ++ ";"

instance CPlusPlus (FuncArg 'Typed) where
  genCPlusPlus :: Int -> FuncArg 'Typed -> String
  genCPlusPlus n (FuncArg (_, t) v mt)
    = genCPlusPlus n (maybe t annotatedTypeToType mt) ++ " " ++ v

buildCPlusPlusFile :: (CPlusPlus p) => p -> String
buildCPlusPlusFile linez = unlines $
  [ "#include <iostream>"
  , "#include <tuple>"
  , ""
  ] ++ [genCPlusPlus 0 linez]

writeCPlusPlusFile :: (CPlusPlus p) => String -> p -> IO String
writeCPlusPlusFile path linez = do
  let cpp = buildCPlusPlusFile linez
  writeFile path cpp
  pure cpp
