{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}

module BackEnd.CPlusPlus where

import           AST.Annotation      (Phase (..))
import           AST.Syntax          (AnnotatedType (..), Expr (..),
                                      FuncArg (FuncArg), Ident (..),
                                      Statement (Assign, While),
                                      annotatedTypeToType)
import           AST.Type            (Type (TBool, TChar, TFun, TInt, TTuple, TUnit, TVar))
import           Data.Char           (toLower)
import           Data.List           (intercalate)
import           Generic.StringUtils (getBetween)

class CPlusPlus a where
  genCPlusPlus :: a -> String

instance CPlusPlus (Expr Typed) where
  genCPlusPlus :: Expr Typed -> String
  genCPlusPlus (LInt _ n)                      = show n
  genCPlusPlus (LBool _ b)                     = map toLower (show b)
  genCPlusPlus (LChar _ c)                     = show c
  genCPlusPlus (LIdent _ n)                    = n
  genCPlusPlus (Tuple (_, t) xs)               = "std::tuple<" ++ getBetween (show t) ++ ">{" ++ intercalate ", " (map genCPlusPlus xs) ++ "}"
  genCPlusPlus (BinaryOperation  _ lhs op rhs) = show lhs ++ " " ++ show op ++ " " ++ show rhs
  genCPlusPlus (UnaryOperation _ op expr)      = show op ++ show expr
  genCPlusPlus (Lambda _ args body)            = "auto lam = [] (" ++ intercalate ", " (map genCPlusPlus args) ++ ") { return " ++ genCPlusPlus body ++ ";}"
  genCPlusPlus (Application _ lam args)        = genCPlusPlus lam ++ "(" ++ intercalate ", " (map genCPlusPlus args) ++ ");"

instance CPlusPlus AnnotatedType where
  genCPlusPlus :: AnnotatedType -> String
  genCPlusPlus = genCPlusPlus . annotatedTypeToType

instance CPlusPlus (Ident 'Typed) where
   genCPlusPlus :: Ident 'Typed -> String
   genCPlusPlus (Ident _ v) = v

instance CPlusPlus Type where
  genCPlusPlus :: Type -> String
  genCPlusPlus TInt           = "int"
  genCPlusPlus TBool          = "bool"
  genCPlusPlus TChar          = "char"
  genCPlusPlus TUnit          = ""
  genCPlusPlus (TVar n)       = n
  genCPlusPlus (TTuple types) = "(" ++ intercalate ", " (map show types) ++ ")"
  genCPlusPlus (TFun i o)     = "std::function<" ++ genCPlusPlus o ++ "(" ++ intercalate ", " (map genCPlusPlus i) ++ ")>"

instance CPlusPlus (Statement 'Typed) where
  genCPlusPlus :: Statement 'Typed -> String
  genCPlusPlus = \case
    While _ c b         -> "while ("
      ++ genCPlusPlus c
      ++ ") {\n"
      ++ intercalate ";\n" (map genCPlusPlus b)
      ++ "}\n"
    Assign (_, t) v _ e -> genCPlusPlus t ++ " " ++ genCPlusPlus v ++ " = " ++ genCPlusPlus e


instance CPlusPlus (FuncArg 'Typed) where
  genCPlusPlus :: FuncArg 'Typed -> String
  genCPlusPlus (FuncArg (_, t) v mt) = show (maybe t annotatedTypeToType mt) ++ " " ++ v

buildCPlusPlusFile :: (CPlusPlus p) => p -> String
buildCPlusPlusFile linez = unlines $  [
    "#include <iostream>",
    "#include <tuple>",
    "",
    "int main() {"
  ] ++ genCPlusPlus linez : ["}"]

writeCPlusPlusFile :: (CPlusPlus p) => String -> p -> IO String
writeCPlusPlusFile path linez = do
  let cpp = buildCPlusPlusFile linez
  writeFile path (buildCPlusPlusFile linez)
  pure cpp
