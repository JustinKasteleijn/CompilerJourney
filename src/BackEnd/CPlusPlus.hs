{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module BackEnd.CPlusPlus where

import           AST.Annotation (Phase (..))
import           AST.Syntax     (Expr (..))
import           AST.Type       (Type (..))
import           Data.Char      (toLower)
import           Data.List      (intercalate)

class CPlusPlus a where
  genCPlusPlus :: a -> String

instance CPlusPlus (Expr Parsed) where
  genCPlusPlus :: Expr 'Parsed -> String
  genCPlusPlus (LInt _ n)                      = show n
  genCPlusPlus (LBool _ b)                     = map toLower (show b)
  genCPlusPlus (LChar _ c)                     = show c
  genCPlusPlus (LIdent _ n)                    = n
  genCPlusPlus (Tuple _ xs)                    = "(" ++ intercalate ", " (map show xs) ++ ")"
  genCPlusPlus (BinaryOperation  _ lhs op rhs) = show lhs ++ " " ++ show op ++ " " ++ show rhs
  genCPlusPlus (UnaryOperation _ op expr)      = show op ++ show expr
  genCPlusPlus (Lambda _ args body)            = "auto lam = [] (" ++ intercalate ", " (map (\arg -> "auto " ++ show arg) args) ++ ") { return " ++ show body ++ ";}"
  genCPlusPlus (Application _ lam args)        = genCPlusPlus lam ++ "(" ++ intercalate ", " (map genCPlusPlus args) ++ ");"

instance CPlusPlus Type where
  genCPlusPlus :: Type -> String
  genCPlusPlus TInt           = "int"
  genCPlusPlus TBool          = "bool"
  genCPlusPlus TChar          = "char"
  genCPlusPlus (TVar n)       = n
  genCPlusPlus (TTuple types) = "(" ++ intercalate ", " (map show types) ++ ")"
  genCPlusPlus (TFun _ _)     = undefined


buildCPlusPlusFile :: [String] -> String
buildCPlusPlusFile linez = unlines $  [
    "#include <iostream>",
    "",
    "int main() {"
  ] ++ linez ++ ["}"]

writeCPlusPlusFile :: String -> [String] -> IO ()
writeCPlusPlusFile path linez = do
  writeFile path (buildCPlusPlusFile linez)
