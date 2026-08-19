{-# LANGUAGE InstanceSigs #-}

module AST.Type where

import           Generic.DebugShow   (DebugShow (debugShow))
import           Generic.ProgramShow (ProgramShow (programShow))
import           Generic.StringUtils (joinWithCommaS)

data Type
  = TVar String
  | TInt
  | TBool
  | TChar

  | TTuple [Type]
  | TFun [Type] Type
  deriving (Eq)

data Scheme = Forall [String] Type

instance Show Type where
  show :: Type -> String
  show (TVar n)          = n
  show  TInt             = "int"
  show TBool             = "bool"
  show TChar             = "char"
  show (TTuple types)    = "tuple (" ++ joinWithCommaS types ++ ")"
  show (TFun params out) = "|" ++ joinWithCommaS params ++ "|" ++ "->" ++ show out

instance DebugShow Type where
  debugShow :: Type -> String
  debugShow = show

instance ProgramShow Type where
  programShow :: Int -> Type -> String
  programShow _ = show
