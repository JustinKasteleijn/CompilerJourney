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
  show  TInt             = "Int"
  show TBool             = "Bool"
  show TChar             = "Char"
  show (TTuple types)    = "Tuple (" ++ joinWithCommaS types ++ ")"
  show (TFun params out) = "|" ++ joinWithCommaS params ++ "|" ++ "->" ++ show out

instance DebugShow Type where
  debugShow :: Type -> String
  debugShow = show

instance ProgramShow Type where
  programShow :: Int -> Type -> String
  programShow _ = show
