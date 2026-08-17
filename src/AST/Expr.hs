{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module AST.Expr where

import           AST.Annotation (Annotated (getAnn), HasType (getType))
import           AST.Syntax     (BinaryOperator (..), Expr (..))
import           AST.Type       (Type)
import           Data.Data      (Proxy (..))

getExprType :: forall p. HasType p => Expr p -> Type
getExprType expr = getType (Proxy @p) (getAnn expr)

data OperatorKind
  = Arithmetic
  | Logical
  | Comparison
  | Equality

kindOf :: BinaryOperator -> OperatorKind
kindOf Add = Arithmetic
kindOf Sub = Arithmetic
kindOf Mul = Arithmetic
kindOf Div = Arithmetic
kindOf Mod = Arithmetic
kindOf Pow = Arithmetic
kindOf And = Logical
kindOf Or  = Logical
kindOf Gt  = Comparison
kindOf Lt  = Comparison
kindOf Gte = Comparison
kindOf Lte = Comparison
kindOf Eq  = Equality
kindOf Neq = Equality
