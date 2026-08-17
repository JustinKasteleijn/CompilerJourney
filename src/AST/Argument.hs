{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AST.Argument where

import           AST.Syntax (FuncArg (FuncArg))

getFunctionArgumentName :: FuncArg p -> String
getFunctionArgumentName (FuncArg _ n _) = n
