{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AST.Argument where

import           AST.Annotation (Phase (Typed))
import           AST.Syntax     (AnnotatedType (ATTVar), FuncArg (FuncArg))
import           GHC.List       (foldl')

getFunctionArgumentName :: FuncArg p -> String
getFunctionArgumentName (FuncArg _ n _) = n

isTVar :: FuncArg 'Typed -> Bool
isTVar (FuncArg _ _ mt) = maybe False isTVar' mt
  where
    isTVar' (ATTVar _ _) = True
    isTVar' _            = False
