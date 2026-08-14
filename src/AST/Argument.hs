{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module AST.Argument where

import           AST.Annotation    (Ann, Annotated (..), Phase,
                                    ShowAnn (showAnn))
import           Data.Proxy        (Proxy (Proxy))
import           Generic.DebugShow (DebugShow (debugShow))


data FuncArg (p :: Phase)
  = FuncArg (Ann p) String

instance Annotated FuncArg where
  getAnn :: FuncArg p -> Ann p
  getAnn (FuncArg ann _) = ann

  setAnn :: Ann p -> FuncArg o -> FuncArg p
  setAnn ann (FuncArg _ arg) = FuncArg ann arg

getFunctionArgumentName :: FuncArg p -> String
getFunctionArgumentName (FuncArg _ n) = n

instance Show (FuncArg p) where
  show :: FuncArg p -> String
  show (FuncArg _ arg) = arg

instance (ShowAnn p) => DebugShow (FuncArg p) where
  debugShow :: FuncArg p -> String
  debugShow (FuncArg ann arg) = "FuncArg(" ++ showAnn (Proxy @p) ann ++ " " ++ arg ++ ")"
