{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module AST.Argument where

import           AST.Annotation    (Ann, Phase, ShowAnn (showAnn))
import           Data.Proxy        (Proxy (Proxy))
import           Generic.DebugShow (DebugShow (debugShow))


data FuncArg (p :: Phase)
  = FuncArg (Ann p) String

getFunctionArgumentName :: FuncArg p -> String
getFunctionArgumentName (FuncArg _ n) = n

instance Show (FuncArg p) where
  show :: FuncArg p -> String
  show (FuncArg _ arg) = arg

instance (ShowAnn p) => DebugShow (FuncArg p) where
  debugShow :: FuncArg p -> String
  debugShow (FuncArg ann arg) = "FuncArg(" ++ showAnn (Proxy @p) ann ++ " " ++ arg ++ ")"
