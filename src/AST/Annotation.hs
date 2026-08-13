{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module AST.Annotation where

import           AST.Type     (Type)
import           Data.Proxy   (Proxy)
import           Generic.Span (Span)

data Phase
  = Parsed
  | Typed
  | Testing

type family Ann (p :: Phase) where
  Ann 'Parsed  = Span
  Ann 'Typed   = (Span, Type)
  Ann 'Testing = ()

class ShowAnn (p :: Phase) where
  showAnn :: Proxy p -> Ann p -> String

instance ShowAnn 'Parsed where
  showAnn :: Proxy 'Parsed -> Span -> String
  showAnn _ sp = "Ann (" ++ show sp ++ ")"

instance ShowAnn 'Typed where
  showAnn :: Proxy 'Typed -> (Span, Type) -> String
  showAnn _ (sp, t) = "Ann (" ++ show sp ++ ", " ++ show t ++ ")"

instance ShowAnn 'Testing where
  showAnn :: Proxy 'Testing -> () -> String
  showAnn _ _ = ""
