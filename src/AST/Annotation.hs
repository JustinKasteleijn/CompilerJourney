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

class Annotated f where
  getAnn :: f p -> Ann p
  setAnn :: Ann p -> f p -> f p

class HasSpan (p :: Phase) where
  getSpan :: Proxy p -> Ann p -> Span

instance HasSpan 'Parsed where
  getSpan :: Proxy 'Parsed -> Ann 'Parsed -> Span
  getSpan _ ann = ann

instance HasSpan 'Typed where
  getSpan :: Proxy 'Typed -> Ann 'Typed -> Span
  getSpan _ = fst

class HasType (p :: Phase) where
  getType :: Proxy p -> Ann p -> Type

instance HasType Typed where
  getType :: Proxy 'Typed -> Ann 'Typed -> Type
  getType _ = snd

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
