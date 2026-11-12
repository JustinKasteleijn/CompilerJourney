module AST.HasIdentifier where

class HasIdentifier a where
  getIdentifier :: a -> Maybe String
