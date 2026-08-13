{-# LANGUAGE InstanceSigs #-}

module Generic.Span where

import           Text.Megaparsec.Pos (SourcePos (..), unPos)

data Span = Span SourcePos SourcePos
  deriving (Eq)

instance Show Span where
  show :: Span -> String
  show (Span s e) =
    showPos s ++ "-" ++ showPos e
    where
      showPos (SourcePos _ l c) =
        show (unPos l) ++ ":" ++ show (unPos c)
