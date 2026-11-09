{-# LANGUAGE InstanceSigs #-}

module Generic.DebugShow where

import           Data.List (intersperse)

class DebugShow a where
  debugShow :: a -> String

instance DebugShow a => DebugShow [a] where
  debugShow :: [a] -> String
  debugShow = mconcat . intersperse "\n" . map debugShow

debugPrint :: (DebugShow a) => [a] -> IO ()
debugPrint = putStrLn . debugShow
