{-# LANGUAGE InstanceSigs #-}

module Generic.DebugShow where

import           Data.Graph (SCC (AcyclicSCC, CyclicSCC))
import           Data.List  (intercalate, intersperse)

class DebugShow a where
  debugShow :: a -> String

instance DebugShow a => DebugShow [a] where
  debugShow :: [a] -> String
  debugShow = mconcat . intersperse "\n" . map debugShow

instance DebugShow a => DebugShow (SCC a) where
  debugShow (AcyclicSCC a) =
    "AcyclicSCC (" ++ debugShow a ++ ")"

  debugShow (CyclicSCC as) =
    "CyclicSCC [" ++ intercalate ", " (map debugShow as) ++ "]"

debugPrint :: (DebugShow a) => [a] -> IO ()
debugPrint = putStrLn . debugShow
