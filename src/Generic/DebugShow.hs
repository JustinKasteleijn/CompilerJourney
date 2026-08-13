module Generic.DebugShow where

class DebugShow a where
  debugShow :: a -> String

debugPrint :: (DebugShow a) => a -> IO ()
debugPrint = putStrLn . debugShow
