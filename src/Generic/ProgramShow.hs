module Generic.ProgramShow where

class ProgramShow a where
  programShow :: Int -> a -> String

indent :: (a -> String) -> Int -> a -> String
indent f n x = replicate (n * 2) ' ' ++ f x

indentS :: Show a => Int -> a -> String
indentS = indent show

newline :: String
newline = "\n"

prettyPrint :: (ProgramShow a) => a -> IO ()
prettyPrint = putStrLn . programShow 0
