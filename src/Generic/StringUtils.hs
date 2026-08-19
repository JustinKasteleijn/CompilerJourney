module Generic.StringUtils where

import           Data.List           (intercalate)
import           Generic.DebugShow   (DebugShow (debugShow))
import           Generic.ProgramShow (ProgramShow (programShow))

joinWith :: String -> (a -> String) -> [a] -> String
joinWith sep f = intercalate sep . map f

joinWithComma :: (a -> String) -> [a] -> String
joinWithComma = joinWith ", "

joinWithCommaS :: Show a => [a] -> String
joinWithCommaS = joinWithComma show

joinWithCommaDS :: DebugShow a => [a] -> String
joinWithCommaDS = joinWithComma debugShow

joinwithCommaPS :: ProgramShow a => Int -> [a] -> String
joinwithCommaPS n = joinWithComma (programShow n)

getBetween :: String -> String
getBetween =
  takeWhile (/= ')') . drop 1 . dropWhile (/= '(')
