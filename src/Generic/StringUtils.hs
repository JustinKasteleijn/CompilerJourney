module Generic.StringUtils where

import           Data.List           (intercalate)
import           Generic.DebugShow   (DebugShow (debugShow))
import           Generic.ProgramShow (ProgramShow (programShow))

showOrEmpty :: (Show a) => Maybe a -> String
showOrEmpty = maybe "" show

joinWith :: String -> (a -> String) -> [a] -> String
joinWith sep f = intercalate sep . map f

joinWithNewline :: (a -> String) -> [a] -> String
joinWithNewline = joinWith "\n"

joinWithNewlineS :: (Show a) => [a] -> String
joinWithNewlineS = joinWithNewline show

joinWithNewlineDS :: (DebugShow a) => [a] -> String
joinWithNewlineDS = joinWithNewline debugShow

joinWithNewlinePS :: (ProgramShow a) => Int -> [a] -> String
joinWithNewlinePS n = joinWithNewline (programShow n)

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

indent :: Int -> String
indent n = replicate (n * 2) ' '

indented :: Int -> String -> String
indented n s = indent n ++ s
