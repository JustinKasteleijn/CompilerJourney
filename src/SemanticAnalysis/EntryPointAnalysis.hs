{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}

module SemanticAnalysis.EntryPointAnalysis where

import           AST.Annotation (Phase (Parsed))
import           AST.Function   (getFunctionName)
import           AST.Syntax     (Statement)

runEntryPointAnalysis :: Either String [Statement 'Parsed] -> [Statement 'Parsed]
runEntryPointAnalysis = \case
  Right xs -> xs
  Left err -> error err

hasMainFunction :: [Statement 'Parsed] -> Either String [Statement 'Parsed]
hasMainFunction xs = if any ((==) "main" . getFunctionName) xs
                     then pure xs
                     else Left "No main function found"
