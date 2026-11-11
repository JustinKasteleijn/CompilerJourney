{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}

module SemanticAnalysis.EntryPointAnalysis where

import           AST.Annotation (Phase (Parsed))
import           AST.Syntax     (Ident (Ident), Statement (VarDecl))

runEntryPointAnalysis :: Either String [Statement 'Parsed] -> [Statement 'Parsed]
runEntryPointAnalysis = \case
  Right xs -> xs
  Left err -> error err

hasMainFunction :: [Statement 'Parsed] -> Either String [Statement 'Parsed]
hasMainFunction xs = if any ((==) "main" . getFunctionName) xs
                     then pure xs
                     else Left "No main function found"
  where
    getFunctionName :: Statement 'Parsed -> String
    getFunctionName (VarDecl _ (Ident _ n) _ _) = n
    getFunctionName _                           = ""
