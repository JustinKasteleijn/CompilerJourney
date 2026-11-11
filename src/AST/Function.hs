{-# LANGUAGE DataKinds #-}

module AST.Function where

import           AST.Annotation (Phase (Parsed))
import           AST.Syntax     (Ident (Ident), Statement (VarDecl))

getFunctionName :: Statement 'Parsed -> String
getFunctionName (VarDecl _ (Ident _ n) _ _) = n
getFunctionName _                           = ""
