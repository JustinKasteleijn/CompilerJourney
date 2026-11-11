{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE KindSignatures #-}

module SemanticAnalysis.StronglyConnectedComponent where

import           AST.Syntax (Expr, Ident (Ident), Statement (Assign, VarDecl))
import           Data.Graph (SCC, stronglyConnComp)

class Graphable a where
  getDependencies :: a -> [String]
  getKey          :: a -> Maybe String

scc :: Graphable a => [a] -> [SCC a]
scc = stronglyConnComp . map mkNode

mkNode :: Graphable a => a -> (a, String, [String])
mkNode = undefined
