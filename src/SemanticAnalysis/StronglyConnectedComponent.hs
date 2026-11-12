{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE KindSignatures #-}

module SemanticAnalysis.StronglyConnectedComponent where

import           AST.HasIdentifier (getIdentifier)
import           AST.Syntax        (Expr (Application, BinaryOperation, Lambda, Tuple, UnaryOperation),
                                    FuncArg,
                                    Statement (Assign, Return, VarDecl, While))
import           Data.Graph        (SCC, stronglyConnComp)
import           Data.Maybe        (mapMaybe)
import qualified Data.Set          as Set

class Graphable a where
  getDependencies :: a -> Set.Set String
  getKey          :: a -> Maybe String

scc :: Graphable a => [a] -> [SCC a]
scc = stronglyConnComp . mapMaybe mkNode

mkNode :: Graphable a => a -> Maybe (a, String, [String])
mkNode x = do
  key <- getKey x
  pure (x, key, Set.toList (getDependencies x))

instance Graphable (Expr p) where
  getDependencies :: Expr p -> Set.Set String
  getDependencies (BinaryOperation _ lhs _ rhs) = getDependencies lhs <> getDependencies rhs
  getDependencies (UnaryOperation _ _ expr)     = getDependencies expr
  getDependencies (Tuple _ xs)                  = foldMap getDependencies xs
  getDependencies (Lambda _ args body)          = foldMap getDependencies args
                                                    `Set.difference` foldMap getDependencies body
  getDependencies (Application _ func args)     = getDependencies func <> foldMap getDependencies args
  getDependencies _                             = mempty

  getKey :: Expr p -> Maybe String
  getKey = getIdentifier

instance Graphable (FuncArg p) where
  getDependencies :: FuncArg p -> Set.Set String
  getDependencies _ = mempty

  getKey :: FuncArg p -> Maybe String
  getKey = getIdentifier

instance Graphable (Statement p) where
  getDependencies :: Statement p -> Set.Set String
  getDependencies (Assign _ _ _ expr)  = getDependencies expr
  getDependencies (VarDecl _ _ _ expr) = getDependencies expr
  getDependencies (While _ cond body)  = getDependencies cond <> foldMap getDependencies body
  getDependencies (Return _ expr)      = getDependencies expr

  getKey :: Statement p -> Maybe String
  getKey = getIdentifier
