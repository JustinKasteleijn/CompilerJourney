{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module SemanticAnalysis.TypeChecker where

import           AST.Type

import           AST.Annotation           (Phase (..))
import           AST.Argument             (getFunctionArgumentName)
import           AST.Expr                 (BinaryOperator (..), Expr (..),
                                           OperatorKind (..),
                                           UnaryOperator (..), kindOf)
import           Control.Monad.Except
import           Control.Monad.RWS.Strict (MonadReader (ask, local), RWS,
                                           runRWS)
import           Control.Monad.State
import           Data.Char                (chr, ord)
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import           Generic.Triple           (fst3)

class Types a where
  ftv   :: a -> Set.Set String
  apply :: Subst -> a -> a

instance Types Type where
  ftv :: Type -> Set.Set String
  ftv (TVar n)       = Set.singleton n
  ftv TInt           = mempty
  ftv TBool          = mempty
  ftv TChar          = mempty
  ftv (TTuple ts)    = foldMap ftv ts
  ftv (TFun ins out) = foldMap ftv ins <> ftv out

  apply :: Subst -> Type -> Type
  apply (Subst s) (TVar n) = case Map.lookup n s of
                       Nothing -> TVar n
                       Just t  -> t
  apply _ t                = t

instance Types Scheme where
  ftv :: Scheme -> Set.Set String
  ftv (Forall vars t) = ftv t `Set.difference` Set.fromList vars

  apply :: Subst -> Scheme -> Scheme
  apply (Subst s) (Forall vars t) =
    let s' = Subst (foldr Map.delete s vars)
      in Forall vars (apply s' t)

instance (Types a, Traversable f) => Types (f a) where
  apply :: Subst -> f a -> f a
  apply s = fmap (apply s)

  ftv :: f a -> Set.Set String
  ftv = foldMap ftv

newtype Subst = Subst { unSubst :: Map.Map String Type }
  deriving (Show)

instance Monoid Subst where
  mempty :: Subst
  mempty = Subst Map.empty

instance Semigroup Subst where
  (<>) :: Subst -> Subst -> Subst
  (Subst s1) <> (Subst s2) = Subst (Map.map (apply (Subst s1)) s2 `Map.union` s1)

type TypeEnv = Map.Map String Scheme

generalize :: TypeEnv -> Type -> Scheme
generalize env t =
  let vars = Set.toList (ftv t `Set.difference` ftv env)
    in Forall vars t

type TI a = ExceptT String (RWS TypeEnv ShowS Int) a

runTI :: TI a -> (Either String a, Int, ShowS)
runTI t = runRWS (runExceptT t) mempty 0

evalTI :: TI a -> Either String a
evalTI = fst3 . runTI

newTypeVar :: TI Type
newTypeVar = do
  s <- get
  put (s + 1)
  return (TVar $ typeVarName s)
 where
   typeVarName :: Int -> String
   typeVarName n =
     let letter = chr (ord 'a' + (n `mod` 26))
         suffix = n `div` 26
      in letter : replicate suffix '\''

instantiate :: Scheme -> TI Type
instantiate (Forall vars t) = do
  newVars <- mapM (const newTypeVar) vars
  let s = Subst $ Map.fromList (zip vars newVars)
  return $ apply s t

unify :: Type -> Type -> TI Subst
unify (TVar n) t                        = varBind n t
unify t (TVar n)                        = varBind n t
unify TInt TInt                         = pure mempty
unify TBool TBool                       = pure mempty
unify (TTuple types) (TTuple types')    = unifyMany types types'
unify (TFun args ret) (TFun args' ret') = do
  s1 <- unifyMany args args'
  s2 <- unify (apply s1 ret) (apply s1 ret')
  pure (s2 <> s1)
unify t1 t2                             = throwError $ "Cannot unify " ++ show t1 ++ " with " ++ show t2

unifyMany :: [Type] -> [Type] -> TI Subst
unifyMany [] []            = pure mempty
unifyMany (t1:ts) (t2:ts') = do
  s1 <- unify t1 t2
  s2 <- unifyMany
           (apply s1 ts)
           (apply s1 ts')
  pure (s2 <> s1)
unifyMany _ _ = throwError "Arity mismatch"

varBind :: String -> Type -> TI Subst
varBind u t | t == TVar u          = return mempty
            | u `Set.member` ftv t = throwError "Cannot construct infinite type"
            | otherwise            = return $ Subst $ Map.singleton u t

class Typeable a where
  {-# MINIMAL check | infer #-}

  check :: a -> Type -> TI Subst
  check x t = do
    (s, t') <- infer x
    s'      <- unify t t'
    pure (s' <> s)

  infer :: a -> TI (Subst, Type)
  infer x = do
    v <- newTypeVar
    s <- check x v
    pure (s, apply s v )

instance Typeable (Expr 'Parsed) where
  infer :: Expr 'Parsed -> TI (Subst, Type)
  infer (LInt _ _)                     = pure (mempty, TInt)
  infer (LBool _ _)                    = pure (mempty, TBool)
  infer (LChar _ _)                    = pure (mempty, TChar)
  infer (LIdent _ n)                   = do
    env <- ask
    case Map.lookup n env of
      Nothing     -> throwError $ "Unbound variable " ++ n
      Just scheme -> do
        t <- instantiate scheme
        pure (mempty, t)
  infer (Tuple _ xs)                   = do
    results <- mapM infer xs
    let substs = map fst results
        types  = map snd results
    pure (mconcat substs, TTuple types)
  infer (BinaryOperation _ lhs op rhs) = do
    (s1, t1) <- infer lhs
    (s2, t2) <- local (apply s1) (infer rhs)
    t3       <- getBinaryType op
    a        <- newTypeVar
    s3       <- unify
                 (apply s2 t3)
                 (TFun [apply s2 t1, t2] a)
    pure (mconcat [s3, s2, s1], apply s3 a)
  infer (UnaryOperation _ op expr)     = do
    (s1, t1) <- infer expr
    t2       <- getUnaryType op
    a        <- newTypeVar
    s2       <- unify
                 (apply s1 t2)
                 (TFun [apply s1 t1] a)
    return (s2 <> s1, apply s2 a)
  infer (Lambda _ args body) = do
    argTypes <- mapM (const newTypeVar) args
    let bindings =
            Map.fromList
            [ (name, Forall [] ty)
            | (name, ty) <- zip (map getFunctionArgumentName args) argTypes
            ]
    (s1, bodyType) <- local (Map.union bindings) (infer body)
    pure
        ( s1
        , TFun (map (apply s1) argTypes) (apply s1 bodyType)
        )
  infer (Application _ lam exprs) = do
    (sf, tf) <- infer lam
    results  <- mapM infer exprs
    retType  <- newTypeVar
    let sargs = mconcat (map fst results)
    let expected = TFun (map snd results) retType
    s3 <- unify (apply sargs tf) expected
    let s = mconcat [sf, sargs, s3]
    pure (s, apply s retType)

getUnaryType :: UnaryOperator -> TI Type
getUnaryType Neg = pure $ TFun [TInt] TInt
getUnaryType Not = pure $ TFun [TBool] TBool

getBinaryType :: BinaryOperator -> TI Type
getBinaryType op =
  case kindOf op of
    Arithmetic -> pure $ TFun [TInt, TInt] TInt
    Logical    -> pure $ TFun [TBool, TBool] TBool
    Comparison -> pure $ TFun [TInt, TInt] TInt -- Has to be refactored later when implementing type classes
    Equality   -> do
      a <- newTypeVar
      pure $ TFun [a, a] a
