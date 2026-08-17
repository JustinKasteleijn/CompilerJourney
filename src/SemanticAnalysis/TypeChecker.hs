{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}

module SemanticAnalysis.TypeChecker where

import           AST.Type

import           AST.Annotation           (Phase (..))
import           AST.Argument             (FuncArg (FuncArg),
                                           getFunctionArgumentName)
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
import           Generic.Triple           (fst3, snd3, thd3)

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
                       Just t  -> apply (Subst s) t
  apply s (TTuple ts)      = TTuple (apply s ts)
  apply s (TFun args body) = TFun (apply s args) (apply s body)
  apply _ t = t

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

instance Types (Expr 'Typed) where
  apply :: Subst -> Expr 'Typed -> Expr 'Typed
  apply s = \case
    LInt            (sp, t) n        -> LInt   (sp, apply s t) n
    LBool           (sp, t) b        -> LBool  (sp, apply s t) b
    LChar           (sp, t) c        -> LChar  (sp, apply s t) c
    LIdent          (sp, t) v        -> LIdent (sp, apply s t) v
    Tuple           (sp, t) xs       -> Tuple  (sp, apply s t) (apply s xs)
    UnaryOperation  (sp, t) op x     -> UnaryOperation (sp, apply s t) op (apply s x)
    BinaryOperation (sp, t) l op r   -> BinaryOperation (sp, apply s t) (apply s l) op (apply s r)
    Lambda          (sp, t) arg body -> Lambda (sp, apply s t) (map (apply s) arg) (apply s body)
    Application     (sp, t) f arg    -> Application (sp, apply s t) (apply s f) (apply s arg)

  ftv :: Expr 'Typed -> Set.Set String
  ftv = \case
    LInt            (_, t)       _        -> ftv t
    LBool           (_, t)       _        -> ftv t
    LChar           (_, t)       _        -> ftv t
    LIdent          (_, t)       _        -> ftv t

    Tuple           (_, t)       xs       -> ftv t <> ftv xs
    UnaryOperation  (_, t)       _ x      -> ftv t <> ftv x
    BinaryOperation (_, t)       l _ r    -> ftv t <> ftv l <> ftv r
    Lambda          (_, t)       _ body   -> ftv t <> ftv body
    Application     (_, t)       f args   -> ftv t <> ftv f <> ftv args

instance Types (FuncArg 'Typed) where
  apply :: Subst -> FuncArg 'Typed -> FuncArg 'Typed
  apply s = \case
    FuncArg (sp, t) v -> FuncArg (sp, apply s t) v

  ftv :: FuncArg 'Typed -> Set.Set String
  ftv (FuncArg (_, t) _) = ftv t

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

  check :: a -> Type -> TI (Subst, Expr 'Typed)
  check x t = do
    (s, t', ast) <- infer x
    s'      <- unify t t'
    pure (s' <> s, ast)

  infer :: a -> TI (Subst, Type, Expr 'Typed)
  infer x = do
    v <- newTypeVar
    (s, ast) <- check x v
    pure (s, apply s v, apply s ast)

instance Typeable (Expr 'Parsed) where
  infer :: Expr 'Parsed -> TI (Subst, Type, Expr 'Typed)
  infer x = do
    (s, t, ast) <- inferExpr x
    return (s, apply s t, apply s ast)
    where
        inferExpr :: Expr 'Parsed -> TI (Subst, Type, Expr 'Typed)
        inferExpr (LInt sp n)                  = pure (mempty, TInt, LInt (sp, TInt) n)
        inferExpr (LBool sp b)                 = pure (mempty, TBool, LBool (sp, TBool) b)
        inferExpr (LChar sp c)                 = pure (mempty, TChar, LChar (sp, TChar) c)
        inferExpr (LIdent sp v)                = do
            env <- ask
            case Map.lookup v env of
              Nothing     -> throwError $ "Unbound variable " ++ v
              Just scheme -> do
                t <- instantiate scheme
                pure (mempty, t, LIdent (sp, t) v)
        inferExpr (Tuple sp xs)                   = do
            results <- mapM infer xs
            let substs = map fst3 results
                types  = map snd3 results
                ast    = map thd3 results
            pure (mconcat substs, TTuple types, Tuple (sp, TTuple types) ast)
        inferExpr (BinaryOperation sp lhs op rhs) = do
            (s1, t1, expr)  <- infer lhs
            (s2, t2, expr') <- local (apply s1) (infer rhs)
            t3              <- getBinaryType op
            a               <- newTypeVar
            s3              <- unify
                                (apply s2 t3)
                                (TFun [apply s2 t1, t2] a)
            let t           = apply s3 a
            pure (mconcat [s3, s2, s1], t, BinaryOperation (sp, t) expr op expr')
        inferExpr (UnaryOperation sp op expr)     = do
            (s1, t1, expr') <- infer expr
            t2              <- getUnaryType op
            a               <- newTypeVar
            s2              <- unify
                                (apply s1 t2)
                                (TFun [apply s1 t1] a)
            let t           = apply s2 a
            return (s2 <> s1, t, UnaryOperation (sp, t) op expr')
        inferExpr (Lambda sp args body) = do
            argTypes <- mapM (const newTypeVar) args
            let typedArgs :: [FuncArg 'Typed]
                typedArgs = zipWith (\(FuncArg sp' name) ty -> FuncArg (sp', ty) name) args argTypes
            let bindings =
                    Map.fromList
                    [ (name, Forall [] ty)
                    | (name, ty) <- zip (map getFunctionArgumentName args) argTypes
                    ]
            (s1, bodyType, bodyAst) <- local (Map.union bindings) (infer body)
            let fullType = TFun (map (apply s1) argTypes) (apply s1 bodyType)
            pure (s1, fullType, Lambda (sp, fullType) typedArgs bodyAst)
        inferExpr (Application sp lam exprs) = do
            (sf, tf, ast) <- infer lam
            results       <- mapM infer exprs
            retType       <- newTypeVar
            let sargs     = mconcat (map fst3 results)
            let expected  = TFun (map snd3 results) retType
            let argsAst   = map thd3 results
            s3            <- unify (apply sargs tf) expected
            let s         = mconcat [s3, sargs, sf]
            let t         = apply s retType
            pure (s, t, Application (sp, t) ast argsAst)

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
