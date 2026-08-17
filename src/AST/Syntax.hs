{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module AST.Syntax where

import           AST.Annotation      (Ann, Annotated (getAnn, setAnn),
                                      Phase (..), ShowAnn (showAnn))
import           Data.Proxy          (Proxy (..))
import           Generic.DebugShow   (DebugShow (debugShow))
import           Generic.ProgramShow (ProgramShow (programShow), indentS,
                                      newline)
import           Generic.StringUtils (joinWithCommaDS, joinWithCommaS)

data Expr (p :: Phase)
  = LInt   (Ann p) Int
  | LChar  (Ann p) Char
  | LBool  (Ann p) Bool
  | LIdent (Ann p) String

  | Tuple           (Ann p) [Expr p]
  | BinaryOperation (Ann p) (Expr p) BinaryOperator (Expr p)
  | UnaryOperation  (Ann p) UnaryOperator (Expr p)

  | Lambda          (Ann p) [FuncArg p] (Expr p)
  | Application     (Ann p) (Expr p) [Expr p]

instance Annotated Expr where
  getAnn :: Expr p -> Ann p
  getAnn (LInt ann _)                = ann
  getAnn (LChar ann _)               = ann
  getAnn (LBool ann _)               = ann
  getAnn (LIdent ann _)              = ann
  getAnn (Tuple ann _)               = ann
  getAnn (BinaryOperation ann _ _ _) = ann
  getAnn (UnaryOperation ann _ _)    = ann
  getAnn (Lambda ann _ _)            = ann
  getAnn (Application ann _ _)       = ann

  setAnn :: Ann p -> Expr p -> Expr p
  setAnn ann (LInt _ n)                     = LInt ann n
  setAnn ann (LChar _ c)                    = LChar ann c
  setAnn ann (LBool _ b)                    = LBool ann b
  setAnn ann (LIdent _ v)                   = LIdent ann v
  setAnn ann (Tuple _ ts)                   = Tuple ann ts
  setAnn ann (BinaryOperation _ lhs op rhs) = BinaryOperation ann lhs op rhs
  setAnn ann (UnaryOperation _ op expr)     = UnaryOperation ann op expr
  setAnn ann (Lambda _ params body)         = Lambda ann params body
  setAnn ann (Application _ lam args)       = Application ann lam args

data BinaryOperator
  = Add
  | Sub
  | Mul
  | Div
  | Pow
  | Mod
  | Lt
  | Gt
  | Lte
  | Gte
  | Eq
  | Neq
  | And
  | Or

data UnaryOperator
  = Neg
  | Not
  deriving (Eq)

instance Show (Expr p) where
  showsPrec _ (LInt _ n) = shows n
  showsPrec _ (LBool _ b) = shows b
  showsPrec _ (LChar _ c) = shows c
  showsPrec _ (LIdent _ x) = showString x

  showsPrec p (BinaryOperation _ left op right) =
    showParen (p > opPrec) $
      showsPrec opPrec left
      . showString (" " ++ show op ++ " ")
      . showsPrec opPrec right
    where
      opPrec = precedence op

  showsPrec p (UnaryOperation _ op expr) =
    showParen (p > 40) $
      showString (show op)
      . showsPrec 40 expr

  showsPrec _ (Tuple _ xs) =
    showString "("
    . showString (joinWithCommaS xs)
    . showString ")"

  showsPrec _ (Lambda _ args body) =
    showString "|"
    . showString (joinWithCommaS args)
    . showString "| { \n"
    . showString (show body)
    . showString "\n}"

  showsPrec _ (Application _ lam expr) =
    showString (show lam)
    .  showString ("(" ++ show expr ++ ")")

precedence :: BinaryOperator -> Int
precedence Add = 6
precedence Sub = 6
precedence Mul = 7
precedence Div = 7
precedence Pow = 8
precedence _   = 1


instance Show BinaryOperator where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Pow = "**"
  show Mod = "%"

  show Lt  = "<"
  show Gt  = ">"
  show Lte = "<="
  show Gte = ">="
  show Eq  = "=="
  show Neq = "!="

  show And = "&&"
  show Or  = "||"


instance Show UnaryOperator where
  show Neg = "-"
  show Not = "!"

instance ShowAnn p => DebugShow (Expr p) where
  debugShow :: Expr p -> String
  debugShow (LInt ann n) =
    "LInt " ++ showAnn (Proxy @p) ann ++ " " ++ show n
  debugShow (LChar ann c) =
    "LChar " ++ showAnn (Proxy @p) ann ++ " " ++ show c
  debugShow (LBool ann b) =
    "LBool " ++ showAnn (Proxy @p) ann ++ " " ++ show b
  debugShow (LIdent ann x) =
    "LIdent " ++ showAnn (Proxy @p) ann ++ " " ++ show x
  debugShow (Tuple ann xs) =
    "Tuple " ++ showAnn (Proxy @p) ann ++ " ["
      ++ joinWithCommaDS xs
      ++ "]"
  debugShow (UnaryOperation ann op expr) =
    "UnaryOperation "
      ++ showAnn (Proxy @p) ann
      ++ " "
      ++ show op
      ++ " ("
      ++ debugShow expr
      ++ ")"
  debugShow (BinaryOperation ann left op right) =
    "BinaryOperation "
      ++ showAnn (Proxy @p) ann
      ++ " ("
      ++ debugShow left
      ++ ") "
      ++ show op
      ++ " ("
      ++ debugShow right
      ++ ")"
  debugShow (Lambda ann params body) =
    "Lambda "
      ++ showAnn (Proxy @p) ann
      ++ " (|"
      ++ joinWithCommaDS params
      ++ " |\n {"
      ++ debugShow body
      ++ "\n }"
  debugShow (Application ann lam expr) =
    "Application "
      ++ showAnn (Proxy @p) ann
      ++ " ("
      ++ debugShow lam
      ++ "("
      ++ joinWithCommaDS expr
      ++ ")"

instance ProgramShow (Expr p) where
  programShow :: Int -> Expr p -> String
  programShow n (Lambda _ params body)
    = "|" ++ joinWithCommaS params ++ "| {" ++ newline
      ++ indentS (n + 1) body
      ++ newline ++ "}"
  programShow n (Application _ lam expr)
    = programShow n lam ++ "(" ++  joinWithCommaS expr ++ ")"
  programShow n expr
    = indentS n expr

data AnnotatedType (p :: Phase)
  = ATInt (Ann p)
  | ATBool (Ann p)
  | ATChar (Ann p)
  | ATTVar (Ann p) String
  | ATTuple (Ann p) [AnnotatedType p]
  | ATFun (Ann p) [AnnotatedType p] (AnnotatedType p)

instance Show (AnnotatedType p) where
  show :: AnnotatedType p -> String
  show (ATInt _)           = "int"
  show (ATBool _)          = "bool"
  show (ATChar _)          = "char"
  show (ATTVar _ v)        = v
  show (ATTuple _ ts)      = "(" ++ joinWithCommaS ts ++ ")"
  show (ATFun _ args body) = "|" ++ joinWithCommaS args ++ "| ->" ++ show body

data FuncArg (p :: Phase)
  = FuncArg (Ann p) String (Maybe (AnnotatedType p))

instance Annotated FuncArg where
  getAnn :: FuncArg p -> Ann p
  getAnn (FuncArg ann _ _) = ann

  setAnn :: Ann p -> FuncArg p -> FuncArg p
  setAnn ann (FuncArg _ arg t) = FuncArg ann arg t

instance Show (FuncArg p) where
  show :: FuncArg p -> String
  show (FuncArg _ arg t) = maybe arg (\x -> ": " ++ show x) t

instance (ShowAnn p) => DebugShow (FuncArg p) where
  debugShow :: FuncArg p -> String
  debugShow (FuncArg ann arg t) = "FuncArg(" ++ showAnn (Proxy @p) ann ++ " " ++ arg ++ ": " ++ show t ++")"

-- data Statement (p :: Phase)
--   = If     (Expr p) [Statement p] [Statement p]
--   | While  (Expr p) [Statement p]
--   | Assign ()
