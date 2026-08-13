{-# LANGUAGE DataKinds #-}

module ASTTest.ExprSpec (spec) where

import Test.Syd
import AST.Expr
import AST.Annotation

spec :: Spec
spec = describe "Expression show test" $ do
  it "Correctly prints precedence for multiply" $ do
    let ann :: Ann 'Testing
        ann = ()

    let expr :: Expr 'Testing
        expr =
          BinaryOperation ann
            (LInt ann 1)
            Mul
            (BinaryOperation ann (LInt ann 2) Add (LInt ann 3))
    show expr `shouldBe` "1 * (2 + 3)"
