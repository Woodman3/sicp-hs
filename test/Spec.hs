module Main where

import Test.Hspec
import Interpreter (eval_expr)

main :: IO ()
main = hspec $ do
    describe "Lisp Interpreter - Basic Arithmetic" $ do
        it "handles addition" $ do
            eval_expr "(+ 1 2)" `shouldBe` Right "Int 3"
            eval_expr "(+ 10 20)" `shouldBe` Right "Int 30"
            eval_expr "(+ (- 5 3) (* 2 4))" `shouldBe` Right "Int 10"

        it "handles subtraction" $ do
            eval_expr "(- 5 3)" `shouldBe` Right "Int 2"
            eval_expr "(- 10 5)" `shouldBe` Right "Int 5"
            eval_expr "(- 100 (+ 50 25))" `shouldBe` Right "Int 25"

        it "handles multiplication" $ do
            eval_expr "(* 2 3)" `shouldBe` Right "Int 6"
            eval_expr "(* 10 5)" `shouldBe` Right "Int 50"
            eval_expr "(* (+ 1 2) (- 5 2))" `shouldBe` Right "Int 9"

        it "handles division" $ do
            eval_expr "(/ 6 2)" `shouldBe` Right "Int 3"
            eval_expr "(/ 100 10)" `shouldBe` Right "Int 10"
            eval_expr "(/ (* 10 5) (+ 2 3))" `shouldBe` Right "Int 10"

        it "handles nested expressions" $ do
            eval_expr "(+ (* 2 3) (- 5 2))" `shouldBe` Right "Int 9"
            eval_expr "(/ (* 10 5) (+ 1 1))" `shouldBe` Right "Int 25"
            eval_expr "(- (+ 10 20) (* 2 5))" `shouldBe` Right "Int 20"

        -- it "handles invalid expressions" $ do
        --     eval_expr "(+ 1)" `shouldThrow` anyException  -- 缺少参数
        --     eval_expr "(1 2 3)" `shouldThrow` anyException  -- 无效的表达式
        --     eval_expr "(+ 1 two)" `shouldThrow` anyException  -- 非数字参数

-- 辅助函数：检查是否是 Left 值
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False