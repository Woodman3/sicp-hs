module Main(main) where

import Test.Hspec
import Interpreter (evalExpr)

main :: IO ()
main = hspec $ do
    describe "Lisp Interpreter - Basic Arithmetic" $ do
        it "handles addition" $ do
            evalExpr "(+ 1 2)" `shouldBe` Right "Int 3"
            evalExpr "(+ 10 20)" `shouldBe` Right "Int 30"
            evalExpr "(+ (- 5 3) (* 2 4))" `shouldBe` Right "Int 10"

        it "handles subtraction" $ do
            evalExpr "(- 5 3)" `shouldBe` Right "Int 2"
            evalExpr "(- 10 5)" `shouldBe` Right "Int 5"
            evalExpr "(- 100 (+ 50 25))" `shouldBe` Right "Int 25"

        it "handles multiplication" $ do
            evalExpr "(* 2 3)" `shouldBe` Right "Int 6"
            evalExpr "(* 10 5)" `shouldBe` Right "Int 50"
            evalExpr "(* (+ 1 2) (- 5 2))" `shouldBe` Right "Int 9"

        it "handles division" $ do
            evalExpr "(/ 6 2)" `shouldBe` Right "Int 3"
            evalExpr "(/ 100 10)" `shouldBe` Right "Int 10"
            evalExpr "(/ (* 10 5) (+ 2 3))" `shouldBe` Right "Int 10"

        it "handles nested expressions" $ do
            evalExpr "(+ (* 2 3) (- 5 2))" `shouldBe` Right "Int 9"
            evalExpr "(/ (* 10 5) (+ 1 1))" `shouldBe` Right "Int 25"
            evalExpr "(- (+ 10 20) (* 2 5))" `shouldBe` Right "Int 20"
        
        it "handles comparison operators" $ do
            evalExpr "(< 1 2)" `shouldBe` Right "Bool True"
            evalExpr "(< 2 1)" `shouldBe` Right "Bool False"
            evalExpr "(> 1 2)" `shouldBe` Right "Bool False"
            evalExpr "(> 2 1)" `shouldBe` Right "Bool True"
            evalExpr "(= 1 1)" `shouldBe` Right "Bool True"
            evalExpr "(= 1 2)" `shouldBe` Right "Bool False"
            evalExpr "(<= 1 2)" `shouldBe` Right "Bool True"
            evalExpr "(<= 2 2)" `shouldBe` Right "Bool True"
            evalExpr "(<= 2 1)" `shouldBe` Right "Bool False"
            evalExpr "(>= 1 2)" `shouldBe` Right "Bool False"
            evalExpr "(>= 2 2)" `shouldBe` Right "Bool True"
            evalExpr "(>= 2 1)" `shouldBe` Right "Bool True"
        
        it "handles logical operators" $ do
            evalExpr "(and #t #t)" `shouldBe` Right "Bool True"
            evalExpr "(and #t #f)" `shouldBe` Right "Bool False"
            evalExpr "(and #f #t)" `shouldBe` Right "Bool False"
            evalExpr "(and #f #f)" `shouldBe` Right "Bool False"
            evalExpr "(or #t #t)" `shouldBe` Right "Bool True"
            evalExpr "(or #t #f)" `shouldBe` Right "Bool True"
            evalExpr "(or #f #t)" `shouldBe` Right "Bool True"
            evalExpr "(or #f #f)" `shouldBe` Right "Bool False"
            evalExpr "(not #t)" `shouldBe` Right "Bool False"
            evalExpr "(not #f)" `shouldBe` Right "Bool True"

        it "handles if expressions" $ do
            evalExpr "(if #t 1 2)" `shouldBe` Right "Int 1"
            evalExpr "(if #f 1 2)" `shouldBe` Right "Int 2"
            evalExpr "(if (< 1 2) 1 2)" `shouldBe` Right "Int 1"
            evalExpr "(if (> 1 2) 1 2)" `shouldBe` Right "Int 2"
            evalExpr "(if (= 1 1) 1 2)" `shouldBe` Right "Int 1"
            evalExpr "(if (= 1 2) 1 2)" `shouldBe` Right "Int 2"
            evalExpr "(if (<= 1 2) 1 2)" `shouldBe` Right "Int 1"
            evalExpr "(if (>= 1 2) 1 2)" `shouldBe` Right "Int 2"
        
        it "handles cond expressions" $ do
            evalExpr "(cond (#t 1) (#f 2))" `shouldBe` Right "Int 1"
            evalExpr "(cond (#f 1) (#t 2))" `shouldBe` Right "Int 2"
            evalExpr "(cond ((< 1 2) 1) ((> 1 2) 2))" `shouldBe` Right "Int 1"
            evalExpr "(cond ((> 1 2) 1) ((< 1 2) 2))" `shouldBe` Right "Int 2"
            evalExpr "(cond ((= 1 1) 1) ((= 1 2) 2))" `shouldBe` Right "Int 1"
            evalExpr "(cond ((= 1 2) 1) ((= 1 1) 2))" `shouldBe` Right "Int 2"
            evalExpr "(cond ((<= 1 2) 1) ((>= 1 2) 2))" `shouldBe` Right "Int 1"
            evalExpr "(cond ((>= 1 2) 1) ((<= 1 2) 2))" `shouldBe` Right "Int 2"

        it "handles begin expressions" $ do
            evalExpr "(begin 1 2 3)" `shouldBe` Right "Int 3"
            evalExpr "(begin (+ 1 2) (- 5 2) (* 2 4))" `shouldBe` Right "Int 8"
            evalExpr "(begin (begin 1 2) 3)" `shouldBe` Right "Int 3"
        
        it "handles define expressions" $ do
            evalExpr "(define x 10) x" `shouldBe` Right "Int 10"
            evalExpr "(define x 10) (define y 20) (+ x y)" `shouldBe` Right "Int 30"
            evalExpr "(define x 10) (define y 20) (define z (+ x y)) z" `shouldBe` Right "Int 30"
        
        it "handles set1 expressions" $ do
            evalExpr "(define x 10) (set1 x 20) x" `shouldBe` Right "Int 20"
            evalExpr "(define x 10) (define y 20) (set1 x (+ x y)) x" `shouldBe` Right "Int 30"
            evalExpr "(define x 10) (define y 20) (set1 x (+ x y)) (set1 y 30) (+ x y)" `shouldBe` Right "Int 60"
        
        it "handles lambda expressions" $ do
            evalExpr "((lambda (x) x) 10)" `shouldBe` Right "Int 10"
            evalExpr "((lambda (x y) (+ x y)) 10 20)" `shouldBe` Right "Int 30"
            evalExpr "((lambda (x y) (+ x y)) 10 (+ 10 20))" `shouldBe` Right "Int 40"


        -- it "handles invalid expressions" $ do
        --     eval_expr "(+ 1)" `shouldThrow` anyException  -- 缺少参数
        --     eval_expr "(1 2 3)" `shouldThrow` anyException  -- 无效的表达式
        --     eval_expr "(+ 1 two)" `shouldThrow` anyException  -- 非数字参数
