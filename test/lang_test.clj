(ns lwb.lang-test
 (:require [clojure.test :refer :all]
           [lwb.lang :refer :all]))

; Operators -----------------------------------------------------------

(deftest impl-test
  (is (= true (impl true true)))
  (is (= false (impl true false)))
  (is (= true (impl false true)))
  (is (= true (impl false false))))

(deftest impl-test'
  (is (= '(or (not a) b) (macroexpand-1 '(impl a b)))))

(deftest equiv-test
  (is (= true (equiv true true)))
  (is (= false (equiv true false)))
  (is (= false (equiv false true)))
  (is (= true (equiv false false))))

(deftest equiv-test'
  (is (= '(and (impl a b) (impl b a)) (macroexpand-1 '(equiv a b)))))

(deftest xor-test
  (is (= false (xor true true)))
  (is (= true (xor true false)))
  (is (= true (xor false true)))
  (is (= false (xor false false))))

(deftest xor-test'
  (is (= '(not (equiv a b)) (macroexpand-1 '(xor a b)))))

(deftest ite-test
  (is (= true  (ite true  true  true)))
  (is (= true  (ite true  true  false)))
  (is (= false (ite true  false true)))
  (is (= false (ite true  false false)))
  (is (= true  (ite false true  true)))
  (is (= false (ite false true  false)))
  (is (= true  (ite false false true)))
  (is (= false (ite false false false))))

(deftest ite-test'
  (is (= '(or (and i t) (and (not i) e) (macroexpand-1 '(ite i t e))))))

; Utility functions with operators ------------------------------------

(deftest op?-test
  (is (= true) (op? 'and))
  (is (= true) (op? 'or))
  (is (= true) (op? 'not))
  (is (= true) (op? 'impl))
  (is (= true) (op? 'equiv))
  (is (= true) (op? 'xor))
  (is (= true) (op? 'ite))
  (is (= false) (op? 'true))
  (is (= false) (op? 1))
  (is (= false) (op? [1]))
  (is (= false) (op? '(x)))
  (is (= false) (op? '#{x})))

(deftest torf?-test
  (is (= true (torf? 'true)))
  (is (= true (torf? 'false)))
  (is (= false (torf? 'and)))
  (is (= false (torf? '(and)))))

(deftest atom?-test
  (is (= true (atom? 'x)))
  (is (= true (atom? 'hello)))
  (is (= false (atom? 'and)))
  (is (= false (atom? 1)))
  (is (= false (atom? '(and)))))

(deftest arity-test
  (is (= 1 (arity 'not)))
  (is (= nil (arity 'x)))
  (is (= 2 (arity 'impl)))
  (is (= -1 (arity 'and)))
  (is (= 3 (arity 'ite))))

(deftest unary?-test
  (is (= true (unary? 'not)))
  (is (= false (unary? 'and)))
  (is (= false (unary? '(and)))))

(deftest binary?-test
  (is (= false (binary? 'not)))
  (is (= false (binary? 'and)))
  (is (= true (binary? 'equiv))))

(deftest ternary?-test
  (is (= false (ternary? 'not)))
  (is (= false (ternary? 'and)))
  (is (= true (ternary? 'ite))))

(deftest nary?-test
  (is (= false (nary? 'not)))
  (is (= false (nary? 'xor)))
  (is (= true (nary? 'or))))
 
; Quantors ------------------------------------------------------------

(deftest quantor?-test
  (is (= false (quantor? 'not)))
  (is (= false (quantor? 'and)))
  (is (= false (quantor? '(and))))
  (is (= true (quantor? 'exists)))
  (is (= true (quantor? 'forall))))

; Equality ------------------------------------------------------------

(deftest eq?-test
  (is (= false (eq? 'not)))
  (is (= false (eq? '[xor])))
  (is (= true (eq? 'eq))))
