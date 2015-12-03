; lwb Logic WorkBench -- Propositional Logic, tests

; Copyright (c) 2014 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop-test
  (:require [clojure.test :refer :all]
            [lwb.prop :refer :all]))

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

; wff?  -----------------------------------------------------------------

(deftest wff?-test
  (is (= true (wff? 'true)))
  (is (= true (wff? 'x)))
  (is (= true (wff? '(and true))))
  (is (= true (wff? '(impl x y))))
  (is (= true (wff? '(ite x y z))))
  (is (= true (wff? '(ite (and x1 x2 x3 x4 x5) y z))))
  (is (= true (wff? '(or (and x1 x2 x3 x4 x5) y z)))))

(deftest wff?-test'
  (is (= false (wff? 1)))
  (is (= false (wff? '(x y))))
  (is (= false (wff? '(and and))))
  (is (= false (wff? '(impl x y z))))
  (is (= false (wff? '(ite x y))))
  (is (= false (wff? '(ite (and x1 x2 x3 x4 and) y z))))
  (is (= false (wff? '(or (and x1 x2 x3 x4 not) y z)))))

; cnf -----------------------------------------------------------------

(deftest literal?-test
  (is (= true (literal? 'true)))
  (is (= true (literal? 'p)))
  (is (= true (literal? '(not q))))
  (is (= false (literal? '(and p q))))
  (is (= false (literal? '(and)))))
  
      
(deftest impl-free-ops-test
  (is (= true (impl-free true)))
  (is (= false (impl-free false)))
  (is (= '(not a) (impl-free '(not a))))
  (is (= '(and a b) (impl-free '(and a b))))
  (is (= '(and a b c) (impl-free '(and a b c))))
  (is (= '(or a b) (impl-free '(or a b))))
  (is (= '(or a b c) (impl-free '(or a b c))))
  (is (= '(or (not a) b) (impl-free '(impl a b))))
  (is (= '(and (or (not a) b) (or (not b) a)) (impl-free '(equiv a b))))
  (is (= '(not (and (or (not a) b) (or (not b) a))) (impl-free '(xor a b))))
  (is (= '(or (and i t) (and (not i) e)) (impl-free '(ite i t e))))
 )   

(deftest impl-free-ops-test'
  (is (= '(not (and (or (not a) b) (or (not b) a))) (impl-free '(not (equiv a b)))))
  (is (= '(or (not (or (not (or (not p) q)) q)) q) (impl-free '(impl (impl (impl p q) q) q)))))

(deftest nnf-test
  (is (= 'a (nnf 'a)))
  (is (= 'true (nnf 'true)))
  (is (= '(not a) (nnf '(not a))))
  (is (= 'a (nnf '(not (not a)))))
  (is (= '(not a) (nnf '(not (not (not a))))))
  (is (= '(and a) (nnf '(and a))))
  (is (= '(and a b) (nnf '(and a b))))
  (is (= '(and a b c) (nnf '(and a b c))))
  (is (= '(or (not a) (not b)) (nnf '(not (and a b)))))
  (is (= '(or (not a) (not b) (not c)) (nnf '(not (and a b c)))))
  (is (= '(and (not a) (not b)) (nnf '(not (or a b)))))
  (is (= '(and (not a) (not b) (not c)) (nnf '(not (or a b c)))))
  )

(deftest cnf-test
  ; more precise: permutations of the clauses and the literals in the clauses are possible
  ; and allowed
  (is (= true (cnf '(and))))
  (is (= true (cnf '(and true))))
  (is (= true (cnf '(and true true))))
  (is (= false (cnf '(and false))))
  (is (= false (cnf '(and false true))))
  (is (= false (cnf '(or))))
  (is (= true (cnf '(or true))))
  (is (= false (cnf '(or false))))
  (is (= true (cnf '(or false true))))
  (is (= false (cnf '(or false false))))

  (is (= '(and (or p) (cnf '(or p)))))
  (is (= '(and (or p) (cnf '(and p)))))
  (is (= '(and (or a)) (cnf 'a)))
  (is (= '(and (or a) (or b) (or c)) (cnf '(and a b c))))
  (is (= '(and (or a c b)) (cnf '(or a (or b c)))))
  
  (is (= '(and (or p q) (or (not p) (not q)) (cnf '(not (equiv p q))))))
  (is (= '(and (or q (not p)) (cnf '(impl (impl (impl p q) q) q)))))
  
  (is (= '(and (or p)) (cnf '(ite true p false))))
  (is (= '(and (or p)) (cnf '(ite false true p))))
  (is (= '(and (or p) (or q (not p)) (or q)) (cnf '(ite p q false))))
)      

(run-tests)