; lwb Logic WorkBench -- Propositional Logic, tests

; Copyright (c) 2014 - 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop-test
  (:require [clojure.test :refer :all]
            [lwb.prop :refer :all]
            [clojure.spec :as s]))

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
  (is (= false) (op? '#{x}))
)

(deftest atom?-test
  (is (= true (atom? 'x)))
  (is (= true (atom? 'hello)))
  (is (= false (atom? 'and)))
  (is (= false (atom? 1)))
  (is (= false (atom? '(and))))
)

(deftest arity-test
  (is (= 1 (arity 'not)))
  (is (= nil (arity 'x)))
  (is (= 2 (arity 'impl)))
  (is (= -1 (arity 'and)))
  (is (= 3 (arity 'ite)))
)

; wff?  -----------------------------------------------------------------

(deftest wff?-test
  (is (= true (wff? 'true)))
  (is (= true (wff? 'x)))
  (is (= true (wff? '(and true))))
  (is (= true (wff? '(impl x y))))
  (is (= true (wff? '(ite x y z))))
  (is (= true (wff? '(ite (and x1 x2 x3 x4 x5) y z))))
  (is (= true (wff? '(or (and x1 x2 x3 x4 x5) y z))))
)

(deftest wff?-test'
  (is (= false (wff? 1)))
  (is (= false (wff? '(x y))))
  (is (= false (wff? '(and and))))
  (is (= false (wff? '(impl x y z))))
  (is (= false (wff? '(ite x y))))
  (is (= false (wff? '(ite (and x1 x2 x3 x4 and) y z))))
  (is (= false (wff? '(or (and x1 x2 x3 x4 not) y z))))
)

; manual test
(comment
  (wff? '(and P Q) :msg)
  (wff? '(and P :Q) :msg)
  )

; model ---------------------------------------------------------------

(deftest model-test
  (is (= true (s/valid? :lwb.prop/model '[P true Q false])))
  (is (= true (s/valid? :lwb.prop/model ['P true 'Q (= 1 2)])))
  (is (= false (s/valid? :lwb.prop/model '[:P true Q false])))
  (is (= false (s/valid? :lwb.prop/model '[P true Q false R])))
  (is (= false (s/valid? :lwb.prop/model '[P true Q false R T])))
)

; eval-phi    ---------------------------------------------------------

(deftest eval-phi-test
  (is (= true (eval-phi '(and P Q) '[P true Q true])))
  (is (= true (eval-phi '(and P Q R) '[P true Q true R true])))
  (is (= true (eval-phi '(ite P Q R) '[P true Q true R false])))
  (is (= true (eval-phi '(impl P Q) '[P false Q true])))
  (is (= false (eval-phi '(impl P Q) '[P true Q false])))
  (is (= false (eval-phi '(and P Q) '[P true Q false])))
)

; truth-table ---------------------------------------------------------

(deftest truth-table-test
  (is (= (truth-table '(and p q))
         {:prop '(and p q),
          :header ['p 'q :result],
          :table [[true true true] [true false false] [false true false] [false false false]]}))
  (is (= (truth-table '(or p q))
         {:prop '(or p q),
          :header ['p 'q :result],
          :table [[true true true] [true false true] [false true true] [false false false]]}))
  (is (= (truth-table '(ite p q false))
         {:prop '(ite p q false),
          :header ['p 'q :result],
          :table [[true true true] [true false false] [false true false] [false false false]]}))
  )

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

(deftest cnf?-test
  (is (= true (s/valid? :lwb.prop/cnf '(and))))
  (is (= false (s/valid? :lwb.prop/cnf '(or))))
  (is (= true (s/valid? :lwb.prop/cnf '(and (or)))))
  (is (= false (s/valid? :lwb.prop/cnf '(and (and)))))
  (is (= true (s/valid? :lwb.prop/cnf '(and (or P Q R) (or (not P) T X Y Z)))))
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

; dnf -----------------------------------------------------------------
(deftest dnf?-test
  (is (= true (s/valid? :lwb.prop/dnf '(or))))
  (is (= false (s/valid? :lwb.prop/dnf '(and))))
  (is (= true (s/valid? :lwb.prop/dnf '(or (and)))))
  (is (= false (s/valid? :lwb.prop/dnf '(or (or)))))
  (is (= true (s/valid? :lwb.prop/dnf '(or (and P Q R) (and (not P) T X Y Z)))))
  )

(deftest dnf-test
  ; more precise: permutations of the clauses and the literals in the clauses are possible
  ; and allowed
  (is (= true (dnf '(and))))
  (is (= true (dnf '(and true))))
  (is (= true (dnf '(and true true))))
  (is (= false (dnf '(and false))))
  (is (= false (dnf '(and false true))))
  (is (= false (dnf '(or))))
  (is (= true (dnf '(or true))))
  (is (= false (dnf '(or false))))
  (is (= true (dnf '(or false true))))
  (is (= false (dnf '(or false false))))

  (is (= '(or (and p) (dnf '(or p)))))
  (is (= '(or (and p) (dnf '(and p)))))
  (is (= '(or (and a)) (dnf 'a)))
  (is (= '(or (and a c b)) (dnf '(and a b c))))
  (is (= '(or (and a) (and b) (and c)) (dnf '(or a (or b c)))))

  (is (= '(or (and (not q) p) (and (not p) q) (dnf '(not (equiv p q))))))
  (is (= '(or (and (not p) (not q) (and q)) (dnf '(impl (impl (impl p q) q) q)))))

  (is (= '(or (and p)) (dnf '(ite true p false))))
  (is (= '(or (and p)) (dnf '(ite false true p))))
  (is (= '(or (and p q)) (dnf '(ite p q false))))
  )

(run-tests)

(comment
  (print-truth-table (truth-table '(and P Q)))
  (print-truth-table (truth-table '(or P Q)))
  (print-truth-table (truth-table '(impl P Q)))
  (print-truth-table (truth-table '(equiv P Q)))
  (print-truth-table (truth-table '(xor P Q)))
  (print-truth-table (truth-table '(ite P Q R)))
  )