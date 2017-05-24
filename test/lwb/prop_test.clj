; lwb Logic WorkBench -- Propositional Logic, tests

; Copyright (c) 2014 - 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop-test
  (:require [clojure.test :refer :all]
            [lwb.prop :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))


(stest/instrument)

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
  (is (= '(or (and i t) (and (not i) e)) (macroexpand-1 '(ite i t e)))))

; Utility functions with operators ------------------------------------

(deftest op?-test
  (is (= true (op? 'and)))
  (is (= true (op? 'or)))
  (is (= true (op? 'not)))
  (is (= true (op? 'impl)))
  (is (= true (op? 'equiv)))
  (is (= true (op? 'xor)))
  (is (= true (op? 'ite)))
  (is (= false (op? 'true)))
  (is (= false (op? 1)))
  (is (= false (op? [1])))
  (is (= false (op? '(x))))
  (is (= false (op? '#{x})))
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

(deftest nary?-test
  (is (= false (nary? 'not)))
  (is (= false (nary? 'x)))
  (is (= false (nary? 'impl)))
  (is (= true (nary? 'and)))
  (is (= false (nary? 'ite)))
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
  (is (= true (s/valid? :lwb.prop/model '{P true Q false})))
  (is (= true (s/valid? :lwb.prop/model {'P true 'Q (= 1 2)})))
  (is (= false (s/valid? :lwb.prop/model '{:P true Q false})))
  (is (= false (s/valid? :lwb.prop/model '{P true Q false R 2})))
)

; eval-phi    ---------------------------------------------------------

(deftest eval-phi-test
  (is (= true (eval-phi '(and P Q) '{P true Q true})))
  (is (= true (eval-phi '(and P Q R) '{P true Q true R true})))
  (is (= true (eval-phi '(ite P Q R) '{P true Q true R false})))
  (is (= true (eval-phi '(impl P Q) '{P false Q true})))
  (is (= false (eval-phi '(impl P Q) '{P true Q false})))
  (is (= false (eval-phi '(and P Q) '{P true Q false})))
)

; truth-table ---------------------------------------------------------

(deftest truth-table-valid?
  (is (= true (s/valid? :lwb.prop/truth-table
                        {:phi '(and p q),
                         :header ['p 'q :result],
                         :table [[true true true] [true false false] [false true false] [false false false]]})))
  (is (= true (s/valid? :lwb.prop/truth-table
                        {:phi '(or p q),
                         :header ['p 'q :result],
                         :table [[true true true] [true false true] [false true true] [false false false]]})))
  (is (= true (s/valid? :lwb.prop/truth-table
                        {:phi '(ite p q false),
                         :header ['p 'q :result],
                         :table [[true true true] [true false false] [false true false] [false false false]]})))
  )

(deftest truth-table-test
  (is (= (truth-table '(and p q))
         {:phi '(and p q),
          :header ['p 'q :result],
          :table [[true true true] [true false false] [false true false] [false false false]]}))
  (is (= (truth-table '(or p q))
         {:phi '(or p q),
          :header ['p 'q :result],
          :table [[true true true] [true false true] [false true true] [false false false]]}))
  (is (= (truth-table '(ite p q false))
         {:phi '(ite p q false),
          :header ['p 'q :result],
          :table [[true true true] [true false false] [false true false] [false false false]]}))
  )

(deftest truth-table-test'
  (is (thrown? Exception (truth-table '(P Q))))
  (is (thrown? Exception (truth-table '(P Q) :true)))
  )

; manual
(comment
  (ptt '(and P Q))
  (ptt '(or P Q))
  (ptt '(xor P Q))
  (ptt '(ite P Q R))
  (ptt '(xor P Q R)) ;=> spec failed
  )

(run-tests)


