; lwb Logic WorkBench -- Propositional Logic, tests

; Copyright (c) 2014 - 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop.nf-test
  (:require [clojure.test :refer :all]
            [lwb.prop.nf :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]))

(stest/instrument)

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
  (is (= true (s/valid? :lwb.prop.nf/cnf '(and))))
  (is (= false (s/valid? :lwb.prop.nf/cnf '(or))))
  (is (= true (s/valid? :lwb.prop.nf/cnf '(and (or)))))
  (is (= false (s/valid? :lwb.prop.nf/cnf '(and (and)))))
  (is (= true (s/valid? :lwb.prop.nf/cnf '(and (or P Q R) (or (not P) T X Y Z)))))
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

  (is (= '(and (or p)) (cnf '(or p))))
  (is (= '(and (or p)) (cnf '(and p))))
  (is (= '(and (or a)) (cnf 'a)))
  (is (= '(and (or a) (or b) (or c)) (cnf '(and a b c))))
  (is (= '(and (or a c b)) (cnf '(or a (or b c)))))
  
  (is (= '(and (or p q) (or (not p) (not q))) (cnf '(not (equiv p q)))))
  (is (= '(and (or q (not p))) (cnf '(impl (impl (impl p q) q) q))))
  
  (is (= '(and (or p)) (cnf '(ite true p false))))
  (is (= '(and (or p)) (cnf '(ite false true p))))
  (is (= '(and (or p) (or q (not p)) (or q)) (cnf '(ite p q false))))
  
  (is (= true (s/valid? (s/spec (s/or :cnf :lwb.prop.nf/cnf :bool boolean?)) (cnf '(and)))))
  (is (= true (s/valid? (s/spec (s/or :cnf :lwb.prop.nf/cnf :bool boolean?)) (cnf '(or P Q R)))))
  (is (= true (s/valid? (s/spec (s/or :cnf :lwb.prop.nf/cnf :bool boolean?)) (cnf '(and (or P Q R) S)))))
  (is (= true (s/valid? (s/spec (s/or :cnf :lwb.prop.nf/cnf :bool boolean?)) (cnf '(and P Q R S)))))
  
)

; dnf -----------------------------------------------------------------
(deftest dnf?-test
  (is (= true (s/valid? :lwb.prop.nf/dnf '(or))))
  (is (= false (s/valid? :lwb.prop.nf/dnf '(and))))
  (is (= true (s/valid? :lwb.prop.nf/dnf '(or (and)))))
  (is (= false (s/valid? :lwb.prop.nf/dnf '(or (or)))))
  (is (= true (s/valid? :lwb.prop.nf/dnf '(or (and P Q R) (and (not P) T X Y Z)))))
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

  (is (= '(or (and p)) (dnf '(or p))))
  (is (= '(or (and p)) (dnf '(and p))))
  (is (= '(or (and a)) (dnf 'a)))
  (is (= '(or (and a c b)) (dnf '(and a b c))))
  (is (= '(or (and a) (and b) (and c)) (dnf '(or a (or b c)))))

  (is (= '(or (and (not q) p) (and (not p) q)) (dnf '(not (equiv p q)))))
  (is (= '(or (and (not p) (not q)) (and q)) (dnf '(impl (impl (impl p q) q) q))))

  (is (= '(or (and p)) (dnf '(ite true p false))))
  (is (= '(or (and p)) (dnf '(ite false true p))))
  (is (= '(or (and p q)) (dnf '(ite p q false))))

  (is (= true (s/valid? (s/spec (s/or :dnf :lwb.prop.nf/dnf :bool boolean?)) (dnf '(and)))))
  (is (= true (s/valid? (s/spec (s/or :dnf :lwb.prop.nf/dnf :bool boolean?)) (dnf '(and P Q R)))))
  (is (= true (s/valid? (s/spec (s/or :dnf :lwb.prop.nf/dnf :bool boolean?)) (dnf '(or (and P Q R) S)))))
  (is (= true (s/valid? (s/spec (s/or :dnf :lwb.prop.nf/dnf :bool boolean?)) (dnf '(and P Q R S)))))
  (is (= true (s/valid? (s/spec (s/or :dnf :lwb.prop.nf/dnf :bool boolean?)) (dnf '(or P Q R S)))))
  )

(run-tests)


