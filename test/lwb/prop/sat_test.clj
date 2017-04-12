; lwb Logic WorkBench -- Propositional Logic, tests

; Copyright (c) 2014 - 2017 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop.sat-test
  (:require [clojure.test :refer :all]
            [lwb.prop :refer :all]
            [lwb.prop.sat :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]))

(stest/instrument)

; dimacs --------------------------------------------------------------

(deftest cl->dimacs-test
  (is (= #{1 2} (#'lwb.prop.sat/cl->dimacs '(or P Q) '{P 1 Q 2})))
  (is (= #{-1 2} (#'lwb.prop.sat/cl->dimacs '(or (not P) Q) '{P 1 Q 2})))
  (is (= #{-1 -2} (#'lwb.prop.sat/cl->dimacs '(or (not P) (not Q)) '{P 1 Q 2})))
  )

(deftest cnf->dimacs-test
  (is (= true (s/valid? :lwb.prop.sat/dimacs (cnf->dimacs '(and (or P Q) (or (not Q) R))))))
  (is (= {:formula '(and (or P Q) (or (not Q) R)), :num-atoms 3, :int-atoms '{1 P, 2 Q, 3 R}, :num-cl 2, :cl-set #{#{1 2} #{-2 3}}}
         (cnf->dimacs '(and (or P Q) (or (not Q) R)))))
  (is (= true (s/valid? :lwb.prop.sat/dimacs (cnf->dimacs '(and (or P Q) (or (not Q) R) (or (not T) (not R) S))))))
  (is (= {:formula   '(and (or P Q) (or (not Q) R) (or (not T) (not R) S)),
          :num-atoms 5,
          :int-atoms '{1 P, 2 Q, 3 R, 4 S, 5 T},
          :num-cl    3,
          :cl-set    #{#{4 -3 -5} #{1 2} #{-2 3}}}
         (cnf->dimacs '(and (or P Q) (or (not Q) R) (or (not T) (not R) S)))))
  )

; sat4j ---------------------------------------------------------------

(deftest sat4j-solve-test
  (is (= true (s/valid? :lwb.prop/model (sat4j-solve (cnf->dimacs '(and (or P Q) (or (not Q) R)))))))
  (is (= true (s/valid? nil? (sat4j-solve (cnf->dimacs '(and (or P) (or (not P))))))))
  )

; tseitin -------------------------------------------------------------

(deftest tseitin-test
  (is (= (tseitin '(impl (and P Q) T))
         '(and
            (or ts1)
            (or T (not ts1) (not ts2))
            (or ts1 ts2)
            (or ts1 (not T))
            (or P (not ts2))
            (or Q (not ts2))
            (or ts2 (not Q) (not P)))))
  (is (= (tseitin '(or P1 (and P2 (impl P3 P4))))
         '(and
            (or ts1)
            (or P1 ts2 (not ts1))
            (or ts1 (not P1))
            (or ts1 (not ts2))
            (or P2 (not ts2))
            (or ts3 (not ts2))
            (or ts2 (not ts3) (not P2))
            (or P4 (not ts3) (not P3))
            (or ts3 P3)
            (or ts3 (not P4)))))
  )

; sat -----------------------------------------------------------------

(deftest sat-test
  (is (= (sat '(and P Q)) '{P true Q true}))
  (is (= (sat '(and P Q R S)) '{P true Q true R true S true}))
  (is (= (sat '(or P1 (and P2 (impl P3 P4)))) '{P1 false P2 true P3 false P4 false}))
  )

; sat? ----------------------------------------------------------------

(deftest sat?-test
  (is (= true (sat? '(and P Q))))
  (is (= true (sat? '(or P Q))))
  (is (= true (sat? '(impl P Q))))
  (is (= true (sat? '(equiv P Q))))
  (is (= true (sat? '(xor P Q))))
  (is (= true (sat? '(ite P Q R))))
  (is (= true (sat? '(or (ite P Q R) (impl P Q) (and R S)))))
  (is (= false (sat? '(and P Q (not P)))))
  )

; valid? --------------------------------------------------------------

(deftest valid?-test
  (is (= true (valid? '(or P (not P)))))
  (is (= false (valid? '(or P Q))))
  )

(run-tests)
