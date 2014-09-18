; lwb Logic WorkBench -- Propositional Logic, test of operators

; Copyright (c) 2014 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop.operator-test
  (:require [clojure.test :refer :all]
            [lwb.prop :refer :all]))

(deftest nand-test
    (is (= false (nand true true)))
    (is (= true (nand true false)))
    (is (= true (nand false true)))
    (is (= true (nand false false))))

(deftest nand-test'
  (is (= '(not (and a b)) (macroexpand '(nand a b)))))

(deftest nor-test
    (is (= false (nor true true)))
    (is (= false (nor true false)))
    (is (= false (nor false true)))
    (is (= true (nor false false))))
    
(deftest nor-test'
  (is (= '(not (or a b)) (macroexpand-1 '(nor a b)))))

(deftest impl-test
    (is (= true (impl true true)))
    (is (= false (impl true false)))
    (is (= true (impl false true)))
    (is (= true (impl false false))))

(deftest impl-test'
  (is (= '(or (not a) b) (macroexpand-1 '(impl a b)))))

(deftest nimpl-test
    (is (= false (nimpl true true)))
    (is (= true (nimpl true false)))
    (is (= false (nimpl false true)))
    (is (= false (nimpl false false))))

(deftest nimpl-test'
  (is (= '(not (or (not a) b))) (macroexpand-1 '(nimpl a b))))

(deftest cimpl-test
    (is (= true (cimpl true true)))
    (is (= true (cimpl true false)))
    (is (= false (cimpl false true)))
    (is (= true (cimpl false false))))

(deftest cimpl-test'
  (is (= '(or (not b) a) (macroexpand-1 '(cimpl a b)))))

(deftest ncimpl-test
    (is (= false (ncimpl true true)))
    (is (= false (ncimpl true false)))
    (is (= true (ncimpl false true)))
    (is (= false (ncimpl false false))))

(deftest ncimpl-test'
  (is (= '(not (cimpl a b)) (macroexpand-1 '(ncimpl a b)))))

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
