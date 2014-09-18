; lwb Logic WorkBench -- Propositional Logic, test of transformation to cnf

; Copyright (c) 2014 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop.cnf-test
  (:require [clojure.test :refer :all]
            [lwb.prop :refer :all]))

(deftest impl-free-ops-test
  (is (= true (impl-free true)))
  (is (= false (impl-free false)))
  (is (= '(not a) (impl-free '(not a))))
  (is (= '(and a b) (impl-free '(and a b))))
  (is (= '(and a b c) (impl-free '(and a b c))))
  (is (= '(or a b) (impl-free '(or a b))))
  (is (= '(or a b c) (impl-free '(or a b c))))
  (is (= '(not (and a b)) (impl-free '(nand a b))))
  (is (= '(not (or a b)) (impl-free '(nor a b))))
  (is (= '(or (not a) b) (impl-free '(impl a b))))
  (is (= '(not (or (not a) b)) (impl-free '(nimpl a b))))
  (is (= '(or (not b) a) (impl-free '(cimpl a b))))
  (is (= '(not (or (not b) a)) (impl-free '(ncimpl a b))))
  (is (= '(and (or (not a) b) (or (not b) a)) (impl-free '(equiv a b))))
  (is (= '(not (and (or (not a) b) (or (not b) a))) (impl-free '(xor a b))))
  (is (= '(or (and i t) (and (not i) e)) (impl-free '(ite i t e))))
 )   

(deftest impl-free-ops-test'
  (is (= '(not (and (or (not a) b) (or (not b) a))) (impl-free '(not (equiv a b)))))
  (is (= '(or (not (or (not (or (not p) q)) q)) q) (impl-free '(impl (impl (impl p q) q) q)))))
  
)
