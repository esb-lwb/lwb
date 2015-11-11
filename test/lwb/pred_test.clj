; lwb Logic WorkBench -- Predicate Logic, tests

; Copyright (c) 2014 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.pred-test
  (:refer-clojure :exclude [var?])
  (:require [clojure.test :refer :all]
            [lwb.pred :refer :all]))

; signature ------------------------------------------------------------------

(def sig {:c  [:const 0]
          :d  [:const 0]
          :f1 [:func 1]
          :f2 [:func 2]
          :f3 [:func 3]
          :r  [:prop 0]
          :P1 [:pred 1]
          :P2 [:pred 2]})

(deftest sig-test
  (is (= true  (const? :c)))
  (is (= true  (const? :d)))
  (is (= true  (const? :e)))
  (is (= false (const? 'f1)))
  (is (= true  (func?  'f1 sig)))
  (is (= false (func?  'P1 sig)))
  (is (= true  (prop?  'r  sig)))
  (is (= false (prop?  'd  sig)))
  (is (= true  (pred?  'P1 sig)))
  (is (= true  (pred?  'P2 sig)))
  (is (= false (pred?  'f3 sig))))

(deftest symb-test
  (is (= true  (op? 'and)))
  (is (= true  (torf? 'true)))
  (is (= true  (quantor? 'forall)))
  (is (= false (quantor? 'all)))
  (is (= true  (eq?  'eq)))
  (is (= false (eq?  '=)))
  (is (= true  (var?  'x sig)))
  (is (= true  (var?  'y sig)))
  (is (= false (var?  :c sig)))
  (is (= true  (prop?  'r  sig)))
  (is (= false (pred?  'f3 sig))))

(deftest op-test
  (is (= '(or (not a) b) (macroexpand-1 '(impl a b)))))

(deftest quantor?-test
  (is (= false (quantor? 'not)))
  (is (= false (quantor? 'and)))
  (is (= false (quantor? '(and))))
  (is (= true (quantor? 'exists)))
  (is (= true (quantor? 'forall))))

(deftest eq?-test
  (is (= false (eq? 'not)))
  (is (= false (eq? '[xor])))
  (is (= true (eq? 'eq))))

(deftest term-test
  (is (= true (term? 'x sig)))
  (is (= true (term? '(f1 y) sig)))
  (is (= true (term? '(f1 :c) sig)))
  (is (= true (term? '(f3 x y z) sig)))
  (is (= true (term? '(f3 (f1 x) (f2 y1 y2) z) sig)))
  (is (thrown? IllegalStateException (term? 'r sig))))

(deftest predicate-test
  (is (= true (predicate? '(P2 x y) sig)))
  (is (= true (predicate? '(P2 x (f1 y)) sig)))
  (is (= true (predicate? '(P1 (f1 :c)) sig)))
  (is (= false (predicate? '((f3 x y z) x) sig)))
  (is (thrown? IllegalStateException (predicate? '(P2 x x x) sig))))

(deftest equality-test
  (is (= true (equality? '(eq x y) sig)))
  (is (= true (equality? '(eq x (f1 y)) sig)))
  (is (= true (equality? '(eq (f1 :c) :d) sig)))
  (is (= true (equality? '(eq (f3 x y z) x) sig)))
  (is (thrown? IllegalStateException (equality? '(eq r x) sig))))

(deftest wff-test
  (is (= true (wff? '(forall [x y] (P2 x y)) sig)))
  (is (= true (wff? '(exists [x y] (and (P1 x) (P1 y))) sig)))
  (is (= true (wff? '(exists [x y] (and (P1 x) (eq x y))) sig)))
  (is (= true (wff? '(forall [x] (exists [y] (P2 x y))) sig)))
  (is (= true (wff? '(P2 x y) sig)))
  (is (= true (wff? '(ite (P2 x y) r (eq :c :d)) sig))))
