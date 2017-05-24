; lwb Logic WorkBench -- Predicate Logic, tests

; Copyright (c) 2014 - 2017 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.pred-test
  (:require [clojure.test :refer :all]
            [lwb.pred :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))

(stest/instrument)

; signature ------------------------------------------------------------------

(def sig {:c  [:const 0]
          :d  [:const 0]
          :f0 [:func 0]
          :f1 [:func 1]
          :f2 [:func 2]
          :f3 [:func 3]
          :r  [:prop 0]
          :P1 [:pred 1]
          :P2 [:pred 2]})

; tests -----------------------------------------------------------------------

(deftest sig-test
  (is (= true (s/valid? :lwb.pred/signature sig)))
  (is (= false (s/valid? :lwb.pred/signature (conj sig [:x [:hi 1]]))))
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
  (is (= true  (boolean? 'true)))
  (is (= true  (quantor? 'forall)))
  (is (= false (quantor? 'all)))
  (is (= true  (eq?  '=)))
  (is (= true (logvar? 'x sig)))
  (is (= true (logvar? 'y sig)))
  (is (= false (logvar? :c sig)))
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
  (is (= true (eq? '=))))

(deftest term?-test
  (is (= true (term? 'x sig)))
  (is (= true (term? 'f0 sig)))
  (is (= true (term? '(f1 y) sig)))
  (is (= true (term? '(f1 :c) sig)))
  (is (= true (term? '(f3 x y z) sig)))
  (is (= true (term? '(f3 (f1 x) (f2 y1 y2) z) sig)))
  (is (= false (term? 'r sig))))

(deftest wff?-test
  (is (= true (wff? '(forall [x y] (P2 x y)) sig)))
  (is (= true (wff? '(exists [x y] (and (P1 x) (P1 y))) sig)))
  (is (= true (wff? '(exists [x y] (and (P1 x) (= x y))) sig)))
  (is (= true (wff? '(forall [x] (exists [y] (P2 x y))) sig)))
  (is (= true (wff? '(P2 x y) sig)))
  (is (= true (wff? '(ite (P2 x y) r (= :c :d)) sig))))

(deftest spec-test
  (is (= true (binding [*signature* sig] (s/valid? :lwb.pred/fml '(forall [x y] (P2 x y))))))
  (is (= true (binding [*signature* sig] (s/valid? :lwb.pred/fml '(= 5 (f2 2 3))))))
  )

; models -----------------------------------------------------------------------

; example for a model 
(def m
   {:univ #{:0 :1}
    :op   [:func 2 (fn [x y] (+ x y))]
    :inv  [:func 1 (fn [x] (- x))]
    :unit [:func 0 :0]
    :R    [:pred 2 (make-pred #{[:1 :1] [:0 :0]})]
    :S    [:pred 3 (make-pred #{[:1 :1 :1] [:2 :2 :2]})]
    :P    [:prop 0 'true]})
; just for the tests

(deftest model-test
  (is (= true (s/valid? :lwb.pred/model m)))
  (is (= 5 ((nth (:op m) 2) 2 3)))
  (is (= {:op [:func 2], :inv [:func 1], :unit [:func 0], :R [:pred 2], :S [:pred 3], :P [:prop 0]}
         (sig-from-model m)))
  )

; evaluation -------------------------------------------------------------------

(deftest eval-test
  (is (= '(forall [x] (forall [y] (= (op x y) ((op y x))))) (#'lwb.pred/unfold-vars '(forall [x y] (= (op x y) ((op y x)))))))
  (is (= '(forall [x] (forall [y] (= (op x y) ((op y x))))) (#'lwb.pred/unfold-vars '(forall [x y] (= (op x y) ((op y x)))))))
  (is (= true (eval-phi 'P m)))
  (is (= false (eval-phi '(and P (not P)) m)))
  (is (= true (eval-phi '(= 5 (op 2 3)) m)))
  (is (= true (eval-phi '(= 2 (inv -2)) m)))
  (is (= true (eval-phi '(forall [x y] (impl (= x y) (R x y))) m)))
  (is (= true (eval-phi '(exists [x] (R x x)) m)))
  (is (= false (eval-phi '(exists [x y] (and (not (= x y)) (R x y))) m)))
  )

(run-tests)