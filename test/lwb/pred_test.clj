; lwb Logic WorkBench -- Predicate Logic, tests

; Copyright (c) 2014 - 2018 Burkhardt Renz, THM. All rights reserved.
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

(def sig1 {'f0 [:func 0]
           'f1 [:func 1]
           'f2 [:func 2]
           'f3 [:func 3]
           'R  [:pred 0]
           'P1 [:pred 1]
           'P2 [:pred 2]})

; tests -----------------------------------------------------------------------

(deftest sig-test
  (is (= true (s/valid? :lwb.pred/signature sig1)))
  (is (= false (s/valid? :lwb.pred/signature (conj sig1 [:x [:hi 1]]))))
  (is (= true  (const? :c)))
  (is (= true  (const? :d)))
  (is (= true  (const? :e)))
  (is (= false (const? 'f1)))
  (is (= true  (func?  'f1 sig1)))
  (is (= false (func?  'P1 sig1)))
  (is (= true  (prop?  'R  sig1)))
  (is (= false (prop?  'D  sig1)))
  (is (= true  (pred?  'P1 sig1)))
  (is (= true  (pred?  'P2 sig1)))
  (is (= false (pred?  'f3 sig1))))

(deftest symb-test
  (is (= true  (op? 'and)))
  (is (= true  (boolean? 'true)))
  (is (= true  (quantor? 'forall)))
  (is (= false (quantor? 'all)))
  (is (= true  (eq?  '=)))
  (is (= true (logvar? 'x sig1)))
  (is (= true (logvar? 'y sig1)))
  (is (= false (logvar? :c sig1)))
  (is (= true  (prop?  'R  sig1)))
  (is (= false (pred?  'f3 sig1))))

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
  (is (= true (term? 'x sig1)))
  (is (= true (term? 'f0 sig1)))
  (is (= true (term? '(f1 y) sig1)))
  (is (= true (term? '(f1 :c) sig1)))
  (is (= true (term? '(f3 x y z) sig1)))
  (is (= true (term? '(f3 (f1 x) (f2 y1 y2) z) sig1)))
  (is (= false (term? 'R sig1))))

(deftest wff?-test
  (is (= true (wff? '(forall [x y] (P2 x y)) sig1)))
  (is (= true (wff? '(exists [x y] (and (P1 x) (P1 y))) sig1)))
  (is (= true (wff? '(exists [x y] (and (P1 x) (= x y))) sig1)))
  (is (= true (wff? '(forall [x] (exists [y] (P2 x y))) sig1)))
  (is (= true (wff? '(P2 x y) sig1)))
  (is (= true (wff? '(ite (P2 x y) R (= :c :d)) sig1))))

(deftest spec-test
  (is (= true (binding [*signature* sig1] (s/valid? :lwb.pred/fml '(forall [x y] (P2 x y))))))
  (is (= true (binding [*signature* sig1] (s/valid? :lwb.pred/fml '(= 5 (f2 2 3))))))
  )

; models -----------------------------------------------------------------------

; example for a model 
(def m-test
   {:univ #{:0 :1 :2}
    'op   [:func 2 (fn [x y] (+ x y))]
    'inv  [:func 1 (fn [x] (- x))]
    'unit [:func 0 :0]
    'R    [:pred 2 (make-pred #{[:1 :1] [:0 :0] [:2 :2]})]
    'S    [:pred 3 (make-pred #{[:1 :1 :1] [:2 :2 :2]})]
    'P    [:pred 0 true]})
; just for the tests

(deftest model-test
  (is (= true (s/valid? :lwb.pred/model m-test)))
  (is (= 5 ((nth ('op m-test) 2) 2 3)))
  #_(is (= ([:func 2], [:func 1], [:func 0], [:pred 2], [:pred 3], [:pred 0])
         (vals (sig-from-model m))))
  )
(comment
  (sig-from-model m)
  (vals (sig-from-model m))
  )

; evaluation -------------------------------------------------------------------

(deftest eval-test
  (is (= '(forall [x] (forall [y] (= (op x y) ((op y x))))) (#'lwb.pred/unfold-vars '(forall [x y] (= (op x y) ((op y x)))))))
  (is (= '(forall [x] (forall [y] (= (op x y) ((op y x))))) (#'lwb.pred/unfold-vars '(forall [x y] (= (op x y) ((op y x)))))))
  (is (= true (eval-phi 'P m-test)))
  (is (= false (eval-phi '(and P (not P)) m-test)))
  (is (= true (eval-phi '(= 5 (op 2 3)) m-test)))
  (is (= true (eval-phi '(= 2 (inv -2)) m-test)))
  (is (= true (eval-phi '(forall [x y] (impl (= x y) (R x y))) m-test)))
  (is (= true (eval-phi '(exists [x] (R x x)) m-test)))
  (is (= false (eval-phi '(exists [x y] (and (not (= x y)) (R x y))) m-test)))
  )

(run-tests)

