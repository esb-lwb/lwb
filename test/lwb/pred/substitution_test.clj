; lwb Logic WorkBench -- Predicate Logic, tests

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.pred.substitution-test
  (:require [clojure.test :refer :all]
            [lwb.pred.substitution :refer :all]))

(deftest vars-in-term-test
  (is (= #{} (vars-in-term :i)))
  (is (= #{} (vars-in-term '(f :i))))
  (is (= '#{x} (vars-in-term '(f x))))
  (is (= '#{x y} (vars-in-term '(f x y))))
  (is (= '#{x y} (vars-in-term '(f x y :e))))
  (is (= '#{x y} (vars-in-term '(f (g (h x y))))))
  (is (thrown? Exception (vars-in-term '(f = x y))))
  (is (thrown? Exception (vars-in-term '(and x y))))
  )

(deftest freefor?-test
  (is (= true (freefor? '(P x) 'x :t)))
  (is (= true
         (freefor? '(forall [x] (and (impl (P x) (Q x)) (S x y))) 'x '(f x y))))
  (is (= true
         (freefor? '(impl (forall [x] (and (P x) (Q x))) (or (not (P x)) (Q y))) 'x '(f x y))))
  (is (= false
         (freefor? '(and (S x) (forall [y] (impl (P x) (Q y)))) 'x '(f x y))))
  (is (= false
         (freefor? '(exists [z] (impl (R z y) (forall [x] (R x y)))) 'y 'x)))
  (is (= true
         (freefor? '(exists [z] (impl (R z y) (forall [x] (R x z)))) 'y 'x)))
  )

(deftest lwb.pred.substitution-test
  (is (= '(P :e) (substitution '(P x) 'x :e)))
  (is (= '(P (f x)) (substitution '(P x) 'x '(f x))))
  (is (= '(P (f (g x))) (substitution '(P x) 'x '(f (g x)))))
  (is (= '(and (P :e) (forall [x] (S x))) (substitution '(and (P x) (forall [x] (S x))) 'x :e)))
  (is (thrown? Exception (substitution '(exists [y] (R x y)) 'x 'y)))
  )

(run-tests)