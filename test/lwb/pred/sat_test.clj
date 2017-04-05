; lwb Logic WorkBench -- Predicate Logic, tests

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.pred.sat-test
  (:require [clojure.test :refer :all]
            [lwb.pred.sat :refer :all]
            [clojure.spec.test :as stest]))
  
(stest/instrument)  

(deftest sat-intern-test1
  (is (= (#'lwb.pred.sat/sat-intern 'R  {:R [:prop 0]} #{:e1 :e2} :one) {:R #{[:e1]}} ))
  (is (= (#'lwb.pred.sat/sat-intern '(not R)  {:R [:prop 0]} #{:e1 :e2} :one) {:R #{}} ))
  (is (= (#'lwb.pred.sat/sat-intern '(and R Q S) {:R [:prop 0] :Q [:prop 0] :S [:prop 0]} #{:e1 :e2} :one)
         {:R #{[:e1]}, :Q #{[:e1]}, :S #{[:e1]}}))
  (is (= (#'lwb.pred.sat/sat-intern '(and R Q (not S)) {:R [:prop 0] :Q [:prop 0] :S [:prop 0]} #{:e1 :e2} :one)
         {:R #{[:e1]}, :Q #{[:e1]}, :S #{}}))
  (is (= (#'lwb.pred.sat/sat-intern '(or R Q S) {:R [:prop 0] :Q [:prop 0] :S [:prop 0]} #{:e1 :e2} :all)
         #{{:R #{[:e1]}, :Q #{}, :S #{[:e1]}}
           {:R #{}, :Q #{[:e1]}, :S #{}}
           {:R #{[:e1]}, :Q #{[:e1]}, :S #{[:e1]}}
           {:R #{[:e1]}, :Q #{}, :S #{}}
           {:R #{}, :Q #{}, :S #{[:e1]}}
           {:R #{}, :Q #{[:e1]}, :S #{[:e1]}}
           {:R #{[:e1]}, :Q #{[:e1]}, :S #{}}} ))
  (is (= (#'lwb.pred.sat/sat-intern '(impl R Q) {:R [:prop 0] :Q [:prop 0]} #{:e1 :e2} :all)
         #{{:R #{[:e1]}, :Q #{[:e1]}} {:R #{}, :Q #{}} {:R #{}, :Q #{[:e1]}}} ))
  (is (= (#'lwb.pred.sat/sat-intern '(equiv R Q) {:R [:prop 0] :Q [:prop 0]} #{:e1 :e2} :all)
         #{{:R #{[:e1]}, :Q #{[:e1]}} {:R #{}, :Q #{}}}))
  (is (= (#'lwb.pred.sat/sat-intern '(xor R Q) {:R [:prop 0] :Q [:prop 0]} #{:e1 :e2} :all)
         #{{:R #{[:e1]}, :Q #{}} {:R #{}, :Q #{[:e1]}}}))
  (is (= (#'lwb.pred.sat/sat-intern '(and R (ite R Q S)) {:R [:prop 0] :Q [:prop 0] :S [:prop 0]} #{:e1 :e2} :one)
         {:R #{[:e1]}, :Q #{[:e1]}, :S #{}} ))
  (is (= (#'lwb.pred.sat/sat-intern '(and (not R) (ite R Q S)) {:R [:prop 0] :Q [:prop 0] :S [:prop 0]} #{:e1 :e2} :one)
         {:R #{}, :Q #{}, :S #{[:e1]}})) 
  )

(deftest sat-intern-test2
  (is (= (#'lwb.pred.sat/sat-intern '(forall [x] (= x (f x))) {:f [:func 1]} #{:e1 :e2} :one) {:f #{[:e2 :e2] [:e1 :e1]}} ))
  (is (= (#'lwb.pred.sat/sat-intern '(exists [x y] (and (not (= x y)) (= x (f x)) (= y (f y)))) {:f [:func 1]} #{:e1 :e2} :all)
         #{{:f #{[:e2 :e2] [:e1 :e1]}}}))
  (is (= (#'lwb.pred.sat/sat-intern '(forall [x] (R x)) {:R [:pred 1]} #{:e1 :e2 :e3} :one)
         {:R #{[:e3] [:e1] [:e2]}}))
  (is (= (#'lwb.pred.sat/sat-intern '(and (forall [x] (R x)) (exists [y] (Q y))) {:R [:pred 1] :Q [:pred 1]} #{:e1 :e2 :e3} :all)
         #{{:R #{[:e3] [:e1] [:e2]}, :Q #{[:e3] [:e1]}}
           {:R #{[:e3] [:e1] [:e2]}, :Q #{[:e1]}}
           {:R #{[:e3] [:e1] [:e2]}, :Q #{[:e3] [:e2]}}
           {:R #{[:e3] [:e1] [:e2]}, :Q #{[:e3]}}
           {:R #{[:e3] [:e1] [:e2]}, :Q #{[:e1] [:e2]}}
           {:R #{[:e3] [:e1] [:e2]}, :Q #{[:e2]}}
           {:R #{[:e3] [:e1] [:e2]}, :Q #{[:e3] [:e1] [:e2]}}}))
  )

(run-tests)