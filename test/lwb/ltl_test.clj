; lwb Logic WorkBench -- Linear Temporla Logic, tests

; Copyright (c) 2017 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.ltl-test
  (:require [clojure.test :refer :all]
            [lwb.ltl :refer :all]
            [lwb.ltl.kripke :as ks]
            [lwb.ltl.sat :refer [sat sat? valid?]]
            [lwb.ltl.eval :refer [eval-phi]]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]))


(stest/instrument)

; wff?  -----------------------------------------------------------------

(deftest wff?-test
  (is (= true (wff? 'true)))
  (is (= true (wff? 'x)))
  (is (= true (wff? '(and true))))
  (is (= true (wff? '(impl x y))))
  (is (= true (wff? '(ite x y z))))
  (is (= true (wff? '(ite (and x1 x2 x3 x4 x5) y z))))
  (is (= true (wff? '(or (and x1 x2 x3 x4 x5) y z))))
  (is (= true (wff? '(always (and P Q)))))
  (is (= true (wff? '(until R (and P Q)))))
  (is (= true (wff? '(atnext (and R (atnext P))))))
  (is (= true (wff? '(atnext (or R (finally P))))))
)

; manual test
(comment
  (wff? '(always P) :msg)
  (wff? '(always P Q) :msg)
  )

; nnf, nnf? -------------------------------------------------------------

(deftest nnf-test
  (is (= 'P (nnf 'P)))
  (is (= '(not P) (nnf '(not P))))
  (is (= 'P (nnf '(not (not P)))))
  (is (= '(or (not P) (not Q)) (nnf '(not (and P Q)))))
  (is (= '(and (not P) (not Q)) (nnf '(not (or P Q)))))
  (is (= '(atnext (not P)) (nnf '(not (atnext P)))))
  (is (= '(release (not P) (not Q)) (nnf '(not (until P Q)))))
  (is (= '(until (not P) (not Q)) (nnf '(not (release P Q)))))
  )

(deftest nnf-test'
  (is (= '(release false P) (nnf '(always P))))
  (is (= '(atnext P) (nnf '(atnext P))))
  (is (= '(atnext (not P)) (nnf '(not (atnext P)))))
  (is (= '(atnext P) (nnf '(atnext (not (not P))))))
  (is (= '(or (and (atnext P) Q) (and (atnext (not P)) R)) (nnf '(ite (atnext P) Q R))))
  (is (= '(until true P) (nnf '(finally P))))
  (is (= '(release (not true) (not P)) (nnf '(not (finally P)))))
  (is (thrown? Exception (nnf '(finally P Q))))
  )

(deftest nnf?-test
  (is (= false (nnf? '(finally P))))
  (is (= true (nnf? '(atnext P))))
  (is (= false (nnf? '(not (atnext P)))))
  (is (= true (nnf? '(atnext (not P)))))
  (is (= true (nnf? '(until (not P) Q))))
  (is (= true (nnf? '(release P Q))))
  (is (= false (nnf? '(not (release P Q)))))
  )

; model aka Kripke structure --------------------------------------------

(def ks1 {:atoms   '#{P Q}
          :nodes   {:s_1 '#{P Q}
                    :s_2 '#{P Q}
                    :s_3 '#{P}}
          :initial :s_1
          :edges   #{[:s_1 :s_2]
                     [:s_2 :s_1]
                     [:s_2 :s_3]
                     [:s_3 :s_3]}})

(def ks2 {:atoms   '#{P}
          :nodes   {:s_1 '#{}
                    :s_2 '#{P}
                    :s_3 '#{}}
          :initial :s_1
          :edges   #{[:s_1 :s_2]
                     [:s_1 :s_3]
                     [:s_2 :s_2]
                     [:s_3 :s_3]}})

(def ks2-x1 {:atoms   '#{Q}
          :nodes   {:s_1 '#{}
                    :s_2 '#{P}    ;; P is not an atom
                    :s_3 '#{}}
          :initial :s_1
          :edges   #{[:s_1 :s_2]
                     [:s_1 :s_3]
                     [:s_2 :s_2]
                     [:s_3 :s_3]}})
(def ks2-x2 {:atoms   '#{P}
             :nodes   {:s_1 '#{}
                       :s_2 '#{P} 
                       :s_3 '#{}}
             :initial :s_4          ;; inital is not a node
             :edges   #{[:s_1 :s_2]
                        [:s_1 :s_3]
                        [:s_2 :s_2]
                        [:s_3 :s_3]}})
(def ks2-x3 {:atoms   '#{P}
             :nodes   {:s_1 '#{}
                       :s_2 '#{P}
                       :s_3 '#{}}
             :initial :s_1       
             :edges   #{[:s_1 :s_4]  ;; :s_4 is not a node
                        [:s_1 :s_3]
                        [:s_2 :s_2]
                        [:s_3 :s_3]}})
(def ks2-x4 {:atoms   '#{P}
             :nodes   {:s_1 '#{}
                       :s_2 '#{P}
                       :s_3 '#{}}
             :initial :s_1
             :edges   #{[:s_1 :s_2] 
                        [:s_1 :s_3]   ;; there is no successor to :s_2
                        [:s_3 :s_3]}})
(deftest model-test
  (is (= true (s/valid? :lwb.ltl.kripke/model ks1)))
  (is (= true (s/valid? :lwb.ltl.kripke/model ks2)))
  (is (= false (s/valid? :lwb.ltl.kripke/model ks2-x1)))
  (is (= false (s/valid? :lwb.ltl.kripke/model ks2-x2)))
  (is (= false (s/valid? :lwb.ltl.kripke/model ks2-x3)))
  (is (= false (s/valid? :lwb.ltl.kripke/model ks2-x4)))
)

; sat sat?, valid? ------------------------------------------------------

; manual tests for sat
(comment 
  (ks/texify (sat '(atnext P)) "sat")
  (ks/texify (sat '(atnext (and P Q))) "sat")
  (ks/texify (sat '(atnext (or P Q))) "sat")
  (ks/texify (sat '(always P)) "sat")
  (ks/texify (sat '(always (finally P))) "sat")
  (ks/texify (sat '(finally (always P))) "sat")
  (ks/texify (sat '(atnext (and P (not P)))) "sat")
  )

(deftest sat?-test
  (is (= true (sat? '(atnext P))))
  (is (= true (sat? '(atnext (and P Q)))))
  (is (= true (sat? '(atnext (or P Q)))))
  (is (= true (sat? '(always P))))
  (is (= true (sat? '(always (finally P)))))
  (is (= true (sat? '(finally (always P)))))
  (is (= false (sat? '(atnext (and P (not P))))))
  )

(deftest valid?-test
  (is (= false (valid? '(atnext P))))
  (is (= true (valid? '(atnext (or P (not P))))))
  (is (= false (valid? '(atnext (and P Q)))))
  (is (= true (valid? '(impl (always P) (atnext P)))))
  (is (= true (valid? '(impl (always (and P Q)) (atnext (or P Q))))))
  (is (= false (valid? '(atnext (or P Q)))))
  (is (= true (valid? '(impl (always P) (finally P)))))
  (is (= true (valid? '(impl (finally P) (until true P)))))
  (is (= false (valid? '(finally (always P)))))
  (is (= true (valid? '(not (atnext (and P (not P)))))))
  )
  
(valid?-test)

; eval-phi    -----------------------------------------------------------

(def ksA {:atoms   '#{P Q}
          :nodes   {:s_1 '#{P Q}
                    :s_2 '#{P Q}
                    :s_3 '#{P}}
          :initial :s_1
          :edges   #{[:s_1 :s_2]
                     [:s_2 :s_1]
                     [:s_2 :s_3]
                     [:s_3 :s_3]}})

(def ksB {:atoms  '#{P}
          :nodes  {:s_0 '#{}
                   :s_1 '#{P}
                   :s_2 '#{}}
          :initial :s_0
          :edges  #{[:s_0 :s_1]
                    [:s_1 :s_1]
                    [:s_0 :s_2]
                    [:s_2 :s_2]}})

(deftest eval-phi-testA
  (is (= true (eval-phi '(always P) ksA)))
  (is (= true (eval-phi '(atnext (or P Q)) ksA)))
  (is (= true (eval-phi '(atnext (and P Q)) ksA)))
  (is (= true (eval-phi '(atnext (atnext (atnext P))) ksA)))
  (is (= false (eval-phi '(atnext (atnext (atnext Q))) ksA)))
  (is (= [:s_1 :s_2 :s_3 :s_3 :s_3] 
         (eval-phi '(atnext (atnext (atnext Q))) ksA :counterexample)))
  (is (= true (eval-phi '(always (impl (not Q) (always (and P (not Q))))) ksA)))
  )

(deftest eval-phi-testB
  (is (= false (eval-phi '(finally P) ksB)))
  (is (= false (eval-phi '(not (finally P)) ksB)))
  (is (= false (eval-phi '(atnext P) ksB)))
  (is (= true (eval-phi '(atnext (or P (not P))) ksB)))
  )

(run-tests)


