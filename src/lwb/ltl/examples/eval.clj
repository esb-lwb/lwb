; lwb Logic WorkBench -- Linear Temporal Logic: Examples Evaluation of LTL

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.ltl.examples.eval
  (:require [lwb.ltl :refer :all]         ; needed for macroexpand-1 of xor etc !!
            [lwb.ltl.eval :refer :all]
            [lwb.ltl.kripke :as ks]       ; needed for instrument
            [clojure.spec.test :as stest]))

(stest/instrument `eval-phi)

; Examples of Kripke structures

(def ks1 {:atoms   '#{P}
          :nodes   {:s_1 '#{P}}
          :initial :s_1
          :edges   #{[:s_1 :s_1]}})

(eval-phi '(always P) ks1)
; => true
(eval-phi 'P ks1)
; => true
(eval-phi '(atnext P) ks1)
; => true
(eval-phi '(finally P) ks1)
; => true
(eval-phi '(impl (always P) (finally P)) ks1)
; => true

(eval-phi '(atnext (not P)) ks1)
; => false
(eval-phi '(atnext (not P)) ks1 :counterexample)
; => [:s_1 :s_1 :s_1]
(eval-phi '(not (finally P)) ks1 :counterexample)
; => [:s_1 :s_1 :s_1]

(def ks2 {:atoms   '#{P Q}
          :nodes   {:s_1 '#{P Q}
                    :s_2 '#{P Q}
                    :s_3 '#{P}}
          :initial :s_1
          :edges   #{[:s_1 :s_2]
                     [:s_2 :s_1]
                     [:s_2 :s_3]
                     [:s_3 :s_3]}})


