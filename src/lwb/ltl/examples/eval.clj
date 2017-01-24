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

(comment
  (ks/texify ks1 "eval")
  )

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

; Example from lecture notes

(def ks2 {:atoms   '#{P Q}
          :nodes   {:s_1 '#{P Q}
                    :s_2 '#{P Q}
                    :s_3 '#{P}}
          :initial :s_1
          :edges   #{[:s_1 :s_2]
                     [:s_2 :s_1]
                     [:s_2 :s_3]
                     [:s_3 :s_3]}})

(comment
  (ks/texify ks2 "eval")
  )

(eval-phi '(always P) ks2)
; => true
(eval-phi '(atnext (or P Q)) ks2)
; => true
(eval-phi '(atnext (and P Q)) ks2)
; => true
(eval-phi '(atnext (atnext (atnext P))) ks2)
; => true
(eval-phi '(atnext (atnext (atnext Q))) ks2)
; => false
(eval-phi '(atnext (atnext (atnext Q))) ks2 :counterexample)
; => [:s_1 :s_2 :s_3 :s_3 :s_3]
(eval-phi '(always (impl (not Q) (always (and P (not Q))))) ks2)
; => true

(def ks3 {:atoms  '#{P}
          :nodes  {:s_0 '#{}
                   :s_1 '#{P}
                   :s_2 '#{}}
          :initial :s_0
          :edges  #{[:s_0 :s_1]
                    [:s_1 :s_1]
                    [:s_0 :s_2]
                    [:s_2 :s_2]}})

(comment
  (ks/texify ks3 "eval")
  )

(eval-phi '(finally P) ks3)
; => false
(eval-phi '(finally P) ks3 :counterexample)
; => [:s_0 :s_2 :s_2]

(eval-phi '(not (finally P)) ks3)
; => false
(eval-phi '(not (finally P)) ks3 :counterexample)
; => [:s_0 :s_1 :s_1 :s_1]

; Microwave oven example from Clarke et al Model Checking p.39
(def oven {:atoms  '#{Start Close Heat Error}
           :nodes  {:s_1 '#{}
                    :s_2 '#{Start Error}
                    :s_3 '#{Close}
                    :s_4 '#{Close Heat}
                    :s_5 '#{Start Close Error}
                    :s_6 '#{Start Close}
                    :s_7 '#{Start Close Heat}}
           :initial :s_1
           :edges  #{[:s_1 :s_2]
                     [:s_1 :s_3]
                     [:s_2 :s_5]
                     [:s_3 :s_1]
                     [:s_3 :s_6]
                     [:s_4 :s_1]
                     [:s_4 :s_3]
                     [:s_4 :s_4]
                     [:s_5 :s_2]
                     [:s_5 :s_3]
                     [:s_6 :s_7]
                     [:s_7 :s_4]}})

(comment
  (ks/texify oven "eval")
  ; not really well arranged!
  )

; p.45
; it's impossible for the oven to be hot with the door open
(eval-phi '(always (until (not Heat) Close)) oven)
; => true

; p.47
; Whenever an illegal sequence of steps occurs, the oven will never heat or will eventually reset
(eval-phi '(impl (always (and (not Close) Start)) (or (always (not Heat)) (finally (not Error)))) oven)
; => true

; Traffic lights
(def tl {:atoms   '#{Green Yellow Red}
         :nodes   {:s_1 '#{Red}
                   :s_2 '#{Green}
                   :s_3 '#{Yellow}}
         :initial :s_1
         :edges   #{[:s_1 :s_2]
                    [:s_2 :s_3]
                    [:s_3 :s_1]}})

(comment
  (ks/texify tl "eval")
  )

; The traffic light is infinitely often Green
(eval-phi '(always (finally Green)) tl)
; true

; Never the lamp is Green and Red
(eval-phi '(always (not (and Green Red))) tl) 
; true

; Finally the lamp is Green and Yellow
(eval-phi '(finally (and Green Yellow)) tl)
; => false
(eval-phi '(finally (and Green Yellow)) tl :counterexample)
; => [:s_1 :s_2 :s_3 :s_1]

; Alternating states
(def alt {:atoms  '#{P Q}
          :nodes  {:s_1 '#{P}
                   :s_2 '#{Q}}
          :initial :s_1
          :edges  #{[:s_1 :s_2]
                    [:s_2 :s_1]}})

(comment
  (ks/texify alt "eval")
  )

(eval-phi '(always (impl P (atnext Q))) alt)
; => true
(eval-phi '(always (not (and P Q))) alt)
; => true
(eval-phi '(always (finally P)) alt)
; => true

(def ks4 {:atoms  '#{P Q R}
          :nodes  {:s_1 '#{P}
                   :s_2 '#{Q}
                   :s_3 '#{R}}
          :initial :s_1
          :edges  #{[:s_1 :s_2]
                    [:s_1 :s_3]
                    [:s_2 :s_3]
                    [:s_2 :s_2]
                    [:s_3 :s_3]}})

(comment
  (ks/texify ks4 "eval")
  )

(eval-phi '(atnext P) ks4)
; => false
(eval-phi '(atnext Q) ks4)
; => false, since it must be true for all paths
(eval-phi '(atnext Q) ks4 :counterexample)
; => [:s_1 :s_3 :s_3]
(eval-phi '(atnext (or Q R)) ks4)
; => true
(eval-phi '(impl Q (atnext R)) ks4)
; => true
(eval-phi '(impl P (atnext (atnext (or Q R)))) ks4)
; => true
(eval-phi '(impl P (atnext (atnext R))) ks4)
; => false

