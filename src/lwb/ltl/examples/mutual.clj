; lwb Logic WorkBench -- Linear Temporal Logic: Mutual Exclusion

; Copyright (c) 2020 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.ltl.examples.mutual
  (:require [lwb.ltl :refer :all]                           ; needed for macroexpand-1 of xor etc !!
            [lwb.ltl.eval :refer :all]
            [lwb.ltl.kripke :as ks]                         ; needed for instrument
            [clojure.spec.test.alpha :as stest]
            [clojure.spec.alpha :as s]))

(stest/instrument `eval-phi)

; Examples of algorithm for mutual exclusion based on a binary semaphore
; n = non-critical actions
; w = wait
; c = critical section
; s = semaphore s=1 true, s=0 false

(def ksx {:atoms   '#{n1 n2 w1 w2 c1 c2 s}
          :nodes   {:s1 '#{n1 n2 s}
                    :s2 '#{w1 n2 s}
                    :s3 '#{n1 w2 s}
                    :s4 '#{c1 n2}
                    :s5 '#{w1 w2 s}
                    :s6 '#{n1 c2}
                    :s7 '#{c1 w2}
                    :s8 '#{w1 c2}
                    }
          :initial :s1
          :edges   #{[:s1 :s2]
                     [:s1 :s3]
                     [:s2 :s4]
                     [:s2 :s5]
                     [:s3 :s5]
                     [:s3 :s6]
                     [:s4 :s1]
                     [:s4 :s7]
                     [:s5 :s7]
                     [:s5 :s8]
                     [:s6 :s1]
                     [:s6 :s8]
                     [:s7 :s3]
                     [:s8 :s2]
                     }})

(s/conform ::ks/model ksx)

(def phi1 '(always (or (not c1) (not c2))))
  
(eval-phi phi1 ksx)
;=> true

(def phi2 '(or (always (finally c1)) (always (finally c2))))

(eval-phi phi2 ksx)
;=> true

(def phi3 '(and (always (finally c1)) (always (finally c2))))

(eval-phi phi3 ksx)
;=> false
(eval-phi phi3 ksx :counterexample)
;=> [:s1 :s3 :s5 :s7 :s3 :s5 :s8 :s2 :s5]
;   now we get a cycle :s5 :s8 :s2 :s5 and 'c1 will never even be true
