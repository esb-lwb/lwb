; lwb Logic WorkBench -- Linear Temporal Logic: Examples of Kripke Structures

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.ltl.examples.kripke
  (:require [lwb.ltl.kripke :refer :all]
            [clojure.spec :as s]))

;;Two simple examples

(def ks1 {:nodes   {:s_1 '#{P Q}
                    :s_2 '#{P Q}
                    :s_3 '#{P}}
          :initial :s_1
          :edges   #{[:s_1 :s_2]
                     [:s_2 :s_1]
                     [:s_2 :s_3]
                     [:s_3 :s_3]}})

(def ks2 {:nodes   {:s_1 '#{}
                    :s_2 '#{P}
                    :s_3 '#{}}
          :initial :s_1
          :edges   #{[:s_1 :s_2]
                     [:s_1 :s_3]
                     [:s_2 :s_2]
                     [:s_3 :s_3]}})

;; valid?
(s/valid? :lwb.ltl.kripke/model ks1)
(s/valid? :lwb.ltl.kripke/model ks2)

;; visualisation 
(dotify ks1 :dot)
(tikzify ks1)

(comment
  (texify ks1 "ks1" :dot)
  (texify ks2 "ks2")
  )
