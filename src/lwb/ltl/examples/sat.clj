; lwb Logic WorkBench -- Linear Temporal Logic: Examples Satisfiability of LTL

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.ltl.examples.sat
  (:require [lwb.ltl.sat :refer :all]
            [lwb.ltl.kripke :as k]))

; Theorems

(def ks1 (sat '(impl (always P) (finally P))))

(comment
  (k/texify ks1 "ks1")
  )

(sat? '(impl (always P) (finally P)))
(valid? '(impl (always P) (finally P)))

(def ks2 (sat '(impl (always P) (atnext P))))

(comment
  (k/texify ks2 "ks2")
  )

(sat? '(impl (always P) (atnext P)))
(valid? '(impl (always P) (atnext P)))

(def ks3 (sat '(impl (atnext P) (finally P))))

(comment
  (k/texify ks3 "ks3")
  )

(sat? '(impl (atnext P) (finally P)))
(valid? '(impl (atnext P) (finally P)))
