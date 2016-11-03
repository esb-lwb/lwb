; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 Tobias Völzel, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.prereqs
  (:require [clojure.zip :as zip]))

(defn substitution?
  "Is `phi'` the substitution of `var` in `phi`?"
  [phi' phi var term]
  true)