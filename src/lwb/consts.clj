; lwb Logic WorkBench -- Constants

; Copyright (c) 2016 - 2019 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.consts
  (:require [clojure.edn :as edn]))

(def ^:const
  rev "2.2.0")

(def ^:const
  rev-date "2020-11-15")

(def ^:const
   welcome (str "This is lwb, Version " rev ", " rev-date))
