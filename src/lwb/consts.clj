; lwb Logic WorkBench -- Constants

; Copyright (c) 2016 - 2018 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.consts
  (:require [clojure.edn :as edn]))

; version and date from project file
(defmacro proj-version []
  (some-> "project.clj" slurp edn/read-string (nth 2)))

(defmacro proj-date []
  (some-> "project.clj" slurp edn/read-string (nth 4)))

(def ^:const
  rev (proj-version))

(def ^:const
  rev-date (proj-date))

(def ^:const
   welcome (str "This is lwb, Version " rev ", " rev-date))
