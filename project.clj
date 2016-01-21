; lwb Logic WorkBench -- Project definition 

; Copyright (c) 2014 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(defproject lwb "1.0.0"
  :description "lwb Logic WorkBench"
  :url "http://homepages.thm.de/~hg11260/lwb.html"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.logic "0.8.10"]
                 [org.clojure/tools.macro "0.1.2"]
                 [org.clojure/math.combinatorics "0.1.1"]
                 [potemkin "0.3.13"]
                 [org.ow2.sat4j/org.ow2.sat4j.core "2.3.5"]]
  :plugins [[lein-marginalia "0.8.0"]])