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
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/math.combinatorics "0.0.8"]]
  :plugins [[lein-marginalia "0.8.0"]])