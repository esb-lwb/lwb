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
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/core.logic "0.8.11"]
                 [net.mikera/core.matrix "0.57.0"]
                 [org.clojure/tools.macro "0.1.5"]
                 [org.clojure/math.combinatorics "0.1.3"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [potemkin "0.4.3"]
                 [org.ow2.sat4j/org.ow2.sat4j.core "2.3.5"]
                 [de.fosd.typechef/javabdd_repackaged_2.10 "0.1"]
                 [ltl2buchi "1.0.0"]]
  :jvm-opts ["-Xms2G"]
  :uberjar-name "lwb.jar")
