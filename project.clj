; lwb Logic WorkBench -- Project definition 

; Copyright (c) 2014 - 2018 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.


; Remember: update rev and date in consts.clj too 
(defproject lwb "1.0.4"
  :date "2018-06-14"
  :description "lwb Logic WorkBench"
  :url "http://homepages.thm.de/~hg11260/lwb.html"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/spec.alpha "0.1.143"]
                 [org.clojure/core.specs.alpha "0.1.24"]
                 [org.clojure/core.logic "0.8.11"]
                 [net.mikera/core.matrix "0.62.0"]
                 [org.clojure/tools.macro "0.1.5"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [potemkin "0.4.5"]
                 [org.ow2.sat4j/org.ow2.sat4j.core "2.3.5"]
                 [de.fosd.typechef/javabdd_repackaged_2.10 "0.1"]
                 [ltl2buchi/ltl2buchi "1.0.0"]
                 [kodkod/kodkod "2.1.0"]]
  :jvm-opts ["-Xms2G"]
  :uberjar-name "lwb.jar")

; ltl2buchi is not available as a maven jar
; compile github/esb-dev/ltl2buchi and put the ltl2buchi.jar into your local maven repo
; lein localrepo install ltl2buchi.jar ltl2buchi 1.0.0

; kodkod is not available as a maven jar
; download kodkod.jar from http://emina.github.io/kodkod/
; lein localrepo install kodkod.jar kodkod 2.1.0
