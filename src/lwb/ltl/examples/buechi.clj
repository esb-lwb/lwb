; lwb Logic WorkBench -- Linear Temporal Logic: Examples of Büchi automata from LTL formulas

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.ltl.examples.buechi
  (:require [lwb.ltl.buechi :refer :all]
            [clojure.spec :as s]))

; The temporal operators

(def ba-always (ba '(always P)))
(s/valid? :lwb.ltl.buechi/ba ba-always)

ba-always

(def ba-atnext (ba '(atnext P)))
(s/valid? :lwb.ltl.buechi/ba ba-atnext)

ba-atnext

(def ba-finally (ba '(finally P)))
(s/valid? :lwb.ltl.buechi/ba ba-finally)

ba-finally

(def ba-until (ba '(until P Q)))
(s/valid? :lwb.ltl.buechi/ba ba-until)

ba-until

(def ba-release (ba '(release P Q)))
(s/valid? :lwb.ltl.buechi/ba ba-release)

ba-release

(def ba-infinitely-often (ba '(always (finally P))))

ba-infinitely-often

(def ba-aif (ba '(always (impl P (finally Q)))))

ba-aif

(def ba-finally-always (ba '(finally (always P))))

ba-finally-always

(def ba-trivial (ba '(always (or P (not P)))))

ba-trivial

(def ba-empty (ba '(always (and P (not P)))))

ba-empty
