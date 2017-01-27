; lwb Logic WorkBench -- Propositional Logic, tests

; Copyright (c) 2014 - 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop.cardinality-test
  (:require [clojure.test :refer :all]
            [lwb.prop :refer :all]
            [lwb.prop.cardinality :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]))

(stest/instrument)

; skoll ---------------------------------------------------------------

(deftest acoll-test
  (is (= true (s/valid? :lwb.prop.cardinality/acoll '(P Q R))))
  (is (= true (s/valid? :lwb.prop.cardinality/acoll '[P Q R])))
  (is (= false (s/valid? :lwb.prop.cardinality/acoll '(P Q :R))))
  (is (= false (s/valid? :lwb.prop.cardinality/acoll '(P Q (and P Q)))))
  )


; min-kof -------------------------------------------------------------

(deftest min-kof-test
  (is (= (min-kof 1 '(P Q)) '((or P Q))))
  (is (= (min-kof 1 '(P Q R)) '((or P Q R))))
  (is (= (min-kof 2 '(P Q R)) '((or P Q) (or P R) (or Q R))))
  )

; max-kof -------------------------------------------------------------

(deftest max-kof-test
  (is (= (max-kof 1 '(P Q)) '((or (not P) (not Q)))))
  (is (= (max-kof 1 '(P Q R)) '((or (not P) (not Q)) (or (not P) (not R)) (or (not Q) (not R)))))
  (is (= (max-kof 2 '(P Q R)) '((or (not P) (not Q) (not R)))))
  )

; kof -----------------------------------------------------------------

(deftest kof-test
  (is (= (kof 1 '(P Q)) '((or P Q) (or (not P) (not Q)))))
  (is (= (kof 1 '(P Q R)) '((or P Q R) (or (not P) (not Q)) (or (not P) (not R)) (or (not Q) (not R)))))
  (is (= (kof 2 '(P Q R)) '((or P Q) (or P R) (or Q R) (or (not P) (not Q) (not R)))))
  )

(run-tests)
