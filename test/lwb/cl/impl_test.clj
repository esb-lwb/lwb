(ns lwb.cl.impl-test
  (:require [lwb.cl.impl :refer :all]
            [clojure.test :refer :all]))

;; Handling of parentheses ----------------------------------------------------

;; Logic relation for combinators ---------------------------------------------

;; Arity of combinators -------------------------------------------------------

(deftest arity-test
  (is (= 3 (arity '[S x y z])))
  (is (= 0 (arity '[x])))
  (is (= 1 (arity '[I x])))
  (is (= 2 (arity '[K x y]))))

;; Storage for combinators ----------------------------------------------------

;; Application of logic relation for one-step expansion or reduction ----------

(defn fixture 
  [f]
  (lwb.cl/def-combinators-ski)
  (f))

(use-fixtures :once fixture)

(run-tests)



