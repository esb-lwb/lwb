; lwb Logic WorkBench -- Linear Temporal Logic

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.ltl
  (:require [lwb.prop :as prop]
            [potemkin :as pot]))

;; # Representation of formulae of ltl

;; The represeantation is like in propositonal logic

;; We have 4 more operators in ltl:

;; * always -- unary
;; * atnext -- unary
;; * finally -- unary
;; * until -- unary

;; We import the operators and so forth from propositional logic
(pot/import-vars
  [lwb.prop impl equiv xor ite atom? torf?])

(defn op?
  "Is `symb` an operator of ltl?"
  [symb]
  (let [operators #{'always 'atnext 'finally 'until}]
    (or (prop/op? symb) (contains? operators symb))))

(defn arity
  "Arity of operator `op`.
   -1 means n-ary, but better use `n-ary?`.
   requires: `op` an operator."
  [op]
  (if
    (prop/op? op) (prop/arity op)
                    (if (= 'until op) 2
                                      1)))

;; ## Is a formula well-formed?
(declare wff?)

(defn op-expr?
  [phi]
  (if (not (op? (first phi)))
    false
    (let [a (arity (first phi))
          c (dec (count phi))]
      (if (and (not= a -1) (not= a c))
        (throw (IllegalStateException. (str "expected operator with arity " a ", got " phi)))
        (every? wff? (rest phi))))))

(defn simple-expr?
  [phi]
  (or (torf? phi) (atom? phi)))

(defn compound-expr?
  [phi]
  (cond
    (not (list? phi)) (throw (IllegalStateException. (str "expected list, got " phi)))
    (empty? phi) (throw (IllegalStateException. "expected not empty list, got '()'."))
    (not (op? (first phi))) (throw (IllegalStateException. (str "expected operator, got " phi)))
    :else (op-expr? phi))) 

(defn wff?
  "Is the proposition `phi` well-formed ?
   `(wff? phi)` returns true or false   
   `(wff? phi :msg)` returns true or a message on the error in `phi`."
  ([phi]
   (wff? phi :bool))
  ([phi mode]
   (try
     (or (simple-expr? phi) (compound-expr? phi))
     (catch Exception e (if (= mode :msg) (.getMessage e) false)))))



