; lwb Logic WorkBench -- Predicate logic

; Copyright (c) 2014 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.pred
  (:require [lwb.lang :refer :all]))

;; ## Utility functions for signatures

(defn- sig-what
  [type symb sig]
  (and (symbol? symb) (= (first ((keyword symb) sig)) type)))

(defn const?
  "Is `symb` a constant in the signatur `sig`"
  [symb sig]
  (sig-what :const symb sig))

(defn func?
  "Is `symb` a function in the signatur `sig`"
  [symb sig]
  (sig-what :func symb sig))

(defn pred?
  "Is `symb` a predicate in the signatur `sig`"
  [symb sig]
  (sig-what :pred symb sig))

(defn prop?
  "Is `symb` an atomic proposition in the signatur `sig`"
  [symb sig]
  (sig-what :prop symb sig))

(defn sarity
  "Arity of `symb` in signature `sig`"
  [symb sig]
  (second ((keyword symb) sig)))

;; ## Well-formed first-order formulae
;; The check whether a formula is well-formed reflects the grammer of
;; the language

;; ### First-order formulae

(defn fol-var?
  "Is `symb` a variable with respect to signature `sig`?"
  [symb sig]
  (if (not (symbol? symb))
    false
    (not (or (op? symb) (torf? symb) (quantor? symb) (const? symb sig)
           (func? symb sig) (pred? symb sig) (prop? symb sig)))))

(declare fof-term?)

(defn fof-compound-term?
  "Is `texpr` a compound term expression with respect to signatur `sig`?"
  [texpr sig]
  (cond
    (not (list? texpr)) (throw (IllegalStateException. (str "expected list, got " texpr)))
    (empty? texpr) (throw (IllegalStateException. "expected not empty list, got '()'."))
    (not (func? (first texpr) sig)) 
      (throw (IllegalStateException. (str "expected function, got " texpr)))
    :else (let [a (sarity (first texpr) sig)] 
            (if (not= (count texpr) (inc a))
              (throw (IllegalStateException. (str "expected arity " a ", got " texpr)))
              (every? #(fof-term? % sig) (rest texpr))))))

(defn fof-single-term?
  "Is `symb` a single term with respect to `sig`?"
  [symb sig]
  (if (not (symbol? symb))
    false
    (or (fol-var? symb sig) (const? symb sig))))

(defn fof-term?
  "Is `texpr` a term?"
  [texpr sig]
  (or (fof-single-term? texpr sig) (fof-compound-term? texpr sig)))

(defn fof-pred?
  "Is `phi` a predicate with respect to the signature `sig`"
  [phi sig]
  (if (or (not (list? phi)) (not (pred? (first phi) sig)))
    false
    (let [a (sarity (first phi) sig)]
      (if (not= (count phi) (inc a))
        (throw (IllegalStateException. (str "expected arity " a ", got " phi)))
        (every? #(fof-term? % sig) (rest phi))))))

(defn fof-decl?
  "Is the given vector a vector of variables with respect to signature `sig`?"
  [decl sig]
  (if (and (vector? decl) (every? #(fol-var? % sig) decl)) 
    true
    (throw (IllegalStateException. (str "expected vector of variables, got " decl)))))

(declare fof-wff?)

(defn fof-qexpr?
  "Is `qexpr` a quantified first order formula with respect to `sig`?"
  [qexpr sig]
  (if (or (not (list? qexpr)) (not (quantor? (first qexpr))))
    false
    (let [decl (second qexpr)
          phi  (nth qexpr 2)]
      (if (not (and (fof-decl? decl sig) (fof-wff? phi sig)))
        (throw (IllegalStateException. (str "expected quantified formula, got " qexpr)))
        true))))

(def qexpr '(forall [x] (P x))))
(fof-decl? (second qexpr) s)
(fof-wff? (nth qexpr 2) s)
(fof-qexpr? '(forall [x] (P x)) s)

;; TODO analog predicates

(def s {:c [:const 0]
        :f [:func 2]
        :f3 [:func 1]
        :r [:prop 1]
        :P [:pred 1]
        :P2 [:pred 2]
        :f2 [:func 0]})

(const? 'c s)
(const? 'c1 s)
(func? 'c1 s)
(func? 'f s)
(pred? 'f s)
(pred? 'P s)
(prop? 'r s)
(fol-var? 'x s)
(fol-var? 't s)
(fol-var? 'P2 s)
(fof-pred? '(P x) s)
(fof-pred? '(P x y) s)
(fof-pred? '(P (f x x)) s)
(fof-pred? '(P (f c x)) s)
(fof-pred? '(P (r c x)) s)
(fof-decl? '[x y z] s)
(fof-decl? '[x y f] s)
(fof-decl? '[x y [x y]] s)
(fol-var? '[x y] s)

(declare fof-simple-expr? fof-compound-expr?)

(defn fof-wff?
  "Is the first order formula `phi` well-formed, with respect to signature `sig` ?
   `(fof-wff? phi sig)` returns true or false   
   `(fof-wff? phi sig :msg)` returns true or a message on the error in `phi`."
  ([phi sig]
   (fof-wff? phi sig :bool))
  ([phi sig mode]
   (try
     (or (fof-simple-expr? phi sig) (fof-compound-expr? phi sig))
     (catch Exception e (if (= mode :msg) (.getMessage e) false)))))

(defn fof-simple-expr?
  [phi sig]
  (or (torf? phi) (prop? phi sig) (fof-pred? phi sig)))

(fof-simple-expr? 'true s)
(fof-simple-expr? 'x s)
(prop? 'r s)
(fof-simple-expr? 'r s)
(torf? '(P x))
(prop? '(P x) s)
(fof-pred? '(P x) s)
(fof-simple-expr? '(P x) s)

(defn fof-unary-expr?
  [phi sig]
  (if (not (op-unary? (first phi)))
    false
    (if (not= 2 (count phi))
      (throw (IllegalStateException. (str "expected unary expression, got " phi)))
      (fof-wff? (second phi) sig))))

(defn fof-binary-expr?
  [phi sig]
  (if (not (op-binary? (first phi)))
    false
    (if (not= 3 (count phi))
      (throw (IllegalStateException. (str "expected binary expression, got " phi)))
      (every? #(fof-wff? % sig) (rest phi)))))

(defn fof-ternary-expr?
  [phi sig]
  (if (not (op-ternary? (first phi)))
    false
    (if (not= 4 (count phi))
      (throw (IllegalStateException. (str "expected ternary expression, got " phi)))
      (every? #(fof-wff? % sig) (rest phi)))))

(defn fof-nary-expr?
  [phi sig]
  (if (not (op-nary? (first phi)))
    false
    (every? #(fof-wff? % sig) (rest phi))))

(defn fof-compound-expr?
  [phi sig]
  (cond
    (not (list? phi)) (throw (IllegalStateException. (str "expected list, got " phi)))
    (empty? phi) (throw (IllegalStateException. "expected not empty list, got '()'."))
    (not (or (op? (first phi)) (quantor? (first phi)))) (throw 
                                                 (IllegalStateException. (str "expected operator or quantor, got " phi)))
    :else (or (fof-unary-expr? phi sig) (fof-binary-expr? phi sig) 
              (fof-ternary-expr? phi sig) (fof-nary-expr? phi sig)
              (fof-qexpr? phi sig))))

(fof-wff? '(and (P x) (P y)) s)
(fof-qexpr? '(forall [x y] (and (P x) (P y))) s)
(fof-compound-expr? '(forall [x y] (and (P x) (P y))) s)
(quantor? (first '(forall [x y] (and (P x) (P y)))))
(fof-wff? '(forall [x y] (and (P x) (P y))) s :msg)
(fof-wff? '(and r (forall [x y] (and (P x) (P y)))) s :msg)


