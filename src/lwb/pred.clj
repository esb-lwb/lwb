; lwb Logic WorkBench -- Predicate logic

; Copyright (c) 2015 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.pred
  (:refer-clojure :exclude [var?])
  (:require [lwb.prop :as prop]
            [potemkin :as pot]))


;; # The language of predicate logic

;; There are acutally many languages of predicate logic, which are
;; defined with respect to a given signature of the non-logical symbols.

;; ## Signatures

;; A signature for a langugae of predicate logic comprises the definition
;; of

;; * constants (which can be seen as functions with arity 0),
;; * functions together with their arity,
;; * propositions (which can be seen as predicates with arity 0)
;; * predicates with their arity

;; Signatures are represented in Clojure as maps.
;; The keys are keywords build from the names of the elements of the
;; signature,
;; the values are vectors with type and arity of the element

;; Example:
;; {:c [:const 0]
;;  :f [:func  2]
;;  :a [:prop  0]
;;  :p [:pred  1]}
;; defines a signature comprising the constant `:c`, the binary function `f`,
;; the proposition `a`, and the unary predicate `p`.

;; Constants are represented as keywords in Clojure. Thus it's not necessary
;; to declare them in the signature. The declaration in the signature is just
;; the documentation, but even an undeclared keyword ist recognized and accepted
;; in a formula.

;; ## Utility functions for signatures

(defn- sig-what
  [type symb sig]
  (and (symbol? symb) (= (first ((keyword symb) sig)) type)))

(defn const?
  "Is `keyword` a constant?"
  [kw]
  (keyword? kw))

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

(defn arity
  "Arity of operator `op` or of `symb` in signature `sig`"
  ([op]
   (prop/arity op))
  ([symb sig]
    (second ((keyword symb) sig))))

;; ## The logical symbols in the language(s) of predicate logic

;; We import the operators and so forth from propositional logic
(pot/import-vars
  [lwb.prop impl equiv xor ite
            op? torf?])

;; ## Quantors in predicate logic
(defn quantor?
  "Is `symb` a quantor?"
  [symb]
  (or (= 'forall symb) (= 'exists symb)))

;; ## Equality in predicate logic
(defn eq?
  "Is `symb` equality in predicate logic??"
  [symb]
  (= 'eq symb))

;; ## Variables
(defn var?
  "Is `symb` a variable with respect to signature `sig`?"
  [symb sig]
  (if (not (symbol? symb))
    false
    (not (or (op? symb) (torf? symb) (quantor? symb) (eq? symb)
             (func? symb sig) (pred? symb sig) (prop? symb sig)))))

;; ## Well-formed first-order formulae
;; The check whether a formula is well-formed reflects the grammer of
;; the language

(declare term?)

(defn compound-term?
  "Is `texpr` a compound term expression with respect to signatur `sig`?"
  [texpr sig]
  (cond
    (not (list? texpr)) (throw (IllegalStateException. (str "expected list, got " texpr)))
    (empty? texpr) (throw (IllegalStateException. "expected not empty list, got '()'."))
    (not (func? (first texpr) sig)) 
      (throw (IllegalStateException. (str "expected function, got " texpr)))
    :else (let [a (arity (first texpr) sig)] 
            (if (not= (count texpr) (inc a))
              (throw (IllegalStateException. (str "expected arity " a ", got " texpr)))
              (every? #(term? % sig) (rest texpr))))))

(defn simple-term?
  "Is `symb` a single term with respect to `sig`?"
  [symb sig]
  (if (not (symbol? symb))
    (const? symb)
    (var? symb sig)))

(defn term?
  "Is `texpr` a term?"
  [texpr sig]
  (or (simple-term? texpr sig) (compound-term? texpr sig)))

(defn predicate?
  "Is `phi` a predicate with respect to the signature `sig`"
  [phi sig]
  (if (or (not (list? phi)) (not (pred? (first phi) sig)))
    false
    (let [a (arity (first phi) sig)]
      (if (not= (count phi) (inc a))
        (throw (IllegalStateException. (str "expected arity " a ", got " phi)))
        (every? #(term? % sig) (rest phi))))))

(defn equality?
  "Is `phi` the equality predicate with respect to the signature `sig`"
  [phi sig]
  (if (or (not (list? phi)) (not (eq? (first phi))))
    false
    (if (not= (count phi) 3)
        (throw (IllegalStateException. (str "expected arity 2, got " phi)))
        (every? #(term? % sig) (rest phi)))))

(defn decl?
  "Is the given vector a vector of variables with respect to signature `sig`?"
  [decl sig]
  (if (and (vector? decl) (every? #(var? % sig) decl)) 
    true
    (throw (IllegalStateException. (str "expected vector of variables, got " decl)))))

(declare wff?)

(defn quantified?
  "Is `qexpr` a quantified first order formula with respect to `sig`?"
  [qexpr sig]
  (if (or (not (list? qexpr)) (not (quantor? (first qexpr))))
    false
    (let [decl (second qexpr)
          phi  (nth qexpr 2)]
      (if (not (and (decl? decl sig) (wff? phi sig)))
        (throw (IllegalStateException. (str "expected quantified formula, got " qexpr)))
        true))))

(defn simple-expr?
  [phi sig]
  (or (torf? phi) (prop? phi sig) (predicate? phi sig) (equality? phi sig)))

(defn op-expr?
  [phi sig]
  (if (not (op? (first phi)))
    false
    (let [a (arity (first phi))
          c (dec (count phi))]
      (if (and (not= a -1) (not= a c))
        (throw (IllegalStateException. (str "expected operator with arity " a ", got " phi)))
        (every? #(wff? % sig) (rest phi))))))
  
(defn compound-expr
  [phi sig]
  (cond
    (not (list? phi)) (throw (IllegalStateException. (str "expected list, got " phi)))
    (empty? phi) (throw (IllegalStateException. "expected not empty list, got '()'."))
    (not (or (op? (first phi)) (quantor? (first phi)))) (throw 
                                                 (IllegalStateException. (str "expected operator or quantor, got " phi)))
    :else (or (op-expr? phi sig) (quantified? phi sig))))

(defn wff?
  "Is the first order formula `phi` well-formed, with respect to signature `sig` ?
   `(fof-wff? phi sig)` returns true or false   
   `(fof-wff? phi sig :msg)` returns true or a message on the error in `phi`."
  ([phi sig]
   (wff? phi sig :bool))
  ([phi sig mode]
   (try
     (or (simple-expr? phi sig) (compound-expr phi sig))
     (catch Exception e (if (= mode :msg) (.getMessage e) false)))))


