; lwb Logic WorkBench -- Propositional Logic: Normal forms

; Copyright (c) 2014 - 2017 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop.nf
  "Normal forms of formulas of propositional logic."
  (:require [lwb.prop :refer (wff? nary? flatten-ops)]
            [clojure.spec :as s]
            [clojure.walk :refer (postwalk)]
            [clojure.set :refer (union intersection subset?)]
            [clojure.math.numeric-tower :refer (expt)]
            [clojure.math.combinatorics :refer (selections)]
            [potemkin :as pot]))

;; We import the operators and so forth from propositional logic.
(pot/import-vars
  [lwb.prop impl equiv xor ite op?])

;; # Normal forms of formulas of propositional logic

;; The namespace `lwb.prop.nf` provides normal forms in propositional logic

;; * negation normal form (nnf)
;; * conjunctive normal form (cnf) 
;; * disjunctive normal form (dnf)

;; ## Negation normal form (nnf)

;; A formula is in negation normal form, if negations appear only in front
;; of atoms.

;; Specification of a literal
(s/def ::literal (s/or :simple-expr :lwb.prop/simple-expr
                       :neg (s/and list? (s/cat :not #{'not} :simple-expr :lwb.prop/simple-expr))))

(defn literal?
  "Checks whether `phi` is a literal, i.e. a propositional atom or its negation."
  [phi]
  (s/valid? ::literal phi))

(s/fdef literal?
        :args (s/cat :phi wff?)
        :ret  boolean?)

(defn impl-free
  "Normalize formula `phi` such that just the operators `not`, `and`, `or` are used."
  [phi]
  (if (literal? phi)
    phi
    (let [op (first phi)]
      (if (contains? #{'and 'or 'not} op)
        (apply list op (map impl-free (rest phi)))
        (let [exp-phi (macroexpand-1 phi)]
          (apply list (first exp-phi) (map impl-free (rest exp-phi))))))))

(s/fdef impl-free
        :args (s/cat :phi wff?)
        :ret  wff?)

(defn nnf
  "Transforms an impl-free formula `phi` into negation normal form."
  [phi]
  (if (literal? phi)
    phi
    (let [[op & more] phi]
      (if (contains? #{'and 'or} op)
        (apply list op (map nnf more))
        (let [[second-op & second-more] (second phi)]
          (if (contains? #{'and 'or} second-op)
            (nnf (apply list (if (= 'and second-op) 'or 'and) (map #(list 'not %) second-more)))
            (nnf (first second-more))))))))

(s/fdef nnf
        :args (s/cat :phi wff?)
        :ret  wff?)

;; ## Conjunctive normal form (cnf)

;; (Standardized) conjunctive normal form     
;; Conjunctive normal form in lwb is defined as a formula of the form
;; `(and (or ...) (or ...) (or ...) ...)` where the clauses contain
;; only literals, no constants --
;; or the trivially true or false formula.

;; I.e. in lwb when transforming formula to cnf, we reduce it to this
;; standard form.

;; Specification of a clause
(s/def ::clause (s/and list? (s/cat :or #{'or} :literals (s/* ::literal))))

;; Specification of conjunctive normal form cnf
(s/def ::cnf (s/and list? (s/cat :and #{'and} :clauses (s/* ::clause))))

;; Caveat reader!       
;; At the first sight `(apply list ...)` and `(list* ...)` seems to give the
;; same result -- BUT
;; the types are different:        
;; `(list? (apply list [:a :b])) => true`       
;; `(list? (list* [:a :b]))      => false`      
;; since we check `list?` in the specs we have to be precise with the types
;; our functions are returning!

;; One may argue that in the spirit of Clojure it's better to work with
;; sequences whatever their implementation would be.       
;; But: we want to use the data structure of a formula as code
;; and as such it has to be a list!

(defn- distr
  "Application of the distributive laws to the given formulas"
  ([phi] phi)
  ([phi-1 phi-2]
    (cond
      (and (not (literal? phi-1)) (= 'and (first phi-1)))
        (apply list 'and (map #(distr % phi-2) (rest phi-1)))
      (and (not (literal? phi-2)) (= 'and (first phi-2)))
        (apply list 'and (map #(distr phi-1 %) (rest phi-2)))
      :else
        (list 'or phi-1 phi-2)))
  ([phi-1 phi-2 & more]
    (reduce distr (distr phi-1 phi-2) more)))

(defn nnf->cnf 
  "Transforms the formula `phi` from nnf to cnf."
  [phi]
  (cond
    (literal? phi) (list 'and (list 'or phi))
    (= '(or) phi) '(and (or))
    (= 'and (first phi)) (apply list 'and (map nnf->cnf (rest phi)))
    (= 'or (first phi)) (apply distr (map nnf->cnf (rest phi)))))

(s/fdef nnf->cnf
        :args (s/cat :phi wff?)
        :ret  wff?)

(defn- clause->sets
  "Transforms a clause `(or ...)` into a map of the sets of `:pos` atoms 
   and `:neg` atoms in the clause."
  [cl]
  (apply merge-with union
         (for [literal (rest cl)]
					  (if (or (symbol? literal) (instance? Boolean literal))
					      {:pos #{literal}}
					      {:neg #{(second literal)}}))))

(defn- red-clmap 
  "Reduces a clause in the form `{:pos #{...} :neg #{...}`    
   by considering:     
   (1) :pos contains true  -> clause is trivially true,   
   (2) :neg contains false -> clause is trivially true,   
   (3) :pos contains false -> false can be deleted,   
   (4) :neg contains true  -> true can be deleted."
  [{:keys [pos neg]}]
  (cond
    (pos? (count (intersection pos neg))) true
    (contains? pos 'true) true
    (contains? neg 'false) true
    :else
      (let [pos' (filter #(not= 'false %) pos), neg' (filter #(not= 'true %) neg)]
        (conj (apply list (concat (apply list pos') (map #(list 'not %) neg'))) 'or))))

(defn- red-cnf
  "Reduces a formula `ucnf` of the form `(and (or ...) (or ...) ...)`."
  [ucnf]
  (let [result (filter #(not= 'true %) (map #(red-clmap (clause->sets %)) (rest ucnf)))]
    (cond
      (some #{'(or)} result) false
      (empty? result) true
      :else (conj (apply list (distinct result)) 'and))))

(defn cnf
  "Transforms `phi` to (standardized) conjunctive normal form cnf."
  [phi]
  (-> phi impl-free nnf nnf->cnf flatten-ops red-cnf))

;; Specification of function `cnf`      
;; `:ret` is in cnf and equivalent to the argument `:phi`.
(s/fdef cnf
        :args (s/cat :phi wff?)
        :ret (s/alt :cnf ::cnf :bool boolean?))

(comment
  ; the spec of the function has a cyclic dependency as a consequence!
  ; :fn #(lwb.prop.sat/valid? (list 'equiv (-> % :args :phi) (-> % :ret))))
  )

(defn cnf?
  "Is `phi` in (standardized) conjunctive normal form?
   `(cnf? phi)` returns true or false.       
   `(cnf? phi :exception-if-not)` returns true or throws an exception describing the error in `phi`."
  ([phi]
   (cnf? phi :bool))
  ([phi mode]
   (let [result (s/valid? ::cnf phi)]
     (or result (if (= mode :exception-if-not) (throw (Exception. (s/explain-str ::cnf phi))) result)))))

(s/fdef cnf?
        :args (s/alt :1-args (s/cat :phi wff?)
                     :2-args (s/cat :phi wff? :mode #{:bool :exception-if-not}))
        :ret boolean?)

;; ### Disjunctive normal form (dnf)

;; Specification of monom
(s/def ::monom (s/and list? (s/cat :and #{'and} :literals (s/* ::literal))))

;; Specification of disjunctive normal form dnf
(s/def ::dnf (s/and list? (s/cat :or #{'or} :monoms (s/* ::monom))))

; helper to transform (cnf (not phi)) to (dnf phi)
(defn- mapdnfi
  "maps clause to monom in transformation to dnf."
  [inner]
  (cond
    (= inner 'or) 'and
    (list? inner) (second inner)
    :else (list 'not inner)))

(defn- mapdnf
  [outer]
  (if (= outer 'and) 'or
                     (apply list (map mapdnfi outer))))

(defn dnf
  "Transforms `phi` to disjunctive normal form dnf."
  [phi]
  (let [cnf (cnf (list 'not phi))]
    ; border case
    (if (boolean? cnf)
      (not cnf)
      (apply list (map mapdnf cnf)))))

;; Specification of function `dnf`
;; `:ret` is in dnf and equivalent to the argument `:phi`.
(s/fdef dnf
        :args (s/cat :phi wff?)
        :ret (s/alt :dnf ::dnf :bool boolean?))

(defn dnf?
  "Is `phi` in (standardized) disjunctive normal form?
   `(dnf? phi)` returns true or false.       
   `(dnf? phi :exception-if-not)` returns true or throws an exception describing the error in `phi`."
  ([phi]
   (dnf? phi :bool))
  ([phi mode]
   (let [result (s/valid? ::dnf phi)]
     (or result (if (= mode :exception-if-not) (throw (Exception. (s/explain-str ::dnf phi))) result)))))

(s/fdef dnf?
        :args (s/alt :1-args (s/cat :phi wff?)
                     :2-args (s/cat :phi wff? :mode #{:bool :exception-if-not}))
        :ret boolean?)

