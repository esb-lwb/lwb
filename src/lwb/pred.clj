; lwb Logic WorkBench -- Predicate logic

; Copyright (c) 2015 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.pred
  (:require [lwb.prop :as prop]
            [clojure.zip :as zip]
            [clojure.walk :as walk]
            [potemkin :as pot]))


;; # The language of predicate logic

;; There are acutally many languages of predicate logic, which are
;; defined with respect to a given signature of the non-logical symbols.

;; ## Signatures

;; A signature for a language of predicate logic comprises the definition
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
;; the documentation, but even an undeclared keyword is recognized and accepted
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
  (= '= symb))

;; ## Variables
(defn logvar?
  "Is `symb` a logical variable with respect to signature `sig`?"
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

(defn nullary-func?
  "Is `symb` a function of arity `0` with respect to `sig`?"
  [symb sig]
  (and (func? symb sig) (= (arity symb sig) 0)))

(defn simple-term?
  "Is `symb` a single term with respect to `sig`?"
  [symb sig]
  (if (not (symbol? symb))
    (const? symb)
    (or (logvar? symb sig) (nullary-func? symb sig))))

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
  (if (and (vector? decl) (every? #(logvar? % sig) decl)) 
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
   `(wff? phi sig)` returns true or false   
   `(wff? phi sig :msg)` returns true or a message on the error in `phi`."
  ([phi sig]
   (wff? phi sig :bool))
  ([phi sig mode]
   (try
     (or (simple-expr? phi sig) (compound-expr phi sig))
     (catch Exception e (if (= mode :msg) (.getMessage e) false)))))


;; ## Models

;; A model is a "world" consisting of a universe, as well as functions and relations 
;; on the universe. A model assigns concrete functions and relations to the
;; function symbols and relation symbols defined in a signature.

;; Models are represented in Clojure as maps.
;; The key `:univ` denotes the universe, the value is a set of constants
;; Functions are given by the keyword build from the name of the function, 
;; together with a vector consisting of the keyword `:func`, the arity of the
;; function and the function itsself.
;; Propositions are given by the keyword build from the name of the
;; proposition together with a vector consisting of the keyword `:prop`,
;; the arity `0` and the value for the proposition i.e. `true` or `false`.   
;; Relations are given by the keyword build from the name of the corresponding
;; predicate symbol together with a vector consisting of the keyword `:pred`, 
;; the arity of the relation and a predicate constrcuted by `make-pred` from
;; the provided relation.

;; Example for a group of 2 elements:
;; {:univ #{:0 :1}
;;  :op   [:func 2 (fn [x y] (body of function for group operation))]
;;  :inv  [:func 1 (fn [x] (body of function for inverse))]
;;  :unit [:func 0 :0]}

;; Example for a family:
;; {:univ #{:eve :adam :joe :susan :anne}
;;  :mother [:pred 2 (fn [mother child] (let [rel #{[:eve :joe] [:eve :susan] [:susan :anne]}]
;;                                        (contains? rel [mother child])))]}

;; Construct an assignment vector from a model, i.e. bindings of the model elements 
;; to symbols according to given keywords
;; :pre well-formed model

(defn- modelmap 
   "Checks type of entry in model and returns binding appropriate to type."
  [[keyw value]]
  (if (= keyw :univ)
    ['univ value]
    [(symbol (name keyw)) (nth value 2)]))

(defn model2assign-vec
  "Makes an assignment vector form the given model"
  [model]
  (vec (mapcat modelmap model)))

(defmacro make-pred 
  "Generates a predicate indicating that the parameters to the predicate
   fulfill the given relation"
  [rel]
  `(fn [& ~'more] (contains? ~rel (vec ~'more))))

;; example for a model 
(def m
  {:univ #{:0 :1}
   :op   [:func 2 (fn [x y] (+ x y))]
   :inv  [:func 1 (fn [x] (- x))]
   :unit [:func 0 :0]
   :r    [:pred 2 (make-pred #{[:1 :1] [:2 :2]})]
   :s    [:pred 3 (make-pred #{[:1 :1 :1] [:2 :2 :2]})]
   :p    [:prop 0 'true]})



;; First step in evaluation of a formula of predicate logic:
;; Unfold the variables in quantor expression, e.g.
;; `(forall [x y] (...))` -> `(forall [x] (forall [y] (...)))`

(defn- quant-expr? [loc]
  "Is loc a quantor expression?"
  (and (zip/branch? loc)
       (or (= (-> loc zip/down zip/node) 'forall) 
           (= (-> loc zip/down zip/node) 'exists))))

(defn- quant-expr+? [loc]
  "Is loc a quantor expression with multiple variables?"
  (and (quant-expr? loc)
       (> (count (-> loc zip/down zip/right zip/node)) 1)))

(defn- unfold-quant [loc]
  "New quantor expression with first variable pulled out"
  (let [quantor (-> loc zip/down zip/node)
        var-vec (-> loc zip/down zip/right zip/node)
        var1-vec [(first var-vec)]
        varr-vec (vec (rest var-vec))
        right (-> loc zip/down zip/right zip/right zip/node)
        edited-loc (zip/edit loc (fn [loc] `(~quantor ~var1-vec (~quantor ~varr-vec ~right))))]
    (-> edited-loc zip/down zip/right)))

(defn unfold-vars [phi]
  "phi with unfolded vars in all quantor expressions"
  (loop [loc (zip/seq-zip (seq phi))]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next
               (if (quant-expr+? loc)
                 (unfold-quant loc)
                 loc))))))

;; examples
(comment
  (def grp-ass '(forall [x y z] (= (op x (op y z)) (op (op x y) z))))
  (def grp-ass' '(exists [x y z] (= (op x (op y z)) (op (op x y) z))))
  (def grp-unit '(forall [x] (= (op x unit) x)))
  (def grp-comm '(forall [x y] (= (op x y) ((op y x)))))

  (wff? grp-ass {:op [:func 2]} :msg)
  (def grp-ass1 (unfold-vars grp-ass))
  (def grp-ass2 (unfold-vars grp-ass'))
  (unfold-vars grp-unit)
  (unfold-vars grp-comm)
  )


(defn- no-more-quant? [phi]
  "does phi have no more quantors?"
  (not (first 
         (filter #(or (= 'forall %) (= 'exists %)) 
                 (flatten phi)))))

(comment
  (no-more-quant? grp-ass)
  (no-more-quant? (rest grp-ass))
  )

(defn- expand-quant [univ loc]
  "expands a quantor into an proposition 
   (and ...) for the forall quantor,
   (or ...)  for the exists quantor,
   by replacing the variables by the items in the universe"
  (let [quantor (-> loc zip/down zip/node)
        var-vec (-> loc zip/down zip/right zip/node)
        var     (first var-vec)
        body (-> loc zip/down zip/right zip/right zip/node)
        edit-func (fn [loc]
                    (let [body-seq (for [item univ] (walk/postwalk-replace {var item} body))]
                      (if (= quantor 'forall)
                        (list* 'and body-seq)
                        (list* 'or body-seq))))]
        (zip/edit loc edit-func)))
  
(defn- elim-quant [univ phi]
  "eliminates all quantors in phi by expaning them"
  (loop [loc (zip/seq-zip (seq phi))]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next
             (if (quant-expr? loc)
               (expand-quant univ loc)
               loc))))))

(defn pred2prop [univ phi]
  "Given a universe, phi is transformed into a proposition.
   phi has to have unfolded vars, i.e. each quantor has exactly 1 var!"
  (let [phi' (loop [cur phi]
              (if (no-more-quant? cur)
                cur
                (recur (elim-quant univ cur))))]
    (prop/flatten-ops phi')))

(comment
  (pred2prop #{:0 :1 :2} grp-ass2)
  (pred2prop #{:0 :1} grp-ass1)
  (pred2prop #{:0 :1} '(and (forall [x] (forall [y] (r x y))) (exists [z] (p z))))
  )

(defn eval-phi
  "Evaluates the formula `phi` with respect to the given 
   model."
  [phi model]
  (let [assign-vec (model2assign-vec model)
        phi' (pred2prop (:univ model) (unfold-vars phi))]
    (binding [*ns* (find-ns 'lwb.pred)]
      (eval `(let ~assign-vec ~phi')))))

