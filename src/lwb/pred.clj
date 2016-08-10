; lwb Logic WorkBench -- Predicate logic

; Copyright (c) 2015 - 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.pred
  (:require [lwb.prop :as prop]
            [clojure.zip :as zip]
            [clojure.walk :as walk]
            [clojure.spec :as s]
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

;;        {:c [:const 0]
;;         :f [:func  2]
;;         :a [:prop  0]
;;         :p [:pred  1]}

;; defines a signature comprising the constant `:c`, the binary function `f`,
;; the proposition `a`, and the unary predicate `p`.

;; Constants are represented as keywords in Clojure. Thus it's not necessary
;; to declare them in the signature. The declaration in the signature is just
;; the documentation, but even an undeclared keyword is recognized and accepted
;; in a formula.

;; Specification of a signature
(s/def ::signature (s/map-of keyword? (s/tuple #{:const :func :prop :pred} nat-int?)))

;; ## Utility functions for signatures

(defn- sig-what
  [type symb sig]
  (and (symbol? symb) (= (first ((keyword symb) sig)) type)))

(defn const?
  "Is `kw` a constant?"
  [kw]
  (keyword? kw))

(defn func?
  "Is `symb` a function in the signature `sig`?"
  [symb sig]
  (sig-what :func symb sig))

(defn pred?
  "Is `symb` a predicate in the signature `sig`?"
  [symb sig]
  (sig-what :pred symb sig))

(defn prop?
  "Is `symb` an atomic proposition in the signature `sig`?"
  [symb sig]
  (sig-what :prop symb sig))

(declare op?)

(defn arity
  "Arity of operator `op` or of `symb` in signature `sig`"
  [symb sig]
  (if (op? symb) (lwb.prop/arity symb)
                 (second ((keyword symb) sig))))

(defn func-0?
  "Is `symb` a function of arity `0` with respect to `sig`?"
  [symb sig]
  (and (func? symb sig) (zero? (arity symb sig))))

;; ## The logical symbols in the language(s) of predicate logic

;; We import the operators and so forth from propositional logic
(pot/import-vars
  [lwb.prop impl equiv xor ite op?])

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
  (if-not (symbol? symb)
    false
    (not (or (op? symb) (boolean? symb) (quantor? symb) (eq? symb)
             (func? symb sig) (pred? symb sig) (prop? symb sig)))))

;; ## Well-formed first-order formulae
;; The check whether a formula is well-formed reflects the grammer of
;; the language

;; Predicates used as specs in clojure.spec can have just one argument.
;; Thus we define a dynamic Var for the signature since alle checks of
;; a formula of the predicate logic have to be done with respect to a
;; given signature.

(def ^{:doc "Var for binding a signature for checking formulae of predicate logic"}
^:dynamic *signature*)

;; Simple terms are constants, variables or nullary functions
(s/def ::simple-term (s/or :const keyword?
                           :logvar #(logvar? % *signature*)
                           :func-0 #(func-0? % *signature*)))

(defn- arity-ok? 
  "Checks arity of functions and predicates"
  [{:keys [op params]}]
  (let [arity (arity op *signature*)]
    (if (= arity -1) true
                     (= arity (count params)))))

;; Complex terms are function calls
(s/def ::compl-term (s/and list? (s/& (s/cat :op #(func? % *signature*) :params (s/* ::term)) arity-ok?)))

;; A term is simple or complex
(s/def ::term (s/or :simple-term ::simple-term
                    :compl-term  ::compl-term))


(defn term?
  "Is `texpr` a single term with respect to `sig`?"
  [texpr sig]
  (binding [*signature* sig]
    (s/valid? ::term texpr)))

;; Predicate
(s/def ::predicate (s/and list? (s/& (s/cat :op #(pred? % *signature*) :params (s/* ::term)) arity-ok?)))

;; Equality
(s/def ::equality (s/and list? (s/cat :op #(= '= %) :param1 ::term :param2 ::term)))

;; Simple expression
(s/def ::simple-expr (s/or :bool boolean?
                           :prop #(prop? % *signature*)
                           :pred ::predicate
                           :eq   ::equality))

;; Declaration of variables
(s/def ::decl (s/and (s/coll-of #(logvar? % *signature*) :into [])  #(pos? (count %))))

;; Quantified expression
(s/def ::quantified (s/and list? (s/cat :quantor quantor? :decl ::decl :fml ::fml)))

;; Expression with operator
(s/def ::op-expr (s/and list? (s/& (s/cat :op op? :params (s/* ::fml)) arity-ok?)))

;; Complex expression
(s/def ::compl-expr (s/or :op-expr ::op-expr
                           :quant   ::quantified))

;; Formula of predicate logic
(s/def ::fml (s/or :simple-expr ::simple-expr
                   :compl-expr  ::compl-expr))

(defn wff?
  "Is the first order formula `phi` well-formed, with respect to signature `sig`?     
   `(wff? phi sig)` returns true or false      
   `(wff? phi sig :msg)` returns true or a message on the error in `phi`."      
  ([phi sig]
   (wff? phi sig :bool))
  ([phi sig mode]
   (binding [*signature* sig]
     (let [result (s/valid? ::fml phi)]
       (or result (if (= mode :msg) (s/explain-str ::fml phi) result))))))

; TODO: hier geht's weiter --------------------------------------------------------------------

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
;; the arity of the relation and a predicate constructed by `make-pred` from
;; the provided relation.

;; Example for a group of 2 elements:
;; {:univ #{0 1}
;;  :op   [:func 2 (fn [x y] (body of function for group operation))]
;;  :inv  [:func 1 (fn [x] (body of function for inverse))]
;;  :unit [:func 0 0]}
;; see lwb.pred.examples.groups

;; Example for a family:
;; {:univ #{:eve :adam :joe :susan :anne}
;;  :mother [:pred 2 '(fn [mother child] (let [rel #{[:eve :joe] [:eve :susan] [:susan :anne]}]
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

(defn sig-from-model [model]
  "Returns the signature from a given model"
  (let [f #(not= (key %) :univ)
        m (fn [[key [v1 v2]]] [key [v1 v2]])]
    (into {} (map m (filter f model)))))

(comment
  ;; example for a model 
  (def m
    {:univ #{:0 :1}
     :op   [:func 2 (fn [x y] (+ x y))]
     :inv  [:func 1 (fn [x] (- x))]
     :unit [:func 0 :0]
     :r    [:pred 2 (make-pred #{[:1 :1] [:2 :2]})]
     :s    [:pred 3 (make-pred #{[:1 :1 :1] [:2 :2 :2]})]
     :p    [:prop 0 'true]})

  (sig-from-model m)
  )


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

