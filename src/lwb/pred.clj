; lwb Logic WorkBench -- Predicate logic

; Copyright (c) 2015 - 2017 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.pred
  (:require [lwb.prop :as prop]
            [clojure.zip :as zip]
            [clojure.walk :as walk]
            [clojure.spec.alpha :as s]
            [clojure.java.browse :as browse]
            [potemkin :as pot]
            [lwb.vis :as vis]))

(defn man
  "Manual"
  []
  (browse/browse-url "https://github.com/esb-dev/lwb/wiki/pred"))

;; # The language of predicate logic

;; There are actually many languages of predicate logic, which are
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
;; the values are vectors with type and arity of the element.

;; ### Specification of a signature
(s/def ::signature (s/map-of keyword? (s/tuple #{:const :func :prop :pred} nat-int?)))

;; Example:

;;        {:c [:const 0]
;;         :f [:func  2]
;;         :a [:prop  0]
;;         :p [:pred  1]}

;; defines a signature comprising the constant `:c`, the binary function `f`,
;; the proposition `a`, and the unary predicate `p`.

;; Constants are represented as keywords or ints in Clojure. Thus it's not necessary
;; to declare them in the signature. The declaration in the signature is just
;; the documentation, but even an undeclared keyword is recognized and accepted
;; in a formula.

;; ## Utility functions for signatures

(defn- sig-what
  [type symb sig]
  (and (symbol? symb) (= (first ((keyword symb) sig)) type)))

(defn const?
  "Is `kw` a constant?"
  [kw]
  (or (keyword? kw) (int? kw)))

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

;; We import the operators and so forth from propositional logic.
(pot/import-vars
  [lwb.prop impl equiv xor ite op?])

;; ### Quantors in predicate logic

(defn quantor?
  "Is `symb` a quantor?"
  [symb]
  (or (= 'forall symb) (= 'exists symb)))

;; ### Equality in predicate logic

(defn eq?
  "Is `symb` equality in predicate logic??"
  [symb]
  (= '= symb))

;; ### Variables

(defn logvar?
  "Is `symb` a logical variable with respect to signature `sig`?"
  [symb sig]
  (if-not (symbol? symb)
    false
    (not (or (op? symb) (boolean? symb) (quantor? symb) (eq? symb)
             (func? symb sig) (pred? symb sig) (prop? symb sig)))))

;; ## Well-formed first-order formulae
;; The check whether a formula is well-formed reflects the grammar of
;; the language.

;; Predicates used as specs in clojure.spec can have just one argument.
;; Thus we define a dynamic Var for the signature since all checks of
;; a formula of the predicate logic have to be done with respect to a
;; given signature.

(def ^:dynamic *signature*
  "Var for binding a signature for checking formulae of predicate logic")

;; **Simple terms** are constants, variables or nullary functions.
(s/def ::simple-term (s/or :const const?
                           :logvar #(logvar? % *signature*)
                           :func-0 #(func-0? % *signature*)))

(defn- arity-ok? 
  "Checks arity of operators, functions and predicates."
  [{:keys [op params]}]
  (let [arity (arity op *signature*)]
    (if (= arity -1) true
                     (= arity (count params)))))

;; **Complex terms** are function calls.
(s/def ::compl-term (s/and list? (s/& (s/cat :op #(func? % *signature*) :params (s/* ::term)) arity-ok?)))

;; A **term** is simple or complex.
(s/def ::term (s/or :simple-term ::simple-term
                    :compl-term  ::compl-term))

(defn term?
  "Is `texpr` a single term with respect to `sig`?"
  [texpr sig]
  (binding [*signature* sig]
    (s/valid? ::term texpr)))

;; **Predicates**
(s/def ::predicate (s/and list? (s/& (s/cat :op #(pred? % *signature*) :params (s/* ::term)) arity-ok?)))

;; **Equality**
(s/def ::equality (s/and list? (s/cat :op #(= '= %) :param1 ::term :param2 ::term)))

;; **Simple expressions** are booleans, propositions, predicates or equality.
(s/def ::simple-expr (s/or :bool boolean?
                           :prop #(prop? % *signature*)
                           :pred ::predicate
                           :eq   ::equality))

;; **Declaration of variables**
(s/def ::decl (s/and (s/coll-of #(logvar? % *signature*) :into [])  #(pos? (count %))))

;; **Quantified expressions**
(s/def ::quantified (s/and list? (s/cat :quantor quantor? :decl ::decl :fml ::fml)))

;; **Expression with operator**
(s/def ::op-expr (s/and list? (s/& (s/cat :op op? :params (s/* ::fml)) arity-ok?)))

;; **Complex expressions** are quantified expressions or expressions with an operator.
(s/def ::compl-expr (s/or :op-expr ::op-expr
                           :quant   ::quantified))

;; **Formulas of predicate logic** are simple or complex expressions.
(s/def ::fml (s/or :simple-expr ::simple-expr
                   :compl-expr  ::compl-expr))

(defn wff?
  "Is the first order formula `phi` well-formed, with respect to signature `sig`?     
   `(wff? phi sig)` returns true or false      
   `(wff? phi sig :exception-if-not)` returns true or throws an exception describing the error in `phi`."      
  ([phi sig]
   (wff? phi sig :bool))
  ([phi sig mode]
   (binding [*signature* sig]
     (let [result (s/valid? ::fml phi)]
       (or result (if (= mode :exception-if-not) (throw (Exception.  ^String (s/explain-str ::fml phi))) result))))))

(s/fdef wff?
        :args (s/cat :fml any? :sig ::signature :mode (s/? #{:bool :exception-if-not}))
        :ret boolean?)

;; ## Models

;; A model is a "world" consisting of a universe, as well as functions and relations 
;; on the universe. A model assigns concrete functions and relations to the
;; function symbols and relation symbols defined in a signature.

;; Models are represented in Clojure as maps:

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

;;     {:univ #{0 1}
;;      :op   [:func 2 (fn [x y] (body of function for group operation))]
;;      :inv  [:func 1 (fn [x] (body of function for inverse))]
;;      :unit [:func 0 0]}

;; see lwb.pred.examples.groups

;; Example for a family:

;;      {:univ #{:eve :adam :joe :susan :anne}
;;       :mother [:pred 2 '(fn [mother child] 
;;                             (let [rel #{[:eve :joe] [:eve :susan] [:susan :anne]}]
;;                                                (contains? rel [mother child])))]}

;; ### Specification of models

(defn- univ-def?
  "Is `x` the definition of a universe?"
  [x]
  (set? x))

(defn- func-def?
  "Is `[key arity def]` the definition of a function?"
  [[key arity def]]
  (and (= key :func) (>= arity 0) (if (pos? arity) (fn? def) true)))

(defn- pred-def?
  "Is `[key arity def]` the definition of a predicate?"
  [[key arity def]]
  (and (= key :pred) (>= arity 1) (fn? def) ))

(defn- prop-def?
  "Is `[key arity def]` the definition of a proposition?"
  [[key arity def]]
  (and (= key :prop) (zero? arity) (boolean? def) ))

(defn- univ-ok?
  "Is the universe defined and a set?"
  [model]
  (and (contains? model :univ) (set? (:univ model))))

;; **Specification** of a model in the predicate logic.
(s/def ::model  (s/and univ-ok? (s/map-of keyword? 
                                         #(or (univ-def? %) (func-def? %) (pred-def? %) (prop-def? %)))))


;; **Caveat reader!**         
;; Formulas of predicate logic can be evaluated with respect to a model.
;; In Clojure the model is used in a let-binding.

;; This may result in some subtle problems, see the following comments
(comment 
  
  (defn my-eval [avec phi]
    (eval `(let ~avec ~phi)))
  
  ; The idea of my-eval is the evaluation of
  ; arbitrary lists where the symbols are
  ; defined in avec.

  ; example:     
  ; in avec the symbol p is defined as true
  ; and the symbol foo as a function adding 2

  (def avec
    ['p   true
     'foo (fn [x] (+ 2 x))])

  ; and here their evaluation
  (my-eval avec '(and p (= 7 (foo 5))))
  ; => true
  
  ; If the function in the assignment vector is a clojure
  ; we get an exception:

  (defn make-adder [n]
    (fn [x] (+ n x)))

  ((make-adder 2) 5)
  ; => 7

  ; here the second definition of a binding vector:
  (def avec'
    ['p true
     'foo (make-adder 2)])

  (my-eval avec' '(and p (= 7 (foo 5))))
  ; CompilerException java.lang.ExceptionInInitializerError
  
  ; There is a ticket concerning this behaviour: 
  ; [Clojure/CLJ-1206](http://dev.clojure.org/jira/browse/CLJ-1206).
  
  ; It's not always possible to eval function objects in Clojure
  
  ; If running into this problem, don't use the function object,
  ; but the code that defines the function:
  
  (def avec''
    '[p true
      foo (make-adder 2)])
  (my-eval avec'' '(and p (= 7 (foo 5))))
  ; => true
  
  ) ; end of remarks

(defmacro make-pred
  "Generates a predicate expressing that the parameters to the predicate
   fulfill the given relation."
  [rel]
  `(fn [& ~'more] (contains? ~rel (vec ~'more))))

(defn sig-from-model
  "Returns the signature from a given model."
  [model]
  (let [f #(not= (key %) :univ)
        m (fn [[key [v1 v2]]] [key [v1 v2]])]
    (into {} (map m (filter f model)))))


;; ## Evaluation of formulas of predicate logic with respect to a given model

;; **First step** in evaluation of a formula of predicate logic:      
;; Unfold the variables in quantor expression, e.g.    
;; `(forall [x y] (...))` ->      
;; `(forall [x] (forall [y] (...)))`

(defn- quant-expr?
  "Is loc a quantor expression?"
  [loc]
  (and (zip/branch? loc)
       (or (= (-> loc zip/down zip/node) 'forall)
           (= (-> loc zip/down zip/node) 'exists))))

(defn- quant-expr+?
  "Is loc a quantor expression with multiple variables?"
  [loc]
  (and (quant-expr? loc)
       (> (count (-> loc zip/down zip/right zip/node)) 1)))

(defn- unfold-quant
  "New quantor expression with first variable pulled out."
  [loc]
  (let [quantor (-> loc zip/down zip/node)
        var-vec (-> loc zip/down zip/right zip/node)
        var1-vec [(first var-vec)]
        varr-vec (vec (rest var-vec))
        right (-> loc zip/down zip/right zip/right zip/node)
        edited-loc (zip/edit loc (fn [_] `(~quantor ~var1-vec (~quantor ~varr-vec ~right))))]
    (-> edited-loc zip/down zip/right)))

(defn- unfold-vars
  "`phi` with unfolded vars in all quantor expressions."
  [phi]
  (if-not (list? phi)
                phi ;border case
  (loop [loc (zip/seq-zip (seq phi))]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next
               (if (quant-expr+? loc)
                 (unfold-quant loc)
                 loc)))))))

;; **Second step**: Elimination of quantors by generating a proposition using
;; the elements of the universe.

(defn- no-more-quant?
  "Does `phi` have no more quantors?"
  [phi]
  (not (first
         (filter #(or (= 'forall %) (= 'exists %))
                 (flatten phi)))))

(defn- expand-quant
  "Expands a quantor into an proposition:       
   (and ...) for the forall quantor,      
   (or ...)  for the exists quantor,         
   by replacing the variables by the items in the universe."
  [univ loc]
  (let [quantor (-> loc zip/down zip/node)
        var-vec (-> loc zip/down zip/right zip/node)
        var     (first var-vec)
        body (-> loc zip/down zip/right zip/right zip/node)
        edit-func (fn [_]
                    (let [body-seq (for [item univ] (walk/postwalk-replace {var item} body))]
                      (if (= quantor 'forall)
                        (apply list 'and body-seq)
                        (apply list 'or body-seq))))]
    (zip/edit loc edit-func)))

(defn- elim-quant
  "Eliminates all quantors in `phi` by expanding them."
  [univ phi]
  (loop [loc (zip/seq-zip (seq phi))]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next
               (if (quant-expr? loc)
                 (expand-quant univ loc)
                 loc))))))

(defn- pred2prop
  "Given a universe, `phi` is transformed into a proposition.        
   Requires: `phi` has to have unfolded vars, i.e. each quantor has exactly 1 var!"
  [univ phi]
  (let [phi' (loop [cur phi]
               (if (no-more-quant? cur)
                 cur
                 (recur (elim-quant univ cur))))]
    (prop/flatten-ops phi')))

;; **Third step**: Constructing a vector for let from the model and 
;; evaluating.

(defn- modelmap 
   "Checks type of entry in model and returns binding appropriate to type."
  [[keyw value]]
  (if (= keyw :univ)
    ['univ value]
    [(symbol (name keyw)) (nth value 2)]))

(defn- model2assign-vec
  "Makes an assignment vector from the given model"
  [model]
  (vec (mapcat modelmap model)))

;; **Finally: the evaluation**

(defn eval-phi
  "Evaluates the formula `phi` with respect to the given 
   model."
  [phi model]
  (let [assign-vec (model2assign-vec model)
        phi' (pred2prop (:univ model) (unfold-vars phi))]
    (binding [*ns* (find-ns 'lwb.pred)]
      (eval `(let ~assign-vec ~phi')))))

(s/fdef eval-phi
        :args (s/& (s/cat :phi any? :model ::model) (fn [param] (wff? (:phi param) (sig-from-model (:model param)))))
        :ret boolean?)

;; ## Visualisation of a formula

(defn texify
  "Generates TeX code for TikZ or a pdf file if filename given.       
   Requires: TeX installation, commands in `lwb.util.shell`."
  ([phi]
   (vis/texify phi))
  ([phi filename]
   (vis/texify phi filename)))

(s/fdef texify
        :args (s/cat :phi wff? :filename (s/? string?))
        :ret  nil?)