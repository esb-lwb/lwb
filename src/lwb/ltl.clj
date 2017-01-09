; lwb Logic WorkBench -- Linear Temporal Logic

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.ltl
  (:require [lwb.prop :as prop]
            [potemkin :as pot]
            [lwb.vis :as vis]
            [clojure.spec :as s]
            [clojure.java.browse :as browse]))

(defn man
  "Manual"
  []
  (browse/browse-url "https://github.com/esb-dev/lwb/wiki/ltl"))

;; # Representation of formulas of ltl

;; The representation is like in propositional logic

;; We have 5 more operators in ltl:

;; * always -- unary
;; * atnext -- unary
;; * finally -- unary
;; * until -- binary
;; * release -- binary

;; We import the operators and so forth from propositional logic
(pot/import-vars
  [lwb.prop impl equiv xor ite])

(defn op?
  "Is `symb` an operator of ltl?"
  [symb]
  (let [operators '#{always atnext finally until release}]
    (or (prop/op? symb) (contains? operators symb))))

(defn atom?
   "Is `symb` an atomar proposition?"
   [symb]
   (and (symbol? symb) (not (op? symb))))

(defn arity
  "Arity of operator `op`.      
   -1 means n-ary.       
   requires: `op` an operator."
  [op]
  (if
    (prop/op? op) (prop/arity op)
                  (if (contains?  '#{until release} op) 2
                                    1)))

;; A simple expression is an atom or a boolean constant.
(s/def ::simple-expr (s/or :bool boolean?
                           :atom atom?))

;; A compound expression is a list of an operator together with
;; several formulas as arguments whose number matches the arity of the operator.
(defn- arity-ok? [{:keys [op params]}]
  (let [arity (arity op)]
    (if (= arity -1) true
                     (= arity (count params)))))

(s/def ::compl-expr (s/and list? (s/& (s/cat :op op? :params (s/* ::fml)) arity-ok?)))

;; A formula is a simple or a complex expression.
(s/def ::fml (s/or :simple-expr ::simple-expr
                   :compl-expr ::compl-expr))

;; For natural deduction in ltl we need a special form of an ltl formula at a certain state

(s/def ::at-fml (s/cat :at #{'at} :state (s/coll-of symbol? :kind vector? :count 1) :fml ::fml))

;; ## Is a formula well-formed?

(defn wff?
  "Is the formula `phi` well-formed?       
   `(wff? phi)` returns true or false.       
   `(wff? phi :msg)` returns true or a message describing the error in `phi`."
  ([phi]
   (wff? phi :bool))
  ([phi mode]
   (let [result (s/valid? ::fml phi)]
     (or result (if (= mode :msg) (s/explain-str ::fml phi) result)))))

(s/fdef wff?
        :args (s/alt :1-args (s/cat :phi any?)
                     :2-args (s/cat :phi any? :mode #{:bool :msg}))
        :ret (s/alt :bool boolean? :msg string?))

;; ## Negation normal form

;; ### Specification of a literal

(s/def ::literal (s/or :simple-expr ::simple-expr
                       :neg (s/and list? (s/cat :not #{'not} :simple-expr ::simple-expr))))

(defn literal?
  "Checks whether `phi` is a literal, i.e. a propositional atom or its negation."
  [phi]
  (s/valid? ::literal phi))

;; ### Specification of negation normal form nnf

(s/def ::nnf-op '#{and or atnext until release})
                   
(s/def ::nnf-compl-expr (s/and list? (s/& (s/cat :op ::nnf-op :params (s/* ::nnf-fml)) arity-ok?)))

(s/def ::nnf-fml (s/or :literal ::literal
                       :compl-expr ::nnf-compl-expr))

;; ### Reduction to the set of operators for nnf

(defn- impl-free
  "Normalize formula `phi` such that just the operators `not`, `and`, `or`, `atnext`, `until`, `release` are used."
  [phi]
  (if (literal? phi)
    phi
    (let [op (first phi)]
      (cond (contains? #{'and 'or 'not 'atnext 'until 'release} op) (apply list op (map impl-free (rest phi)))
            (= op 'finally) (apply list 'until true (map impl-free (rest phi)))
            (= op 'always) (apply list 'release false (map impl-free (rest phi)))
            :else (let [exp-phi (macroexpand-1 phi)] (apply list (first exp-phi) (map impl-free (rest exp-phi))))))))

;; ### Transformation to nnf

(defn nnf
  "Transforms `phi` into negation normal form."
  [phi]
  (let [phi' (impl-free phi)]
    (if (literal? phi')
      phi'
      (let [[op & more] phi']
        (if (= op 'not)
          (let [[inner-op & inner-more] (second phi')]
            (cond
              (= inner-op 'and) (nnf (apply list 'or (map #(list 'not %) inner-more)))
              (= inner-op 'or) (nnf (apply list 'and (map #(list 'not %) inner-more)))
              (= inner-op 'atnext) (nnf (apply list 'atnext (map #(list 'not %) inner-more)))
              (= inner-op 'until) (nnf (apply list 'release (map #(list 'not %) inner-more)))
              (= inner-op 'release) (nnf (apply list 'until (map #(list 'not %) inner-more)))
              :else (nnf (first inner-more))))
          (apply list op (map nnf more)))))))

(s/fdef nnf
        :args (s/cat :phi wff?)
        :ret (s/alt :nnf ::nnf-fml :bool boolean?))

;; ### Check for nnf

(defn nnf?
  "Is `phi` in negation normal form?
   `(nnf? phi)` returns true or false.       
   `(nnf? phi :msg)` returns true or a message describing the error in `phi`."
  ([phi]
   (nnf? phi :bool))
  ([phi mode]
   (let [result (s/valid? ::nnf-fml phi)]
     (or result (if (= mode :msg) (s/explain-str ::nnf-fml phi) result)))))

(s/fdef nnf?
        :args (s/alt :1-args (s/cat :phi wff?)
                     :2-args (s/cat :phi wff? :mode #{:bool :msg}))
        :ret (s/alt :bool boolean? :msg string?))

;; ## Visualisation of a formula

(defn texify
  "Generates TeX code for TikZ or a pdf file if filename given.      
   Requires: TeX installation, commands `texipdf` and `open`."
  ([phi]
   (vis/texify phi))
  ([phi filename]
   (vis/texify phi filename)))

