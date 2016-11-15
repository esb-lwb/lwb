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
            [lwb.ltl.kripke :as kripke]
            [clojure.spec :as s]
            [clojure.string :as str]))

(defn man
  "Manual"
  []
  (let [info (str/join \newline
                       ["lwb - Linear Temporal Logic"
                        "Namespace lwb.ltl"
                        "Functions:"
                        "- (wff? phi), (wff? phi :msg)"
                        "  Is the formul phi of linear temporal logic well-formed?"
                        "  e.g. (wff? '(always P))"
                        "- (texify phi), (texify phi filename)"
                        "  generates pdf of phi"
                        "  e.g. (texify '(always P) \"myfile\")"])]
    (println info)
    (println)
    (kripke/man)))

;; # Representation of formulas of ltl

;; The representation is like in propositional logic

;; We have 4 more operators in ltl:

;; * always -- unary
;; * atnext -- unary
;; * finally -- unary
;; * until -- binary

;; We import the operators and so forth from propositional logic
(pot/import-vars
  [lwb.prop impl equiv xor ite atom?])

(defn op?
  "Is `symb` an operator of ltl?"
  [symb]
  (let [operators '#{always atnext finally until}]
    (or (prop/op? symb) (contains? operators symb))))

(defn arity
  "Arity of operator `op`.      
   -1 means n-ary.       
   requires: `op` an operator."
  [op]
  (if
    (prop/op? op) (prop/arity op)
                  (if (= 'until op) 2
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

;; ## Visualisation of a formula

(defn texify
  "Generates TeX code for TikZ or a pdf file if filename given.      
   Requires: TeX installation, commands `texipdf` and `open`."
  ([phi]
   (vis/texify phi))
  ([phi filename]
   (vis/texify phi filename)))

