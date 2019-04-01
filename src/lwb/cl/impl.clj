; lwb Logic WorkBench -- Combinatory logic - internals

; Copyright (c) 2019  Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.cl.impl
  (:refer-clojure :exclude [==])
  (:require [clojure.walk :as walk]
            [clojure.spec.alpha :as s]
            [clojure.core.logic :refer :all]
            [clojure.set :as set]))

;; Handling of parentheses

(defn add-parens
  "Adds left-associative parentheses to a seq of symbols or subterms."
  [[first second & more]]
  (let [ret (list first second)]
    (if more
      (recur (list* ret more))
      ret)))

(defn rm-outer-parens
  "Removes all outer parens of a seq"
  [tseq]
  (if (= 1 (count tseq))
    (if (seq? (first tseq))
      (recur (first tseq))
      tseq)
    tseq))

(defn max-parens-seq
  [seq]
  (walk/postwalk
    #(if (seq? %)
       (let [seq' (rm-outer-parens %)]
         (if (= 1 (count seq'))
           seq'
           (add-parens seq')))
       %)
    seq))

(defn min-parens-seq
  [seq]
  (let [seqn (max-parens-seq seq)]
    (walk/postwalk
      #(if (seq? %)
         (let [f (first %)]
           #_(println %)
           (if (seq? f)
             (concat f (rest %))
             %))
         %)
      seqn)))

;; Logic relation for combinators

(defn vars
  "The set of variables in `term`."
  [term]
  (->> (flatten term)
       (filter #(s/valid? :lwb.cl.spec/variable %))
       (into #{})))

(defn gen-fresh-args
  "Vector of fresh variables for the relation to be constructed"
  [redex effect]
  (let [r-vars (vars redex)
        e-vars (vars effect)]
    (vec (set/union r-vars e-vars))))

(defn gen-cl-term'
  "Code from a sequence with all parentheses"
  [seqterm]
  (map (fn [item] (if (seq? item)
                    (list* 'list (gen-cl-term' item))   
                    (if (s/valid? :lwb.cl.spec/combinator item)
                      (list 'quote item)
                      item)))
       seqterm))

(defn gen-cl-term
  "Code from a term without all parentheses"
  [term]
  (let [seqterm (max-parens-seq (seq term))
        result (gen-cl-term' seqterm)]
    (if (= 1 (count result))
      (first result)
      (list* 'list result))))

(defn gen-cl-rel
  [redex effect]                                            ;; both terms
  (let [fresh-args (gen-fresh-args redex effect)
        redex-term (gen-cl-term redex)
        effect-term (gen-cl-term effect)]
    (list 'fn (vector 'term 'q) (list 'fresh fresh-args
                                      (list '== 'term redex-term)
                                      (list '== 'q effect-term)))))

;; Storage for combinators

(def combinator-store
  (atom {}))

(defn combs
  "The set of combinators in `term`."
  [term]
  (->> (flatten term)
       (filter #(s/valid? :lwb.cl.spec/combinator %))
       (into #{})))

(defn combs-keys
  "The set of combinator keys in `term`."
  [term]
  (into #{} (map keyword (combs term))))

(defn make-comb
  [redex effect]
  (let [combs (combs redex)
        key (keyword (first combs))]
    (if (not key) (throw (AssertionError. (str "Redex '" redex "' has no combinator!")))
                  (hash-map key {:redex     redex
                                 :effect    effect
                                 :logic-rel (binding [*ns* (find-ns 'lwb.cl.impl)]
                                              (eval (gen-cl-rel redex effect)))}))))

;; Application of logic relation for one-step expansion or reduction

(defn apply'
  [rule-fn seqterm mode]
  (if (= mode :red)
    (run 1 [q] (rule-fn seqterm q))
    (run 1 [q] (rule-fn q seqterm))))

(defn apply*
  [term rule-fn i mode] ;; pre: term has max parentheses
  (let [counter (atom 0)]
    (walk/prewalk
      #(let [rep (apply' rule-fn % mode)]
         (if (and (not (vector? %)) (not-empty rep))
             (if (= i (swap! counter inc)) (first rep) %)
           %))
      term)))
