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
            [clojure.set :as set]
            [lwb.nd.error :refer :all]
            [clojure.zip :as zip]))

;; Handling of parentheses ----------------------------------------------------

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
           (if (seq? f)
             (concat f (rest %))
             %))
         %)
      seqn)))

;; Logic relation for combinators ---------------------------------------------

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

;; Arity of combinators -------------------------------------------------------

(defn arity
  "Arity of redex"
  [redex]
  (if (and (= 1 (count redex)) (symbol? (first redex))) ; just one symbol
    0
    (loop [sterm (max-parens-seq (seq redex)) arity 0]
      (if (symbol? sterm)
        arity
        (recur (first sterm) (inc arity))))))

;; Storage for combinators ----------------------------------------------------

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
  (set (map keyword (combs term))))

(defn make-comb
  [redex effect]
  (let [combs (combs redex)
        key (keyword (first combs))]
    (if-not key (throw (AssertionError. (str "Redex '" redex "' has no combinator!")))
                (hash-map key {:redex     redex
                               :effect    effect
                               :arity     (arity redex)
                               :logic-rel (binding [*ns* (find-ns 'lwb.cl.impl)]
                                            (eval (gen-cl-rel redex effect)))}))))

(defn get-rule-fn
  "Get logic function from combinator store."
  [comb-key]
  (if-let [f (get-in @combinator-store [comb-key :logic-rel])]
    f
    (throw (ex-error (str "Combinator " comb-key " not defined.")))))

(defn get-arity
  [comb-key]
  (get-in @combinator-store [comb-key :arity]))

;; Application of logic relation for one-step expansion or reduction ----------

(defn apply'
  [rule-fn seqterm mode]
  (if (= mode :red)
    (run 1 [q] (rule-fn seqterm q))
    (run 1 [q] (rule-fn q seqterm))))

(defn apply*
  [term rule-fn i mode] ;; Pre: term has max parentheses
  (let [counter (atom 0)]
    (walk/prewalk
      #(let [rep (apply' rule-fn % mode)]
         ;(println "replace " % " -> " rep)
         (if (and (not (vector? %)) (not-empty rep))
             (if (= i (swap! counter inc)) (first rep) %)
           %))
      term)))

(defn reducible?
  "Is the given sterm reducible with combinator comb with arity arity?"
  [sterm comb arity]
  (loop [count 0 st sterm]
    (cond
      (= arity count) (if (= st comb) true false)           ; test this first!
      (not (seq? st)) false                                 ; don't test on list? since sterm may be a lazy seq!
      :else (recur (inc count) (first st)))))
;; an alternative check: (= comb (first (flatten sterm)))   ; could be even faster, but not so selective

(defn apply**
  [sterm]
  "Lookup for first combinator in term and try to reduce"
  (let [comb (->> (flatten sterm) (filter #(s/valid? :lwb.cl.spec/combinator %)) (first))]
    (when-not (nil? comb)
      (let [comb-key (keyword comb)
            rule-fn (get-rule-fn comb-key)]
        (first (apply' rule-fn sterm :red))))))
        
(defn apply-z
  "One-step reduction (with first combinator in sterm)"
  [sterm]
  (loop [loc (zip/seq-zip sterm)]
    (let [node (zip/node loc)
          new-node (if-let [r (apply** node)] r node)
          new? (not= new-node node)
          new-loc (if new?
                    (zip/replace loc new-node)
                    loc)
          new-state (if new? :stop)
          next-loc (zip/next new-loc)]
      (if (or (zip/end? loc) (= :stop new-state))
        (zip/root new-loc)
        (recur next-loc)))))

;; Weak reduction -------------------------------------------------------------

(defn vec'
  [sterm]
  (if (symbol? sterm) [sterm] (vec sterm)))

(defn weak-reduce
  "Reduce the `sterm`"
  [sterm counter limit cycle trace]
  (let [steps (atom [])]
    (loop [current-sterm sterm
           result {:reduced sterm :no-steps 0 :cycle :unknown :overrun false}
           counter counter]
      (if cycle (swap! steps conj current-sterm))
      (let [red (apply-z current-sterm)                     ;; one step with apply
            found (when-not (= red current-sterm) red)]     ;; reduced term found
        (if (and found trace) (println (str counter ": " (vec' (min-parens-seq found)))))
        (cond (nil? found) result
              ;; timeout? -> Exception, the result is never returned
              (.isInterrupted (Thread/currentThread)) (assoc result :result found :timeout true :no-steps counter)
              ;; overrun
              (>= counter limit 1) (assoc result :reduced found :no-steps counter :overrun true)
              ;; cycle
              (and cycle (some #(= found %) @steps)) (assoc result :reduced found :cycle true :steps @steps :no-steps counter)
              :else (recur found (assoc result :reduced found :no-steps counter) (inc counter)))))))

