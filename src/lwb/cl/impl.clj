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

(defn get-rule-fn
  "Get logic function from combinator store."
  [comb-key]
  (if-let [f (get-in @combinator-store [comb-key :logic-rel])]
    f
    (throw (ex-error (str "Combinator " comb-key " not defined.")))))

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

(comment
  ;; Implementation with walk/prewalk)
  (defn apply**
    [sterm comb-key]
    ;; Pre:  combinator is defined
    (let [rule-fn (get-in @combinator-store [comb-key :logic-rel])
          comb (symbol (name comb-key))
          counter (atom 0)]
      (walk/prewalk
        #(let [do? (= comb (first (flatten %)))
               rep (if do? (apply' rule-fn % :red) ())]
           (if-let [replace (first rep)]
             (if (= 1 (swap! counter inc)) replace %)
             %))
        sterm)))

  (def apply**m
    (memoize apply**))
  )

;; Implementation with zipper
;; is a bit more performant, but not much :)

(defn apply-z
  "One-step reduction"
  [sterm comb-key]
  (let [rule-fn (get-rule-fn comb-key)
        comb (symbol (name comb-key))]
    (loop [loc (zip/seq-zip sterm)]
      (let [node (zip/node loc)
            new-node (if (= comb (first (flatten node))) 
                       (if-let [r (first (apply' rule-fn node :red))] r node)
                       node)
            new? (not= new-node node)
            new-loc (if new?
                        (zip/replace loc new-node)
                        loc)
            new-state (if new? :stop)
            next-loc (zip/next new-loc)]
          (if (or (zip/end? loc) (= :stop new-state))
            (zip/root new-loc)
            (recur next-loc))))))

(defn apply-z'
  "Multi-step reduction.
   Beware of infinite loops! Should be called inside `(with-timeout ...)`."
  [sterm comb-key]
  (let [rule-fn (get-rule-fn comb-key)
        comb (symbol (name comb-key))]
    (loop [loc (zip/seq-zip sterm)]
      (let [node (zip/node loc)
            new-node (if (= comb (first (flatten node)))
                       (if-let [r (first (apply' rule-fn node :red))] r node)
                       node)
            new? (not= new-node node)
            new-loc (if new?
                      (zip/replace loc new-node)
                      loc)
            next-loc (zip/next new-loc)]
        (cond (zip/end? next-loc) ;; must be next-loc to prevent a replace at the :end of the zipper!
                (zip/root new-loc)
              ;; timeout? Stops if thread is interruopted
              (.isInterrupted (Thread/currentThread)) (zip/root new-loc)
              :else (recur next-loc))))))

;; Weak reduction -------------------------------------------------------------

(defn vec'
  [sterm]
  (if (symbol? sterm) [sterm] (vec sterm)))

(comment
  ;; old version
  (defn weak-reduce
    "Reduce the `sterm` using the set `combs` of combinators "
    [sterm limit cycle trace timeout]
    (if trace (println (str 0 ": " (vec' (min-parens-seq sterm)))))
    (let [steps (atom [])
          start-time (System/currentTimeMillis)
          timeouts (* timeout 1000)]
      (loop [current-sterm sterm
             result {:reduced sterm :no-steps 0 :cycle :unknown :overrun false}
             counter 1]
        (if cycle (swap! steps conj current-sterm))
        (let [combs (combs-keys [current-sterm])
              found (first (filter #(not= current-sterm %) (for [s combs] (apply-z' current-sterm s))))]
          (if (and found trace) (println (str counter ": " (vec' (min-parens-seq found)))))
          (cond (nil? found) result
                ;; timeout
                (and (> timeouts 0) (> (- (System/currentTimeMillis) start-time) timeouts)) (assoc result :reduced found :timeout counter)
                ;; overrun
                (> counter limit) (assoc result :reduced found :overrun true)
                ;; cycle
                (and cycle (some #(= found %) @steps)) (assoc result :reduced found :cycle true :steps @steps)
                :else (recur found (assoc result :reduced found :no-steps counter) (inc counter)))))))
  )


(defn weak-reduce
  "Reduce the `sterm` using the set `combs` of combinators "
  [sterm combs counter algo timeout limit cycle trace]
  (let [steps (atom [])
        algo (if (= algo :one-step-red) apply-z apply-z')]
    (loop [current-sterm sterm
           result {:reduced sterm :no-steps 0 :cycle :unknown :overrun false}
           counter counter]
      (if cycle (swap! steps conj current-sterm))
      (let [found (first (filter #(not= current-sterm %) (for [s combs] (algo current-sterm s))))]
        (if (and found trace) (println (str counter ": " (vec' (min-parens-seq found)))))
        (cond (nil? found) result
              ;; timeout? -> Exception, the result is never returned
              (.isInterrupted (Thread/currentThread)) (assoc result :result found :timeout true :no-steps counter)
              ;; overrun
              (>= counter limit 1) (assoc result :reduced found :no-steps counter :overrun true)
              ;; cycle
              (and cycle (some #(= found %) @steps)) (assoc result :reduced found :cycle true :steps @steps :no-steps counter)
              :else (recur found (assoc result :reduced found :no-steps counter) (inc counter)))))))

