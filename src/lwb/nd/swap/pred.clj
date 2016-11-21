; lwb Logic WorkBench -- Natural deduction, check for unify in pred

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.swap.pred
  (:require [lwb.nd.swap.common :refer :all]
            [lwb.pred :refer :all]
            [clojure.spec :as s]
            [lwb.nd.proof :as proof]
            [lwb.prop :as prop]))

(defn- body-type
  "In pred we can have three possibilities:        
  (1) :actual the statement is like `(actual ?1)`     
  (2) :fml    the statement is a formula
  (3) :equal  the statement is an equality
  Given a body returns the type."
  [body]
  (cond
    (and (seq? body) (= 'actual (first body)))   :actual
    (and (seq? body) (= '= (first body)))        :equal
    :else                                        :fml))

(defn- swap-type
  "In pred we can have three possibilities:        
  (1) :actual `old` occurs in an actual statement
  (2) :fml    `old` occurs in a formula
  (2) :equal  `old` occurs in an equality
  Given a vector with indexed bodies, returns this type         
  Requires the vector of indexed bodies is not empty."
  [proof old]
  (let [ib  (involved-bodies proof old)
        bts (set (map #(body-type (second %)) ib))]
    (cond
      (contains? bts :actual) :actual    ; order matters
      (contains? bts :fml)    :fml
      (contains? bts :equal)  :equal )))
      
;; Checks depending on the type of replacement

;; Unfortunately the specs for terms and formula of the predicate logic
;; involve a given signature.     
;; In natural deduction, we don't want to do so. So we need new specs for
;; terms and formulas

(s/def ::term keyword?)

(s/def ::predicate (s/and list? (s/cat :op symbol? :params (s/* ::term))))

(s/def ::equality (s/and list? (s/cat :op #(= '= %) :param1 ::term :param2 ::term)))

(s/def ::simple-expr (s/or :bool boolean?
                           :prop symbol?
                           :pred ::predicate
                           :eq   ::equality))

(s/def ::decl (s/coll-of symbol? :kind vector? :count 1))

(s/def ::quantified (s/and list? (s/cat :quantor quantor? :decl ::decl :fml ::fml)))

(defn- arity-ok? [{:keys [op params]}]
  (let [arity (prop/arity op)]
    (if (= arity -1) true
                     (= arity (count params)))))

(s/def ::op-expr (s/and list? (s/& (s/cat :op op? :params (s/* ::fml)) arity-ok?)))

(s/def ::compl-expr (s/or :op-expr ::op-expr
                          :quant   ::quantified))

(s/def ::fml (s/or :simple-expr ::simple-expr
                   :compl-expr  ::compl-expr))
(defn- check-fml
  "`new` must be a wellformed formula of predicate logic.        
   Exception otherwise."
  [new]
  (if-not (s/valid? ::fml new)
    (throw (Exception. (format "'%s' is not a valid formula of predicate logic." new)))))

(defn- check-equal
  "`new` must be an expression for a term.   
   Exception otherwise."
  [new]
  (if-not (s/valid? ::term new)
    (throw (Exception. (format "'%s' is not a term." new)))))

(defn- find-actual-plno
  "Find the pline in `proof `with an actual statzement containing `old`.        
   Require: the swap-type is `:actual`.     
   We expect that there is exactly one such `plno`."
  [proof old]
  (let [ib (involved-bodies proof old)
        rel-fn (fn [[_ body]] (and (seq? body) (= 'actual (first body))))
        ib' (filter rel-fn ib)]
    (first (map first ib'))))

(comment
  
  (def pr1
  [{:plid 1, :roth :premise, :body '(exists [x] (P x))}
   [{:plid 4, :body '(actual i), :roth :assumption, :refs nil}
    {:plid 5, :body '(P ?1), :roth :assumption, :refs nil}
    [{:plid 8, :body '(actual ?2), :roth :assumption, :refs nil}
     {:plid 9, :body :todo, :roth nil, :refs nil}
     {:plid 10, :body '(P ?2), :roth nil, :refs nil}]
    {:plid 7, :body '(forall [x] (P x)), :roth :forall-i, :refs [[8 10]]}]
   {:plid 2, :body '(forall [x] (P x)), :roth :exists-e, :refs [1 [4 7]]}] )
  
  (swap-type pr1 '?1)
  (swap-type pr1 '?2)
  
  (find-actual-plno pr1 '?1)
  (find-actual-plno pr1 '?2)
  
  (def s (proof/scope pr1 (proof/pline-at-plno pr1 4)))

  (take-while #(not= 8 (:plid %)) s)
  s
  )

(defn- fresh?
  "The element `new` is not already in scope."
  [proof plno new]
  (let [curr-scope (proof/scope proof (proof/pline-at-plno proof plno))
        plid (proof/plno->plid proof plno)
        ; we must consider only plines above the pline at plno
        curr-scope' (take-while #(not= plid (:plid %)) curr-scope)
        rel-fn (fn [body] (and (seq? body) (= 'actual (first body))))
        curr-elements (set (map second (filter rel-fn (map :body curr-scope'))))]
    #_(println curr-scope)
    (not (contains? curr-elements new))))

(defn- check-actual
  "We have to check that       
   (1) `new` is a keyword, and
   (2) that's a fresh one."
  [proof old new]
  (if (not (keyword? new))
    (throw (Exception. (format "'%s' must be a keyword indicating an element of the universe ." new))))
  (let [actual-plno (find-actual-plno proof old)]
    (if (not (fresh? proof actual-plno new))
        (throw (Exception. (format "'%s' must be a fresh state." new))))))

(defn check-swap
  "Check whether `old` and `new` can be swapped in `proof`.        
   Throws exception if not."
  [proof old new]
  (let [st (swap-type proof old)]
    (try
      (case st
        :actual (check-actual proof old new)
        :fml    (check-fml new)
        :equal  (check-equal new))
      (catch IllegalArgumentException e
        (throw (Exception. (format "There is no '%s' in the proof." old))))
      (catch Exception e
        (throw (Exception. (.getMessage e)))))))
