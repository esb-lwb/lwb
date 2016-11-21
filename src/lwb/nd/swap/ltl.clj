; lwb Logic WorkBench -- Natural deduction, check for unify in ltl

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.swap.ltl
  (:require [lwb.nd.swap.common :refer :all]
            [lwb.ltl :refer :all]
            [lwb.nd.proof :as proof]
            [clojure.spec :as s]
            [lwb.nd.proof :as proof]))

(defn- body-type
  "In ltl we can have three possibilities:        
  (1) :state the statement is a proposition at a certain state, e.g. `(at [i] A)`        
  (2) :rel   a relational statement, e.g. `(<= i j)` or `(succ i j)`
  (3) :alone the statement is not relational and has no state e.g `V2`.     
  Given a body returns the type."  
  [body]
  (cond
    (symbol? body)       :alone    ; order of conds relevant!!
    (= 'at (first body)) :state
    :else                :rel))

(defn- separate-state
  "Splits an non-relational proposition into the state and the proposition in that state.       
   e.g. `(at [i] (and A B))` ->  `[i (and A B)]       
   Requires: `body` is not a relational statement."
  [body]
  [(first (second body)) (nth body 2)])

(defn- swap-type
  "In ltl we can have four possibilities:        
  (1) :alone `old` occurs alone in a body      
  (2) :state `old` occurs in at and only there       
  (3) :rel   `old` occurs in a relational statement     
  (4) :prop  `old` occurs in the proposition at a certain state       
  Given a vector with indexed bodies, returns this type         
  Requires the vector of indexed bodies is not empty."
  [proof old]
  (let [ib  (involved-bodies proof old)
        bts (set (map #(body-type (second %)) ib))]
    (cond
      (= #{:alone} bts)    :alone
      (contains? bts :rel) :rel
      (= #{:state} bts)    (let [[state prop] (separate-state (second (first ib)))]
                             (if (= old state) :state :prop)))))

(comment
  (def pr1 [{:plid 1, :roth :premise, :body '(at [i] (finally A))}
            [{:plid 4, :body '(<= i j), :roth :assumption, :refs nil}
             {:plid 5, :body '(at [j] A), :roth :assumption, :refs nil}
             {:plid 6, :body :todo, :roth nil, :refs nil}
             {:plid 7, :body 'V1, :roth nil, :refs nil}]
            {:plid 8, :body 'V1, :roth :finally-e, :refs [1 [4 7]]}
            {:plid 3, :body :todo, :roth nil}
            {:plid 2, :body '(at [j] X), :roth nil}])

  (def pr2 [{:plid 1, :roth :premise, :body '(at [i] (finally A))}
            [{:plid 4, :body '(<= V1 j), :roth :assumption, :refs nil}
             {:plid 5, :body '(at [j] A), :roth :assumption, :refs nil}
             {:plid 6, :body :todo, :roth nil, :refs nil}
             {:plid 7, :body :todo, :roth nil, :refs nil}]
            {:plid 3, :body :todo, :roth nil}
            {:plid 2, :body 'X, :roth nil}])

  (def pr3 [{:plid 1, :roth :premise, :body '(at [i] (finally A))}
            [{:plid 4, :body '(<= i V1), :roth :assumption, :refs nil}
             {:plid 5, :body '(at [j] A), :roth :assumption, :refs nil}
             {:plid 6, :body :todo, :roth nil, :refs nil}
             {:plid 7, :body :todo, :roth nil, :refs nil}]
            {:plid 3, :body :todo, :roth nil}
            {:plid 2, :body 'X, :roth nil}])

  (def pr4 [{:plid 1, :roth :premise, :body '(at [i] (finally A))}
            [{:plid 4, :body '(<= i j), :roth :assumption, :refs nil}
             {:plid 5, :body '(at [j] V1), :roth :assumption, :refs nil}
             {:plid 6, :body :todo, :roth nil, :refs nil}
             {:plid 7, :body :todo, :roth nil, :refs nil}]
            {:plid 3, :body :todo, :roth nil}
            {:plid 2, :body 'X, :roth nil}])

  (def pr5 [{:plid 1, :roth :premise, :body '(at [V1] (finally A))}
            [{:plid 4, :body '(<= i j), :roth :assumption, :refs nil}
             {:plid 5, :body '(at [j] A), :roth :assumption, :refs nil}
             {:plid 6, :body :todo, :roth nil, :refs nil}
             {:plid 7, :body :todo, :roth nil, :refs nil}]
            {:plid 3, :body :todo, :roth nil}
            {:plid 2, :body 'X, :roth nil}])

  (swap-type pr1 'V1)
  (swap-type pr2 'V1)
  (swap-type pr3 'V1)
  (involved-bodies pr4 'V1)
  (swap-type pr4 'V1)
  (swap-type pr5 'V1)

  (def pr6 [{:plid 1, :roth :premise, :body '(at [i] (finally A))}
            [{:plid 4, :body '(<= i ?1), :roth :assumption, :refs nil}
             {:plid 5, :body '(at [?1] A), :roth :assumption, :refs nil}
             {:plid 6, :body :todo, :roth nil, :refs nil}
             {:plid 7, :body '(at [j] A), :roth nil, :refs nil}]
            {:plid 2, :body '(at [j] A), :roth :finally-e, :refs [1 [4 7]]}])
  (swap-type pr6 '?1)
  )

; Checks depending of type of replacement

(defn- check-alone
  [new]
  "`new` must be a wellformed ltl formula at a certain state.       
   Exception otherwise."
  (if-not (s/valid? :lwb.ltl/at-fml new)
    (throw (Exception. (format "'%s' is not a valid ltl formula at a certain state." new)))))

(defn- check-state
  [new]
  "`new` must be a symbol named with a single small character.       
   Exception otherwise."
  (if-not (and (symbol? new) (= 1 (count (name new))) (let [ch (first (name new))] (<= (int \a) (int ch) (int \z))))
    (throw (Exception. (format "'%s' is not a valid state symbol." new)))))

(defn- find-rel-plno
  "Find the pline in `proof `with a relational expression containing `old`.        
   Require: the swap-type is `:rel`.     
   We expect that there is exactly one such `plno`."
  [proof old]
  (let [ib (involved-bodies proof old)
        rel-fn (fn [[_ body]] (not= 'at (first body)))  ; filter bodies without state
        ib' (filter rel-fn ib)
        rel-fn' (fn [[_ body]] (and (seq? body) (= 3 (count body)) (or (= old (second body)) (= old (nth body 2)))))
        ib'' (filter rel-fn' ib')]
    (first (map first ib''))))

(defn- assumption?
  "The pline at `plno` in `proof` is an assumption?      
   Requires: `plno` is valid."
  [proof plno]
  (= :assumption (:roth (proof/pline-at-plno proof plno))))

(defn- fresh?
  "The state `new` is not already in scope."
  [proof plno new]
  (let [curr-scope (proof/scope proof (proof/pline-at-plno proof plno))
        plid (proof/plno->plid proof plno)
        ; we must consider only plines above the pline at plno
        curr-scope' (take-while #(not= plid (:plid %)) curr-scope)
        rel-fn (fn [body] (and (seq? body) (= 'at (first body))))
        curr-states (set (map #(first (second %)) (filter rel-fn (map :body curr-scope'))))]
    (not (contains? curr-states new))))

(defn- succ?
  "The relational expression at `plno` of `proof` is `succ`.      
   Requires: `plno` is valid and has a relational expression as body."
  [proof plno]
  (= 'succ (first (:body (proof/pline-at-plno proof plno)))))

(defn- fresh-in-succ?
  "The new state `new` is fresh in the expression `body` with operator `succ`.     
   Requires: `body` is such an expression."
  [body old new]
  (let [arg1 (second body)
        arg2 (nth body 2)]
    (if (= arg1 old) (not= arg2 new) (not= arg1 new))))

(defn- check-rel
  "We have to consider the follwing cases:       
   expression `succ`: the `new` state must be different from the other one       
   expression `<=` and `:roth` = `:assumption`: the `new` state must be fresh one"
  [proof old new]
  ; new is a symbol for a state
  (check-state new)
  (let [rel-plno (find-rel-plno proof old)]
    (cond
      (succ? proof rel-plno)
      (if (not (fresh-in-succ? (:body (proof/pline-at-plno proof rel-plno)) old new))
        (throw (Exception. (format "'%s' must differ from the other argument in a succ expression." new))))
      (assumption? proof rel-plno)
      (if (not (fresh? proof rel-plno new))
        (throw (Exception. (format "'%s' must be a fresh state." new)))))))

(defn- check-prop
  "`new` must be a wellformed ltl formula.       
  Exception otherwise."
  [new]
  (if-not (s/valid? :lwb.ltl/fml new)
    (throw (Exception. (format "'%s' is not a valid ltl formula." new)))))

(defn check-swap
  "Check whether `old` and `new` can be swapped in `proof`.        
   Throws exception if not."
  [proof old new]
  (let [st (swap-type proof old)]
    (try 
    (case st
      :alone (check-alone new)
      :state (check-state new)
      :rel   (check-rel proof old new)
      :prop  (check-prop new))
    (catch IllegalArgumentException e
      (throw (Exception. (format "There is no '%s' in the proof." old))))
    (catch Exception e 
      (throw (Exception. (.getMessage e)))))))
