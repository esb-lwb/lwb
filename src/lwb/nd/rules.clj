; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 Tobias Völzel, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.rules
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]))

;; global storage for the rules and theorems (roths, so to say) of the current logic.

(def roths
  "Global storage for rules and theorems"
  (atom {}))

(defn reset-roths
  "Resets the internal storage for rules"
  [] (reset! roths {}))

;; Magic symbol for the logic variable for the conclusion in the logic relation
;; This symbol should not be used as the name of a proposition in rules and theorems
(def conclusion-number
  "Magic number for the logic variable for the conclusion in the logic relation.    
   Symbols of the form `qnnnn`should not be used as the name of a proposition in rules and theorems."
  3961)

(def reserved
  "Reserved terms in construction the logic relation."
  '#{not and or impl infer
     forall exists actual = substitution
     at succ atnext always finally until <=});; # Transformation of rules and theorems to core.logic relations



;; NEW LOGIC (add additional keywords that should not be handled like symbols)
;; those "keywords" will not be handled as symbols but constants
(def keywords #{'truth 'contradiction 'true 'false})

;; ## Functions for the structure of the rule or theorem

;; the structure of the roth determines how to use the rule or theorem
;; and which steps are okay

;; Naming conventions for the structure of a rule or theorem

;; `g` = given
;; `c` = conclusion
;; `e` = extra input not part of the current proof
;; `?` = queried parameter of the logic relation

;; `m` = mandatory
;; `o` = optional
;; `1` = at least one them
;; `b` = at most all but one of them


(defn- map-givens-f
  [idx given]
  (cond
    (and (list? given) (= 'infer (first given))) :g?
    (and (list? given) (= 'succ (first given))) :gm
    (zero? idx) :gm
    :else :go
    ))

(defn- map-givens-b
  [idx given]
  (cond
    (and (list? given) (= 'infer (first given))) :g?
    (and (list? given) (= 'substitution (first given))) :g?
    (zero? idx) :go
    :else :gb
    ))

(defn roth-structure-f
  "Structure of the logic relation of the rule or theorem.
   Result e.g.: `[:gm :gm :g? :g? :co]`      "
  [given extra conclusion]
  (let [has-actual (some #(and (list? %) (= 'actual (first %))) given)
        has-subst (some #(and (list? %) (= 'substitution (first %))) given)
        concl-cnt (count conclusion)
        vg1 (vec (concat (map-indexed map-givens-f given) (map (constantly :em) extra)))
        ; in case we have :given of the form (actual t) or extra input -> all givens are mandatory
        vg2 (if (or has-actual (contains? (set vg1) :em)) (mapv #(if (= % :go) :gm %) vg1) vg1)
        ; in case we have mandatory and optional arguments -> just one of them must be provided
        vg (if (= (set vg2) #{:gm :go}) (mapv (constantly :g1) vg2) vg2)]
    (cond
      (= (first vg) :g?) nil                                ; first given is an infer -> backward only
      has-subst nil                                         ; a given is a substitution -> backward only
      (and (= concl-cnt 1) (contains? (set vg) :g?)) (conj vg :co)
      :else (into vg (repeat concl-cnt :c?)))))

(defn roth-structure-b
  "Structure of the logic relation of the rule or theorem with the given `id`.      
   Result e.g.: `[:cm :gb :gb]`"
  [given extra conclusion]
  (let [is-subst (contains? (set (flatten conclusion)) 'substitution)
        vg1 (map-indexed map-givens-b given)
        ; in case we have only :go and :gb -> all :gb
        vg2 (if (= (set vg1) #{:go :gb}) (mapv (constantly :gb) vg1) vg1)
        ; in case we have just one given
        vg (if (= (count vg2) 1) [:g?] vg2)]
    (cond
      (> (count conclusion) 1) nil                          ; multiple conclusions -> forwad only
      (not-empty extra) nil                                 ; extra input -> forward only
      (empty? vg) nil                                       ; rule is axiom -> forward only
      is-subst nil                                          ; conclusion is a substitution -> forward only
      :else (into [:cm] vg))))

;; the following two functions are just for test purposes

(defn roth-structure-forward
  "Structure of the logic relation of the rule or theorem with the given `roth-id`.      
   Result e.g.: `[:gm :gm :g? :g? :co]`"
  [roth-id]
  (:forward (roth-id @roths)))

(defn roth-structure-backward
  "Structure of the logic relation of the rule or theorem with the given `roth-id`.      
   Result e.g.: `[:cm :gb :gb]`"
  [roth-id]
  (:backward (roth-id @roths)))

;; ## Functions for generating the core.logic relations that represent roths

(defn- gen-arg
  "If the expr is a symbol, it's the argument.         
   If the expr is a list, we generate an alias from the operator together with a number."
  [expr n]
  (cond
    (symbol? expr) expr
    (list? expr) (symbol (str (first expr) n))
    :else (throw (Exception. (str "Can't generate argument for the logic relation from \"" expr "\"")))))

(defn- gen-args
  "Generates the top level arguments for the logic relation from the given premises and potentially extra arguments..     
   e.g. `[a (and a b) (not b)] => [a and2 not3]`"
  [given]
  (let [numbers (take (count given) (iterate inc 1))]
    (mapv gen-arg given numbers)))



(defn- gen-term
  "Converts a given list into a quoted sequence      
   '(and a b) => (list (quote and) a b)"
  [arg]
  (cond
    (contains? keywords arg) (list `quote arg)
    (symbol? arg) arg
    (vector? arg) (mapv gen-term arg)
    (and (list? arg) (contains? reserved (first arg))) (let [op (list `quote (first arg))
                                                             params (mapv gen-term (rest arg))]
                                                         (list* `list op params))
    (and (list? arg) (not (contains? reserved (first arg)))) (let [op (first arg)
                                                                   params (mapv gen-term (rest arg))]
                                                               (list* `list op params))
    ))

(defn- gen-body-row
  "Converts an argument and an given input into a unify row for the logic relation     
   `[and1 (and a b)] -> (== and1 ``(~'and ~a ~b))`"
  [arg g]
  (cond
    (contains? keywords g) `(== ~arg ~(list `quote arg))
    (symbol? g) ()
    (list? g) `(== ~arg ~(gen-term g))
    :else (throw (Exception. (str "Can't create unify constraint from " arg " " g)))))

(defn- gen-body
  "Generates all rows for the body of the function, removes empty ones"
  [args given]
  (remove empty? (map gen-body-row args given)))

(defn- gen-fresh-arg
  "Extracts symbols in arg"
  [arg]
  (cond
    (contains? keywords arg) []
    (contains? reserved arg) []
    (symbol? arg) [arg]
    (list? arg) (vec (flatten (map gen-fresh-arg arg)))
    (vector? arg) (vec (flatten (map gen-fresh-arg arg)))))

(defn- gen-fresh-args
  "Generates the arguments for the fresh function in the logic relation"
  [given conclusion]
  (let [gvars (flatten (map gen-fresh-arg given))
        cvars (flatten (map gen-fresh-arg conclusion))]
    (vec (distinct (concat gvars cvars)))))

(defn- gen-conclusion-row
  "Converts a conclusion variable and an input into a unify row for the logic relation
   `q1 (and a b) => (== q1 ``(~'and ~a ~b))`"
  [q c]
  `(== ~q ~(cond
             (contains? keywords c) (list `quote c)
             (symbol? c) c
             (list? c) (gen-term c))))

(defn- gen-conclusions
  "Generates all rows for the conclusions"
  [conclusion qs]
  (map gen-conclusion-row qs conclusion))

(defn- gen-prereq-row
  "Converts a function call from the prerequisites into a valid core.logic restriction"
  [prereq]
  `(== ~prereq true))

(defn- gen-prereqs
  "Generates all rows for the prerequisites for the project in core.logic,     
  i.e. `project [args] (== (prereq ...) true)`"
  [prereqs fresh-args qs]
  (list (conj (map gen-prereq-row prereqs)
              (conj fresh-args qs) `project)))

(defn gen-roth-relation
  "Takes the speccification of a rule or theorem and builds a core.logic relation that represents that roth     
   e.g. \"and-i\" `[a b] => [(and a b)]`     
   `(fn [a b q1]`     
    `(fresh []`      
    `(== q1 `(~'and ~a ~b))))`"
  [prereq given extra conclusion]
  (let [qs (mapv #(symbol (str %1 %2)) (take (count conclusion) (cycle ['q])) (take (count conclusion) (iterate inc conclusion-number)))
        allargs (vec (concat given extra))
        args (gen-args allargs)
        fresh-args (apply vector (clojure.set/difference (set (gen-fresh-args given conclusion)) (set args)))
        body (gen-body args given)
        concs (gen-conclusions conclusion qs)
        prereqs (when-not (nil? prereq) (gen-prereqs prereq fresh-args qs))
        fn-body (conj (concat body concs prereqs) fresh-args `fresh)]
    `(fn ~(apply conj args qs)
       ~fn-body)))

;; ## Utility functions for roths

(defn make-relation
  "Gives the code for the logic relation for the given `id` of a roth in `@roths`.    
   The functions helps to check that the generation process is okay."
  [roth-id]
  (if (contains? @roths roth-id)
    (let [r (roth-id @roths)]
      (gen-roth-relation (:prereq r) (:given r) (:extra r) (:conclusion r)))
    (throw (Exception. (str roth-id " not found in @roths.")))))

(comment
(make-relation :and-i)
(make-relation :exists-e)
(make-relation :not-forall->exists-not)
(def r
  {:given '[(not (forall [x] (P x)))], :conclusion '[(exists [x] (not (P x)))]}
  )
(gen-roth-relation (:prereq r) (:given r) (:extra r) (:conclusion r))
)

(defn roth-exists?
  "Does a certain rule/theorem exist?"
  [roth-id]
  (contains? @roths roth-id))

(defn roth-forward?
  "Returns true if the rule/theorem can be used forwards"
  [roth-id]
  (some? (:forward (roth-id @roths))))

(defn roth-pattern
  "The call pattern for a application of the roth.    
   mode can be `:forward` or `:backward`. "
  [roth-id mode]
  (mode (roth-id @roths)))

(defn roth-backward?
  "Returns true if the rule/theorem can be used backwards"
  [roth-id]
  (some? (:backward (roth-id @roths))))

(defn- gen-relargs
  "Gets the arguments for the logic relation from args."
  [args]
  (let [counter (atom -1)
        tr (fn [arg] (if (= :? arg)
                       (symbol (str \q (swap! counter inc)))
                       (list `quote arg)))]
    (mapv tr args)))

;; This is the function that actually runs core.logic 

(defn apply-roth
  "Takes the `roth-id` of a rule or a theorem and applies the according logical relation,
   with the given `args` where `:?` stands for the unknown.     
   Requires: a roth with `roth-id` exists      
             the global atom `roths` provides the logic relation for the roth in `:logic-rel`  
   Returns:  a vector of formulas which size is equal to the number of `:?`s in the args."
  [roth-id args]
  (let [rel-args (gen-relargs args)
        run-args (vec (filter symbol? rel-args))
        result (first (eval (list `run* run-args (list* (:logic-rel (roth-id @roths)) rel-args))))]
    (if-not (vector? result) [result] result)))

