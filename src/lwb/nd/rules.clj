; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 Tobias Völzel, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.rules
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [lwb.nd.io :refer [rules theorems trivials]]
            [clojure.math.combinatorics :refer [permutations]]
            [clojure.spec :as s]))

;; # Transformation of rules and theorems to core.logic relations

;; ## Specification of rules and theorems

;; The following specs describe the internal representation of rules and so on

;; Expressions in rules
(s/def ::expr (s/or :list list? :symbol symbol?))

;; Key of rule or theorem
(s/def ::key keyword?)

;; Prerequisite for the application of the rule
(s/def ::prereq (s/nilable (s/coll-of ::expr :kind vector?)))

;; Given premises
(s/def ::given (s/coll-of ::expr :kind vector))

;; Conclusion
(s/def ::conclusion (s/coll-of ::expr :kind vector))

;; Applicability of the rule
(s/def ::forward (s/nilable boolean?))
(s/def ::backward (s/nilable boolean?))

;; Entity map for the body of a rule
(s/def ::rule-body (s/keys :un-req [::given ::conclusion ::prereq ::forward ::backward]))

;; Rule
(s/def ::rule (s/cat :key ::key :body ::rule-body))


;; NEW LOGIC (add additional keywords that should not be handled like symbols)
;; those "keywords" will not be handled as symbols but constants
(def keywords #{'truth 'contradiction 'true 'false})

;; ## Functions for generating the core.logic relations to represents a certain rule

(defn- gen-arg 
  "If the expr is a symbol, it's the argument.         
   If the expr is a list, we generate an alias from the operator together with a number."
  [expr n]
  (cond 
    (symbol? expr) expr
    (list? expr)   (symbol (str (first expr) n))
    :else (throw (Exception. (str "Can't generate argument for the logic relation from \"" expr "\"")))))
    
(defn- gen-args
  "Generates the top level arguments for the logic relation from the given premises.     
   e.g. `[a (and a b) (not b)] => [a and2 not3]`"
  [given]
  (let [numbers (take (count given) (iterate inc 1))]
    (vec (map gen-arg given numbers))))

;; The following is quite tricky

(defn- get-term-arg
  "Converts a given arg into a term argument       
   `a => (list a)`       
   `truth => (list (quote truth))` - \"truth\" is a keyword       
   `(and a b) => (list (concat (list (quote and)) (list a) (list b)))`       
   `[x (not y)] => (list (apply vector (concat (list x) (list (concat (list (quote not)) (list y))))))`       
   (all functions are written with their related namespace)"       
  [arg]
  (cond
    (contains? keywords arg) (list `list (list `quote arg))
    (symbol? arg) (list `list arg)
    (list? arg) (list `list (concat (list `concat) 
                                    (list (list `list (list `quote (first arg)))) 
                                    (map get-term-arg (rest arg))))
    (vector? arg) (list `list (list `apply `vector (concat (list `concat) (map get-term-arg arg))))
    :else (throw (Exception. (str "Can't generate term-argument from " arg)))))

(defn- gen-term
  "Converts a given list into a quoted sequence      
   `(and a b) => ``(~'and ~a ~b)`"
  [given]
  (let [args (map get-term-arg (rest given))
        operator (list `list (list `quote (first given)))
        result (conj args operator)]
    (conj (list (conj result `concat)) `seq)))

(defn- gen-body-row
  "Converts an argument and an given input into a unify-logic-row:     
   `[and1 (and a b)] -> (== and1 ``(~'and ~a ~b))`"
  [arg g]
  (cond 
    (contains? keywords g) `(== ~arg ~(list `quote arg))
    (symbol? g) ()
    (list? g) `(== ~arg ~(gen-term g))
    :else (throw (Exception. (str "Can't create body-row from " arg " " g)))))

(defn- gen-body
  "Generates all rows for the body of the function, removes empty ones"     
  [args given]
  (remove empty? (map gen-body-row args given)))

(defn- gen-fresh-arg
  "Extracts symbols in arg"
  [arg]
  (cond 
    (contains? keywords arg) []
    (symbol? arg) [arg]
    (list? arg) (vec (flatten (map gen-fresh-arg (rest arg))))
    (vector? arg) (vec (flatten (map gen-fresh-arg arg)))))

    ;(list? arg) (reduce #(concat %1 (gen-fresh-arg %2)) [] (rest arg))
    ;(vector? arg) (reduce #(concat %1 (gen-fresh-arg %2)) [] arg))) ;TODO Vektoren auslesen??

(defn- gen-fresh-args
  "Generates the arguments for the core.logic/fresh function"
  [given conclusion]
  (let [gvars (flatten (map gen-fresh-arg given))
        cvars (flatten (map gen-fresh-arg conclusion))]
    (vec (distinct (concat gvars cvars)))))

(defn- gen-result-row
  "Converts a result-variable and an input into a unify-logic-row    
   `q1 (and a b) => (== q1 ``(~'and ~a ~b))`"
  [q c]
  `(== ~q ~(cond 
              (contains? keywords c) (list `quote c)
              (symbol? c) c
              (list? c) (gen-term c))))

(defn- gen-result
  "Generates all rows for the conclusions"
  [conclusion qs]
  (map gen-result-row qs conclusion))

(defn- gen-prereq-row
  "Converts a function call from the prerequisites into a valid core.logic restriction"
  [prereq]
  `(== ~prereq true))

(defn- gen-prereqs
  "Generates all rows for the prerequisites"
  [prereqs fresh-args qs]
  (list (conj (map gen-prereq-row prereqs) 
              (vec (concat fresh-args qs)) `project)))

(defn- gen-logic-function
  "Takes given and conclusions from a rule and builds a core.logic relation that will represent that rule     
   e.g. \"and-i\" `[a b] => [(and a b)]`     
   `(fn [a b q1]`     
    `(fresh []`      
    `(== q1 `(~'and ~a ~b))))`"
  [prereq given conclusion]
  (let [qs   (map #(symbol (str %1 %2)) (take (count conclusion) (cycle ['q])) (take (count conclusion) (iterate inc 1)))
        args (gen-args given)
        fresh-args (apply vector (clojure.set/difference (set (gen-fresh-args given conclusion)) (set args)))
        body   (gen-body args given)
        result (gen-result conclusion qs)
        prereqs (when-not (nil? prereq) (gen-prereqs prereq fresh-args qs))
        fn-body (conj (concat body result prereqs) fresh-args `fresh)]
    `(fn ~(apply conj args qs)
       ~fn-body)))

;; ## Utility functions for rules

(defn- make-rule
  "Takes either a map or the name of an existing rule or theorem to create
   the appropriate core.logic-function for this rule"
  [rule]
  (cond
    (map? rule)
    (gen-logic-function (:prereq rule) (:given rule) (:conclusion rule))
  
    (string? rule)
    (let [r (or ((keyword rule) @rules) ((keyword rule) @theorems))]
      (gen-logic-function (:prereq r) (:given r) (:conclusion r)))
    :else (throw (Exception. (str "The argument you provided is neither a legal rule-map nor the name of a valid rule or theorem (" rule ")")))))

(defn get-rule
  "Returns the rule/theorem if it exists"
  [name]
  (let [rule ((keyword name) @rules)
        theorem ((keyword name) @theorems)]
    (or rule 
        (or theorem (throw (Exception. (str "A rule/theorem \"" name "\" doesn't exist")))))))

(defn rule-exist?
  "Does a certain rule/theorem exist?"
  [name]
  (if (or ((keyword name) @rules)
          ((keyword name) @theorems)) true false))
 
(defn rule-givens
  "Returns the number of givens for the certain rule/theorem"
  [name]
  (let [rule (or ((keyword name) @rules) ((keyword name) @theorems))]
    (count (:given rule))))
 
(defn rule-conclusions
  "Returns the number of conclusions for the certain rule/theorem"
  [name]
  (let [rule (or ((keyword name) @rules) ((keyword name) @theorems))]
    (count (:conclusion rule))))

(defn rule-forward?
  "Returns true if the rule/theorem can be used forwards"
  [name]
  (if-let [rule (or ((keyword name) @rules) ((keyword name) @theorems))]
    (if (:forward rule)
      true
      false)
    (throw (Exception. (str "A rule/theorem \"" name "\" doesn't exist")))))

(defn rule-backward?
  "Returns true if the rule/theorem can be used backwards"
  [name]
  (if-let [rule (or ((keyword name) @rules) ((keyword name) @theorems))]
    (if (:backward rule)
      true
      false)
    (throw (Exception. (str "A rule/theorem \"" name "\" doesn't exist")))))
         
;; NOTE - right now "apply-rule" can't separate arguments from lines and user-inputs, which may cause absurd behavior 

(defn apply-rule 
  "Applies the rule/theorem (rule [string or map]) either forward or backward (forward?) 
   on the given parameters (args & optional).     
   The obligatory arguments (args) will always be the first arguments passed to 
   the core.logic relation (in different permutations).     
   The optional arguments (optional) will be mixed with the generated logical arguments and  
   will passed last to the core.logic relation (in different permutations).     
   This way it is ensured that \"given\"-arguments will never handled as \"conclusion\"-arguments and vice versa."
  [rule forward? args & [optional]]
  (let [rule-map (cond
                   (map? rule) rule
                   (string? rule) (or ((keyword rule) @rules) ((keyword rule) @theorems))
                   :else (throw (Exception. "RULES | Wrong type of argument: \"rule\" has to be a string or a map.")))
        frule (if forward? rule-map (assoc rule-map :given (:conclusion rule-map) :conclusion (:given rule-map)))
        obligatory-args (map #(conj (list %) `quote) args)
        optional-args   (map #(conj (list %) `quote) optional)
        logic-args-num (- (+ (count (:given frule)) (count (:conclusion frule)))
                          (+ (count obligatory-args) (count optional-args)))
        logic-args (vec (map #(symbol (str %1 %2))
                                 (take logic-args-num (cycle ['q]))
                                 (take logic-args-num (iterate inc 1))))
        fn (eval (make-rule frule))
        results (if (empty? optional)
                (for [x (permutations obligatory-args)]
                  (eval (list `run* logic-args (conj (concat x logic-args) fn))))
                (for [x (permutations obligatory-args)
                      y (permutations (concat optional-args logic-args))]
                  (eval (list `run* logic-args (conj (concat x y) fn)))))]
    (map first (remove empty? results))))

(defn apply-trivials
  "Applies all trivial theorems to the given form and returns the first successful result.     
   To extend the predefined trivial theorems use the \"import-trivials\" function (ns: io)"
  [form]
  (let [names (map #(subs % 1) (map str (map key @trivials)))
        f (fn [name arg]
            (run* [q] ((eval (make-rule ((keyword name) @trivials))) arg q)))
        results (map #(f % form) names)
        res (first (drop-while empty? results))]
    res))

