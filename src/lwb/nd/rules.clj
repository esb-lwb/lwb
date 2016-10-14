; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 Tobias Völzel, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.rules
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [lwb.nd.storage :refer [roths theorems]]
            [clojure.math.combinatorics :refer [permutations]]
            [clojure.spec :as s]))

;; # Transformation of rules and theorems to core.logic relations

;; ## Specification of rules and theorems

;; The following specs describe the internal representation of rules and so on

;; Expressions in rules
(s/def ::expr (s/or :list list? :symbol symbol?))

;; Id of rule or theorem
(s/def ::id keyword?)

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
(s/def ::rule (s/cat :id ::id :body ::rule-body))


;; NEW LOGIC (add additional keywords that should not be handled like symbols)
;; those "keywords" will not be handled as symbols but constants
(def keywords #{'truth 'contradiction 'true 'false})

;; ## Functions for the structure of the rule or theorem

(defn roth-structure
  "Structure of rule or theorem with the given `id`.      
   Result is a map `:no-given`, `no-infer`.
   requires: `id` is a valid rule id."
  [id]
  (let [roth (id @roths)
        all  (count (:given roth))
        no-infer (count (filter #(and (list? %) (= 'infer (first %))) (:given roth)))]
    {:no-given (- all no-infer) :no-infer no-infer}))

;; the structure of the roth determines how to use the rule or theorem
;; and which steps are okay

;; ## Functions for generating the core.logic relations that represent rules

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
    (mapv gen-arg given numbers)))

(defn- gen-term
  "Converts a given list into a quoted sequence      
   '(and a b) => (list (quote and) a b)"
  [arg]
  (cond
    (contains? keywords arg) (list `quote arg)
    (symbol? arg) arg
    (vector? arg) (mapv gen-term arg)
    (list? arg) (let [op (list `quote (first arg))
                      params (mapv gen-term (rest arg))]
                  (list* `list op params))
    ))

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

(defn gen-logic-function
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

(defn make-rule
  "Takes either a map or the id of an existing rule or theorem to create
   the appropriate core.logic-function for this rule"
  [rule]
  (cond
    (map? rule)
    (gen-logic-function (:prereq rule) (:given rule) (:conclusion rule))
  
    (keyword? rule)
    (let [r (or (rule @roths) (rule @theorems))]
      (gen-logic-function (:prereq r) (:given r) (:conclusion r)))
    :else (throw (Exception. (str "The argument you provided is neither a legal rule-map nor the id of a valid rule or theorem (" rule ")")))))

(defn get-rule
  "Returns the rule/theorem if it exists"
  [id]
  (let [rule (id @roths)
        theorem (id @theorems)]
    (or rule 
        (or theorem (throw (Exception. (str "A rule/theorem \"" id "\" doesn't exist")))))))

(defn rule-exist?
  "Does a certain rule/theorem exist?"
  [id]
  (if (or (id @roths)
          (id @theorems)) true false))
 
(defn rule-givens
  "Returns the number of givens for the certain rule/theorem"
  [id]
  (let [rule (or (id @roths) (id @theorems))]
    (count (:given rule))))
 
(defn rule-conclusions
  "Returns the number of conclusions for the certain rule/theorem"
  [id]
  (let [rule (or (id @roths) (id @theorems))]
    (count (:conclusion rule))))

(defn rule-forward?
  "Returns true if the rule/theorem can be used forwards"
  [id]
  (if-let [rule (or (id @roths) (id @theorems))]
    (if (:forward rule)
      true
      false)
    (throw (Exception. (str "A rule/theorem \"" id "\" doesn't exist")))))

(defn rule-backward?
  "Returns true if the rule/theorem can be used backwards"
  [id]
  (if-let [rule (or (id @roths) (id @theorems))]
    (if (:backward rule)
      true
      false)
    (throw (Exception. (str "A rule/theorem \"" id "\" doesn't exist")))))
         
;; NOTE - right now "apply-rule" can't separate arguments from lines and user-inputs, which may cause absurd behavior 

#_(defn apply-rule 
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
                   (keyword? rule) (or (rule @roths) (rule @theorems))
                   :else (throw (Exception. "RULES | Wrong type of argument: \"rule\" has to be a keyword or a map.")))
        frule (if forward? rule-map (assoc rule-map :given (:conclusion rule-map) :conclusion (:given rule-map)))
        obligatory-args (map #(conj (list %) `quote) args)
        optional-args   (map #(conj (list %) `quote) optional)
        logic-args-num (- (+ (count (:given frule)) (count (:conclusion frule)))
                          (+ (count obligatory-args) (count optional-args)))
        logic-args (mapv #(symbol (str %1 %2))
                                 (take logic-args-num (cycle ['q]))
                                 (take logic-args-num (iterate inc 1)))
        fn (eval (make-rule frule))
        results (if (empty? optional)
                (for [x (permutations obligatory-args)]
                  (eval (list `run* logic-args (conj (concat x logic-args) fn))))
                (for [x (permutations obligatory-args)
                      y (permutations (concat optional-args logic-args))]
                  (eval (list `run* logic-args (conj (concat x y) fn)))))]
    (map first (remove empty? results))))

; apply-rule without permutations

; without permutations on must in certain situations use unify
; but on the other side one needs not to make choices
; but: if we have more  than 2 givens and make a step backward
;      from the concept of the rules does the order of givens not play a role!!

(defn apply-rule
  [rule forward? args & [optional]]
  (let [rule-map (cond
                   (map? rule) rule
                   (keyword? rule) (or (rule @roths) (rule @theorems))
                   :else (throw (Exception. "Wrong type of argument: \"rule\" has to be a keyword or a map.")))
        frule (if forward? rule-map (assoc rule-map :given (:conclusion rule-map) :conclusion (:given rule-map)))
        obligatory-args (mapv #(list `quote %) args)
        optional-args   (mapv #(list `quote %) optional)
        logic-args-num (- (+ (count (:given frule)) (count (:conclusion frule)))
                          (+ (count obligatory-args) (count optional-args)))
        logic-args (mapv #(symbol (str %1 %2))
                         (take logic-args-num (cycle ['q]))
                         (take logic-args-num (iterate inc 1)))
        logic-rel (eval (make-rule frule))]
        (eval (list `run* logic-args (list* logic-rel (concat obligatory-args optional-args logic-args))))))

(comment

  (apply-rule :and-i false '[(and A B)])
  (apply-rule' :and-i false '[(and A B)])
  (apply-rule' :and-i true '[A B])
  (apply-rule :or-e false '[X])
  (apply-rule' :or-e false '[X])
  (apply-rule :or-e false '[X] '[(or A B)])
  (apply-rule' :or-e false '[X] '[(or A B)])

  (def args '[A B])
  (map #(conj (list %) `quote) args)
  (def args' (map #(list `quote %) args))
  args'

  (def fnx (eval (make-rule :and-i)))
  (make-rule :and-i)

  (def logic-args-num 1)
  (def logic-args (mapv #(symbol (str %1 %2))
                        (take logic-args-num (cycle ['q]))
                        (take logic-args-num (iterate inc 1))))
  logic-args

  (list 'run* logic-args (conj (concat args' logic-args) fnx))
  (eval (list 'run* logic-args (conj (concat args' logic-args) fnx)))

  (concat args' logic-args)
  (list `run* logic-args (list* fnx (concat args' logic-args)))
  (eval (list `run* logic-args (list* fnx (concat args' logic-args))))
  )

#_(defn apply-trivials
  "Applies all trivial theorems to the given form and returns the first successful result.     
   To extend the predefined trivial theorems use the \"import-trivials\" function (ns: io)"
  [form]
  (let [ids (map key @trivials)
        f (fn [id arg]
            (run* [q] ((eval (make-rule (id @trivials))) arg q)))
        results (map #(f % form) ids)
        res (first (drop-while empty? results))]
    res))


(comment
  ; man kann die Regel vorwärts und rückwärts auswerten, ohne sie zu ändern
  ; es längt alles an der Bestückung der Parameter
  (def and-i (eval (make-rule (:and-i @roths))))
  (run 1 [q1] (and-i 'A 'B q1))
  (run 1 [q1 q2] (and-i q1 q2 '(and A B)))
  (run 1 [q1 q2] (and-i q1 q2 '(and (or A B) B)))

  (keyword? :?)

  ; ergibt die Argumente für die Relation
  (defn- gen-relargs [args]
    (let [counter (atom -1)
          tr (fn [arg] (if (= :? arg)
                         (symbol (str \q (swap! counter inc)))
                         (list `quote arg)))]
      (mapv tr args)))

  #_(defn apply-rule'
    [logic-rel args]
    (let [rel-args (gen-relargs args)
          run-args (vec (filter symbol? rel-args))]
      (eval (list `run* run-args (list* logic-rel rel-args)))))
  
  ; Diese Variante von apply-rule setzt voraus, dass der Speicher für die Regeln bereits die
  ; logische Relation enthält.
  (defn apply-roth
    "Takes the id `roth` of a rule or a theorem and applies the according logical relation,
    with the given `args` where `:?` stands for the unknown.     
    Result is given as a vector which size is equal to the number of `:?`s in the args."
    [roth args]
    (let [rel-args (gen-relargs args)
          run-args (vec (filter symbol? rel-args))
          result (first (eval (list `run* run-args (list* (:logic-rel (roth @roths)) rel-args))))]
      (if-not (vector? result) [result] result)))
 
  (lwb.nd.io/import-rules "resources/nd/rules-prop.edn")
  
  ; and-i
  (lwb.nd.repl/show-roth :and-i)
  (apply-roth :and-i '(A B :?))            ; Standardanwendung (stepf :and-i n m)
  (apply-roth :and-i '(A :? :?))           ; möglich (stepf :and-i n)
  (apply-roth :and-i '(:? B :?))           ; möglich (stepf :and-i :? m)
  (apply-roth :and-i '(:? :? (and A B)))   ; Standardanwendung (stepb :and-i k)
  (apply-roth :and-i '(A :? (and A B)))    ; möglich (stepb :and-i k n)
  (apply-roth :and-i '(:? B (and A B)))    ; möglich (stepb :and-i k :? m)
  
  ; and-e1
  (lwb.nd.repl/show-roth :and-e1)
  (apply-roth :and-e1 '((and A B) :?))     ; Standardanwendung (stepf :and-e1 n)
  (apply-roth :and-e1 '(:? A))             ; möglich (stepb :and-e1 k)

  ; and-e2
  (lwb.nd.repl/show-roth :and-e2)
  (apply-roth :and-e2 '((and A B) :?))     ; Standardanwendung (stepf :and-e2 n)
  (apply-roth :and-e2 '(:? B))             ; möglich (stepb :and-e1 k)

  ; or-i1
  (lwb.nd.repl/show-roth :or-i1)
  (apply-roth :or-i1 '(A :?))              ; Standardanwendung (stepf :or-i1 n)
  (apply-roth :or-i1 '(:? (or A B)))       ; Standardanwendung (stepb :or-i1 k)
              
  ; or-i2
  (lwb.nd.repl/show-roth :or-i2)
  (apply-roth :or-i2 '(B :?))              ; Standardanwendung (stepf :or-i2 m)
  (apply-roth :or-i2 '(:? (or A B)))       ; Standardanwendung (stepb :or-i2 k)
  
  ; or-e
  (lwb.nd.repl/show-roth :or-e)
  (apply-roth :or-e '((or A B) :? :? :?))  ; Standardanwendung (step-f :or-e n)
  (apply-roth :or-e '((or A B) :? :? X))   ; Standardanwendung (step-f :or-e n k) -- :? für infers automatisch
  (apply-roth :or-e '(:? :? :? X))         ; Standardanwendung (step-b :or-e k) 
  (apply-roth :or-e '((or A B) :? :? X))   ; Standardanwendung (step-b :or-e k n) -- :? für infers automatisch
                                           ; man sieht dass hier step-f und step-b bis auf die Reihenfolge identisch sind
  
  ; impl-i
  (lwb.nd.repl/show-roth :impl-i)
  (apply-roth :impl-i '(:? (impl A B)))    ; Standardanwendung (step-b :impl-i k)
                                           ; step-f ist nicht erlaubt, weil all givens infers sind
                                           ; Vergleiche tnd, wo step-f erlaubt ist, weil es keine givens und keine infers gibt

  ; impl-e
  (lwb.nd.repl/show-roth :impl-e)
  (apply-roth :impl-e '((impl A B) A :?))  ; Standardanweisung (step-f :impl-e n m)
  (apply-roth :impl-e '((impl A B) :? :?)) ; möglich (step-f :impl-e n) -- ergäbe (impl A B)... A B
  (apply-roth :impl-e '((impl A B) :? B))  ; möglich (step-f :impl-e n :? k) -- ergäbe (impl A B) ... A B
  (apply-roth :impl-e '(:? :? B))          ; möglich (step-b :impl-e k) -- ergäbe ... (impl V1 B) ... V1 B
  (apply-roth :impl-e '(:? A B))           ; möglich (step-b :impl-e k ? m) -- ergäbe A ... (impl A B) B
  (apply-roth :impl-e '((impl A B) :? B))  ; möglich? (step-b :impl-e k n) -- ergäbe (impl A B) ... A B
  
  ; not-i
  (lwb.nd.repl/show-roth :not-i)
  (apply-roth :not-i '(:? (not A)))        ; Standardanwendung (step-b :not-i k)
  
  ; not-e
  (lwb.nd.repl/show-roth :not-e)
  (apply-roth :not-e '((not A) A :?))      ; Standardanwendung (step-b :not-i m n)
  (apply-roth :not-e '((not A) :? :?))     ; möglich (step-f :not-e m) -- ergäbe (not A) ... A contradiction 
  (apply-roth :not-e '(:?  A :?))          ; möglich (step-f :not-e :? n) -- ergäbe A ... (not A) contradiction
  (apply-roth :not-e '(:? :? contradiction)) ; möglich (step-b :not-e k) -- ergäbe (not V1) ...  V1 contradiction
  (apply-roth :not-e '((not A) :? contradiction)) ; möglich (step-b :not-e k m) -- ergäbe (not A) ...  A contradiction
  (apply-roth :not-e '(:? A contradiction)) ; möglich (step-b :not-e k :? n) -- ergäbe A ...  (not A) contradiction
  
  ; raa
  (lwb.nd.repl/show-roth :raa)
  (apply-roth :raa '(:? A))                ; Standardanwendung (step-b :raa k)
  
  ; efq
  (lwb.nd.repl/show-roth :efq)
  (apply-roth :efq '(contradiction :?))    ; Standardanwendung (step-f :efq m)
  (apply-roth :efq '(:? A))                ; Standardanwendung (step-f :efq m)

  ; tnd
  (lwb.nd.repl/show-roth :tnd)
  (apply-roth :tnd '(:?))                  ; Standardanwendung (step-f :tnd)
                                           ; wenn es mehrere Möglichkeiten gibt tnd einzufügen, wo? an der ersten Stelle
  
  ; notnot-i
  ; als Beispiel für 1 given, 1 conclusion
  (lwb.nd.repl/show-roth :notnot-i)
  (apply-roth :notnot-i '(A :?))           ; Standardanwendung (step-f :notnot-i m)
  (apply-roth :notnot-i '(:? (not (not A))))  ; Standardanwendung (step-b :not-not-i k)
  
  ; notnot-e
  (lwb.nd.repl/show-roth :notnot-e)
  (apply-roth :notnot-e '((not (not A)) :?))  ; Standardanwendung (step-f :not-not-e m)
  (apply-roth :notnot-e '(:? A))           ; Standardanwendung (step-b :not-not-e k)
  ; alle Theoreme mit 1 given und 1 conclusion gehen so
  
  ; mt
  (lwb.nd.repl/show-roth :mt)
  ; siehe mp
  
  ; contrap
  (lwb.nd.repl/show-roth :contrap)
  ; siehe notnot-i
  (apply-roth :contrap '((impl A B) :?))  ; Standardanwendung (step-f :contrap m)
  
  ; pierce
  (lwb.nd.repl/show-roth :pierce)
  ; siehe tnd
  (apply-roth :pierce '(:?))              ; Standardanwendung (step-f :pierce)
  
  )
