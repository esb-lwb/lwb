; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 Tobias Völzel, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.rules
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [lwb.nd.storage :refer [roths]]
            [clojure.spec :as s]))

;; Magic symbol for the logic variable for the conclusion in the logic relation
;; This symbol should not be used as the name of a proposition in rules and theorems
(def
  ^{:doc "Magic number for the logic variable for the conclusion in the logic relation.    
          Symbols of the form `qnnnn`should not be used as the name of a proposition in rules and theorems."}
  conclusion-number 6571)

;; # Transformation of rules and theorems to core.logic relations

;; ## Specification of rules and theorems

;; The following specs describe the internal representation of rules and theorems (called roths)

;; Expressions in rules
(s/def ::expr (s/or :list list? :symbol symbol?))

;; Id of roth
(s/def ::id keyword?)

;; Prerequisite for the application of the rule
(s/def ::prereq (s/nilable (s/coll-of ::expr :kind vector?)))

;; Given premises
(s/def ::given (s/coll-of ::expr :kind vector))

;; Extra arguments for the evaluation of a rule i.e. :equal-e
(s/def ::extra (s/nilable (s/coll-of ::expr :kind vector)))

;; Conclusion
(s/def ::conclusion (s/coll-of ::expr :kind vector))

;; Entity map for the body of a rule
(s/def ::rule-body (s/keys :un-req [::given ::extra ::conclusion ::prereq]))

;; Rule
(s/def ::rule (s/cat :id ::id :body ::rule-body))

;; Entity map for the body of a theorem
(s/def ::theorem-body (s/keys :un-req [::given ::conclusion]))

;; Theorem
(s/def ::theorem (s/cat :id ::id :body ::theorem-body))

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
    (= idx 0) :gm
    :else :go
    ))

(defn- map-givens-b
  [idx given]
  (cond
    (and (list? given) (= 'infer (first given))) :g?
    (= idx 0) :go
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

(defn roth-structure-forward
  "Structure of the logic relation of the rule or theorem with the given `id`.      
   Result e.g.: `[:gm :gm :g? :g? :co]`      
   requires: @roths    
             `id` is a valid rule id."
  [roth-id]
  (let [roth (roth-id @roths)]
    (:forward roth)))

(defn roth-structure-backward
  "Structure of the logic relation of the rule or theorem with the given `id`.      
   Result e.g.: `[:cm :gb :gb]`      
   requires: @roths    
             `id` is a valid rule id."
  [id]
  (let [roth (id @roths)]
    (:backward roth)))

(comment

  (roth-structure-forward :and-i)
  ; => [:g1 :g1 :c?] 
  (roth-structure-backward :and-i)
  ; => [:cm :gb :gb] 

  (roth-structure-forward :and-e1)
  ; => [:gm :c?] 
  (roth-structure-backward :and-e1)
  ; => [:cm :g?] 

  (roth-structure-forward :or-i1)
  ; => [:gm :c?] 
  (roth-structure-backward :or-i1)
  ; => [:cm :g?] 

  (roth-structure-forward :or-e)
  ; => [:gm :g? :g? :co] 
  (roth-structure-backward :or-e)
  ; => [:cm :go :g? :g?] 

  (roth-structure-forward :impl-i)
  ; => nil
  (roth-structure-backward :impl-i)
  ; => [:cm :g?] 

  (roth-structure-forward :impl-e)
  ; => [:g1 :g1 :c?] 
  (roth-structure-backward :impl-e)
  ; => [:cm :gb :gb] 

  (roth-structure-forward :not-i)
  ; => nil
  (roth-structure-backward :not-i)
  ; => [:cm :g?] 

  (roth-structure-forward :not-e)
  ; => [:g1 :g1 :c?] 
  (roth-structure-backward :not-e)
  ; => [:cm :gb :gb] 

  (roth-structure-forward :raa)
  ; => nil
  (roth-structure-backward :raa)
  ; => [:cm :g?] 

  (roth-structure-forward :efq)
  ; => [:gm :c?] 
  (roth-structure-backward :efq)
  ; => [:cm :g?] 

  (roth-structure-forward :notnot-i)
  ; => [:gm :c?] 
  (roth-structure-backward :notnot-i)
  ; => [:cm :?g] 

  (roth-structure-forward :tnd)
  ; => [:c?] 
  (roth-structure-backward :tnd)
  ; => nil

  (roth-structure-forward :forall-i)
  ; => nil
  (roth-structure-backward :forall-i)
  ; => [:cm :g?] 

  (roth-structure-forward :forall-e)
  ; => [:gm :gm :c?] 
  (roth-structure-backward :forall-e)
  ; => nil

  (roth-structure-forward :exists-i)
  ; => nil 
  (roth-structure-backward :exists-i)
  ; => [:cm :gb :gb] 

  (roth-structure-forward :exists-e)
  ; => [:gm :g? :co] 
  (roth-structure-backward :exists-e)
  ; => [:cm :go :g?] 

  (roth-structure-forward :equal-i)
  ; => [:c?] 
  (roth-structure-backward :equal-i)
  ; => nil

  (roth-structure-forward :equal-e)
  ; => [:gm :gm :em :em :c?] 
  (roth-structure-backward :equal-e)
  ; => nil
  )

(comment
  ; load ltl

  (roth-structure-forward :finally-e)
  ; => [:gm :c? :c?]
  (roth-structure-backward :finally-e)
  ; => nil
  )


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
    (list? arg) (let [op (list `quote (first arg))
                      params (mapv gen-term (rest arg))]
                  (list* `list op params))))

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
    (symbol? arg) [arg]
    (list? arg) (vec (flatten (map gen-fresh-arg (rest arg))))
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
        allargs (into [] (concat given extra))
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
  (make-relation :equal-e)
  )

(defn roth-exists?
  "Does a certain rule/theorem exist?"
  [roth-id]
  (contains? @roths roth-id))

(defn get-roth
  "Returns the rule/theorem if it exists"
  [roth-id]
  (if (roth-exists? roth-id)
    (roth-id @roths)
    (throw (Exception. (str roth-id " not found in @roths.")))))

;; TODO!!!
(defn given-cnt
  "Returns the number of givens for the certain rule/theorem"
  [roth-id]
  (count (:given (roth-id @roths))))

(defn concl-cnt
  "Returns the number of conclusions for the certain rule/theorem"
  [roth-id]
  (count (:conclusion (roth-id @roths))))

(defn rule-forward?
  "Returns true if the rule/theorem can be used forwards"
  [id]
  true)

(defn rule-backward?
  "Returns true if the rule/theorem can be used backwards"
  [id]
  true)

; apply-rule without permutations

; without permutations on must in certain situations use unify
; but on the other side one needs not to make choices
; but: if we have more  than 2 givens and make a step backward
;      from the concept of the rules does the order of givens not play a role!!

(defn apply-rule
  [rule forward? args & [optional]]
  (let [rule-map (cond
                   (map? rule) rule
                   (keyword? rule) (rule @roths)
                   :else (throw (Exception. "Wrong type of argument: \"rule\" has to be a keyword or a map.")))
        frule (if forward? rule-map (assoc rule-map :given (:conclusion rule-map) :conclusion (:given rule-map)))
        obligatory-args (mapv #(list `quote %) args)
        optional-args (mapv #(list `quote %) optional)
        logic-args-num (- (+ (count (:given frule)) (count (:conclusion frule)))
                          (+ (count obligatory-args) (count optional-args)))
        logic-args (mapv #(symbol (str %1 %2))
                         (take logic-args-num (cycle ['q]))
                         (take logic-args-num (iterate inc 1)))
        ; setzt voraus, dass make-relation eine map versteht. Das ist aber nicht mehr so
        logic-rel (eval (make-relation frule))]
    (eval (list `run* logic-args (list* logic-rel (concat obligatory-args optional-args logic-args))))))


;; wird nicht mehr gebraucht
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
  ; es hängt alles an der Bestückung der Parameter
  (def and-i (eval (make-rule (:and-i @roths))))
  (run 1 [q1] (and-i 'A 'B q1))
  (run 1 [q1 q2] (and-i q1 q2 '(and A B)))
  (run 1 [q1 q2] (and-i q1 q2 '(and (or A B) B)))

  (keyword? :?)
  )

; ergibt die Argumente für die Relation
(defn- gen-relargs [args]
  (let [counter (atom -1)
        tr (fn [arg] (if (= :? arg)
                       (symbol (str \q (swap! counter inc)))
                       (list `quote arg)))]
    (mapv tr args)))

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


(comment
  ; and-i
  (lwb.nd.repl/show-roth :and-i)
  (apply-roth :and-i '(A B :?))                             ; Standardanwendung (stepf :and-i n m)
  (apply-roth :and-i '(A :? :?))                            ; möglich (stepf :and-i n)
  (apply-roth :and-i '(:? B :?))                            ; möglich (stepf :and-i :? m)
  (apply-roth :and-i '(:? :? (and A B)))                    ; Standardanwendung (stepb :and-i k)
  (apply-roth :and-i '(A :? (and A B)))                     ; möglich (stepb :and-i k n)
  (apply-roth :and-i '(:? B (and A B)))                     ; möglich (stepb :and-i k :? m)

  ; and-e1
  (lwb.nd.repl/show-roth :and-e1)
  (apply-roth :and-e1 '((and A B) :?))                      ; Standardanwendung (stepf :and-e1 n)
  (apply-roth :and-e1 '(:? A))                              ; möglich (stepb :and-e1 k)

  ; and-e2
  (lwb.nd.repl/show-roth :and-e2)
  (apply-roth :and-e2 '((and A B) :?))                      ; Standardanwendung (stepf :and-e2 n)
  (apply-roth :and-e2 '(:? B))                              ; möglich (stepb :and-e1 k)

  ; or-i1
  (lwb.nd.repl/show-roth :or-i1)
  (apply-roth :or-i1 '(A :?))                               ; Standardanwendung (stepf :or-i1 n)
  (apply-roth :or-i1 '(:? (or A B)))                        ; Standardanwendung (stepb :or-i1 k)

  ; or-i2
  (lwb.nd.repl/show-roth :or-i2)
  (apply-roth :or-i2 '(B :?))                               ; Standardanwendung (stepf :or-i2 m)
  (apply-roth :or-i2 '(:? (or A B)))                        ; Standardanwendung (stepb :or-i2 k)

  ; or-e
  (lwb.nd.repl/show-roth :or-e)
  (apply-roth :or-e '((or A B) :? :? :?))                   ; Standardanwendung (step-f :or-e n)
  (apply-roth :or-e '((or A B) :? :? X))                    ; Standardanwendung (step-f :or-e n k) -- :? für infers automatisch
  (apply-roth :or-e '(:? :? :? X))                          ; Standardanwendung (step-b :or-e k) 
  (apply-roth :or-e '((or A B) :? :? X))                    ; Standardanwendung (step-b :or-e k n) -- :? für infers automatisch
  ; man sieht dass hier step-f und step-b bis auf die Reihenfolge identisch sind

  ; impl-i
  (lwb.nd.repl/show-roth :impl-i)
  (apply-roth :impl-i '(:? (impl A B)))                     ; Standardanwendung (step-b :impl-i k)
  ; step-f ist nicht erlaubt, weil all givens infers sind
  ; Vergleiche tnd, wo step-f erlaubt ist, weil es keine givens und keine infers gibt

  ; impl-e
  (lwb.nd.repl/show-roth :impl-e)
  (apply-roth :impl-e '((impl A B) A :?))                   ; Standardanweisung (step-f :impl-e n m)
  (apply-roth :impl-e '((impl A B) :? :?))                  ; möglich (step-f :impl-e n) -- ergäbe (impl A B)... A B
  (apply-roth :impl-e '(:? A :?))                           ; möglich (step-f :impl-e :? m) -- ergäbe A ... (impl A V1) V1 ... 
  (apply-roth :impl-e '((impl A B) :? B))                   ; möglich (step-f :impl-e n :? k) -- ergäbe (impl A B) ... A B
  (apply-roth :impl-e '(:? :? B))                           ; möglich (step-b :impl-e k) -- ergäbe ... (impl V1 B) ... V1 B
  (apply-roth :impl-e '(:? A B))                            ; möglich (step-b :impl-e k ? m) -- ergäbe A ... (impl A B) B
  (apply-roth :impl-e '((impl A B) :? B))                   ; möglich? (step-b :impl-e k n) -- ergäbe (impl A B) ... A B

  ; not-i
  (lwb.nd.repl/show-roth :not-i)
  (apply-roth :not-i '(:? (not A)))                         ; Standardanwendung (step-b :not-i k)

  ; not-e
  (lwb.nd.repl/show-roth :not-e)
  (apply-roth :not-e '((not A) A :?))                       ; Standardanwendung (step-b :not-i m n)
  (apply-roth :not-e '((not A) :? :?))                      ; möglich (step-f :not-e m) -- ergäbe (not A) ... A contradiction 
  (apply-roth :not-e '(:? A :?))                            ; möglich (step-f :not-e :? n) -- ergäbe A ... (not A) contradiction
  (apply-roth :not-e '(:? :? contradiction))                ; möglich (step-b :not-e k) -- ergäbe (not V1) ...  V1 contradiction
  (apply-roth :not-e '((not A) :? contradiction))           ; möglich (step-b :not-e k m) -- ergäbe (not A) ...  A contradiction
  (apply-roth :not-e '(:? A contradiction))                 ; möglich (step-b :not-e k :? n) -- ergäbe A ...  (not A) contradiction

  ; raa
  (lwb.nd.repl/show-roth :raa)
  (apply-roth :raa '(:? A))                                 ; Standardanwendung (step-b :raa k)

  ; efq
  (lwb.nd.repl/show-roth :efq)
  (apply-roth :efq '(contradiction :?))                     ; Standardanwendung (step-f :efq m)
  (apply-roth :efq '(:? A))                                 ; Standardanwendung (step-f :efq m)

  ; tnd
  (lwb.nd.repl/show-roth :tnd)
  (apply-roth :tnd '(:?))                                   ; Standardanwendung (step-f :tnd)
  ; wenn es mehrere Möglichkeiten gibt tnd einzufügen, wo? an der ersten Stelle

  ; notnot-i
  ; als Beispiel für 1 given, 1 conclusion
  (lwb.nd.repl/show-roth :notnot-i)
  (apply-roth :notnot-i '(A :?))                            ; Standardanwendung (step-f :notnot-i m)
  (apply-roth :notnot-i '(:? (not (not A))))                ; Standardanwendung (step-b :not-not-i k)

  ; notnot-e
  (lwb.nd.repl/show-roth :notnot-e)
  (apply-roth :notnot-e '((not (not A)) :?))                ; Standardanwendung (step-f :not-not-e m)
  (apply-roth :notnot-e '(:? A))                            ; Standardanwendung (step-b :not-not-e k)
  ; alle Theoreme mit 1 given und 1 conclusion gehen so

  ; mt
  (lwb.nd.repl/show-roth :mt)
  ; siehe mp

  ; contrap
  (lwb.nd.repl/show-roth :contrap)
  ; siehe notnot-i
  (apply-roth :contrap '((impl A B) :?))                    ; Standardanwendung (step-f :contrap m)

  ; pierce
  (lwb.nd.repl/show-roth :pierce)
  ; siehe tnd
  (apply-roth :pierce '(:?))                                ; Standardanwendung (step-f :pierce)

  )

(comment                                                    ;pred   

  ; forall-i
  (lwb.nd.repl/show-roth :forall-i)
  (apply-roth :forall-i '(:? (forall [x] phi)))             ; (step-b :forall-i k)
  ; nur step-b möglich

  ; forall-e
  (lwb.nd.repl/show-roth :forall-e)
  (apply-roth :forall-e '((forall [x] phi) :? :?))          ; (step-f :forall-e m)
  (apply-roth :forall-e '((forall [x] phi) (actual t) :?))  ; (step-f :forall-e m n)
  ; nur step-f möglich, da conclusion eine substitution ist, d.h. wir müssen phi kennen

  ; exists-i
  (lwb.nd.repl/show-roth :exists-i)
  (apply-roth :exists-i '(:? :? (exists [x] phi)))          ; (step-b :exists-i k)
  (apply-roth :exists-i '((actual t) :? (exists [x] phi)))  ; (step-b :exists-i k m)
  ; wenn in den given eine substitution vorkommt, ist kein step-f möglich

  ; exists-e
  (lwb.nd.repl/show-roth :exists-e)
  (apply-roth :exists-e '((exists [x] (P (x))) :? :?))      ; (step-f :exists-e m)
  (apply-roth :exists-e '((exists [x] (P (x))) :? X))       ; (step-f :exists-e m k) oder (step-b :exists-e k m)
  (apply-roth :exists-e '(:? :? X))                         ; (step-b ;exists-e k)

  ; equal-i
  (lwb.nd.repl/show-roth :equal-i)
  (apply-roth :equal-i '(:?))                               ; (step-f :equal-i) wie tnd

  ; equal-e
  (lwb.nd.repl/show-roth :equal-e)
  (apply-roth :equal-e '((= a b) (P a) (P z) z :?))         ; (step-f :equal-e m n u1 u2)
  ; dies ist die einzig sinnvolle Möglichkeit, d.h. wir brauchen alle given, weil wir ein prereq haben!

  )

(comment                                                    ;ltl
  (apply-roth :finally-e '((at [i] (finally A)) :? :?))     ; (step-f :finally-e m)
  ; bisher einzige Regel mit mehreren Conclusions!!!!  kann dann natürlich nur vorwärts ausgeführt werden!!

  )
