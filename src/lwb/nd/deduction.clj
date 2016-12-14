; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 - 2016 Tobias Völzel, Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.deduction
  (:require [clojure.walk :as walk]
            [lwb.nd.proof :refer :all]
            [lwb.nd.swap.prop :as sprop]
            [lwb.nd.swap.pred :as spred]
            [lwb.nd.swap.ltl :as sltl]
            [lwb.nd.rules :as rules]))

;; # Application of deduction steps to proofs

;; The basic idea: 
;; 
;; 1. We generate a logical relation for core.logic that is used to determine
;; the result of the application of a rule or a theorem to the proof.
;; 2. We put the result into the proof at the appropriate position.

;; ## Helper functions

(defn- range-args-f
  "Range of allowed arguments in a forward step."
  [pattern]
  (let [g1 (count (filter #{:g1} pattern))
        range-g1 (if (zero? g1) [0 0] [1 g1])
        gm (count (filter #{:gm} pattern))
        range-gm [gm gm]
        em (count (filter #{:em} pattern))
        range-em [em em]
        co (count (filter #{:co} pattern))
        range-co (if (zero? co) [0 0] [0 co])]
    (mapv + range-g1 range-gm range-em range-co)))

(defn- range-args-b
  "Range of allowed arguments in a backward step."
  [pattern]
  (let [gb (count (filter #{:gb} pattern))
        range-gb (if (zero? gb) [0 0] [0 (dec gb)])
        go (count (filter #{:go} pattern))
        range-go (if (zero? go) [0 0] [0 go])
        cm (count (filter #{:cm} pattern))
        range-cm [cm cm]]
    (mapv + range-gb range-go range-cm)))

(defn- type-args-ok?
  "Are the args numbers or formulas according to the pattern?"
  [pattern argsv]
  (let [pattern' (map #(first (name %)) (filter #(not= \? (second (name %))) pattern))
        check #(if (or (= %1 \g) (= %1 \c)) (number? %2) (or (symbol? %2) (list? %2)))]
    (every? identity (map check pattern' (filter #(not= :? %) argsv)))))

(defn- max-given
  "Maximal `plno` of the parameters for the givens."
  [match-pattern]
  (if (= 1 (count match-pattern))                           ; just one conclusion 
    0
    (apply max
           (->> match-pattern
                (filter #(= \g (first (name (first %)))))
                (map second)
                (filter number?)))))

(defn- match-argsv-f
  "Assigns the arguments to the pattern of a forward step.     
   Returns match-pattern.      
   requires: roth exists, forward is allowed"
  [roth argsv]
  ; get the pattern for forward step of the roth
  (loop [pattern (rules/roth-pattern roth :forward)
         args argsv
         result []]
    (let [p1 (first pattern) a1 (first args)]
      (if (nil? p1)
        result
        (cond
          (nil? a1) (recur (rest pattern) args (conj result [p1 :?]))
          (= p1 :gm) (recur (rest pattern) (rest args) (conj result [p1 a1]))
          (= p1 :g1) (recur (rest pattern) (rest args) (conj result [p1 a1]))
          (= p1 :em) (recur (rest pattern) (rest args) (conj result [p1 a1]))
          (= p1 :g?) (recur (rest pattern) args (conj result [p1 :?]))
          (= p1 :c?) (recur (rest pattern) args (conj result [p1 :?]))
          (= p1 :co) (recur (rest pattern) args (conj result [p1 a1])))))))

(defn- match-argsv-b
  "Assigns the arguments to the pattern of a backward step.     
   Returns match-pattern.      
   requires: roth exists, backward is allowed"
  [roth argsv]
  ; get the pattern for backward step of the roth
  (loop [pattern (rules/roth-pattern roth :backward)
         args argsv
         result []]
    (let [p1 (first pattern) a1 (first args)]
      (if (nil? p1)
        result
        (cond
          (nil? a1) (recur (rest pattern) args (conj result [p1 :?]))
          (= p1 :cm) (recur (rest pattern) (rest args) (conj result [p1 a1]))
          (= p1 :gb) (recur (rest pattern) (rest args) (conj result [p1 a1]))
          (= p1 :go) (recur (rest pattern) (rest args) (conj result [p1 a1]))
          (= p1 :g?) (recur (rest pattern) args (conj result [p1 :?])))))))

(defn- rel-params
  "Parameters for the logic relation given the match pattern for the call."
  [proof match-pattern]
  (->> match-pattern
       (map second)
       (mapv #(if (number? %) (plbody-at-plno proof %) %))))

(defn- find-next-todo-plno
  "`plno` of the next todo line following proof line with `plno`.      
  A result of `0` means that there is no such line.        
  requires: `n` is not out of bound of the proof."
  [proof n]
  (let [pv (vec (flatten proof))
        pv' (subvec pv n)
        no (first (keep-indexed #(when (= :todo (:body %2)) %1) pv'))]
    (if (nil? no) 0 (+ (inc n) no))))

(defn- plid-to-manip
  "Gives the `plid` of the todo line where the manipulation of the proof must occur.
  A result of `0` means that there is no such line."
  [proof match-pattern]
  (let [max-g (max-given match-pattern)
        todo-plno (find-next-todo-plno proof max-g)]
    (if (zero? todo-plno) 0 (plno->plid proof todo-plno))))

(defn- concl-okay?
  "Checks whether an optional conclusion is below the todo line."
  [match-pattern todoline-no]
  (let [concl (filter #(and (= (first %) :co) (number? (second %))) match-pattern)]
    (if (empty? concl) true
                       (> (second (first concl)) todoline-no))))

(defn- scope-okay?
  "Checks whether all arguments are in the same scope."
  [proof match-pattern]
  (let [plnos (filter number? (map second match-pattern))]
    (if (empty? plnos) true
                       (let [max-plno (apply max plnos)
                             plines (map #(pline-at-plno proof %) plnos)
                             curr-scope (scope proof (pline-at-plno proof max-plno))]
                         (every? #(contains? (set curr-scope) %) plines)))))

(defn- no-ref-to-todoline?
  "Checks if there is no ref to a todoline"
  [proof match-pattern]
  (let [plnos (filter number? (map second match-pattern))]
    (if (empty? plnos) true
                       (empty? (filter todoline?
                                       (map #(pline-at-plno proof %) plnos))))))

(defn- check-user-input
  "Checks of user input to a proof step, independent of the direction of the step."
  [proof roth argsv]
  ; does the rule or theorem exist?
  (if (not (rules/roth-exists? roth))
    (throw (Exception. (format "There's no such rule or theorem: %s" roth))))
  ; argsv is empty or has at least ref to a line number
  (if (and (not-empty argsv) (empty? (filter number? argsv)))
    (throw (Exception. (format "There must be at least one ref to a proof line in your arguments: %s" (str argsv)))))
  ; are the line numbers in argsv distinct?
  (if (and (not-empty argsv) (not (apply distinct? (filter number? argsv))))
    (throw (Exception. (format "There are duplicates in your arguments: %s" (str argsv)))))
  ; are the line numbers in argvs in the range of the proof?
  (if (and (not-empty argsv) (not-empty (filter number? argsv)))
    (let [pl-cnt (count (flatten proof))
          nos (filter number? argsv)
          low (apply min nos)
          high (apply max nos)]
      (if (not (and (pos? low) (<= high pl-cnt)))
        (throw (Exception. (format "Line numbers must refer to lines in the proof: %s" (str argsv))))))))

(defn- check-user-input-f
  "Checks the user input for a forward proof step, given as a roth and a vector.      
   Throws exceptions if input not valid, and      
   analyzes how to proceed the step. "
  [proof roth argsv]
  (check-user-input proof roth argsv)                       ; may throw exceptions
  ; can the roth be used in a forward step?
  (if (not (rules/roth-forward? roth))
    (throw (Exception. (format "This rule can't be used in a forward step: %s" roth))))
  (let [pattern (rules/roth-pattern roth :forward)
        range (range-args-f pattern)]
    ; size of argsv okay?
    (if (not (and (>= (count argsv) (first range)) (<= (count argsv) (second range))))
      (throw (Exception. (format "The number of arguments following the rule or theorem must be in the range: %s" range))))
    ; kind of args okay?
    (if (not (type-args-ok? pattern argsv))
      (throw (Exception. (format "Type of arguments doesn't match the call pattern: %s" pattern))))
    (let [match-pattern (match-argsv-f roth argsv)
          todo-plid (plid-to-manip proof match-pattern)]
      (if (zero? todo-plid)
        (throw (Exception. "Arguments refering givens must be above a todo line")))
      (if-not (no-ref-to-todoline? proof match-pattern)
        (throw (Exception. "Arguments may not refer todo lines")))
      (if-not (empty? (filter #(and (= :gm (first %)) (not (number? (second %)))) match-pattern))
        (throw (Exception. (format "Argument to a :gm must be a line number: %s" pattern))))
      (if (not (concl-okay? match-pattern (plid->plno proof todo-plid)))
        (throw (Exception. "Arguments refering conclusions must be below a todo line")))
      (if (not (scope-okay? proof match-pattern))
        (throw (Exception. "Arguments must all be in the same scope.")))
      {:match-pattern match-pattern
       :todo-plid     todo-plid
       :refs-pattern  (map #(if (number? (second %)) [(first %) (plno->plid proof (second %))] %) match-pattern)})))

(defn- check-user-input-b
  "Checks the user input for a backward proof step, given as a roth and a vector.      
   Throws exceptions if input not valid, and      
   analyzes how to proceed the step. "
  [proof roth argsv]
  (check-user-input proof roth argsv)                       ; may throw exceptions
  ; can the roth be used in a backward step?
  (if (not (rules/roth-backward? roth))
    (throw (Exception. (format "This rule can't be used in a backward step: %s" roth))))
  (let [pattern (rules/roth-pattern roth :backward)
        range (range-args-b pattern)
        no-args (count (filter #(not= :? %) argsv))]
    ; size of argsv okay?
    (if (not (and (>= no-args (first range)) (<= no-args (second range))))
      (throw (Exception. (format "The number of arguments following the rule or theorem must be in the range: %s" range))))
    ; kind of args okay?
    (if (not (type-args-ok? pattern argsv))
      (throw (Exception. (format "Type of arguments doesn't match the call pattern: %s" pattern))))
    (let [match-pattern (match-argsv-b roth argsv)
          concl-plid (plno->plid proof (first argsv))]
      (if-not (nil? (:roth (pline-at-plid proof concl-plid)))
        (throw (Exception. "The first argument must not refer an unsolved proof line")))
      (if-not (no-ref-to-todoline? proof match-pattern)
        (throw (Exception. "Arguments may not refer todo lines")))
      (if (not (scope-okay? proof match-pattern))
        (throw (Exception. "Arguments must all be in the same scope.")))
      {:match-pattern match-pattern
       :concl-plid    concl-plid
       :refs-pattern  (map #(if (number? (second %)) [(first %) (plno->plid proof (second %))] %) match-pattern)})))

(defn- upgrade-refs-pattern
  "Upgrades `refs-pattern` with the refs to new proof line in `new-plines.`        
   requires: #new-plines = # of :? in refs-pattern, this should be guaranteed since the call of the logic relation is ok."
  [refs-pattern new-plines]
  (loop [rp refs-pattern
         np new-plines
         result []]
    (let [rp1 (first rp) np1 (first np) np1-plid (if (vector? np1) [(:plid (first np1)) (:plid (last np1))] (:plid np1))]
      (if (nil? rp1)
        result
        (cond
          ; if a :co was not given by the user, it's like a :c?
          (and (= (second rp1) :?) (= (first rp1) :co)) (recur (rest rp) (rest np) (conj result [:c? np1-plid]))
          ; if a :go was not given by the user, it's like a :g?
          (and (= (second rp1) :?) (= (first rp1) :go)) (recur (rest rp) (rest np) (conj result [:g? np1-plid]))
          (= (second rp1) :?) (recur (rest rp) (rest np) (conj result [(first rp1) np1-plid]))
          :else (recur (rest rp) np (conj result rp1)))))))

(defn- upgrade-refs
  "Upgrades the refs in the given vector or proof at the given plids with roth and refs."
  [proof plids roth refs]
  (let [old-plines (mapv #(pline-at-plid proof %) plids)
        upg-plines (mapv #(assoc % :roth roth :refs refs) old-plines)]
    (vec (reduce #(replace-plid %1 (:plid %2) %2) proof upg-plines))))

;; ## Interface for user of deduction

(defn step-f
  "Deduction step forward on `proof` with `roth` and the argument vector `argsv`."
  [proof roth argsv]
  (let [infos (check-user-input-f proof roth argsv)
        rel-params (rel-params proof (:match-pattern infos))
        result (rules/apply-roth roth rel-params)
        todo-plid (:todo-plid infos)]
    ;; no result
    (if (nil? (first result))
      (throw (Exception. (format "Rule or theorem not applicable, check id and arguments: %s" (into (vector roth) argsv)))))
    ;; new proof lines
    (let [new-plines (new-plines result)
          refs-pattern (:refs-pattern infos)
          refs-pattern' (upgrade-refs-pattern refs-pattern new-plines)
          refs (mapv second (filter #(contains? #{\g \e} (first (name (first %)))) refs-pattern'))
          new-plines' (upgrade-refs new-plines (mapv second (filter #(= :c? (first %)) refs-pattern')) roth refs)
          proof' (upgrade-refs proof (mapv second (filter #(= :co (first %)) refs-pattern')) roth refs)
          new-proof (vec (reduce #(add-above-plid %1 todo-plid %2) proof' new-plines'))]
      (normalize new-proof))))

(defn step-b
  "Deduction step backward on `proof` with `roth` and the argument vector `argsv`."
  [proof roth argsv]
  (let [infos (check-user-input-b proof roth argsv)
        shift1 (fn [v] (conj (vec (rest v)) (first v)))
        rel-params (shift1 (rel-params proof (:match-pattern infos)))
        result (rules/apply-roth roth rel-params)
        todo-plid (:concl-plid infos)]
    ;; no result
    (if (nil? (first result))
      (throw (Exception. (format "Rule or theorem not applicable, check id and arguments: %s" (into (vector roth) argsv)))))
    ;; new proof lines
    (let [new-plines (new-plines result)
          refs-pattern (:refs-pattern infos)
          refs-pattern' (upgrade-refs-pattern refs-pattern new-plines)
          refs (mapv second (filter #(= \g (first (name (first %)))) refs-pattern'))
          proof' (upgrade-refs proof (mapv second (filter #(= :cm (first %)) refs-pattern')) roth refs)
          new-proof (vec (reduce #(add-above-plid %1 todo-plid %2) proof' new-plines))]
      (normalize new-proof))))

(defn swap
  "Exchanges all instances of symbol `old` inside the `proof` with `new`.     
   Requires: `old`  has the form `?n` with an integer `n`.       
   Throws exception if `old` does not have that form.        
   Throws exception if `new` is not allowed depending on current logic and proof."
  [proof logic old new mode]
  ; check old
  (if-not (qmsymbol? old)
    (throw (Exception. (format "'%s' is not a symbol of the form '?n'. You can only swap such symbols." old))))
  ; check depending on logic and proof
  (case logic
    :prop (sprop/check-swap proof old new)
    :pred (spred/check-swap proof old new)
    :ltl  (sltl/check-swap proof old new mode))
  ; no exception so far, hence replacing old by new is now okay
  (normalize
    (walk/postwalk
      (fn [node]
        (if (map? node)
          (cond
            (symbol? (:body node)) (if (= (:body node) old)
                                     (assoc node :body new)
                                     node)
            (list? (:body node)) (assoc node :body (walk/prewalk-replace {old new} (:body node)))
            :else node)
          node))
      proof)))

(defn subclaim
  "Introduces the subclaim `fml` into `proof` at the first todo line."
  [proof fml]
  (let [todo-line (find-next-todo-plno proof 1)
        new-line  (new-pline fml)
        new-proof (add-above-plid proof (plno->plid proof todo-line) new-line)]
    (normalize new-proof)))
