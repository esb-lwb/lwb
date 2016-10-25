; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 Tobias Völzel, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.deduction
  (:require [lwb.nd.proof :refer :all]
            [lwb.nd.rules :as rules]))

;; atoms to provide unique ids for items and variables
(def id (atom 0))
(defn new-id []
  (swap! id inc))

(def var-id (atom 0))
(defn new-var []
  (symbol (str 'V (swap! var-id inc))))
;; ---------------------------------------------------

;; functions for removing duplicate entries from the proof
;; TODO: überarbeiten und in proof verschieben
(defn find-duplicates
  "Finds duplicates entries inside the proof that can be deleted.
   Returns a map with the deletable ids as key and the replacement items as value.
   Only items without rule, in the same scope and the same subproof will be marked as deletable"
  ([proof] (find-duplicates proof proof))
  ([proof sub]
   (let [scope (get-scope proof (last sub))
         duplicates (disj (set (map first (filter #(> (val %) 1) (frequencies (map :body (remove vector? scope)))))) :todo)
         duplicate-items (filter #(contains? duplicates (:body %)) scope)
         equals (into [] (map val (group-by :body duplicate-items)))
         fn-smap (fn [equals]
                   (let [remain (map :plid (filter :roth equals))
                         delete (map :plid (filter (set sub) (remove :roth equals)))] ; just items from the actual sub can be deleted
                     (reduce #(assoc %1 %2 (last remain)) {} delete)))
         ids (apply merge (map fn-smap equals))]
     (reduce #(if (vector? %2) (merge %1 (find-duplicates proof %2)) %1) ids sub))))

(defn adjust-ids
  "Replaces all occurences of a certain ID inside proof with another.
   Provide ids as a map with keys = IDs to replace | vals = replacement"
  [proof ids]
  (let [regex (java.util.regex.Pattern/compile (clojure.string/join "|" (map #(str "\\b" % "\\b") (map key ids))))
        smap (apply merge (map #(hash-map (str (key %)) (if (list? (val %))
                                                          (clojure.string/join " " (val %))
                                                          (str (val %)))) ids))]
    (if (not-empty ids)
      (clojure.walk/postwalk
        (fn [node]
          (if (vector? node)
            node
            (if (string? (:roth node))
              (assoc node :roth (clojure.string/replace (:roth node) regex #(get smap %)))
              node)))
        proof)
      proof)))

(defn remove-duplicates
  "Removes all duplicate entries from proof and adjusting the changed IDs"
  [proof]
  (let [duplicates (find-duplicates proof)
        ;; if an item is the unproved result of a subproof and its proved duplicate is outside
        ;; deleting the result due to its duplicate would lead to a strange structure inside the proof, 
        ;; therefore the item isn't deleted, but instead changed to refer to the outside duplicate
        ;; e.g. "Generalisation" (temporal-logic)
        ;; initial situation        ==>    without fix              ==>    with fix
        ;; 1 (at x a) :premise             1 (at x a) :premise             1 (at x a) :premise
        ;; ----------------------          ----------------------          ----------------------
        ;; 2 (<= x y) :assumption          2 (<= x y) :assumption          2 (<= x y) :assumption
        ;; 3 ...                           ----------------------          3 (at x a) "repeat" (1)
        ;; 4 (at x a)                      3 (at x (always x)) "always-i"  ----------------------
        ;; ----------------------                               ([2 1])    4 (at x (always a)) "always-i" 
        ;; 5 (at x (always a)) "always-i"                                                       ([2 3])
        ;;                      ([2 4])
        fn-proved-results (fn [map [id1 id2]]
                            (let [delete-item (get-item proof (id-to-line proof id1))
                                  replace-item (get-item proof (id-to-line proof id2))
                                  delete-scope (get-scope proof delete-item)]
                              (if (and (= delete-item (last delete-scope))
                                       (not= delete-scope (get-scope proof replace-item)))
                                (assoc map id1 id2) map)))
        proved-results (reduce fn-proved-results {} duplicates)
        fn-replace (fn [p [id1 id2]]
                     (let [item (get-item p (id-to-line p id1))]
                       (replace-item p item {:plid   id1
                                             :body (:body item)
                                             :roth (str "\"repeat\" (" id2 ") []")})))
        new-proof1 (reduce fn-replace proof proved-results)
        deletions (reduce dissoc duplicates (map key proved-results))
        delete-items (map #(get-item proof (id-to-line proof %)) (map key deletions))
        new-proof2 (reduce remove-item new-proof1 delete-items)]
    (adjust-ids new-proof2 deletions)))

(defn remove-todos
  "Removes all \"...\" lines, if all lines inside the (sub)proof are solved (rule =! nil)"
  [proof]
  (let [solved (< (count (remove :roth (remove #(= (:body %) :todo) (remove vector? proof)))) 1)]
    (loop [p proof
           np []]
      (cond
        (empty? p) np
        (vector? (first p)) (recur (subvec p 1) (conj np (remove-todos (first p))))
        :else
        (if (and solved (= (:body (first p)) :todo))
          (recur (subvec p 1) np)
          (recur (subvec p 1) (conj np (first p))))))))

(defn check-duplicates
  "Removes duplicate lines, adjust leftover ids and remove \"...\" lines if possible"
  [proof]
  (remove-todos (remove-duplicates proof)))
;; -------------------------------------------------------

;; functions for special forms (e.g. infer, substitution)
;; NEW LOGIC (insert your new special forms here) 
(defn substitution
  "Substitutes an variable identifier inside a predicate formula with another
   e.g. (substitution '(P x) 'x 'Z) => (P Z)"
  [formula old new]
  (cond
    (not (list? formula))
    (throw (Exception. (str "The argument \"" formula "\" is not a list and therefore can't be substituted."
                            "Maybe you have to provide optional arguments for the step you trying to accomplish.")))

    (contains? (set (flatten formula)) new)
    (throw (Exception.
             (str "Substitution failed. The identifier \"" new "\" is already used inside the formula \"" formula "\"")))

    :else (clojure.walk/postwalk-replace {old new} formula)))

(defn between [x y] [x y])

(defn infer
  "Creates a (sub)proof for premises and formula in the structure needed for further actions.
   \"premises\" can be a single object, a vector of objects or skipped
   \"formula\" needs to be a single object
   \"superproof?\" decides if its a proof or subproof"
  ([formula] (infer [] formula))
  ([premises formula & [superproof?]]
   (let [desc (if superproof? :premise :assumption)
         prem (if (vector? premises)
                (into [] (map #(hash-map :plid (new-id)
                                         :body %
                                         :rule desc) premises))
                [{:plid (new-id) :body premises :rule desc}])
         todo {:plid (new-id) :body :todo :rule nil}
         form {:plid (new-id) :body formula :rule nil}]
     (check-duplicates (conj prem todo form)))))

(defn re-infer
  "Returns a (sub)proof from the internal structure back to a depiction like \"(infer [premises] formula)\""
  [proof]
  (let [premises (into [] (map #(:body %) (filter #(or (= (:rule %) :assumption)
                                                       (= (:rule %) :premise)) proof)))
        prem-args (if (> (count premises) 1) premises (first premises))]
    `(~'infer ~prem-args ~(:body (last proof)))))

#_(defn proof
  "Creates a new superproof
   This is the entry point for new deductions"
  ([formula] (proof [] formula))
  ([premises formula]
   (reset! id 0)
   (reset! var-id 0)
   (apply infer [premises formula true])))
;; ------------------------------------------------------

;; helping and utility functions
(defn get-proofed-items
  "Only returns items that are either vectors or have a rule (they are prooved)"
  [items]
  (filter #(or (vector? %)
               (:rule %)) items))

(defn get-unproofed-items
  "Only returns items that are neither vectors nor have a rule (they are unprooved so far)"
  [items]
  (remove #(or (vector? %)
               (:rule %)) items))

(defn item-to-rule-arg
  "Converts a item to a rule argument for the core.logic functions"
  [item]
  (if (not (vector? item))
    (if (not (map? item))
      item
      (:body item))
    (re-infer item)))

(defn get-item-id
  "Returns the id for the item. 
   Item can also be a subproof (vector)"
  [item]
  (if (not (vector? item))
    (:plid item)
    [(:plid (first item)) (:plid (last item))]))
;; -----------------------------

;; functions for creating new proof items from given bodies
(defn init-vars
  "Replaces the automatically created variables from core.logic (\"_0\", \"_1\" etc.)
   with new unique identifiers (\"V1\", \"V2\" etc.)"
  [bodies]
  (let [vars (set (filter #(.startsWith (str %) "_") (flatten bodies)))
        smap (reduce #(assoc %1 %2 (new-var)) {} vars)
        new-bodies (map #(if (symbol? %)
                          (if (contains? vars %) (get smap %) %)
                          (clojure.walk/prewalk-replace smap %)) bodies)]
    new-bodies))

;; NEW LOGIC (if you created new functions for special forms, you have correctly eval them here)
(defn eval-body
  "Searches for keywords (infer, substitution) inside body and evaluates them. Otherwise returns body"
  [body]
  (if (and (list? body)
           (contains? #{'infer 'substitution} (first body)))
    (condp = (first body)
      'infer (let [prem (if (vector? (second body))
                          (into [] (map #(eval-body %) (second body)))
                          (eval-body (second body)))
                   conc (eval-body (second (rest body)))]
               (eval (conj (map #(list `quote %) (list prem conc)) `infer)))
      'substitution (eval (conj (map #(list `quote (eval-body %)) (rest body)) `substitution)))
    body))

(defn create-item
  "Creates a new item from body and [optional] rule
   IMPORTANT: This function is only used by \"create-items\", because this ensures
   that no lazy-sequences are left inside the new items.
   If you only want to create one item use --> (first (create-items bodies))"
  ([body] (create-item body nil))
  ([body rule]
   (let [newbody (eval-body body)]
     (if (vector? newbody)
       newbody
       {:plid   (new-id)
        :body newbody
        :rule rule}))))

(defn create-items
  "Consumes a collection of bodies and a [optional] rule and creates items for the internal proof structure"
  ([bodies] (create-items bodies nil))
  ([bodies rule]
   (let [newb (init-vars bodies)
         ;; to ensure that all bodies of all items are either symbols or lists, convert all lazy-seq (they come from rule evaluation) to lists
         non-lazy (clojure.walk/postwalk (fn [node]
                                           (if (instance? clojure.lang.LazySeq node)
                                             (apply list node)
                                             node)) newb)]
     (map #(create-item % rule) non-lazy))))
;; --------------------------------------------------------

;; functions for advancing the proofs state (steps, choose-options, unify)
(defn check-args
  "Checks the arguments for errors and irregularities. 
   If nothing is found returns a map with additional information for further proceeding."
  [proof rule args forward?]
  ;; separate lines and user-inputs and check the right number of user-inputs
  (let [lines (filter number? args)
        user-inputs (remove number? args)
        num-inputs (count (filter #(.startsWith (str %) "_:")
                                  (if forward?
                                    (:given (rules/get-roth rule))
                                    (:conclusion (rules/get-roth rule)))))]
    (cond
      (not= (count user-inputs) num-inputs)
      (throw (Exception. (str "Wrong number of User-Inputs (rule: " num-inputs ", you: " (count user-inputs) ")")))

      ;; tests not regarding user-inputs

      (not (rules/roth-exists? rule))
      (throw (Exception. (str "A rule named \"" rule "\" does not exists.")))

      (and forward? (not (rules/roth-forward? rule)))
      (throw (Exception. (str "The rule \"" rule "\" is not marked for forward use.")))

      (and (not forward?) (not (rules/roth-backward? rule)))
      (throw (Exception. (str "The rule \"" rule "\" is not marked for backward use.")))

      ;; for forward rules with no premises (e.g. equality introduction)
      (and (empty? args)
           forward?
           (zero? (rules/given-cnt rule)))
      {:todo         (first (filter #(= (:body %) :todo) (flatten proof)))
       :obligatories []
       :optional     []}

      (not (apply distinct? lines))
      (throw (Exception. "There are duplicate lines in the arguments"))

      (some #(< % 1) (flatten lines))
      (throw (Exception. "There are no line numbers less than 1"))

      (some #(> % (count (flatten proof))) (flatten lines))
      (throw (Exception. (str "There are no line numbers greater than " (count (flatten proof)))))

      :else
      (let [lastline (last (sort-by #(if (vector? %) (first %) %) lines))
            items (map #(get-item proof %) lines)
            obligatories (concat (if forward? (get-proofed-items items) (get-unproofed-items items))
                                 user-inputs)               ;; add the user-inputs to the obligatory line-items
            optional (if forward? (get-unproofed-items items) (get-proofed-items items))
            numObligatories (if forward? (rules/given-cnt rule) (rules/concl-cnt rule))
            numOptionals (dec (if forward? (rules/concl-cnt rule) (rules/given-cnt rule)))
            scope (get-scope proof (get-item proof lastline))
            todos (filter #(= (:body %) :todo) scope)]
        (cond
          (not-every? #(contains? (set scope) %) items)
          (throw (Exception. "Not all lines are in the same scope"))

          (> (count todos) 1)
          (throw (Exception. "There can't be more than one \"...\" line inside your scope"))

          ;; only backward steps need an empty line to work towards to
          (and (not forward?)
               (< (count todos) 1))
          (throw (Exception. "There is no \"...\" line inside your scope to work towards to"))

          (some #(contains? (set items) %) todos)
          (throw (Exception. (str "Can't use a \"...\" line for " (if forward? "forward" "backward") " resulting")))

          (not= (count obligatories) numObligatories)
          (throw (Exception. (str (if (> (count obligatories) numObligatories)
                                    "Too many "
                                    "Not enough ") (if forward?
                                                     "proofed lines (rule != nil)"
                                                     "unproofed lines (rule = nil)") " for this rule. You need exactly " numObligatories)))

          (> (count optional) numOptionals)
          (throw (Exception. (str "Too many [optional] " (if forward?
                                                           "unproofed lines (rule = nil)"
                                                           "proofed lines (rule != nil)") " for this rule. You can have at a max " numOptionals)))

          :else {:todo         (first todos)
                 :obligatories obligatories
                 :optional     optional})))))

(defn unify
  "Unifies all instances of `old` inside the `proof` with `new`."
  [proof old new]
  (if (symbol? old)
    (check-duplicates
      (clojure.walk/postwalk
        (fn [node]
          (if (map? node)
            (cond
              (symbol? (:body node)) (if (= (:body node) old)
                                       (assoc node :body new)
                                       node)
              (list? (:body node)) (assoc node :body (clojure.walk/prewalk-replace {old new} (:body node)))
              :else node)
            node))
        proof))
    (throw (Exception. (str "\"" old "\" is not a symbol. You can only unify symbols (not lists, vectors, etc.)")))))

(defn choose-option
  "Chooses option num on line to be inserted into proof.
   In case there is nothing to choose or the num is invalid, throws an exception."
  [proof line num]
  (let [item (get-item proof line)
        options (:body item)
        opt (get options num)]
    (cond
      (not (map? options))
      (throw (Exception. (str "There is nothing to choose in line " line)))

      (nil? opt)
      (throw (Exception. (str "There is no option \"" num "\" to choose")))

      :else
      (let [items (if (vector? opt) opt [opt])
            new-items (create-items items (:rule item))
            p1 (reduce #(add-after-item %1 item %2) proof new-items)
            ;; adjust the ids of the proof to point on the newly created items instead of the old "choose-item" before checking for duplicates
            p2 (adjust-ids p1 {(:plid item) (apply list (map #(if (vector? %) [(:plid (first %)) (:plid (last %))] (:plid %)) new-items))})]

        (check-duplicates (remove-item p2 item))))))

;; NEW LOGIC (prepare you formulas for the use in "step-f-inside" if you have to, see the temporal example)
;; maybe rules of the logic you're using have to be prepared to be used inside of a line
;; e.g. for temporal logik you have to ensure that input and output have the same timestamp (at x) and then remove it for later use
(defn prep-temporal
  [rule]
  (let [given (first (:given rule))
        conclusion (first (:conclusion rule))]
    (if (and (= (first given) 'at)
             (= (first conclusion) 'at))
      (if (= (second given) (second conclusion))
        {:plid         (:id rule)
         :given      [(first (drop 2 given))]
         :conclusion [(first (drop 2 conclusion))]}
        (throw (Exception.
                 (str "The rule \"" rule "\" is not usable for an inside step due to the two different time points of the premise and the conclusion"))))
      rule)))

(defn step-f-inside
  [proof rule line]
  (cond
    (> (rules/given-cnt rule) 1)
    (throw (Exception.
             (str "The rule " rule " needs more than 1 premise. Inside-Steps can only be executed with rules that need exactly 1 premise.")))

    (> (rules/concl-cnt rule) 1)
    (throw (Exception.
             (str "The rule " rule " has more than 1 conclusion. Inside-Steps only work with rules that have exactly 1 conclusion.")))

    (not (number? line))
    (throw (Exception. (str "\"" line "\" is not a line number.")))

    :else
    (let [;info (check-args proof rule [line] true)
          r (prep-temporal (rules/get-roth rule))
          rule-exe (fn [node]
                     (let [res (apply rules/apply-roth (conj [r true] [node] []))]
                       (if (empty? res)
                         node
                         (first res))))
          item (get-item proof line)
          body (:body item)
          new-body (clojure.walk/postwalk rule-exe body)
          new-item {:plid   (new-id)
                    :body new-body
                    :rule (pr-str rule (list (:plid item)))}]
      (if (= body new-body)
        (do
          (println "Inside-step hasn't changed anything")
          proof)
        (check-duplicates (add-after-item proof item new-item))))))

#_(defn trivial
    [proof line]
    (let [item (get-item proof line)
          body (:body item)
          new-body (clojure.walk/postwalk
                     (fn [node]
                       (if (list? node)
                         (let [res (rules/apply-trivials node)]
                           (if (empty? res) node (first res)))
                         node))
                     body)
          new-item (first (create-items [new-body]))]
      (if (= body new-body)
        (do (println "\"trivial\" hasn't changed anything.") proof)
        (if (:rule item)
          (check-duplicates (add-after-item proof item (assoc new-item :rule (str "\"trivial\" (" (:id item) ")"))))
          (if (or (true? new-body)
                  ;; for temporal logic "(at x true)"
                  (and (list? new-body)
                       (= (first new-body) 'at)
                       (true? (second (rest new-body)))))
            (check-duplicates (replace-item proof item {:id   (:id item)
                                                        :body (:body item)
                                                        :rule (str "\"trivial\" ()")}))
            (check-duplicates (replace-item (add-before-item proof item new-item)
                                            item
                                            {:id   (:id item)
                                             :body (:body item)
                                             :rule (str "\"trivial\" (" (:id new-item) ")")})))))))


; range of allowed arguments depending on the pattern of the roth
; brute force, works forward and backward provided that the patterns are not mixed!
(defn range-args
  [pattern]
  (let [g1 (count (filter #{:g1} pattern))
        range-g1 (if (zero? g1) [0 0] [1 g1])
        gm (count (filter #{:gm} pattern))
        range-gm [gm gm]
        gb (count (filter #{:gb} pattern))
        range-gb (if (zero? gb) [0 0] [0 (dec gb)])
        go (count (filter #{:go} pattern))
        range-go (if (zero? go) [0 0] [0 go])
        em (count (filter #{:em} pattern))
        range-em [em em]
        cm (count (filter #{:cm} pattern))
        range-cm [cm cm]
        co (count (filter #{:co} pattern))
        range-co (if (zero? co) [0 0] [0 co])]
    (mapv + range-g1 range-gm range-gb range-go range-em range-cm range-co)
    ))

(defn max-given
  "Maximal `plno` of a given parameter."
  [pattern]
  (apply max
         (->> pattern
              (filter #(= \g (first (name (first %)))))
              (map second)
              (filter number?))))

; was sind die Voraussetzungen?
; rule existiert
; forward ist erlaubt
; Zahl der Argumente ist erlaubt?? -- oder dies hier checken???
(defn match-argsv-f
  [roth argsv]
  ; get the pattern for forward step of the roth
  (loop [pattern (rules/roth-pattern roth :forward)
         args    argsv
         result  []]
    (let [p1 (first pattern) a1 (first args)]
      (if (nil? p1) 
        result
        (cond
          (nil? a1)  (recur (rest pattern) args (conj result [p1 :?]))
          (= p1 :gm) (recur (rest pattern) (rest args) (conj result [p1 a1]))
          (= p1 :g1) (recur (rest pattern) (rest args) (conj result [p1 a1]))
          (= p1 :em) (recur (rest pattern) (rest args) (conj result [p1 a1]))
          (= p1 :g?) (recur (rest pattern) args (conj result [p1 :?]))
          (= p1 :c?) (recur (rest pattern) args (conj result [p1 :?]))
          (= p1 :co) (recur (rest pattern) args (conj result [p1 a1]))
          ;(and (= p1 :co) (not (nil? a1))) (recur (rest pattern) args (conj result [p1 a1])); wird (nil? a1) nicht scxhon oben erledigt??
        )))))
                              
; wie geht die Logik?
; :gm -> argument übernehmen
; :g1 -> argument übernehmen
; :em -> argument übernehmen
; :g? -> :? einfüllem
; :co -> wenn noch ein Argument, dann einfüllen
; keine argumente mehr -> mit :? auffüllem

; Voraussetzungen -- siehe oben!!
(defn match-argsv-b
  [roth argsv]
  ; get the pattern for backward step of the roth
  (loop [pattern (rules/roth-pattern roth :backward)
         args    argsv
         result  []]
    (let [p1 (first pattern) a1 (first args)]
      (if (nil? p1)
        result
        (cond
          (nil? a1)  (recur (rest pattern) args (conj result [p1 :?]))
          (= p1 :cm) (recur (rest pattern) (rest args) (conj result [p1 a1]))
          (= p1 :gb) (recur (rest pattern) (rest args) (conj result [p1 a1]))
          (= p1 :go) (recur (rest pattern) (rest args) (conj result [p1 a1]))
          (= p1 :g?) (recur (rest pattern) args (conj result [p1 :?]))
          )))))

; wie geht die Logik?
; :cm -> argument übernehmen
; :gb -> argument übernehmen

(defn rel-params
  "Parameters for the logic relation given the pattern for the call."
  [proof pattern]
  (->> pattern
      (map second)
      (mapv #(if (number? %) (plbody proof %) %))
       ))

; das muss man schon beim check der Argumente verwenden!!
(defn find-next-todo-plno
  "`plno` of the next todo line following proof line with `plno`.
  A result of `0` means that there is no such line."
  [proof n]
  (let [pv  (vec (flatten proof))
        pv' (subvec pv n)
        no  (first (keep-indexed #(when (= :todo (:body %2)) %1) pv'))]
    (if (nil? no) 0 (+ (inc n) no))))
  
(defn plid-to-manip
  "Gives the `plid` of the todo line where the manipulation of the proof must occur."
  [proof pattern]
  (let [max-g (max-given pattern)
        todo-plno (find-next-todo-plno proof max-g)]
    (plno->plid proof todo-plno)))

(defn- check-user-input
  "Checks of user input to a proof step, independent of the direction of the step."
  [proof roth argsv]
  ; does the rule or theorem exist?
  (if (not (rules/roth-exists? roth))
    (throw (Exception. (format "There no such rule or theorem: %s" roth))))
  ; are the line numbers in argsv distinct?
  (if (and (not-empty argsv) (not (apply distinct? (filter number? argsv))))
    (throw (Exception. (format "There are duplicates in your arguments: %s" (str argsv)))))
  ; are the line numbers in argvs in the range of the proof?
  (if (not-empty argsv)
    (let [pl-cnt (count (flatten proof))
          nos (filter number? argsv)
          low (apply min nos)
          high (apply max nos)]
      (if (not (and (> low 0) (<= high pl-cnt)))
        (throw (Exception. (format "Line numbers must refer lines in the proof: %s" (str argsv)))))))
  )

(defn check-user-input-f
  "Checks the user input for a forward proof step, given as a roth and a vector.      
   Throws exceptions if input not valid, and      
   analyzes how to proceed the step. "
  [proof roth argsv]
  (check-user-input proof roth argsv) ; may throw exceptions
  (cond
    (not (rules/roth-forward? roth))
     (throw (Exception. (format "This rule can't be used in a forward step: %s"  roth))))
  ; TODO hier geht's weiter!!
  )

(defn step-f
  [proof roth argsv]
  (check-user-input-f proof roth argsv)
  #_(let [pattern (match-argsv-f roth argsv)
        
        rel-params (rel-params proof pattern)
        todo-plid (plid-to-manip proof pattern)
        result (rules/apply-roth roth rel-params)
        
        ]
  [pattern todo-plid result]))


(def p1
  [{:plid 1, :rule :premise, :body 'A}
   {:plid 2, :rule :premise, :body 'B}
   {:plid 4, :body :todo, :rule nil}
   {:plid 3, :body '(and A B), :rule nil}])

(step-f p1 :and-i [1 2])
(step-f p1 :and-i [1 1])
(step-f p1 :impl-i [4])
(step-f p1 :x-i [1 2])
(step-f p1 :or-i1 [1])
(step-f p1 :or-i2 [1])
(step-f p1 :and-e1 [4])
(step-f p1 :and-e2 [4])
(step-f p1 :tnd [])

(def p2
  [{:plid 1, :rule :premise, :body '(or A B)}
   {:plid 4, :body :todo, :rule nil}
   {:plid 3, :body 'X, :rule nil}])

(step-f p2 :or-e [1])
(step-f p2 :or-e [1 3])

#_(defn step-f
  "Performs a forward step on proof by applying rule on the lines"
  [proof rule & lines]
  (let [info (check-args proof rule lines true)
        todo-item (:todo info)
        obligatory-items (:obligatories info)
        optional-items (:optional info)
        ;; separate ids (from lines) and inputs (from user-input))
        obligatory-ids (map get-item-id (filter map? obligatory-items))
        obligatory-user-input (into [] (remove map? obligatory-items))

        obligatory-args (into [] (map item-to-rule-arg obligatory-items))
        optional-args (into [] (map item-to-rule-arg optional-items))
        rule-result (apply rules/apply-roth (conj [rule true] obligatory-args optional-args))]
    ;; the user-inputs will be attached to the :rule of a new line, after the source lines
    (if (empty? rule-result)
      (throw (Exception. (str "Incorrect parameters for the rule \"" rule "\". Please check the description.")))
      ;; add the used rule to the optional items
      (let [p1 (reduce #(replace-item %1 %2 {:plid   (:plid %2)
                                             :body (:body %2)
                                             :rule (pr-str rule obligatory-ids obligatory-user-input)}) proof optional-items)]
        (if (> (count rule-result) 1)
          ;; more than one possible result, the user has to decide which one fits his needs
          (add-before-item p1
                           todo-item
                           {:plid   (new-id)
                            :body (apply merge (map-indexed #(hash-map (inc %1) %2) rule-result))
                            :rule (pr-str rule obligatory-ids obligatory-user-input)})
          ;; only one possible result (which can contain several items to insert)
          (let [result (if (vector? (first rule-result)) (first rule-result) rule-result)
                new-items (create-items result (pr-str rule obligatory-ids obligatory-user-input))]
            ;; if there is no empty line, insert everthing behind the last obligatory item
            (check-duplicates
              (if todo-item
                (reduce #(add-before-item %1 todo-item %2) p1 new-items)
                (reduce #(add-after-item %1 (last obligatory-items) %2) p1 new-items)))))))))

(defn step-b
  "Performs a backward step on proof by applying rule on the lines"
  [proof rule & lines]
  (let [info (check-args proof rule lines false)
        todo-item (:todo info)
        obligatory-items (:obligatories info)
        optional-items (:optional info)
        optional-ids (map get-item-id optional-items)
        ;; separate user-inputs from obligatory-items
        obligatory-user-input (into [] (remove map? obligatory-items))
        obligatory-args (into [] (map item-to-rule-arg obligatory-items))
        optional-args (into [] (map item-to-rule-arg optional-items))
        rule-result (apply rules/apply-roth (conj [rule false] obligatory-args optional-args))]
    (cond
      (empty? rule-result)
      (throw (Exception. "Incorrect parameters for the given rule"))

      (> (count rule-result) 1)
      ;; more than one possible result, the user has to decide which one fits his needs
      (let [id (new-id)
            p1 (reduce #(replace-item %1 %2 {:plid   (:plid %2)
                                             :body (:body %2)
                                             :rule (pr-str rule (conj optional-ids id) obligatory-user-input)}) proof obligatory-items)]
        (add-after-item p1
                        todo-item
                        {:plid   id
                         :body (apply merge (map-indexed #(hash-map (inc %1) %2) rule-result))
                         :rule nil}))
      :else
      ;; only one possible result (which can contain several items to insert)
      (let [result (if (vector? (first rule-result)) (first rule-result) rule-result)
            new-items (create-items result)
            new-ids (map get-item-id new-items)
            p1 (reduce #(replace-item %1 %2 {:plid   (:plid %2)
                                             :body (:body %2)
                                             :rule (pr-str rule (concat new-ids optional-ids) obligatory-user-input)}) proof obligatory-items)
            ;; add proved items (e.g. subproofs) before the "..."-item and unproved items after it
            ;; That's wrong in the case of 'or-e'!!
            ; a hack !! TODO: rethink this, it would be better to allow 'or-e' to be forward!!
            ; all unproved
            all-unproved (let [unproved-items' (remove #(or (vector? %) (not (nil? (:rule %)))) new-items)]
                           (= (first new-items) (first unproved-items')))
            proved-items (if all-unproved () (filter #(or (vector? %)
                                                          (not (nil? (:rule %)))) new-items))
            unproved-items (if all-unproved (reverse new-items) (remove #(or (vector? %)
                                                                             (not (nil? (:rule %)))) new-items))

            p2 (reduce #(add-after-item %1 todo-item %2) p1 unproved-items)]
        (check-duplicates (reduce #(add-before-item %1 todo-item %2) p2 proved-items))))))


