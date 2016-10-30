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
   Returns a map with the deletable plids as key and the replacement plids as value.
   Only plines without rule, in the same scope and the same subproof will be marked as deletable"
  ([proof] (find-duplicates proof proof))
  ([proof sub]
   (let [scope (get-scope proof (last sub))
         ;; duplicates = duplicate bodies in scope but not :todo
         duplicates (disj (set (map first (filter #(> (val %) 1) (frequencies (map :body (remove vector? scope)))))) :todo)
         ;; duplicate-plines = the pline with these duplicate bodies
         duplicate-plines (filter #(contains? duplicates (:body %)) scope)
         ;; duplicate-plines grouped into a vactor of vector of plines with equal body
         equals (into [] (map val (group-by :body duplicate-plines)))
         fn-smap (fn [equals]
                   (let [remain (map :plid (filter :roth equals))
                         delete (map :plid (filter (set sub) (remove :roth equals)))] ; just plines from the actual sub can be deleted
                     (reduce #(assoc %1 %2 (last remain)) {} delete)))
         ;; map with pairs of plids where the first can be replace by the second
         plid-map (apply merge (map fn-smap equals))]
     (reduce #(if (vector? %2) (merge %1 (find-duplicates proof %2)) %1) plid-map sub))))

(defn- adjust-refs
  "Upgrades the refs in the given proof according to the plid-map"
  [proof plid-map]
  (let [old-plines (flatten proof) ;; just all the plines of the current proof
        upg-plines (mapv #(assoc % :roth (:roth %) :refs (clojure.walk/prewalk-replace plids-map (:refs %))) old-plines)]
    (vec (reduce #(replace-plid %1 (:plid %2) %2) proof upg-plines))))

(def p12
  [{:plid 1 :body 'A :roth :and-i}
   {:plid 3 :body :todo}
   {:plid 6 :body 'C :roth :x :refs [2 7]}
   {:plid 5 :body 'B}
   {:plid 7 :body 'B :roth :x}
   {:plid 2 :body 'A :roth nil}
   ])

(def plids-map (find-duplicates p12))
plids-map

(adjust-refs p12 plids-map)

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
                            (let [delete-pline (pline-at-plid proof id1)
                                  replace-pline (pline-at-plid proof id2)
                                  delete-scope (get-scope proof delete-pline)]
                              (if (and (= delete-pline (last delete-scope))
                                       (not= delete-scope (get-scope proof replace-pline)))
                                (assoc map id1 id2) map)))
        proved-results (reduce fn-proved-results {} duplicates)
        fn-replace (fn [p [id1 id2]]
                     (let [pline (pline-at-plid p id1)]
                       (replace-plid p pline {:plid   id1
                                              :body (:body pline)
                                              :roth "repeat"
                                              :refs [id2]})))
        new-proof1 (reduce fn-replace proof proved-results)
        deletions (reduce dissoc duplicates (map key proved-results))
        delete-plines (map #(pline-at-plid proof %) (map key deletions))
        new-proof2 (reduce remove-item new-proof1 delete-plines)]
    (adjust-refs new-proof2 deletions)))

(remove-duplicates p12)

(defn check-duplicates
  "Removes duplicate lines, adjust leftover plids and removes todo lines if possible"
  [proof]
  (-> proof
      remove-duplicates
      remove-todo-lines))
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
    (every? identity (map check pattern' argsv))))

(defn max-given
  "Maximal `plno` of the parameters for the givens."
  [match-pattern]
  (if (= 1 (count match-pattern))   ; just one conclusion 
    0
    (apply max
           (->> match-pattern
                (filter #(= \g (first (name (first %)))))
                (map second)
                (filter number?)))))

(max-given [[:c? :?]])
(max-given [[:gm 2] [:c? :?]])

(defn match-argsv-f
  "Assigns the arguments to the pattern of a forward step.     
   Returns match-pattern.      
   requires: roth exists, forward is allowed"
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
          (= p1 :co) (recur (rest pattern) args (conj result [p1 a1])) )))))
                              
(defn match-argsv-b
  "Assigns the arguments to the pattern of a backward step.     
   Returns match-pattern.      
   requires: roth exists, backward is allowed"
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
          (= p1 :g?) (recur (rest pattern) args (conj result [p1 :?])) )))))

(defn rel-params
  "Parameters for the logic relation given the match pattern for the call."
  [proof match-pattern]
  (->> match-pattern
      (map second)
      (mapv #(if (number? %) (plbody proof %) %))))

(defn- find-next-todo-plno
  "`plno` of the next todo line following proof line with `plno`.
  A result of `0` means that there is no such line.
  requires: n is not out of bound of the proof."
  [proof n]
  (let [pv  (vec (flatten proof))
        pv' (subvec pv n)
        no  (first (keep-indexed #(when (= :todo (:body %2)) %1) pv'))]
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
                             plines (map #(pline proof %) plnos)
                             scope (get-scope proof (pline proof max-plno))]
                         (every? #(contains? (set scope) %) plines)))))


(defn- check-user-input
  "Checks of user input to a proof step, independent of the direction of the step."
  [proof roth argsv]
  ; does the rule or theorem exist?
  (if (not (rules/roth-exists? roth))
    (throw (Exception. (format "There's no such rule or theorem: %s" roth))))
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
  ; can the roth be used in a forward step?
  (if (not (rules/roth-forward? roth))
     (throw (Exception. (format "This rule can't be used in a forward step: %s"  roth))))
  (let [pattern (rules/roth-pattern roth :forward)
        range   (range-args-f pattern)]
    ; size of argsv okay?
    (if (not (and (>= (count argsv) (first range)) (<= (count argsv) (second range))))
      (throw (Exception. (format "The number of arguments following the rule or theorem must be in the range: %s"  range))))
    ; kind of args okay?
    (if (not (type-args-ok? pattern argsv))
      (throw (Exception. (format "Type of arguments doesn't match the call pattern: %s" pattern))))
    (let [match-pattern (match-argsv-f roth argsv)
          todo-plid (plid-to-manip proof match-pattern)]
      (if (zero? todo-plid)
        (throw (Exception. "Arguments refering givens must be above a todo line")))
      ; TODO: keines der Argumente zeigt auf einen todo-line
      ; TODO: :gm muss eine Nummer haben
      ; TODO: mindestens eines der g1 muss eine Nummer haben
      ; TODO: sieht so aus, dass type-args-ok komplexer ausgelegt werden muss
      (if (not (concl-okay? match-pattern (plid->plno proof todo-plid)))
        (throw (Exception. "Arguments refering conclusions must be below a todo line")))
      (if (not (scope-okay? proof match-pattern))
        (throw (Exception. "Arguments must all be in the same scope.")))
      {:match-pattern match-pattern
       :todo-plid     todo-plid
       :refs-pattern  (map #(if (number? (second %)) [(first %) (plno->plid proof (second %))] %) match-pattern)}))
  )

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
          (= (second rp1) :?)                           (recur (rest rp) (rest np) (conj result [(first rp1) np1-plid]))
          :else                                         (recur (rest rp) np (conj result rp1)) )))))

(defn- upgrade-refs
  "Upgrades the refs in the given vector or proof at the given plids with roth and refs."
  [proof plids roth refs]
  (let [old-plines (mapv #(pline proof (plid->plno proof %)) plids)
        upg-plines (mapv #(assoc % :roth roth :refs refs) old-plines)]
    (vec (reduce #(replace-plid %1 (:plid %2) %2) proof upg-plines))
  ))

(defn step-f
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
          refs (mapv second (filter #(= \g (first (name (first %)))) refs-pattern'))
          new-plines' (upgrade-refs new-plines (mapv second (filter #(= :c? (first %)) refs-pattern')) roth refs)
          proof' (upgrade-refs proof (mapv second (filter #(= :co (first %)) refs-pattern')) roth refs)
          new-proof (vec (reduce #(add-above-plid %1 todo-plid %2) proof' new-plines'))
          ]
      (check-duplicates new-proof)
      )
    ))
    

(def proof1
  [{:plid 1, :roth :premise, :body 'A}
   {:plid 5, :roth :premise, :body 'B}
   {:plid 4, :body :todo, :roth nil}
   {:plid 3, :body '(and A B), :roth nil}])

(comment
  (step-f proof1 :and-i [1 2])
  (step-f proof1 :and-e1 [1])
  (step-f proof1 :and-i [:x 1])
  (step-f proof1 :impl-i [4])
  (step-f proof1 :x-i [1 2])
  (step-f proof1 :or-i1 [1])
  (step-f proof1 :or-i2 [2])
  (step-f proof1 :and-e1 [4])
  (step-f proof1 :and-e2 [4])
  (step-f proof1 :tnd [])
  (step-f proof1 :equal-e [1 2 'A 'B])
  (step-f proof1 :equal-e [1 2 3 'B])
  )  

(def proof2
  [{:plid 8, :roth :premise, :body '(or A B)}
   {:plid 4, :body :todo, :roth nil}
   {:plid 3, :body 'X, :roth nil}])

(comment
  ; Wenn subproof, dann wird die plid der ersten Zeile als Vektor genommen
  (step-f proof2 :or-e [1])
  (step-f proof2 :or-e [1 3])
  )

(defn check-user-input-b
"Checks the user input for a backward proof step, given as a roth and a vector.      
 Throws exceptions if input not valid, and      
 analyzes how to proceed the step. "
[proof roth argsv]
(check-user-input proof roth argsv) ; may throw exceptions
; can the roth be used in a forward step?
(if (not (rules/roth-backward? roth))
  (throw (Exception. (format "This rule can't be used in a forward step: %s"  roth))))
(let [pattern (rules/roth-pattern roth :backward)
      range   (range-args-b pattern)]
  ; size of argsv okay?
  (if (not (and (>= (count argsv) (first range)) (<= (count argsv) (second range))))
    (throw (Exception. (format "The number of arguments following the rule or theorem must be in the range: %s"  range))))
  ; kind of args okay?
  (if (not (type-args-ok? pattern argsv))
    (throw (Exception. (format "Type of arguments doesn't match the call pattern: %s" pattern))))
  (let [match-pattern (match-argsv-b roth argsv)
        concl-plid (plno->plid proof (first argsv))]
    ; TODO: concl-plid zeigt wirkliuch auf eine conclusion line
    ; TODO: alle g Argumente müssen oberhalb einer todo-line sein und oberhalb von concl
    ; TODO: keines der Argumente zeigt auf einen todo-line
    ; TODO: höchstens alle -1 der gb muss eine Nummer haben
    ; TODO: sieht so aus, dass type-args-ok komplexer ausgelegt werden muss
    (if (not (scope-okay? proof match-pattern))
      (throw (Exception. "Arguments must all be in the same scope.")))
    {:match-pattern match-pattern
     :concl-plid    concl-plid
     :refs-pattern  (map #(if (number? (second %)) [(first %) (plno->plid proof (second %))] %) match-pattern)}))
)

(defn- shift1
  "Shifts first element of vector `v` at the end of the vector."
  [v]
  (conj (vec (rest v)) (first v)))

(shift1 [1 2 3])

(defn step-b
  [proof roth argsv]
  (let [infos (check-user-input-b proof roth argsv)
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
          new-proof (vec (reduce #(add-above-plid %1 todo-plid %2) proof' new-plines))
          ]
      (check-duplicates new-proof)
      )
    ))
    

(comment
  (step-b proof1 :and-i [4])
  (step-b proof1 :and-i [4 1])
  (step-b proof2 :or-e [3])
  (step-b proof2 :or-e [3 1])
  )