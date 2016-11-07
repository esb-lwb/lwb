; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 Tobias Völzel, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.proof
  (:require [clojure.spec :as s]
            [clojure.set :as set]
            [clojure.zip :as zip]
            [lwb.pred.substitution :refer [substitution]]))

;; # Proof in natural deduction

;; Each proof line has a unique id `plid`.
(def plid 
  "Global counter for the ids of proof lines."
  (atom 0))

(defn new-plid
  "Generates new plid for proof lines.     
   Uses global `plid`." 
  []
  (swap! plid inc))

(defn reset-plid
  "Resets global `plid`."
  []
  (reset! plid 0))

;; Some rules generate new logical variables, e.g. new atomic propositions
;; They get new names beginning with the letter `V` and follwed by a unique number.

;; Caveat: Don't use this naming convention for propositions in formulas.

(def lvno
  "Global counter for the numbers of new variables."
  (atom 0))

(defn new-vsymbol
  "Generates a new name for a variable.     
   Uses global `lvno`."
  []
  (symbol (str \V (swap! lvno inc))))

(defn reset-lvno
  "Resets global `lvno`."
  []
  (reset! lvno 0))

(declare normalize)
(defn proof
  "Gives a new proof for the premises and the conclusion.     
   Uses global atoms `plid` and `lvno`."
  [premises conclusion]
  (do
    (reset-plid)
    (reset-lvno)
    (let [premises-vec (if-not (vector? premises) [premises] premises)
          premises-lines (vec (map #(hash-map :plid (new-plid) :body % :roth :premise) premises-vec))
          proof (conj premises-lines {:plid (new-plid) :body conclusion :roth nil})]
      (normalize proof))))

;; ## Functions to access proof lines 

(defn plid->plno
  "Returns the line number `plno` of the proof line with the given `plid`."
  [proof plid]
  (let [flat-proof (flatten proof)]
    (first (keep-indexed #(when (= plid (:plid %2)) (inc %1)) flat-proof))))

(defn plno->plid
  "Returns the id `plid` of the proof line at line number `plno` in the `proof`."
  [proof pos]
  (let [pline (nth (flatten proof) (dec pos) nil)]
    (:plid pline)))

(defn pline-at-plno
  "Proof line of the `proof` at line with number `plno`.      
   requires: `plno` valid."
  [proof plno]
  (let [fp (flatten proof)]
    (nth fp (dec plno))))

(defn plbody-at-plno
  "Body of the proof line at `plno`.     
   requires: `plno` valid."
  [proof plno]
  (:body (pline-at-plno proof plno)))

(defn pline-at-plid
  "Proof line of the `proof` at line with id `plid`.      
   requires: `plid` valid."
  [proof plid]
  (pline-at-plno proof (plid->plno proof plid)))

(defn scope
  "Returns the scope for an item inside a proof
   e.g. proof = [1 2 [3 4] [5 6 7] 8] & item = 5
   => scope = [1 2 [3 4] 5 6 7]"
  [proof pline]
  (if (contains? (set proof) pline)
    proof
    (loop [p proof
           result []]
      (cond (empty? p) nil
            (vector? (first p))
            (if-let [s (scope (first p) pline)]
              (vec (concat result s))
              (recur (subvec p 1) (conj result (first p))))
            :else (recur (subvec p 1) (conj result (first p)))))))

;; ## Functions for editing a proof

(defn add-above-plid
  "Adds a proof line or a subproof above the item with the given `plid`.    
   requires: the added plines have new plids!"
  [proof plid pline-or-subproof]
  (loop [loc (zip/vector-zip proof)]
    (if (zip/end? loc)
      (zip/node loc)
      (if (= (:plid (zip/node loc)) plid)
        (recur (zip/next (zip/insert-left loc pline-or-subproof)))
        (recur (zip/next loc))))))

(defn replace-plid
  "Replaces the proof line with the given `plid` with the new proof line.    
   requires: there are no more references to the old proof line."
  [proof plid pline]
  (loop [loc (zip/vector-zip proof)]
    (if (zip/end? loc)
      (zip/node loc)
      (if (= (:plid (zip/node loc)) plid)
        (recur (zip/next (zip/replace loc pline)))
        (recur (zip/next loc))))))

(defn remove-plid
  "Removes the proof line with the given `plid`.    
   requires: there are no more references to that proof line."
  [proof plid]
  (loop [loc (zip/vector-zip proof)]
    (if (zip/end? loc)
      (zip/node loc)
      (if (= (:plid (zip/node loc)) plid)
        (recur (zip/next (zip/remove loc)))
        (recur (zip/next loc))))))

;; ## Functions for the normalization of a proof

;; ### Adding todo lines if necessary

(defn new-todo-line
  "Generates a new todo line    
   Uses global atom `plid`!"
  []
  {:plid (new-plid), :body :todo, :roth nil})

(defn unproved-line?
  "Checks whether a proof line has no roth."
  [pline]
  (and (not= :todo (:body pline)) (nil? (:roth pline))))

(defn todoline?
  "Checks whether a proof line is a todo line."
  [pline]
  (= :todo (:body pline)))

(defn- insert-above?
  "Must a todo line be inserted above of this loc?    
  (1) the current proof line is not a subproof   
  and
  (2) the current proof line is not solved     
  and    
  (3) there is not already a todo line left of the current line."
  [loc]
  (let [prev-loc (zip/left loc)
        curr-line (zip/node loc)
        prev-line (when prev-loc (zip/node prev-loc))]
    (and (not (zip/branch? loc)) (unproved-line? curr-line) (not (todoline? prev-line)))))

(defn add-todo-lines
  "Returns proof with todo lines added.     
   Whenever there is a line in a proof without a rule, we have to insert a todo line
   above the proof line."
  [proof]
  (loop [loc (zip/next (zip/vector-zip proof))]
    (if (zip/end? loc)
      (zip/node loc)
      (if (insert-above? loc)
        (recur (zip/next (zip/insert-left loc (new-todo-line))))
        (recur (zip/next loc))))))

;; ### Removing  todo lines if necessary

(defn- remove-current?
  "Is the current loc a todo line and should it line be removed?    
  (1) the todo line is followed by a pline which is solved     
  or    
  (2) the todo line is at the end of a subproof or proof     
  or     
  (3) the todo line is follwed by another todo line     
  or
  (4) the todo line is followed by a subproof."
  [loc]
  (and (todoline? (zip/node loc))
       (or (and (not (nil? (zip/right loc)))
                     (not (nil? (:roth (zip/node (zip/right loc))))))
           (= (zip/rightmost loc) loc)
           (and (not (nil? (zip/right loc))) (todoline? (zip/node (zip/right loc))))
           (zip/branch? (zip/right loc)))))

(defn remove-todo-lines
  "Returns proof where todo lines that are solved are removed."
  [proof]
  (loop [loc (zip/vector-zip proof)]
    (if (zip/end? loc)
      (zip/node loc)
      (if (remove-current? loc)
        (recur (zip/next (zip/remove loc)))
        (recur (zip/next loc))))))

(def p [{:plid 1, :roth :premise, :body 'P1}
 {:plid 2, :roth :premise, :body 'P2}
 {:plid 4, :body :todo, :roth nil}
 {:plid 3, :body '(and P1 P2), :roth :and-i, :refs [1 2]}])

(remove-todo-lines p)
;; ### Handling duplicate bodies in a proof

(defn find-duplicates
  "Finds duplicates bodies in the proof that can be deleted.      
   Returns a map with the deletable plids as key and the replacement plids as value.     
   Only plines without rule, in the same scope and the same subproof will be marked as deletable."
  ([proof] (find-duplicates proof proof))
  ([proof sub]
   (let [dupl-scope (scope proof (last sub))
         ;; duplicates = duplicate bodies in scope but not :todo
         duplicates (disj (set (map first (filter #(> (val %) 1) (frequencies (map :body (remove vector? dupl-scope)))))) :todo)
         ;; duplicate-plines = the pline with these duplicate bodies
         duplicate-plines (filter #(contains? duplicates (:body %)) dupl-scope)
         ;; duplicate-plines grouped into a vactor of vector of plines with equal body
         equals (vec (map val (group-by :body duplicate-plines)))
         fn-smap (fn [equals]
                   (let [remain (map :plid (filter :roth equals))
                         delete (map :plid (filter (set sub) (remove :roth equals)))] ; just plines from the actual sub can be deleted
                     (reduce #(assoc %1 %2 (last remain)) {} delete)))
         ;; map with pairs of plids where the first can be replace by the second
         plid-map (apply merge (map fn-smap equals))]
     (reduce #(if (vector? %2) (merge %1 (find-duplicates proof %2)) %1) plid-map sub))))

(defn- adjust-refs
  "Upgrades the refs in the given proof according to the plid-map."
  [proof plid-map]
  (let [old-plines (vec (filter #(not-empty (set/intersection (set (keys plid-map)) (set (flatten (:refs %))))) (flatten proof))) 
        upg-plines (mapv #(assoc % :roth (:roth %) :refs (clojure.walk/prewalk-replace plid-map (:refs %))) old-plines)]
    (vec (reduce #(replace-plid %1 (:plid %2) %2) proof upg-plines))))

(defn remove-duplicates
  "Removes all duplicate bodies from proof and adjusts the changed ids of proof lines."
  [proof]
  (let [duplicates (find-duplicates proof)
        ;; if a body is the conclusion of a subproof but above already proved,
        ;; it's not deleted but marked with :repeat.
        fn-proved-results (fn [map [id1 id2]]
                            (let [delete-pline (pline-at-plid proof id1)
                                  replace-pline (pline-at-plid proof id2)
                                  delete-scope (scope proof delete-pline)
                                  plno1 (plid->plno proof id1)
                                  plno2 (plid->plno proof id2)]
                              (if (and (= delete-pline (last delete-scope))
                                       (or (not= delete-scope (scope proof replace-pline))
                                           (contains? #{:premise :assumption} (:roth replace-pline))))
                                (assoc map id1 id2) map)))
        proved-results (reduce fn-proved-results {} duplicates)
        fn-replace (fn [p [id1 id2]]
                     (let [pline (pline-at-plid p id1)]
                       (replace-plid p id1 {:plid id1
                                              :body (:body pline)
                                              :roth :repeat
                                              :refs [id2]})))
        new-proof1 (reduce fn-replace proof proved-results)
        deletions (reduce dissoc duplicates (map key proved-results))
        delete-plids (keys deletions)
        new-proof2 (reduce remove-plid new-proof1 delete-plids)]
    (adjust-refs new-proof2 deletions)))

(defn normalize
  "Removes duplicate lines, adjust leftover plids and removes todo lines if possible"
  [proof]
  (-> proof
      add-todo-lines
      remove-duplicates
      remove-todo-lines))

(defn replace-lvars
  "Replaces logical variables from core.logic like `_0` by generated variable names `V1`.     
  This must be done with all the formulas generated by a run of core.logic simultaneously."
  [bodies]
  (let [lvars (set (filter #(.startsWith (str %) "_") (flatten bodies)))
        smap (reduce #(assoc %1 %2 (new-vsymbol)) {} lvars)]
    (mapv #(if (symbol? %)
              (if (contains? lvars %) (get smap %) %)
              (clojure.walk/prewalk-replace smap %)) bodies )))

(defn eval-subs
  [[subst phi var t]]
  (substitution phi var t))

(declare new-subproof)
(defn new-pline
  "Creates a new proof line or subproof from body and [optional] rule and refs."
  ([body] (new-pline body nil nil))
  ([body roth refs]
   ;; handle special case that body = (infer ... or body = (substitution ...)
   (cond
     (and (seq? body) (= (first body) 'infer)) (new-subproof body)
     (and (seq? body) (= (first body) 'substitution)) {:plid (new-plid) :body (eval-subs body) :roth roth :refs refs}
     :else {:plid (new-plid) :body body :roth roth :refs refs})))

(defn new-subproof
  "Creates a new subproof from infer clause."
  [[infer assumptions claim]]
  (let [a  (if (vector? assumptions) assumptions [assumptions])
        al (mapv #(new-pline % :assumption nil) a)
        t  (new-pline :todo)
        cl (new-pline claim)]
    (conj al t cl)))

(defn new-plines
  "Creates all the new pline, that must be added to the proof"
  [bodies]
  (let [bodies' (replace-lvars bodies)
        ; a hack for handling lazy sequences!!
        non-lazy-bodies (clojure.walk/postwalk (fn [node]
                                          (if (instance? clojure.lang.LazySeq node)
                                            (apply list node)
                                            node)) bodies')]
    (mapv new-pline non-lazy-bodies)))

(defn proved?
  "Checks if a proof is fully proved.     
   Requires: the proof is normalized."
  [proof]
  (if (empty? proof)
    (throw (Exception. "The proof is empty")))
  (let [plines (flatten proof)]
    (empty? (filter #(= :todo (:body %))plines ))))
