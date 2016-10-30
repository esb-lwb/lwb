; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 Tobias Völzel, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.proof
  (:require [clojure.spec :as s]
            [clojure.zip :as zip]))

;; IDs for proof line
(def plid 
  "Global conter for the ids of proof lines."
  (atom 0))

(defn new-plid
  "Generates new id for proof lines.     
   Uses global `plid`." 
  []
  (swap! plid inc))

(defn reset-plid
  "Resets global `plid`."
  []
  (reset! plid 0))

;; Names for free logical variables, i.e. new variables
(def lvno
  "Global counter for the numbers if new variables."
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

;; Structure of a proof line

;; Keys in `:pline`
(s/def ::plid int?)
(s/def ::body (s/or :symbol symbol? :fml list? :keyword #{:todo}))
(s/def ::roth (s/nilable keyword?))
(s/def ::refs vector?)

;; A proof line has a unique proof line id `:plid`,      
;; a `:body` which is a formula or a special keyword,     
;; then the name of the rule or theorem `:roth` and if the rule is specified the      
;; `:refs` i.e. the plids of proof lines to which the application of the rule references
(s/def ::pline
  (s/keys :req-un [::plid ::body ::roth] :opt-un [::refs]))

;; A proof is a nested vector of proof lines and subproofs
(s/def ::proof (s/and vector? (s/* (s/or :pline ::pline :subproof ::proof))))

(defn new-todo-line
  "Generates a new todo line    
   Uses global atom `plid`!"
  []
  {:plid (new-plid), :body :todo, :roth nil})

(defn- unproved-line?
  "Checks whether a proof line has no roth."
  [pline]
  (and (not= :todo (:body pline)) (nil? (:roth pline))))

(defn- todoline?
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
        prev-line (if prev-loc (zip/node prev-loc) nil)]
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

; the implementation assumes that if there is a todo line,
; it is above of a regular line
(defn- remove-current?
  "Is the current loc a todo line and should it line be removed?    
  (1) the todo line is followed by a pline which is solved     
  or    
  (2) the todo line is followed by a subproof."
  [loc]
  (and (todoline? (zip/node loc))
       (or (not (nil? (:roth (zip/node (zip/right loc)))))
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

(defn add-below-plid
  "Adds a proof line or a subproof below the item with the given `plid`.    
   requires: the added plines have new plids!"
  [proof plid pline-or-subproof]
  (loop [loc (zip/vector-zip proof)]
    (if (zip/end? loc)
      (zip/node loc)
      (if (= (:plid (zip/node loc)) plid)
        (recur (zip/next (zip/insert-right loc pline-or-subproof)))
        (recur (zip/next loc))))))

(add-below-plid
  [{:plid 21, :body :todo, :roth nil}
   {:plid 1, :body '(or P (not P)), :roth nil}]
  21
  {:plid 2, :body 'A, :roth :x}
  )

(add-below-plid
  [{:plid 21, :body :todo, :roth nil}
   {:plid 1, :body '(or P (not P)), :roth nil}]
  1
  {:plid 2, :body 'A, :roth :x}
  )

(add-below-plid
  [{:plid 21, :body :todo, :roth nil}
   {:plid 1, :body '(or P (not P)), :roth nil}]
  21
  [{:plid 2, :body 'A, :roth :x}
   {:plid 3, :body 'A, :roth :x}]
  )

(add-below-plid
  [{:plid 21, :body :todo, :roth nil}
   [{:plid 22, :body :todo, :roth nil}
    {:plid 23, :body 'A, :roth :and-e}]
   {:plid 1, :body '(or P (not P)), :roth nil}]
  22
  [{:plid 2, :body 'A, :roth :x}
   {:plid 3, :body 'A, :roth :x}]
  )

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

(add-above-plid
  [{:plid 21, :body :todo, :roth nil}
   {:plid 1, :body '(or P (not P)), :roth nil}]
  1
  {:plid 2, :body 'A, :roth :x}
  )

(add-above-plid
  [{:plid 21, :body :todo, :roth nil}
   {:plid 1, :body '(or P (not P)), :roth nil}]
  21
  {:plid 2, :body 'A, :roth :x}
  )

(add-above-plid
  [{:plid 21, :body :todo, :roth nil}
   {:plid 1, :body '(or P (not P)), :roth nil}]
  21
  [{:plid 2, :body 'A, :roth :x}
   {:plid 3, :body 'A, :roth :x}]
  )

(add-above-plid
  [{:plid 21, :body :todo, :roth nil}
   [{:plid 22, :body :todo, :roth nil}
    {:plid 23, :body 'A, :roth :and-e}]
   {:plid 1, :body '(or P (not P)), :roth nil}]
  23
  [{:plid 2, :body 'A, :roth :x}
   {:plid 3, :body 'A, :roth :x}]
  )

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

(remove-plid
  [{:plid 21, :body :todo, :roth nil}
   {:plid 1, :body '(or P (not P)), :roth nil}]
  1
  )

(remove-plid
  [{:plid 21, :body :todo, :roth nil}
   {:plid 1, :body '(or P (not P)), :roth nil}]
  21
  )

(remove-plid
  [{:plid 21, :body :todo, :roth nil}
   [{:plid 22, :body :todo, :roth nil}
    {:plid 24, :body :todo, :roth nil}
    {:plid 23, :body 'A, :roth :and-e}]
   {:plid 1, :body '(or P (not P)), :roth nil}]
  24
  )

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

(replace-plid
  [{:plid 21, :body :todo, :roth nil}
   {:plid 1, :body '(or P (not P)), :roth nil}]
  1
  {:plid 2, :body 'A, :roth :x}
  )

(replace-plid
  [{:plid 21, :body :todo, :roth nil}
   {:plid 1, :body '(or P (not P)), :roth nil}]
  21
  {:plid 2, :body 'A, :roth :x}
  )

; geht auch -- nötig??
(replace-plid
  [{:plid 21, :body :todo, :roth nil}
   {:plid 1, :body '(or P (not P)), :roth nil}]
  21
  [{:plid 2, :body 'A, :roth :x}
   {:plid 3, :body 'A, :roth :x}]
  )

(replace-plid
  [{:plid 21, :body :todo, :roth nil}
   [{:plid 22, :body :todo, :roth nil}
    {:plid 23, :body 'A, :roth :and-e}]
   {:plid 1, :body '(or P (not P)), :roth nil}]
  23
  {:plid 2, :body 'A, :roth :x}
  )

(defn proof
  "Gives a new proof for the premises and the conclusion.
   Uses global atom `plid`."
  [premises conclusion]
  (do
    (reset-plid)
    (reset-lvno)
    (let [premises-vec (if-not (vector? premises) [premises] premises)
          premises-lines (vec (map #(hash-map :plid (new-plid) :body % :roth :premise) premises-vec))
          proof-lines (conj premises-lines {:plid (new-plid) :body conclusion :roth nil})]
      (add-todo-lines proof-lines))))

(comment
  (proof '[A B] 'X)
  (proof '[A B C] 'X)
  (proof 'A 'X)
  (proof [] 'X)
  (lwb.nd.printer/pprint (proof '[A B] 'X))
  (lwb.nd.printer/pprint (proof '[A B] '(impl A B)))
  )

(def p1
  [{:plid 1, :roth :premise, :body 'A}
   {:plid 2, :roth :premise, :body 'B}
   {:plid 4, :body :todo, :roth nil}
   {:plid 3, :body '(and A B), :roth nil}])

(defn pline 
  "Proof line of the `proof` at line with number `plno`.      
   requires: `plno` valid."
  [proof plno]
  (let [fp (flatten proof)]
    (nth fp (dec plno))))

(pline p1 1)
(pline p1 4)

(defn plbody
  "Body of the proof line at `plno`.     
   requires: `plno` valid."
  [proof plno]
  (:body (pline proof plno)))

(plbody p1 1)
(plbody p1 4)

(defn replace-lvars
  "Replaces logical variables from core.logic like `_0` by generated variable names `V1`.     
  This must be done with all the formulas generated by a run of core.logic simultaneously."
  [bodies]
  (let [lvars (set (filter #(.startsWith (str %) "_") (flatten bodies)))
        smap (reduce #(assoc %1 %2 (new-vsymbol)) {} lvars)]
    (mapv #(if (symbol? %)
              (if (contains? lvars %) (get smap %) %)
              (clojure.walk/prewalk-replace smap %)) bodies )))

(reset-lvno)
(replace-lvars ['(and _0 _1) '(and _0 (or _1 _0) _2)])

; TODO: substitution bei new-pline

(declare new-subproof)
(defn new-pline
  "Creates a new proof line or subproof from body and [optional] rule and refs."
  ([body] (new-pline body nil nil))
  ([body rule refs]
   ;; handle special case that body = (infer ... or body = (substitution ...)
   (cond
     (and (seq? body) (= (first body) 'infer)) (new-subproof body)
     (and (seq? body) (= (first body) 'substitution)) :substitution
     :else {:plid (new-plid) :body body :roth rule :refs refs})))

(defn new-subproof
  "Creates a new subproof from infer clause."
  [[infer assumptions claim]]
  (let [a  (if (vector? assumptions) assumptions [assumptions])
        al (mapv #(new-pline % :assumption nil) a)
        t  (new-pline :todo)
        cl (new-pline claim)]
    (conj al t cl)))

(new-pline '(infer phi psi))
(new-pline '(infer [phi x] psi))
(new-pline '(and A B) :and-i [1 2])
(new-pline '(and A B))
(new-pline 'A)
(new-pline '(infer phi psi))

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

(new-plines ['A '(and A B) '(infer A x)])
;; TODO

(defn get-item
  "Returns the item from proof on line. 
   line x => returns item on line x
   line [x y] => returns subproof starting on line x (including all contained items and/or subproofs)"
  [proof line]
  (if (not (vector? line))
    (nth (flatten proof) (dec line))
    (loop [p proof
           l 1]
      (cond
        (empty? p) nil
        (= (first line) l) (first p)
        (vector? (first p)) (recur (into [] (concat (first p) (subvec p 1))) (inc l))
        :else (recur (subvec p 1) (inc l))))))

; seems to be not used
#_(defn line-to-id
    "Returns the id for the given line"
    [proof line]
    (if (not (vector? line))
      (:plid (nth (flatten proof) (dec line)))
      [(line-to-id proof (first line)) (line-to-id proof (last line))]))


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

(def p1
  [{:plid 21, :body :todo, :roth nil}
   [{:plid 22, :body :todo, :roth nil}
    {:plid 23, :body 'A, :roth :and-e}
    [{:plid 321, :body :todo, :roth :nil}]]
   {:plid 1, :body '(or P (not P)), :roth nil}]
  )
(plno->plid p1 1)
(plno->plid p1 2)
(plno->plid p1 3)
(plno->plid p1 4)
(plno->plid p1 5)
(plno->plid p1 6) ;=> nil

(plid->plno p1 21)
(plid->plno p1 22)
(plid->plno p1 1)
(plid->plno p1 11) ;=> nil

(defn pline-at-plid
  "Returns the pline the the given plid."
  [proof plid]
  (pline proof (plid->plno proof plid)))

(defn get-scope
  "Returns the scope for an item inside a proof
   e.g. proof = [1 2 [3 4] [5 6 7] 8] & item = 5
   => scope = [1 2 [3 4] 5 6 7]"
  [proof item]
  (if (contains? (set proof) item)
    proof
    (loop [p proof
           scope []]
      (cond (empty? p) nil
            (vector? (first p))
            (if-let [s (get-scope (first p) item)]
              (into [] (concat scope s))
              (recur (subvec p 1) (conj scope (first p))))
            :else (recur (subvec p 1) (conj scope (first p)))))))

(get-scope [1 2 3] 2)
(get-scope [1 2 [3 4] [5 6 7] 8] 2)

(defn proved?
  "Checks if a proof is fully proved.
   If not throws an Exception with a description which lines are still unproved"
  [proof]
  (if (empty? proof)
    (throw (Exception. "The proof is empty/There is no proof")))
  (let [unproved (loop [p proof
                        u []]
                   (cond
                     (empty? p) u
                     (and (map? (first p))
                          (nil? (:roth (first p)))) (recur (subvec p 1) (conj u (first p)))
                     (vector? (first p)) (recur (into [] (concat (first p) (subvec p 1))) u)
                     :else (recur (subvec p 1) u)))]
    (if (not-empty unproved)
      (throw (Exception. (str "There are still unproved lines inside the proof
        (" (clojure.string/join " " (map #(pline-at-plid proof %) (map :id unproved))) ")")))
      true)))

;; -----------------
;; functions for editing the proof
(defn edit-proof
  [proof item newitem mode]
  (let [index (.indexOf proof item)]
    (if (not= index -1)
      (condp = mode
        :add-before (with-meta (into [] (concat (subvec proof 0 index) [newitem] (subvec proof index))) {:found? true})
        :add-after (with-meta (into [] (concat (subvec proof 0 (inc index)) [newitem] (subvec proof (inc index)))) {:found? true})
        :replace (with-meta (into [] (concat (subvec proof 0 index) [newitem] (subvec proof (inc index)))) {:found? true})
        :remove (with-meta (into [] (concat (subvec proof 0 index) (subvec proof (inc index)))) {:found? true}))
      (loop [p proof
             res []]
        (cond
          (empty? p) (with-meta res {})
          (vector? (first p))
          (let [v (edit-proof (first p) item newitem mode)]
            (if (:found? (meta v))
              (with-meta (into [] (concat res [v] (subvec p 1))) {:found? true})
              (recur (subvec p 1) (conj res v))))
          :else (recur (subvec p 1) (conj res (first p))))))))

; not used
#_(defn add-after-line
    [proof after newitem]
    (let [item (get-item proof after)]
      edit-proof proof item newitem :add-after))

(defn add-after-item
  [proof after newitem]
  (edit-proof proof after newitem :add-after))

; not used
#_(defn add-before-line
    [proof before newitem]
    (let [item (get-item proof before)]
      (edit-proof proof item newitem :add-before)))

(defn add-before-item
  [proof before newitem]
  (edit-proof proof before newitem :add-before))

(defn remove-item
  [proof item]
  (edit-proof proof item nil :remove))

; not used
#_(defn replace-line
    [proof line newitem]
    (let [item (get-item proof line)]
      (edit-proof proof item newitem :replace)))

(defn replace-item
  [proof item newitem]
  (edit-proof proof item newitem :replace))
;; -------------------------------
