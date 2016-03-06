; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 Tobias Völzel, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.proof)
 
;; utility functions
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

(defn line-to-id
  "Returns the id for the given line"
  [proof line]
  (if (not (vector? line))
    (:id (nth (flatten proof) (dec line)))
    [(line-to-id proof (first line)) (line-to-id proof (last line))]))

(defn id-to-line
  "Returns the line, which contains the item, with the given id"
  [proof id]
	(if (not (vector? id))
	  (loop [p (flatten proof)
	         l 1]
	    (if (= (:id (first p)) id)
	      l
	      (recur (rest p) (inc l))))
	  [(id-to-line proof (first id)) (id-to-line proof (last id))]))

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
                          (nil? (:rule (first p)))) (recur (subvec p 1) (conj u (first p)))
                     (vector? (first p)) (recur (into [] (concat (first p) (subvec p 1))) u)
                     :else (recur (subvec p 1) u)))]
    (if (not-empty unproved)
      (throw (Exception. (str "There are still unproved lines inside the proof
        (" (clojure.string/join " " (map #(id-to-line proof %) (map :id unproved))) ")")))
      true)))

;; -----------------
;; functions for editing the proof
(defn edit-proof
  [proof item newitem mode]
  (let [index (.indexOf proof item)]
    (if (not= index -1)
      (condp = mode
        :add-before (with-meta (into [] (concat (subvec proof 0 index) [newitem] (subvec proof index))) {:found? true})
        :add-after  (with-meta (into [] (concat (subvec proof 0 (inc index)) [newitem] (subvec proof (inc index)))) {:found? true})
        :replace    (with-meta (into [] (concat (subvec proof 0 index) [newitem] (subvec proof (inc index)))) {:found? true})
        :remove     (with-meta (into [] (concat (subvec proof 0 index) (subvec proof (inc index)))) {:found? true}))
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

(defn add-after-line
  [proof after newitem]
  (let [item (get-item proof after)]
     edit-proof proof item newitem :add-after)) 

(defn add-after-item
  [proof after newitem]
  (edit-proof proof after newitem :add-after))

(defn add-before-line
  [proof before newitem]
  (let [item (get-item proof before)]
    (edit-proof proof item newitem :add-before)))

(defn add-before-item
  [proof before newitem]
  (edit-proof proof before newitem :add-before))

(defn remove-item
  [proof item]
  (edit-proof proof item nil :remove))

(defn replace-line
  [proof line newitem]
  (let [item (get-item proof line)]
    (edit-proof proof item newitem :replace)))

(defn replace-item
  [proof item newitem]
  (edit-proof proof item newitem :replace))
;; -------------------------------
