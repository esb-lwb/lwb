; lwb Logic WorkBench -- Predicate logic - Substitution

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.pred.substitution
  (:require [clojure.set :as set]
            [lwb.pred :refer [const? op? quantor? eq?]]
            [clojure.zip :as zip]))

(defn vars-in-term
  "Given a term `t` returns the set of variables in `t`.      
   Remark: Since we have no given signature here, we can't distinguish variables and unary functions.     
   requires: `t` is a term without unary functions."
  [t]
  (let [variable? #(and (symbol? %) (not (or (op? %) (boolean? %) (quantor? %) (eq? %))))
        seq-ok?   #(and (seq? %) (not (or (op? (first %)) (boolean? (first %)) (quantor? (first %)) (eq? (first %)))))]
  (cond
    (const? t)     #{}
    (variable? t)  #{t}
    (seq-ok? t)    (reduce #(set/union %1 (vars-in-term %2)) #{} (rest t))
    :else          (throw (Exception. (format "Should be a term in a substitution, not: %s" t))))))

(defn bounded?
  "Is variable `var` bounded in `path`?"
  [path var]
  (let [bounded-vars (reduce #(if (quantor? (first %2))
                               (set (concat %1 (second %2))) %1) #{} path)]
    (contains? bounded-vars var)))

(defn paths-with-free-var
  "Returns a vector with all paths in `phi` in which there is a free occurence of `var`."
  [phi var]
  (let [paths (loop [loc (zip/next (zip/seq-zip phi))
                     result []]
                (cond
                  (zip/end? loc) result
                  (= (first loc) var) (recur (zip/next loc) (conj result (zip/path loc)))
                  :else (recur (zip/next loc) result)))]
    (vec (filter #(not (bounded? % var)) paths))))

(defn in-scope?
  "Is `var1` in the scope of a quantor for `var2` in vector `pathv`?"
  [pathv var1 var2]
  (let [paths-with-var2 (filter #(and (quantor? (first %)) (contains? (set (second %)) var2)) pathv)]
    (some true? (map #(contains? (set (flatten %)) var1) paths-with-var2))))

;; A term `t` for a variable `var` in a formula `phi` iff there is no free `var` that's in the scope
;; of any of the variables in `t`.     
;; In other words: In the substitution of `var` by `t` no variable in `t` gets in the scope of a quantor.

(defn freefor? 
  "Is the term `t` free for variable `var` in `phi`?"
  [phi var t]
  (let [paths (paths-with-free-var phi var)
        tvars (set/difference (vars-in-term t) #{var}) ; we don't need to check var itself
        scope? (fn [tvar paths] (map #(in-scope? % var tvar) paths))]
    (not-any? true? (flatten (map #(scope? %1 paths) tvars)))))

(defn substitution
  "Substitution of variable `var` by term `t` in formaula `phi`."
  [phi var t]
  (if (not (freefor? phi var t))
    (throw (Exception. (format "The term %s is not free for %s in the formula %s." t var phi))))
  (apply list
         (loop [loc (zip/seq-zip phi)]
           (if (zip/end? loc)
             (zip/node loc)
             (if (and (= var (zip/node loc)) (not (bounded? (zip/path loc) var)))
               (recur (zip/next (zip/replace loc t)))
               (recur (zip/next loc))))))
  )

