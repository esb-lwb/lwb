; lwb Logic WorkBench -- Natural deduction, checks for unify

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.swap.common)

(defn flat 
  "Like clojure.core/flatten but works with symbols too."
  [s]
  (if (sequential? s)
    (mapcat flat s)
    (list s)))

(defn involved-bodies
  "Vector of pairs of plno and body of plines in the `proof` that contain `old`.
   Requires: `old` is a symbol, not a list."
  [proof old]
  (let [bodies (map :body (flatten proof))
        involved?  (fn [idx-pline] (contains? (set (flat (second idx-pline))) old))]
    (vec (filter involved? (map-indexed (fn [idx pline][(inc idx) pline]) bodies)))))
