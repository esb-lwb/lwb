; lwb Logic WorkBench -- Visualisation of formulae

; Copyright (c) 2016 Burkhardt Renz, Juan Markowich THM. 
; All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.vis
  (:require [clojure.zip :as zip]))

(def ^:private tikz-header
  "\\documentclass{standalone}
   \\usepackage[english]{babel}
   \\usepackage{tikz-qtree}
   \\tikzset{every tree node/.style={rectangle,rounded corners=5mm}}
   \\tikzset{edge from parent/.append style={->}}
   \\begin{document}
   \\begin{tikzpicture}
   \\Tree")

(def ^:private tikz-footer
  "\\end{tikzpicture}
   \\end{document}")

(defn- first?
  [loc]
  (nil? (-> loc zip/left)))

(defn- end?
  [loc]
  (= :end (-> loc zip/node)))

(defn- mark-end-of-branch [phi]
  (loop [loc (zip/seq-zip (seq phi))]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next
               (if (zip/branch? loc)
                 (let [inserted-loc (zip/insert-right (-> loc zip/down zip/rightmost) :end)]
                   (-> inserted-loc zip/leftmost))
                 loc))))))

(mark-end-of-branch '(and (or p q) q))

(defn- mapfn
  [loc]
  (let [n (zip/node loc)]
    (cond
      (vector? n)       ""                                  ; already processed
      (first? loc)                                          ; head with special case of quantor
                        (if (or (= n 'forall) (= n 'exists))
                          (let [n' (-> loc zip/next zip/node)]
                            (str " [.\\node{$" n " " n' "$}; "))
                          (str " [.\\node{$" n "$};"))
      (end? loc)       " ]"                                 ; last in list
      :else (str " $" n "$"))))                             ; in the middle of the list

(defn- vis-tikz-body
  "Visualization with tikz"
  [phi]
  (let [phi' (mark-end-of-branch phi)
        loc (zip/seq-zip (seq phi'))]
    (apply str
           (map mapfn (filter (complement zip/branch?) 
                              (take-while (complement zip/end?)
                                  (iterate zip/next loc)))))))

(vis-tikz-body '(and (or p q) q))
(vis-tikz-body '(or (and (or p q) q) r))
(vis-tikz-body '(forall [x] (or p q) q))

(defn vis
  "Visualisation of the syntax tree of formula phi.
   Generates code for tikz."
  [phi]
  (let [tikz-body (vis-tikz-body phi)]
    (str tikz-header "\n" tikz-body "\n" tikz-footer)))

(println (vis '(and (or p q) q)))

(def grp-axioms-classic
  '(and
     (forall [x y z] (= (op x (op y z)) (op (op x y) z)))
     (exists [unit] (and
                      (forall [x] (= (op x unit) x))
                      (forall [x] (exists [inv] (= (op x  inv) unit)))))))
(println (vis grp-axioms-classic))

