; lwb Logic WorkBench -- Propositional Logic bdd

; Copyright (c) 2016 Mathias Gutenbrunner, Jens Lehnhäuser and Burkhardt Renz, THM.
; All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop.bdd
  (:require [lwb.prop :refer :all]
            [clojure.java.shell :as shell])
  (:import  (net.sf.javabdd JFactory BDD)))

; Example
(def phi1 '(or (and a c) (and b d)))
(def phi2 '(and (or a b) (not (or c d))))

; function from prop
(atoms-of-phi phi1)

; All functions with binary decision diagrams have to be executed in the
; context of an initialized BDDFactory, see the documentation of JavaBDD and buddy.
; Due to the construction of buddy and JavaBDD the used factory is a singleton.
(defmacro with-bddf
  "`binding` is a vector that assigns a BDDFactory to a symbol.
   The body is evaluated in a try block, finally the BDDFactory
   is reset."
  [binding & body]
  `(let ~binding
     (try ~@body
     (catch Exception e# (str "caught: " (.getMessage e#)))
     (finally (.done ~(binding 0))))))

; Initializing the JFactory
(defn init-bddf
  "(init-bddf :small)   inits the JFactory for small formulae,
   (init-bddf :medsize) inits the JFactory for medium size formulae,
   (init-bddf :large)   inits the JFactory for large formulae,
   (init-bddf nodesize cachesize) inits the JFactory
   'Typical values according to the documentation of bdd_init of buddy."
  ([type]
   (case type
     :small (init-bddf 10000 1000))
     :medsize (init-bddf 100000 10000)
   (init-bddf 1000000 100000))
  ([nodesize cachesize]
   (JFactory/init nodesize cachesize)))

(defn- atoms-as-bddis
  "The JavaBDD Factory `bddf` returns a map of BDD objects for each atom of formula `phi`.
   Since atoms-of-phi gives the atoms in alphabetic order, the index of the atom in the
   factory is according to the alphabetic position."
  [bddf phi]
  (let [atoms (atoms-of-phi phi), c (count atoms)]
    (if (zero? c)
      {}
      (do
        (.setVarNum bddf c)
        (zipmap atoms (map #(.ithVar bddf %) (range c)))))))

(atoms-as-bddis (init-bddf :small) '(and p q))
(atoms-as-bddis (init-bddf :small) 'true)
(atoms-as-bddis (init-bddf :small) phi1)

(def ^:private functions
  ^{:doc "Mapping of operators used with prop to JavaBDD methods"}
  {'not   #(.not   ^BDD %1)
   'and   #(.and   ^BDD %1 ^BDD %2)
   'or    #(.or    ^BDD %1 ^BDD %2)
   'impl  #(.imp   ^BDD %1 ^BDD %2)
   'equiv #(.biimp ^BDD %1 ^BDD %2)
   'xor   #(.xor   ^BDD %1 ^BDD %2)
   'ite   #(.ite   ^BDD %1 ^BDD %2 ^BDD %3)})

(defn- build-bddi-recur
  "Inner part of the function that builds a BDD object using the BDD Factory."
  [bddf atom-map phi]
  (if (simple-expr? phi)
    (cond (= phi 'true) (.one bddf)
          (= phi 'false) (.zero bddf)
          :else (get atom-map phi))
    (let [op (first phi)]
      (case (arity op)
        1 ((functions op) (build-bddi-recur bddf atom-map (second phi)))
        2 ((functions op) (build-bddi-recur bddf atom-map (second phi))
            (build-bddi-recur bddf atom-map (nth phi 2)))
        3 ((functions op) (build-bddi-recur bddf atom-map (second phi))
            (build-bddi-recur bddf atom-map (nth phi 2))
            (build-bddi-recur bddf atom-map (nth phi 3)))
        -1 (reduce (functions op) (map #(build-bddi-recur bddf atom-map %1) (rest phi)))
        ))))

(defn build-bddi
  "Uses the `bddf` to build a BDD object for the formula `phi`."
  [bddf phi]
  (let [atom-map (atoms-as-bddis bddf phi)]
    (build-bddi-recur bddf atom-map phi)))

(def bddi1 (build-bddi (init-bddf :small) phi1))
(def bddi2 (build-bddi (init-bddf :small) phi2))

; From the internal daat structure of a binary decision diagram, we
; build a Clojure data structure represnting the bdd.

; The bdd is a vector of node, each node has a unique identifiying `no`,
; the symbol for the `atom` of the node, as well as the number of the
; child in the false branch `lo-no`and the number of the child in the
; true branch `hi-no`.

; We have two special nodes for the sinks.

;; Definition of a Node in the representation of the bdd
(defrecord Node [no atom lo-no hi-no])

(def false-node (Node. 0 'false 0 0))
(def true-node  (Node. 1 'true 1 1))

; While building the vector of nodes for a bddi we use a
; map

;; The map with the two sinks
(def ^:private base-map {'false false-node 'true true-node})

; is the entry with key already visited?
(defn- visited? [bddi bdd-map]
  (not (nil? (:lo-no (get bdd-map bddi)))))

(visited? 'true base-map)
(visited? 'false base-map)

(nil? (get base-map bddi1))
(nil? (get base-map 'true))


; inserts new entry for bddi in transient bdd-map
; returns bdd-map
(defn- init-node [bddi bdd-map]
  (if (nil? (get bdd-map bddi))
    (conj! bdd-map [bddi (Node. (count bdd-map) (.var bddi) nil nil)])
    bdd-map))

(init-node bddi1 (transient base-map))
(persistent! (init-node bddi1 (transient base-map)))

; gives key of bddi in graph
(defn- key-bddi [bddi]
  (cond
    (.isZero bddi) 'false
    (.isOne  bddi) 'true
    :else bddi))

; processes a bbdi and manipulates transient bdd-map
; returns bdd-map
(defn- process [bddi bdd-map]
  (let [map1 (init-node bddi bdd-map)]
    (if (visited? bddi bdd-map)
      bdd-map                    ; already visited -> nothing to do
      (let [lo-bddi (key-bddi (.low  bddi))
            map2 (init-node lo-bddi map1)
            hi-bddi (key-bddi (.high bddi))
            map2 (init-node hi-bddi map2)
            lo-no (:no (get map2 lo-bddi))
            hi-no (:no (get map2 hi-bddi))
            no    (:no (get map2 bddi))]
          (conj! map2 [bddi (Node. no (.var bddi) lo-no hi-no)])))))

(defn- build-bdd-recur [bddi bdd-map]
  (cond
    (.isZero bddi) bdd-map
    (.isOne  bddi) bdd-map
    :else (let [map1 (process bddi bdd-map)
                map2 (build-bdd-recur (.low bddi) map1)]
            (build-bdd-recur (.high bddi) map2))))

(defn build-bdd
  "Generates a Clojure representation of the given `bddi`.
   Must be called within the context of a BDD Factory.
   Atoms of the formula from which the ddbi was build are
   noted as there index."
  [bddi]
  (cond
    (.isZero bddi) [(Node. 0 'false 0 0)]
    (.isOne  bddi) [(Node. 1 'true  1 1)]
    :else (into [] (vals (persistent! (build-bdd-recur bddi (transient base-map)))))))

(build-bdd bddi1)

(defn- reasonable-bddf
  "gives a JFactory based on the number of the variables in the
   formula `phi`."
  [phi]
  (let [c (count (atoms-of-phi phi))]
    (cond
      (< c 20) (init-bddf :small)
      (< c 50) (init-bddf :medsize)
      :else    (init-bddf :large))))

(defn- syms-for-atoms
  [phi bdd-vec]
  (let [atom-vec (into [] (atoms-of-phi phi))
        tx (map (fn [node]
                  (if
                    (< (:no node) 2)
                    node
                    (Node. (:no node) (nth atom-vec (:atom node)) (:lo-no node) (:hi-no node)))))]
    (into [] tx bdd-vec)))


; the preferred interface to lwb/prob/bdd:
(defn bdd
  "bdd initializes the JFactory with a reasonable
   size depending on the number of atoms in `phi`,
   generates the internal bdd and transforms it to
   the represantation of the bdd in Cloujre."
  [phi]
  (with-bddf [bddf (reasonable-bddf phi)]
             (let [bddi (build-bddi bddf phi)]
               (syms-for-atoms phi (build-bdd bddi)))))

(bdd 'true)
(bdd 'false)
(bdd phi1)
(bdd phi2)
(bdd 'p)
(bdd '(not p))
(bdd '(and p q))
(bdd '(or p q))
(bdd '(or (and a b) (and a c) (and b c))) ; Knuth Fig. 21

; TODO
(defn sat
  "Gives an assignment vector for `phi` if the formula is satisfiable, nil if not.
   If `phi` is trivially valid, the result is true.
   Mode `:all` returns a sequence of all the satisfying assignments."
  ([phi]
   (sat phi :one))
  ([phi mode]
   (cond
     ; border cases
     (= phi 'true) true
     (= phi 'false) nil
     :else
     (case mode
       :all
       phi
       ; TODO
       ; default
       phi
       ; TODO
       ))))

(defn sat?
  "Is `phi` satisfiable?"
  [phi]
  (if (nil? (sat phi)) false true))

(defn valid?
  "Is `phi` valid?"
  [phi]
  (not (sat? (list 'not phi))))

; helper
(defn- dot-line
  [node]
  (cond
    (= (:no node) 0) "0 [shape=box label=\"\\bot\", style=filled, height=0.3, width=0.3]];\n"
    (= (:no node) 1) "1 [shape=box label=\"\\top\", style=filled];\n"
    :else
    (str (:no node) " [label=\"" (name (:atom node)) "\"];\n"
         (:no node) " -> " (:lo-no node) " [style=dotted];\n"
         (:no node) " -> " (:hi-no node) " [style=filled];\n")))

(defn vis
  "Visualisation of the bdd for the formula `phi`.
   Generates code for graphviz (dot)."
  [phi]
  (let [bdd (bdd phi)
        dot-head "digraph G {\n"
        dot-tail "}"
        dot-lines (apply str (map dot-line bdd))]
    (str dot-head dot-lines dot-tail)))


(vis '(or (and a b) (and a c) (and b c))) ; Knuth Fig. 21

(def ^:private tikz-header
  "\\documentclass{standalone}
   \\standaloneconfig{border=8pt}
   \\usepackage{MnSymbol}
   \\usepackage[english]{babel}
   \\usepackage{tikz}
   \\usetikzlibrary{arrows,shapes}
   \\begin{tikzpicture}[>=stealth']
   \\begin{document}")

;\\tikzset{every node/.style={shape=rectangle,minimum size=6mm,rounded corners=3mm,draw}}

(def ^:private tikz-footer
  "\\end{tikzpicture}
   \\end{document}")

(defn- vis-tikz
  [phi filename]
  (let [dot-code (vis phi)]
    (spit (str filename ".dot") dot-code)
    (:out (shell/sh "dot2tex" "-ftikz" "-tmath" "--codeonly" (str filename ".dot")))))

(vis-tikz '(or (and a b) (and a c) (and b c)) "temp") ; Knuth Fig. 21

(defn vis-pdf
  "Makes a pdf file with the visualisation of the bdd for `phi`.
  <filename> is the name of the file to be generated, must have no extension.
  The function uses the shell command 'dot' that generates the pdf from dot code,
  and 'open' to show the generated file."
  [phi filename]
  (let [tikz-body (vis-tikz phi filename)
        tex-code (str tikz-header "\n" tikz-body "\n" tikz-footer)]
    (spit (str filename ".tex") tex-code)
    (shell/sh "texi2pdf" (str filename ".tex"))
    (shell/sh "open" (str filename ".pdf"))))

(vis-pdf '(or (and x_1 x_2) (and x_1 x_3) (and x_2 x_3)) "majority") ; Knuth Fig. 21



