; lwb Logic WorkBench -- Propositional Logic - Binary Decision Diagrams

; Copyright (c) 2016 Mathias Gutenbrunner, Jens Lehnhäuser and Burkhardt Renz, THM.
; All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

;; # Binary Decision Diagrams

;; Binary decision diagrams from formulae of the propositional logic are build
;; using the [JavaBDD library](http://javabdd.sourceforge.net). JavaBDD is an
;; implementation in Java of the C/C++ library [BuDDy](http://buddy.sourceforge.net/manual/main.html).
;;
;; Our library is just a rather thin wrapper for JavaBDD in Clojure.

(ns lwb.prop.bdd
  (:require [lwb.prop :refer :all]
            [clojure.string :as str]
            [clojure.math.combinatorics :refer (selections)]
            [clojure.java.shell :as shell]
            [clojure.spec :as s])
  (:import  (net.sf.javabdd JFactory BDD)))

;; All functions with binary decision diagrams have to be executed in the
;; context of an initialized BDDFactory, see the documentation of JavaBDD and BuDDy.     
;; Since we use JFactory, we can have multiple factories, but if one wants to
;; use the BuDDyFactory one have to bear in mind that the BuDDyFactory is a
;; singleton.

(defmacro with-bddf
  "`binding` is a vector that assigns a BDDFactory to a symbol.      
   The body is evaluated in a try block, finally the BDDFactory
   is reset."
  [binding & body]
  `(let ~binding
     (try ~@body
     (catch Exception e# (str "caught: " (.getMessage e#)))
     (finally (.done ^JFactory (~binding 0))))))

; Initializing the JFactory
(defn init-bddf
  "Initializing the JFactory      
   (init-bddf :small)   inits the JFactory for small formulae,    
   (init-bddf :medsize) inits the JFactory for medium size formulae,    
   (init-bddf :large)   inits the JFactory for large formulae,    
   (init-bddf nodesize cachesize) inits the JFactory    
   Typical values according to the documentation of bdd_init of BuDDy."
  ([type]
   (case type
     :small   (init-bddf 10000 1000)
     :medsize (init-bddf 100000 10000)
     (init-bddf 1000000 100000)))
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
        (.setVarNum ^JFactory bddf c)
        (zipmap atoms (map #(.ithVar ^JFactory bddf %) (range c)))))))

(def ^:private functions
  ^{:doc "Mapping of operators of prop to JavaBDD methods"}
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
  (if (s/valid? :lwb.prop/simple-expr phi)
    (cond (= phi 'true)  (.one  ^JFactory bddf)
          (= phi 'false) (.zero ^JFactory bddf)
          :else (get atom-map phi))
    (let [op (first phi) a (arity op)]
      (case a
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

;; From the internal data structure of a binary decision diagram, we
;; build a Clojure data structure representing the bdd.

;; The bdd is a vector of nodes, each node has a unique identifiying `no`,
;; the symbol for the `atom` of the node, as well as the number of the
;; child in the false branch `lo-no`and the number of the child in the
;; true branch `hi-no`.

;; We have two special nodes for the sinks.

;; Definition of a Node in the representation of the bdd
(defrecord Node [no atom lo-no hi-no])

;; Nodes for verum and falsum
(def ^:private false-node (Node. 0 'false 0 0))
(def ^:private true-node  (Node. 1 'true 1 1))

;; While building the vector of nodes for a bddi we use a map 

;; The map with the two sinks
(def ^:private base-map {'false false-node 'true true-node})

(defn- visited?
  "Has the entry with key already been visited?"
  [bddi bdd-map]
  (not (nil? (:lo-no (get bdd-map bddi)))))

(defn- init-node
  "Inserts new entry for bddi in transient bdd-map    
   returns bdd-map"
  [bddi bdd-map]
  (if (nil? (get bdd-map bddi))
    (conj! bdd-map [bddi (Node. (count bdd-map) (.var ^BDD bddi) nil nil)])
    bdd-map))

(defn- key-bddi
  "returns key of bddi in graph"
  [bddi]
  (cond
    (.isZero ^BDD bddi) 'false
    (.isOne  ^BDD bddi) 'true
    :else bddi))

(defn- process
  "Processes a bbdi and manipulates transient bdd-map     
   returns bdd-map"
  [bddi bdd-map]
  (let [map1 (init-node bddi bdd-map)]
    (if (visited? bddi map1)
      map1                    ; already visited -> nothing to do
      (let [lo-bddi (key-bddi (.low  ^BDD bddi))
            map2 (init-node lo-bddi map1)
            hi-bddi (key-bddi (.high ^BDD bddi))
            map3 (init-node hi-bddi map2)
            lo-no (:no (get map3 lo-bddi))
            hi-no (:no (get map3 hi-bddi))
            no    (:no (get map3 bddi))]
          (conj! map3 [bddi (Node. no (.var ^BDD bddi) lo-no hi-no)])))))

(defn- build-bdd-recur [bddi bdd-map]
  (cond
    (.isZero ^BDD bddi) bdd-map
    (.isOne  ^BDD bddi) bdd-map
    :else (->> bdd-map
               (process bddi)
               (build-bdd-recur (.low  ^BDD bddi))
               (build-bdd-recur (.high ^BDD bddi)))))

(defn build-bdd
  "Generates a Clojure representation of the given `bddi`.
   Must be called within the context of a BDD Factory.
   Atoms of the formula from which the bddi was build are
   noted as there name."
  [bddi]
  (cond
    (.isZero ^BDD bddi) [(Node. 0 'false 0 0)]
    (.isOne  ^BDD bddi) [(Node. 1 'true  1 1)]
    :else (vec (vals (persistent! (build-bdd-recur bddi (transient base-map)))))))

(defn reasonable-bddf
  "gives a JFactory based on the number of the variables in the
   formula `phi`."
  [phi]
  (let [c (count (atoms-of-phi phi))]
    (cond
      (< c 20) (init-bddf :small)
      (< c 50) (init-bddf :medsize)
      :else    (init-bddf :large))))

(defn- syms-for-atoms
  "A vector of nodes for the formula `phi` and the corresponding vector of nodes
   with the internal numbering of atoms where these are replaced by the symbols for
   the atoms."
  [phi bdd-vec]
  (let [atom-vec (vec (atoms-of-phi phi))
        tx (map (fn [node]
                  (if
                    (< (:no node) 2)
                    node
                    (Node. (:no node) (nth atom-vec (:atom node)) (:lo-no node) (:hi-no node)))))]
    (into [] tx bdd-vec)))

;; ### The preferred interface to lwb.prob.bdd:

(defn bdd
  "`bdd` initializes the JFactory with a reasonable
   size depending on the number of atoms in `phi`,
   generates the internal bdd and transforms it to
   the representation of the bdd in Clojure."
  [phi]
  (with-bddf [bddf (reasonable-bddf phi)]
             (let [bddi (build-bddi bddf phi)]
               (syms-for-atoms phi (build-bdd bddi)))))


;; Representation of a binary decision diagram in Clojure: a vector of Nodes
(s/def ::bdd (s/coll-of #(instance? Node %)))

(s/fdef bdd
        :args wff?
        :ret ::bdd)

(defn- tf1-vec
  "Transforms byte vector result from AllSatIterator to get an assignment vector"
  [phi bvec]
  (let [atom-vec (vec (atoms-of-phi phi))
        tx (map-indexed (fn [idx a]
        (if (zero? a)
          [(nth atom-vec idx) 'false]
          [(nth atom-vec idx) 'true])))]
    (into[] (comp tx (mapcat identity)) bvec)))

(defn- idx-in-vec
  "Returns seq of indexes of number -1 in vec"
  [bvec]
  (let [tx (comp
             (map-indexed vector) (filter #(= -1 (second %))) (map first))]
    (sequence tx bvec)))

(defn- tf2-vec
  "Replaces in `bvec` -1 by the truth value given in `sel`."
  [bvec sel]
  (let [assign-map (zipmap (idx-in-vec bvec) sel)
        tfn (fn [[idx value]]
              (if (= value -1)
                (get assign-map idx)
                value))]
    (vec (map tfn (map-indexed vector bvec)))))

(defn- tfa-vec
  "Gives all possible assignment of truth values from a given
   vector."
  [bvec]
  (let [c  (count (filter #(= -1 %) bvec))
        sels (selections [0 1] c)]
    (map #(tf2-vec bvec %) sels)))

(defn sat
  "Gives a model for `phi` if the formula is satisfiable, nil if not.
   If `phi` is trivially valid, the result is true.
   Mode `:all` returns a sequence of all the models."
  ([phi]
   (sat phi :one))
  ([phi mode]
   (with-bddf [bddf (reasonable-bddf phi)]
       (let [bddi (build-bddi bddf phi)
             iseq (iterator-seq (.allsat bddi))]
         (cond
           ; border cases
           (.isOne bddi) 'true
           (.isZero bddi) 'nil
           :else
           (case mode
             :all (map #(tf1-vec phi %) (mapcat tfa-vec (vec (map vec iseq))))
             (tf1-vec phi (first (map vec iseq)))))))))

(s/fdef sat
        :args wff?
        :ret (s/nilable (s/or :verum true? :model :lwb.prop/model)))

(defn sat?
  "Is `phi` satisfiable?"
  [phi]
  (if (nil? (sat phi)) false true))

(s/fdef sat?
        :args wff?
        :ret boolean?)

(defn valid?
  "Is `phi` valid?"
  [phi]
  (not (sat? (list 'not phi))))

(s/fdef valid?
        :args wff?
        :ret boolean?)

; helper functions for visualisation

(defn- process-atom
  "Generates texcode for atoms;
   Since '{' and '}' are a reserved character in Clojure,
   one can use '<' and '>'
   as characters for grouping subscripts e.g."
  [node]
  (-> (name (:atom node))
      (str/replace \< \{)
      (str/replace \> \})))

(defn- dot-line
  "Gives code for a `node` on the dot language."
  [node]
  (cond
    (zero? (:no node)) "0 [shape=box label=\"\\bot\"];\n"
    (= (:no node) 1)   "1 [shape=box label=\"\\top\"];\n"
    :else
    (str (:no node) " [label=\"" (process-atom node) "\", style=\"shape=rectangle,minimum size=6mm,rounded corners=3mm\"];\n"
         (:no node) " -> " (:lo-no node) " [style=dotted];\n"
         (:no node) " -> " (:hi-no node) " [style=filled];\n")))

;; ### Visualisation with graphviz

(defn vis-dot
  "Visualisation of the bdd for the formula `phi`.
   Generates code for graphviz (dot)."
  [phi]
  (let [bdd (bdd phi)
        dot-head "digraph G {\n"
        dot-tail "}"
        dot-lines (str/join (map dot-line bdd))]
    (str dot-head dot-lines dot-tail)))


;; ### Visualisation with tikz

(def ^:private tikz-header
  "\\documentclass{standalone}
   \\standaloneconfig{border=8pt}
   \\usepackage{MnSymbol}
   \\usepackage[english]{babel}
   \\usepackage{tikz}
   \\usetikzlibrary{arrows,shapes}
   \\begin{document}
   \\begin{tikzpicture}[>=stealth']\n")

(def ^:private tikz-footer
  "\\end{tikzpicture}
   \\end{document}")

(defn vis-tikz
  "Uses `dot2tex` to get the code of a picture environment in `tikz`.
   Result sometimes has to be reworked."
  [phi]
  (let [dot-code (vis-dot phi)]
    (:out (shell/sh "dot2tex" "-ftikz" "-tmath" "-s" "--codeonly" :in dot-code))))

(defn vis-pdf
  "Makes a pdf file with the visualisation of the bdd for `phi`.
  `filename` is the name of the file to be generated, must have no extension.
  Uses `dot2tex` and `texi2pdf`, finally the
  generated file is opened by the command `open`."
  [phi filename]
  (let [tikz-body (vis-tikz phi)
        tex-code (str tikz-header "\n" tikz-body "\n" tikz-footer)
        tex-file (str filename ".tex")]
        (spit tex-file tex-code)
        (shell/sh "texi2pdf" tex-file))
        (shell/sh "open" (str filename ".pdf")))


