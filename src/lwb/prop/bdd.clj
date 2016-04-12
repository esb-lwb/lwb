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
  "the JavaBDD Factory `bddf` returns a map of BDD Objects for each atom of formula `phi`."
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
  ^{:doc "mapping of operators to JavaBDD methods"}
  {'not   #(.not   ^BDD %1)
   'and   #(.and   ^BDD %1 ^BDD %2)
   'or    #(.or    ^BDD %1 ^BDD %2)
   'impl  #(.imp   ^BDD %1 ^BDD %2)
   'equiv #(.biimp ^BDD %1 ^BDD %2)
   'xor   #(.xor   ^BDD %1 ^BDD %2)
   'ite   #(.ite   ^BDD %1 ^BDD %2 ^BDD %3)})

(defn- build-bddi-recur
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
  [bddf phi]
  (let [atom-map (atoms-as-bddis bddf phi)]
    (build-bddi-recur bddf atom-map phi)))

(def bddi1 (build-bddi (init-bddf :small) phi1))
(def bddi2 (build-bddi (init-bddf :small) phi2))
; how to represent the bdd in clojure?

; durchlaufe bdd und indexiere
; setzt voraus, dass es nicht true ist oder false, diese Fälle müssen extra behandelt werden

(defrecord Node [no var lo-no hi-no])

(def graph (atom {}))

(def false-node ['false (Node. 0 'false 0 0)])
(def true-node  ['true  (Node. 1 'true 1 1)])

(swap! graph conj false-node)
(swap! graph conj true-node)
@graph

; TODO: naming of functions

; next number for a new node in the representation of the bdd
; Remark: could be implemented as a sequence generator, which
; is more efficient.
(defn- next-no [graph]
  (inc (apply max (map :no (vals graph)))))

@graph
(next-no @graph)

; is the entry with key already visited?
(defn- visited? [bddi graph]
  (not (nil? (:lo-no (get graph bddi)))))

(visited? 'true @graph)
(visited? 'false @graph)

; inserts new entry for bddi in atom graph
(defn- init-node [bddi a-graph]
  (if (not (contains? @a-graph bddi))
    (swap! a-graph conj [bddi (Node.  (next-no @graph) (.var bddi) nil nil)])))

; gives key of bddi in graph
(defn- key-bddi [bddi]
  (cond
    (.isZero bddi) 'false
    (.isOne  bddi) 'true
    :else bddi))

; processes a bbdi and changes a-graph
(defn- process [bddi a-graph]
    (init-node bddi a-graph)
    (if (visited? bddi @a-graph)
      a-graph                    ; already visited -> nothing to do
      (let [lo-bddi (key-bddi (.low  bddi))
            hi-bddi (key-bddi (.high bddi))]
        (init-node lo-bddi a-graph)
        (init-node hi-bddi a-graph)
        (let [lo-no (:no (get @a-graph lo-bddi))
              hi-no (:no (get @a-graph hi-bddi))
              no    (:no (get @a-graph bddi))]
          (swap! graph conj [bddi (Node. no (.var bddi) lo-no hi-no)])))))

(defn- build-bdd-recur [bddi a-graph]
  (cond
    (.isZero bddi) a-graph
    (.isOne  bddi) a-graph
    :else (do
            (process bddi a-graph)
            (build-bdd-recur (.low  bddi) a-graph)
            (build-bdd-recur (.high bddi) a-graph))))

(defn build-bdd
  "Generates a Clojure representation of the given `bddi`.
   Must be called within the context of a BDD Factory."
  [bddi]
  (cond
    (.isZero bddi) [(Node. 0 'false 0 0)]
    (.isOne  bddi) [(Node. 1 'true  1 1)]
    :else
    (let [a-graph (atom {})
          false-node ['false (Node. 0 'false 0 0)]
          true-node  ['true  (Node. 1 'true  1 1)]]
      (swap! a-graph conj false-node)
      (swap! a-graph conj true-node)
      (into[] (vals @(build-bdd-recur bddi a-graph))))))

(build-bdd bddi1)


; the preferred interface to lwb/prob/bdd:
; TODO:

(defn bdd-i
  "bdd-i generates the internal bdd for `phi`
   within the context of the JFactory."
  [phi]
  phi)


(defn bdd
  "bbd initializes the JFactory with a reasonable
   size depending on the number of atoms in `phi`,
   generates the internal bdd and transforms it to
   the represantation of the bdd in Cloujre."
  [phi]
  phi)

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

(defn vis
  "Visualisation of the bdd for the formula `phi`.
   Generates code for graphviz (dot)."
  [phi]
  phi)

(defn vis-pdf
  "Makes a pdf file with the visualisation of the bdd for `phi`.
  <filename> is the name of the file to be generated, must have no extension.
  The function uses the shell command 'dot' that generates the pdf from dot code,
  and 'open' to show the generated file."
  [phi filename]
  (let [dot-code (vis phi)]
    (spit (str filename ".dot") dot-code)
    (shell/sh "dot" (str "-Tpdf" filename ".dot") (str "-o" filename ".pdf"))
    (shell/sh "open" (str filename ".pdf"))))

; TODO: represent the table as a clojure object
(defn bdd-table
  [bddf phi]
  (let [bdd  (build-bdd bddf phi)]
    (.printTable bddf bdd)))

(with-bddf [bddf (init-bddf :small)]
           (bdd-table bddf phi1))
(with-bddf [bddf (init-bddf :small)]
           (bdd-table bddf phi2))
(with-bddf [bddf (init-bddf :small)]
           (bdd-table bddf phi3))
(with-bddf [bddf (init-bddf :small)]
           (bdd-table bddf '(equiv p q)))
(with-bddf [bddf (init-bddf :small)]
           (bdd-table bddf '(impl p q)))
(with-bddf [bddf (init-bddf :small)]
           (bdd-table bddf '(or (not p) q)))
(with-bddf [bddf (init-bddf :small)]
           (bdd-table bddf 'true))

; TODO: use the clojure representation of a bdd to generate dot code
(defn bdd-dot
  [bddf phi]
  (let [bdd  (build-bdd bddf phi)]
    (.printDot bdd)))

(with-bddf [bddf (init-bddf :small)]
           (bdd-dot bddf phi1))

(with-bddf [bddf (init-bddf :small)]
           (bdd-dot bddf phi2))

; TODO: sat with bdd



