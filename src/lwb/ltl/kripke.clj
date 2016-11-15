; lwb Logic WorkBench -- Linear Temporal Logic: Kripke Structures

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.ltl.kripke
  (:require [lwb.prop :refer [atom?]]
            [clojure.string :as str]
            [clojure.java.shell :as shell]
            [clojure.spec :as s]))

(defn man
  "Manual"
  []
  (let [info (str/join \newline
                       ["lwb - Linear Temporal Logic - Kripke Structures"
                        "Namespace lwb.ltl.kripke"
                        "Specs:"
                        "- ::model"
                        "  Structure of a model for ltl, aka Kripke structure"
                        "Functions:"
                        "- (dotify ks {:dot|:neato})"
                        "  generates code for graphviz of the Kripke structure ks"
                        "  e.g. (dotify ks :dot)"
                        "- (tikzify ks {:dot|:neato})"
                        "  generates code for tikz of the Kripke structure ks"
                        "  e.g. (tikzify ks :dot)"
                        "- (texify ks filename), (texify ks filename {:dot|:neato})"
                        "  generates pdf of the Kripke structure ks"
                        "  e.g. (texify ks \"myfile\")"])]
    (println info)))

;; # Kripke structures

;; Kripke structures are models for formulas of the linear temporal logic.

;; The nodes of a Kripke structure are represented by a pair of a keyword and a set of the
;; atoms of the structure that are true at that node.

;; The edges of a Kripke structure are vectors of the keywords of the nodes that are
;; connected by that edge.

;; The keyword :initial indicates the starting state of the Kripke model.
 
;; Composed together a Kripke structure is a map of a map of :nodes and a set of :edges

;; Here is the specification:
(s/def ::nodes (s/map-of keyword? (s/coll-of atom? :kind set? :distinct true :into #{})))
(s/def ::initial keyword?)
(s/def ::edges (s/coll-of (s/tuple keyword? keyword?) :into #{}))

;; For Kripke structure we have the following constraints:

;; 1. the initial node is an element of the node set
;; 2. all nodes of edges are elements of the node set
;; 3. for each node there is at least one edge which starts at that node

(defn kripke-ok?
  [{:keys [nodes initial edges]}]
  (let [nodeset    (into #{} (keys nodes))
        nodeset'   (into #{} (flatten (seq edges)))
        nodeset''  (into #{} (map first (seq edges)))]
    (and (contains? nodeset initial)
         (= nodeset nodeset')
         (= nodeset nodeset''))))

;; The final specification of a Kripke structure, i.e. a model in the linear temporal logic.
(s/def ::model (s/and kripke-ok? (s/keys :req-un [::nodes ::initial ::edges])))

;; ## Visualisation of a Kripke structure

;; We use dot2tex which in many cases gives good results,
;; but sometimes we should rework the result.

;; dot2tex produces tikz code using the automata library.
;; For improving the results:

;; 1. try option :neato instead of default option :dot
;; 2. Rework the generated tikz code

; helper functions for the visualisation

(defn- process-name
  "Generates texcode for names;
   Since '{' and '}' are a reserved character in Clojure,
   one can use '<' and '>'
   as characters for grouping subscripts e.g."
  [name]
  (str/replace name #"<|>" {"<" "{", ">" "}"}))

(defn- dot-node
  "Gives code for a node `[key atoms]` on the dot language."
  [[[key atoms] initial?]]
  (let [ proc-atoms (str/join "," (map process-name atoms))
        str-atoms (str "\\{" proc-atoms "\\}")]
    (str (name key) " [style=\"state, " (if initial? "initial, initial text=" "") "\",
      texlbl=\"$" (process-name (name key)) "$\\\\$" str-atoms "$\"];\n")))

(defn- dot-edge
  "Gives code for an edge `[left right]` on the dot language."
  [[left right]]
  (str (name left)  " -> " (name right) (if (= left right) " [topath=\"loop above\"]" "") ";\n"))

(defn dotify
  "Visualisation of the Kripke structure `ks`.
   mode :dot or :neato
   Generates code for graphviz (dot)."
  [ks mode]
  (let [dot-head (str "digraph G {\n "(if (= mode :dot) "rankdir=LR;\n" ""))
        dot-tail "}"
        dot-nodes (str/join (map dot-node (zipmap (:nodes ks) (map #(= (:initial ks) (first %)) (:nodes ks)))))
        dot-edges (str/join (map dot-edge (:edges ks)))]
    (str dot-head dot-nodes dot-edges dot-tail)))

(def ^:private tikz-header
  "\\documentclass{standalone}
   \\standaloneconfig{border=8pt}
   \\usepackage{MnSymbol}
   \\usepackage[english]{babel}
   \\usepackage{tikz}
   \\usetikzlibrary{arrows,shapes,automata}
   \\begin{document}
   \\begin{tikzpicture}[>=stealth']\n
   \\tikzstyle{every state}=[align=center]\n
   ")

(def ^:private tikz-footer
  "\\end{tikzpicture}
   \\end{document}")

(defn tikzify
  "Uses `dot2tex` to get the code of a picture environment in `tikz`.      
   Option :neato or :dot (default)       
   Result sometimes has to be reworked."
  ([ks]
   (tikzify ks :dot))
  ([ks mode]
    (let [dot-code (dotify ks mode)
          prog     (name mode)]
    (:out (shell/sh "dot2tex" (str "--prog=" prog) "-ftikz" "--styleonly" "--codeonly" :in dot-code)))))

(defn texify
  "Makes a pdf file with the visualisation of the Kripke structure `ks`.      
  `filename` is the name of the file to be generated, must have no extension.      
  `mode` can be `:dot` (default) oder `:neato`, determining which renderer of      
  graphviz is used. Further processing is done by `dot2tex` and `texi2pdf`.         
  Finally the generated file is opened by the command `open`."
  ([ks filename]
   (texify ks filename :dot))
  ([ks filename mode]
     (let [tikz-body (tikzify ks mode)
           tex-code (str tikz-header "\n" tikz-body "\n" tikz-footer)
           tex-file (str filename ".tex")]
       (spit tex-file tex-code)
       (shell/sh "texi2pdf" tex-file))
       (shell/sh "open" (str filename ".pdf"))))



