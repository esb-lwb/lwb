; lwb Logic WorkBench -- Linear Temporal Logic

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.ltl
  (:require [lwb.prop :as prop]
            [clojure.string :as str]
            [clojure.java.shell :as shell]
            [potemkin :as pot]))

;; # Representation of formulae of ltl

;; The representation is like in propositional logic

;; We have 4 more operators in ltl:

;; * always -- unary
;; * atnext -- unary
;; * finally -- unary
;; * until -- unary

;; We import the operators and so forth from propositional logic
(pot/import-vars
  [lwb.prop impl equiv xor ite atom? torf?])

(defn op?
  "Is `symb` an operator of ltl?"
  [symb]
  (let [operators #{'always 'atnext 'finally 'until}]
    (or (prop/op? symb) (contains? operators symb))))

(defn arity
  "Arity of operator `op`.
   -1 means n-ary, but better use `n-ary?`.
   requires: `op` an operator."
  [op]
  (if
    (prop/op? op) (prop/arity op)
                    (if (= 'until op) 2
                                      1)))

;; ## Is a formula well-formed?
(declare wff?)

(defn op-expr?
  [phi]
  (if (not (op? (first phi)))
    false
    (let [a (arity (first phi))
          c (dec (count phi))]
      (if (and (not= a -1) (not= a c))
        (throw (IllegalStateException. (str "expected operator with arity " a ", got " phi)))
        (every? wff? (rest phi))))))

(defn simple-expr?
  [phi]
  (or (torf? phi) (atom? phi)))

(defn compound-expr?
  [phi]
  (cond
    (not (list? phi)) (throw (IllegalStateException. (str "expected list, got " phi)))
    (empty? phi) (throw (IllegalStateException. "expected not empty list, got '()'."))
    (not (op? (first phi))) (throw (IllegalStateException. (str "expected operator, got " phi)))
    :else (op-expr? phi)))

(defn wff?
  "Is the proposition `phi` well-formed ?
   `(wff? phi)` returns true or false
   `(wff? phi :msg)` returns true or a message on the error in `phi`."
  ([phi]
   (wff? phi :bool))
  ([phi mode]
   (try
     (or (simple-expr? phi) (compound-expr? phi))
     (catch Exception e (if (= mode :msg) (.getMessage e) false)))))

; # Kripke structures

; The nodes of a Kripke structure are represented by a pair of a keyword and a set of the
; atoms of the structure that are true at that node.
; The edges of a Kripke structure are vectors of the keywords of the nodes that are
; connected by that edge. There is a reserved keyword for the starting node of a
; Kripke structure, namely :sp
;
; Composed together a Kripke structure is a map of a map of :nodes and a set of :edges
;
; Here are two examples
;
(comment
  (def ks1 {:nodes {:s_0 '#{P Q}
                    :s_1 '#{P Q}
                    :s_2 '#{P}}
            :edges #{[:sp  :s_0]
                     [:s_0 :s_1]
                     [:s_1 :s_0]
                     [:s_1 :s_2]
                     [:s_2 :s_2]}})

  (def ks2 {:nodes {:s_0 '#{}
                    :s_1 '#{P}
                    :s_2 '#{}}
            :edges #{[:sp :s_0]
                     [:s_0 :s_1]
                     [:s_0 :s_2]
                     [:s_1 :s_1]
                     [:s_2 :s_2]}})
  )

; ## Visualisation of a Kripke structure

; helper functions for the visualisation

(defn- process-name
  "Generates texcode for names;
   Since '{' and '}' are a reserved character in Clojure,
   one can use '<' and '>'
   as characters for grouping subscripts e.g."
  [name]
  (-> name
      (str/replace \< \{)
      (str/replace \> \})))

(defn- dot-node
  "Gives code for a node `[key atoms]` on the dot language."
  [[key atoms]]
  (let [proc-atoms (str/join "," (map #(process-name %) atoms))
        str-atoms (str "\\{" proc-atoms "\\}")]
    (str (name key) " [label=\"" (name key) "\", style=\"shape=rectangle,align=left,minimum size=6mm,rounded corners=3mm\",
      texlbl=\"$" (process-name (name key)) "$\\\\" str-atoms "\"] ;\n")))

(defn- dot-edge
  "Gives code for an edge `[left right]` on the dot language."
  [[left right]]
  (str (name left)  " -> " (name right) ";\n"))

(defn vis-dot
  "Visualisation of the Kripke structure `ks`.
   Generates code for graphviz (dot)."
  [ks]
  (let [dot-head "digraph G {\n rankdir=LR;\n"
        dot-tail "}"
        dot-sp "sp [style=invisible];\n"
        dot-nodes (apply str (map dot-node (:nodes ks)))
        dot-edges (apply str (map dot-edge (:edges ks)))]
    (str dot-head dot-sp dot-nodes dot-edges dot-tail)))

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
  [ks]
  (let [dot-code (vis-dot ks)]
    (:out (shell/sh "dot2tex" "-ftikz" "--codeonly" :in dot-code))))

(defn vis-pdf
  "Makes a pdf file with the visualisation of the a Kripke structure `ks`.
  `filename` is the name of the file to be generated, must have no extension.
  `mode` can be `:tikz` (default) oder `:dot`.
  In case `:dot` the function uses the command `dot`from graphviz.
  In case `:tikz` it uses furthermore `dot2tex` and `texi2pdf`.
  In both cases the generated file is opened by the command `open`."
  ([ks filename]
   (vis-pdf ks filename :tikz))
  ([ks filename mode]
   (if (= mode :dot)
     (let [dot-code (vis-dot ks)]
       (shell/sh "dot" "-Tpdf" "-o" (str filename ".pdf") :in dot-code))
     (let [tikz-body (vis-tikz ks)
           tex-code (str tikz-header "\n" tikz-body "\n" tikz-footer)
           tex-file (str filename ".tex")]
       (spit tex-file tex-code)
       (shell/sh "texi2pdf" tex-file)))
   (shell/sh "open" (str filename ".pdf"))))


(comment
  (vis-pdf ks1 "ks1")
  (vis-pdf ks2 "ks2")
  )

