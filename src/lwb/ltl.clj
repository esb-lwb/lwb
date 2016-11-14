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
            [potemkin :as pot]
            [lwb.vis :as vis]))

;; # Representation of formulae of ltl

;; The representation is like in propositional logic

;; We have 4 more operators in ltl:

;; * always -- unary
;; * atnext -- unary
;; * finally -- unary
;; * until -- binary

;; We import the operators and so forth from propositional logic
(pot/import-vars
  [lwb.prop impl equiv xor ite atom?])

(defn op?
  "Is `symb` an operator of ltl?"
  [symb]
  (let [operators '#{always atnext finally until}]
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
  (if-not (op? (first phi))
    false
    (let [a (arity (first phi))
          c (dec (count phi))]
      (if (and (not= a -1) (not= a c))
        (throw (IllegalStateException. (str "expected operator with arity " a ", got " phi)))
        (every? wff? (rest phi))))))

(defn simple-expr?
  [phi]
  (or (boolean? phi) (atom? phi)))

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

;; ## Visualisation of a formula

(defn texify
  "Generates TeX code for TikZ or a pdf file if filename given.
   Requires: TeX installation, commands texipdf and open"
  ([phi]
   (vis/texify phi))
  ([phi filename]
   (vis/texify phi filename)))

; # Kripke structures

; The nodes of a Kripke structure are represented by a pair of a keyword and a set of the
; atoms of the structure that are true at that node.
; The edges of a Kripke structure are vectors of the keywords of the nodes that are
; connected by that edge.
; the keyword :initial indicates the starting state of the Kripke model.
;
; Composed together a Kripke structure is a map of a map of :nodes and a set of :edges
;
; Here are two examples
;
(comment
  (def ks1 {:nodes {:s_0 '#{P Q}
                    :s_1 '#{P Q}
                    :s_2 '#{P}}
            :initial :s_0
            :edges #{[:s_0 :s_1]
                     [:s_1 :s_0]
                     [:s_1 :s_2]
                     [:s_2 :s_2]}})

  (def ks2 {:nodes {:s_0 '#{}
                    :s_1 '#{P}
                    :s_2 '#{}}
            :initial :s_0
            :edges #{[:s_0 :s_1]
                     [:s_0 :s_2]
                     [:s_1 :s_1]
                     [:s_2 :s_2]}})
  )

; ## Visualisation of a Kripke structure

; ### We use dot2tex which in many cases gives good results,
;     but sometimes we should rework the result.
;     dot2tex produces tikz code using the automata library.
;     For improving the results:
;     1. try option :neato instead of default option :dot
;     2. Rework the generated tikz code

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
  "Makes a pdf file with the visualisation of the a Kripke structure `ks`.
  `filename` is the name of the file to be generated, must have no extension.
  `mode` can be `:dot` (default) oder `:neato`, determing which renderer of
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

(comment
  (dotify ks1 :dot)
  (tikzify ks1)
  (texify ks1 "ks1" :dot)
  (texify ks2 "ks2")
  )

