; lwb Logic WorkBench -- Visualisation of formulae

; Copyright (c) 2016 Burkhardt Renz, Juan Markowich THM. 
; All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.vis
  (:require [clojure.zip :as zip]
            [clojure.string :as str]
            [clojure.java.shell :as shell]))

(def ^:private tikz-header
  "\\documentclass{standalone}
   \\standaloneconfig{border=8pt}
   \\usepackage{MnSymbol}
   \\usepackage[english]{babel}
   \\usepackage{tikz-qtree}
   \\tikzset{every tree node/.style={shape=rectangle,minimum size=6mm,rounded corners=3mm,draw}}
   \\begin{document}
   \\begin{tikzpicture}
   \\Tree")

(def ^:private tikz-footer
  "\\end{tikzpicture}
   \\end{document}")

(defn- first?
  "Is loc the most left location of siblings?"
  [loc]
  (nil? (-> loc zip/left)))

(defn- end?
  "Is loc a node marked with :end?"
  [loc]
  (= :end (-> loc zip/node)))

(defn- mark-end-of-branch [phi]
  "To facilitate the generation of code in tikz, we mark the ends of
   lists with :end"
  (loop [loc (zip/seq-zip (seq phi))]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next
               (if (zip/branch? loc)
                 (let [inserted-loc (zip/insert-right (-> loc zip/down zip/rightmost) :end)]
                   (-> inserted-loc zip/leftmost))
                 loc))))))

(defn- process-head
  "Generates texcode for the head of a list"
  [node]
  (let [symbols {:and   "\\land"
                 :or    "\\lor"
                 :not   "\\lnot"
                 :impl  "\\to"
                 :equiv "\\leftrightarrow"
                 :true  "\\top"
                 :false "\\bot"
                 :xor "\\oplus"
                 :ite "\\mathsf{ite}"
                 :always "\\medsquare"
                 :finally "\\lozenge"
                 :asap    "\\medcircle"
                 :until   "\\mathcal{U}"}
        key      (keyword (name node))]
      (if (contains? symbols key)
        (str " [.\\node{$" (key symbols) "$};")
        (str " [.\\node{$" node "$};")))) 

(defn- process-quantor
  "Generates texcode for quantors"
  [node vars]
  (let [quantors {:forall "\\forall"
                  :exists "\\exists"}
        key      (keyword (name node))]
    (str " [.\\node{$" (key quantors) " " 
         (str/join "\\, " vars) "$};")))

(defn- process-atom
  "Generates texcode for atoms;
   Since '{' and '}' are a reserved character in Clojure, 
   one can use '<' and '>'
   as characters for grouping subscripts e.g."
  [node]
  (let [node-str   (str node)
        node-str'  (str/replace node-str \< \{)
        node-str'' (str/replace node-str' \> \})]
    (str " $" node-str'' "$"))
  )

(defn- mapfn
  "Mapping function that generates the tikz code from the traversing of the tree."
  [loc]
  (let [n (zip/node loc)]
    (cond
      (vector? n)       "" ; already processed
      (first? loc)         ; head with special case of quantor
                        (if (or (= n 'forall) (= n 'exists))
                          (let [n' (-> loc zip/next zip/node)]
                            (process-quantor n n'))
                          (process-head n))
      (end? loc)       " ]"    ; last in list
      :else (process-atom n ))))   ; in the middle of the list

(defn- vis-tikz-body
  "Visualization with tikz, the body"
  [phi]
  (let [phi-n (if (symbol? phi) (list phi) phi) ; special case
        phi'  (mark-end-of-branch phi-n)
        loc (zip/seq-zip (seq phi'))]
    (apply str
           (map mapfn (filter (complement zip/branch?) 
                              (take-while (complement zip/end?)
                                  (iterate zip/next loc)))))))

(defn vis
  "Visualisation of the syntax tree of formula phi.
   Generates code for tikz."
  [phi]
  (let [tikz-body (vis-tikz-body phi)]
    (str tikz-header "\n" tikz-body "\n" tikz-footer)))

(defn vis-pdf
  "Makes a pdf file with the visualisation of the syntaxtree of phi.
  <file> is the name of the file to be generated, must have no extension.
  The function uses the shell command 'texi2pdf' that compiles tex code,
  and 'open' to show the generated file."
  [phi filename]
  (let [tex-code (vis phi)]
    (spit (str filename ".tex") tex-code)
    (shell/sh "texi2pdf" (str filename ".tex"))
    (shell/sh "open" (str filename ".pdf"))))

;; Examples
(comment
  (vis-pdf '(and (or p_<12> q) q) "simple")

  (def grp-axioms-classic
    '(and
       (forall [x y z] (= (op x (op y z)) (op (op x y) z)))
       (exists [unit] (and
                        (forall [x] (= (op x unit) x))
                        (forall [x] (exists [inv] (= (op x inv) unit)))))))

  (vis-pdf grp-axioms-classic "group-axioms")

  (vis-pdf '(or (and (or p q) q) r) "simple2")

  (def ltl-phi
    '(and
       (always p)
       (finally q)
       (asap r)
       (until s t)))

  (vis-pdf ltl-phi "ltl")

  ; Exercise 4
  ; (a)
  (println (vis 'P))
  ; (b)
  (println (vis '(and P Q)))
  ; (c)
  (println (vis '(impl (and P (not Q)) (not P))))
  (println (vis '(or p_1 (and P_2 (impl P_3 P_4) )))
  )
