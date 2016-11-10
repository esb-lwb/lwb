; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 Tobias Völzel, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.printer
  (:require [lwb.consts :refer [rev]]
            [lwb.nd.proof :refer [plid->plno]]
            [clojure.walk :as walk]
            [clojure.pprint :as pp]
            [clojure.zip :as zip]
            [clojure.string :as str]
            [clojure.java.shell :as shell])
  (:import (java.time.format DateTimeFormatter)
           (java.time LocalDate)))

;; # Printing the proof

;; ## Printing to the REPL's stdout

(def distance
  "Distance between left and right edge."
  40)

(def divider-length
  "Length of the divider for subproofs."
  25)

(defn- plid-plno-map
  "Mapping of ids of pline to line numbers."
  [proof]
  (let [fp (flatten proof)]
    (into {} (map-indexed #(vector (:plid %2) (inc %1)) fp))))

(defn- pprint-line
  "Prints a proof line."
  [proof depth pline plid-plno-map]
  (let [line (plid->plno proof (:plid pline))
        body (str (if (= (:body pline) :todo) "..." (:body pline)))
        refs (walk/postwalk-replace plid-plno-map (:refs pline))
        roth (str " " (:roth pline) " " refs)]
    (print (pp/cl-format nil "~3d: " line))
    (when (pos? depth)
      (print " ")
      (dotimes [_ depth] (print "| ")))
    (print body)
    (dotimes [_ (- distance (count body) (if (pos? depth) (inc (* 2 depth)) 0))] (print " "))
    (println roth)))

(defn pprint
  "Prints the proof."
  ([proof] (pprint proof proof 0))
  ([proof sub depth]
   (let [smap (plid-plno-map proof)]
     (print "     ")
     (when (pos? depth)
       (print " ")
       (dotimes [_ (dec depth)] (print "| ")))
     (dotimes [_ (- divider-length depth)] (print "--"))
     (println)
     (doseq [pline sub]
       (if (vector? pline)
         (pprint proof pline (inc depth))
         (pprint-line proof depth pline smap)))
     (print "     ")
     (when (pos? depth)
       (print " ")
       (dotimes [_ (dec depth)] (print "| ")))
     (dotimes [_ (- divider-length depth)] (print "--"))
     (println))))

;; ## Generating TeX-Code

(defn- height
  "Height of a tree of nested vectors."
  [tree]
  (if-let [sub-trees (seq (filter vector? tree))]
    (inc (apply max (map height sub-trees)))
    0))

(def lsymbols
  "Logical symbols in TeX."
  {'and           "\\land\\ "
   'or            "\\lor\\ "
   'not           "\\lnot\\ "
   'impl          "\\to\\ "
   'truth         "\\top\\ "
   'contradiction "\\bot\\ "
   'forall        "\\forall "
   'exists        "\\exists "
   'atnext        "\\medcircle\\,\\ "
   'always        "\\medsquare\\,\\ "
   'finally       "\\lozenge\\,\\ "
   'until         "\\mathcal{\\:U\\:}\\ "})

(def lroths
  "TeX symbols for rules."
  {:and-i     "$\\land\\mathsf{i}$"
   :and-e1    "$\\land\\mathsf{e}_1$"
   :and-e2    "$\\land\\mathsf{e}_2$"
   :or-i1     "$\\lor\\mathsf{i}_1$"
   :or-i2     "$\\lor\\mathsf{i}_2$"
   :or-e      "$\\lor\\mathsf{e}$"
   :impl-i    "$\\to\\mathsf{i}$"
   :impl-e    "$\\to\\mathsf{e}$"
   :not-i     "$\\lnot\\mathsf{i}$"
   :not-e     "$\\lnot\\mathsf{e}$"
   :forall-i  "$\\forall\\mathsf{i}$"
   :forall-e  "$\\forall\\mathsf{e}$"
   :exists-i  "$\\exists\\mathsf{i}$"
   :exists-e  "$\\exists\\mathsf{e}$"
   :equal-i   "$=\\mathsf{i}$"
   :equal-e   "$=\\mathsf{e}$"
   :atnext-i  "$\\medcircle\\,\\mathsf{i}$"
   :atnext-e  "$\\medcircle\\,\\mathsf{e}$"
   :always-i  "$\\medsquare\\,\\mathsf{i}$"
   :always-e  "$\\medsquare\\,\\mathsf{e}$"
   :finally-i "$\\lozenge\\,\\mathsf{i}$"
   :finally-e "$\\lozenge\\,\\mathsf{e}$"
   :until-i   "$\\mathcal{\\:U\\:}\\mathsf{i}$"
   :until-e   "$\\mathcal{\\:U\\:}\\mathsf{e}$"
   :not-until "$\\lnot\\mathcal{\\:U\\:}$"})

(defn- replace-sym
  "Substitutes all symbols in `term` according to `subst-map` and
   makes text in math mode of all other terms."
  [body]
  (cond (symbol? body)
        (if (contains? lsymbols body) (body lsymbols)
                                      (str "\\text{" body "}"))
        (keyword? body)
        (str "\\dots")
        :else (loop [loc (zip/seq-zip (seq body))]
                (if (zip/end? loc)
                  (zip/node loc)
                  (let [node (zip/node loc)]
                    (cond
                      (contains? lsymbols node) (recur (zip/replace loc (node lsymbols)))
                      (vector? node) (recur (zip/replace loc (str "\\text{" (first node) "}")))
                      (and (symbol? node) (= loc (zip/rightmost loc))) (recur (zip/replace loc (str "\\text{" node "}")))
                      (symbol? node) (recur (zip/replace loc (str "\\text{" node "}\\ ")))
                      :else (recur (zip/next loc))))))))

(defn- replace-roth
  "Formats roth for TeX output"
  [roth]
  (cond
    (nil? roth) "~"
    (contains? lroths roth) (roth lroths)
    :else (str "$\\mathsf{" (name roth) "}$")))

(defn- tex-code-line
  "Prints a proof line in TeX.       
   If the proof line is the last one in a subproof, there's no newline."
  [pline plid-plno-map last?]
  (let [body (replace-sym (:body pline))
        refs (walk/postwalk-replace plid-plno-map (:refs pline))
        roth (str (replace-roth (:roth pline)) "~" refs)]
    (println body "&" roth (if last? "" "\\\\"))))

(defn- tex-code
  "Returns the TeX-code of the proof."
  ([proof] (tex-code proof proof 0))
  ([proof sub depth]
   (let [smap (plid-plno-map proof)]
     ; begin of proof
     (when (zero? depth)
       (println "\\begin{minipage}{2in}")
       (println (str "\\begin{logicproof}{" (height proof) "}")))
     (when (pos? depth)
       (println "\\begin{subproof}"))
     ; plines and subproofs
     (doseq [pline sub]
       (if (vector? pline)
         (tex-code proof pline (inc depth)))
       (if (nil? (:plid pline))
         (println "\\end{subproof}")
         (tex-code-line pline smap (= pline (last sub)))))
     ; end of proof
     (when (zero? depth)
       (println "\\end{logicproof}")
       (println "\\end{minipage}")))))

(defn- tex-header
  "Header for full TeX file"
  []
  (let [date (.format (DateTimeFormatter/ofPattern "yyyy-MM-dd") (LocalDate/now))]
    (str/join \newline
              [(str "% " (format "Generated by lwb rev %s at %s" rev date))
               "\\documentclass{article}"
               "\\usepackage{amssymb}"
               "\\usepackage{MnSymbol}"
               "\\usepackage{logicproof}"
               "\\begin{document}"])))

(def tex-footer
  "Footer for full TeX file"
  (str/join "\newline"
            ["\\end{document}"]))

;; ## Interface for user of generation of tex code

(defn texify
  "Generates Tex code or pdf for the proof."
  ([proof] (tex-code proof))
  ([proof filename]
   (let [tex-body (with-out-str (tex-code proof))]
     (spit (str filename ".tex") (str (tex-header) tex-body tex-footer))
     (shell/sh "texi2pdf" (str filename ".tex"))
     (shell/sh "open" (str filename ".pdf")))))
