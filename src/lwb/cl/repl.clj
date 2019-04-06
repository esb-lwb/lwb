; lwb Logic WorkBench -- Interactive combinatory logic

; Copyright (c) 2019  Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.cl.repl
  (:refer-clojure :exclude [==])
  (:require [lwb.cl :refer :all]
            [lwb.cl.spec :refer :all]
            [clojure.core.logic :refer :all]
            [lwb.cl.printer :as printer]
            [lwb.nd.error :refer :all]
            [clojure.walk :as walk]))
  

;; The global stores for the current session ----------------------------------

;; A session is a vector of lines
;; a line is a hash-map with keys :lno line number, :term term [S x y z] e.g. :rule e.g. [:exp :S 1] 

(def ^:private session-store
  "Atom that holds the current session."
  (atom []))

(defn current-session
  "Current session."
  []
  @session-store)

(defn show
  "Print the current state of the session."
  []
  (printer/print-session (current-session)))

;; Some steps generate fresh variables.
;; They get new names beginning with the questionb mark `?` and followed by a unique number.

(def ^:private qmno
  "Global counter for the numbers of new question marks."
  (atom 0))

(defn- new-qmsymbol
  "Generates a new name for a question mark.     
   Uses global `qmno`."
  []
  (symbol (str \? (swap! qmno inc))))

(defn- reset-qmno
  "Resets global `qmno`."
  []
  (reset! qmno 0))

(defn- qmsymbol?
  "Is `s` a symbol of the form `?n`?"
  [s]
  (and (symbol? s) (re-matches #"\?\d+$" (name s))))

;; Starting a session ----------------------------------------------------------

(defn session
  "Starts a new session.     
   Modifies: atom `session-store`."
  [given-term]
  (reset-qmno)
  (let [given {:lno 1, :term (min-parens given-term), :rule [:given]}]
    (reset! session-store [given])
    (show)))

;; Doing steps in the session --------------------------------------------------

; helper functions
(defn- replace-lvars
  "Replaces logical variables from core.logic like `_0` by generated variable names `?1`."     
  [term]
  (let [lvars (set (filter #(.startsWith (str %) "_") (flatten term)))
        smap (reduce #(assoc %1 %2 (new-qmsymbol)) {} lvars)]
    (walk/prewalk-replace smap term)))

(defn undo
  "Undoes last step in a session."
  []
  (if (> (count (current-session)) 1)
    (swap! session-store pop)
    (println "There is no step to be undone!"))
  (show))

(defn- appx
  "Reduction or expansion in current session."
  [comb pos step-fn step-type]
  (try
    ;; error handling
    (if (not (comb-defined? comb))
      (throw (ex-error (str "Combinator " comb " not yet defined."))))
    (let [current-term (:term (last (current-session)))
          new-term (replace-lvars (step-fn current-term comb pos))]
      (if (= new-term current-term)
        (throw (ex-warning (format "Combinator %s has no effect at position %d" comb pos)))
        (swap! session-store conj {:lno (inc (count (current-session))) :term new-term :rule [step-type comb pos]}))
      (show))
    (catch Exception e
      (handle-exception e)
      (show)))) 

(defn red
  "Reduction in current session with the given combinator at position `i` (default 1)."
  ([comb]
   (red comb 1))
  ([comb pos]
   (appx comb pos one-step-red :red)))

(defn exp
  "Expansion in current session with the given combinator at position `i` (default 1)."
  ([comb]
   (exp comb 1))
  ([comb pos]
   (appx comb pos one-step-exp :exp)))

(defn swap
  "Replaces a fresh variable of the form `?n` with the given term."
  [var term]
  (try
    (if (not (wff? term))
      (throw (ex-error (str "Term should be well-formed, '" term "' is not!"))))
    (if (not (qmsymbol? var))
      (throw (ex-error "The var to be substituted must be of the form `?n``")))
    (let [new (for [line (current-session)]
                (merge line {:term (subst (:term line) var term)}))]
      (reset! session-store (vec new)))
    (show)
    (catch Exception e
      (handle-exception e)
      (show)))) 

