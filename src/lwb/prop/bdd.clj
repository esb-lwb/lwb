; lwb Logic WorkBench -- Propositional Logic bdd

; Copyright (c) 2016 Mathias Gutenbrunner, Jens Lehnhäuser and Burkhardt Renz, THM.
; All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop.bdd
  (:require [lwb.prop :refer :all])
  (:import  (net.sf.javabdd JFactory BDD)))

; Example
(def phi1 '(and (or a b) (not (or c d))))
(def phi2 '(and p (not p)))
(def phi3 '(or p (not p)))

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
   'Typical values for nodesize are 10.000 nodes for small test examples
   and up to 1.000.000 nodes for large examples. A cache size of 10.000 seems
   to work good even for large examples, but lesser values should do it for smaller
   examples.' (from the documentation of bdd_init of buddy."
  ([type]
   (case type
     :small (init-bddf 10000 1000))
     :medsize (init-bddf 100000 10000)
   (init-bddf 1000000 10000))
  ([nodesize cachesize]
   (JFactory/init nodesize cachesize)))

(defn- atoms-as-bdds
  "the JavaBDD Factory `bddf` returns a map of BDD Objects for each atom of formula `phi`."
  [bddf phi]
  (let [atoms (atoms-of-phi phi), c (count atoms)]
    (if (zero? c)
      {}
      (do
        (.setVarNum bddf c)
        (zipmap atoms (map #(.ithVar bddf %) (range c)))))))

(atoms-as-bdds (init-bddf :small) '(and p q))
(atoms-as-bdds (init-bddf :small) 'true)

(def ^:private functions
  ^{:doc "mapping of operators to BDD functions"}
  {'not   #(.not   ^BDD %1)
   'and   #(.and   ^BDD %1 ^BDD %2)
   'or    #(.or    ^BDD %1 ^BDD %2)
   'impl  #(.imp   ^BDD %1 ^BDD %2)
   'equiv #(.biimp ^BDD %1 ^BDD %2)
   'xor   #(.xor   ^BDD %1 ^BDD %2)
   'ite   #(.ite   ^BDD %1 ^BDD %2 ^BDD %3)})

(defn- build-bdd-inner
  [bddf atom-map phi]
  (if (simple-expr? phi)
    (cond (= phi 'true) (.one bddf)
          (= phi 'false) (.zero bddf)
          :else (get atom-map phi))
    (let [op (first phi)]
      (case (arity op)
        1 ((functions op) (build-bdd-inner bddf atom-map (second phi)))
        2 ((functions op) (build-bdd-inner bddf atom-map (second phi))
            (build-bdd-inner bddf atom-map (nth phi 2)))
        3 ((functions op) (build-bdd-inner bddf atom-map (second phi))
            (build-bdd-inner bddf atom-map (nth phi 2))
            (build-bdd-inner bddf atom-map (nth phi 3)))
        -1 (reduce (functions op) (map #(build-bdd-inner bddf atom-map %1) (rest phi)))
        ))))

(defn build-bdd
  [bddf phi]
  (let [atom-map (atoms-as-bdds bddf phi)]
    (build-bdd-inner bddf atom-map phi)))

(type 'false)
(build-bdd (init-bddf :small) '(and p q))
(build-bdd (init-bddf :small) 'p)
(build-bdd (init-bddf :small) 'true)
(build-bdd (init-bddf :small) 'false)

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

(wff? 'true)



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
(with-bddf [bddf (init-bddf :small)]
           (bdd-dot bddf phi3))

; TODO: sat with bdd
