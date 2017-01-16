; lwb Logic WorkBench -- Linear Temporal Logic: Examples of Büchi automata from LTL formulas

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.ltl.examples.buechi
  (:require [lwb.ltl.buechi :refer :all]
            [clojure.spec :as s]
            [clojure.java.io :as io])
  (:import (gov.nasa.ltl.graphio Writer Writer$Format)
           (java.io PrintStream StringReader StringWriter ByteArrayOutputStream)
           (javax.xml.transform.stream StreamSource StreamResult)
           (javax.xml.transform TransformerFactory)))

;; Output of Graph from LTL2Buchi in Format SM

(defn sm-write
  ([graph]
  (let [writer (Writer/getWriter Writer$Format/SM System/out)]
    (.write writer graph)))
  ([graph filename]
   (let [printstream (PrintStream. filename)
         writer (Writer/getWriter Writer$Format/SM printstream)]
     (.write writer graph)))
  )

;; Output of Graph from LTL2Buchi in Format XML

; Pretty printing XML (from Nurullah Akkaye, see https://nakkaya.com/2010/03/27/pretty-printing-xml-with-clojure/
(defn ppxml [xml]
  (let [in (StreamSource.
             (StringReader. xml))
        writer (StringWriter.)
        out (StreamResult. writer)
        transformer (.newTransformer
                      (TransformerFactory/newInstance))]
    (.setOutputProperty transformer
                        javax.xml.transform.OutputKeys/INDENT "yes")
    (.setOutputProperty transformer
                        "{http://xml.apache.org/xslt}indent-amount" "2")
    (.setOutputProperty transformer
                        javax.xml.transform.OutputKeys/METHOD "xml")
    (.transform transformer in out)
    (-> out .getWriter .toString)))

(defn xml-write
  [graph]
  (let [baos (ByteArrayOutputStream.)
        ps   (PrintStream. baos)
        writer (Writer/getWriter Writer$Format/XML ps)]
    (.write writer graph)
    (-> (String. (.toByteArray baos))
        ppxml
        println)))


(def o02 '(and P Q))
(def g02 (translate o02))
(xml-write g02)
(sm-write g02)

(paths (ba o02))
(paths (ba '(always P)))

(def o03 '(or P Q))
(def g03 (translate o03))
(xml-write g03)

(paths (ba o03))

(def ba12  (translate '(and (until P Q) (atnext (not P)))))
(sm-write ba12)
(def ba12  (ba '(and (until P Q) (atnext (not P)))))
ba12

(def g12 (ba->Graph ba12))

(sm-write g12)



