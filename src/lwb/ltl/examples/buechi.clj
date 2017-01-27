; lwb Logic WorkBench -- Linear Temporal Logic: Examples of Büchi automata from LTL formulas

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.ltl.examples.buechi
  (:require [lwb.ltl.buechi :refer :all])
  (:import (gov.nasa.ltl.graphio Writer Writer$Format)
           (java.io PrintStream StringReader StringWriter ByteArrayOutputStream)
           (javax.xml.transform.stream StreamSource StreamResult)
           (javax.xml.transform TransformerFactory OutputKeys)
           (gov.nasa.ltl.graph Graph)))

;; Output of Graph from LTL2Buchi in Format SM

(defn sm-write
  ([graph]
  (let [writer (Writer/getWriter Writer$Format/SM System/out)]
    (.write writer ^Graph graph)))
  ([graph filename]
   (let [printstream (PrintStream. ^String filename)
         writer (Writer/getWriter Writer$Format/SM printstream)]
     (.write writer ^Graph graph)))
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
                        OutputKeys/INDENT "yes")
    (.setOutputProperty transformer
                        "{http://xml.apache.org/xslt}indent-amount" "2")
    (.setOutputProperty transformer
                        OutputKeys/METHOD "xml")
    (.transform transformer in out)
    (-> out .getWriter .toString)))

(defn xml-write
  [graph]
  (let [baos (ByteArrayOutputStream.)
        ps   (PrintStream. baos)
        writer (Writer/getWriter Writer$Format/XML ps)]
    (.write writer ^Graph graph)
    (-> (String. (.toByteArray baos))
        ppxml
        println)))


(comment
  (def b1 (translate '(and P Q)))
  (xml-write b1)
  (sm-write b1)

  (def b2 (translate '(or P Q)))
  (xml-write b2)
  (sm-write b2)

  (def b3 (translate '(always P)))
  (xml-write b3)
  (sm-write b3)

  (def b4 (translate '(and (until P Q) (atnext (not P)))))
  (xml-write b4)
  (sm-write b4)

  (def b5 (translate '(impl (always P) (atnext P))))
  (xml-write b5)
  (sm-write b5)
  
  ; empty graph
  (def b6 (translate '(not (impl (always P) (atnext P)))))
  (xml-write b6)
  (sm-write b6)
  )



