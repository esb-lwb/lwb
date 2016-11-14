; lwb Logic WorkBench --
; Examples: Visualisation of formulas

; Copyright (c) 2016 Burkhardt Renz, Juan Markowich THM. 
; All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop.examples.vis
  (:require [lwb.vis :refer :all]))


(def grp-axioms-classic
  '(and
     (forall [x y z] (= (op x (op y z)) (op (op x y) z)))
     (exists [unit] (and
                      (forall [x] (= (op x unit) x))
                      (forall [x] (exists [inv] (= (op x inv) unit)))))))

(def ltl-phi
  '(and
     (always p)
     (finally q)
     (atnext r)
     (until s t)))

; interactive part
(comment  
  
  (texify '(and (or p_<12> q) q) "simple")
  
  (texify grp-axioms-classic "group-axioms")

  (texify '(or (and (or p q) q) r) "simple2")

  (texify ltl-phi "ltl")
  )
