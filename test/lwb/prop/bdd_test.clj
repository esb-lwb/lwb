; lwb Logic WorkBench -- Propositional Logic, tests

; Copyright (c) 2014 - 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop.bdd-test
  (:require [clojure.test :refer :all]
            [lwb.prop :refer :all]
            [lwb.prop.bdd :refer :all]
            [clojure.spec :as s]))


; bdd ----------------------------------------------------------------
(deftest bdd-test
  (is (= (bdd 'true) [#lwb.prop.bdd.Node{:no 1, :atom true, :lo-no 1, :hi-no 1}]))
  (is (= (bdd 'false) [#lwb.prop.bdd.Node{:no 0, :atom false, :lo-no 0, :hi-no 0}]))
  (is (= (bdd 'p) [#lwb.prop.bdd.Node{:no 0, :atom false, :lo-no 0, :hi-no 0}
                   #lwb.prop.bdd.Node{:no 1, :atom true, :lo-no 1, :hi-no 1}
                   #lwb.prop.bdd.Node{:no 2, :atom p, :lo-no 0, :hi-no 1}]))
  (is (= (bdd '(not p)) [#lwb.prop.bdd.Node{:no 0, :atom false, :lo-no 0, :hi-no 0}
                         #lwb.prop.bdd.Node{:no 1, :atom true, :lo-no 1, :hi-no 1}
                         #lwb.prop.bdd.Node{:no 2, :atom p, :lo-no 1, :hi-no 0}]))
  (is (= (bdd '(and p q))[#lwb.prop.bdd.Node{:no 0, :atom false, :lo-no 0, :hi-no 0}
                          #lwb.prop.bdd.Node{:no 1, :atom true, :lo-no 1, :hi-no 1}
                          #lwb.prop.bdd.Node{:no 2, :atom p, :lo-no 0, :hi-no 3}
                          #lwb.prop.bdd.Node{:no 3, :atom q, :lo-no 0, :hi-no 1}]))
  (is (= (bdd '(or p q)) [#lwb.prop.bdd.Node{:no 0, :atom false, :lo-no 0, :hi-no 0}
                         #lwb.prop.bdd.Node{:no 1, :atom true, :lo-no 1, :hi-no 1}
                         #lwb.prop.bdd.Node{:no 2, :atom p, :lo-no 3, :hi-no 1}
                         #lwb.prop.bdd.Node{:no 3, :atom q, :lo-no 0, :hi-no 1}]))
  (is (= (bdd '(impl p q)) [#lwb.prop.bdd.Node{:no 0, :atom false, :lo-no 0, :hi-no 0}
                            #lwb.prop.bdd.Node{:no 1, :atom true, :lo-no 1, :hi-no 1}
                            #lwb.prop.bdd.Node{:no 2, :atom p, :lo-no 1, :hi-no 3}
                            #lwb.prop.bdd.Node{:no 3, :atom q, :lo-no 0, :hi-no 1}]))
  (is (= (bdd '(equiv p q)) [#lwb.prop.bdd.Node{:no 0, :atom false, :lo-no 0, :hi-no 0}
                             #lwb.prop.bdd.Node{:no 1, :atom true, :lo-no 1, :hi-no 1}
                             #lwb.prop.bdd.Node{:no 2, :atom p, :lo-no 3, :hi-no 4}
                             #lwb.prop.bdd.Node{:no 3, :atom q, :lo-no 1, :hi-no 0}
                             #lwb.prop.bdd.Node{:no 4, :atom q, :lo-no 0, :hi-no 1}]))
  (is (= (bdd '(ite p q r)) [#lwb.prop.bdd.Node{:no 0, :atom false, :lo-no 0, :hi-no 0}
                             #lwb.prop.bdd.Node{:no 1, :atom true, :lo-no 1, :hi-no 1}
                             #lwb.prop.bdd.Node{:no 2, :atom p, :lo-no 3, :hi-no 4}
                             #lwb.prop.bdd.Node{:no 3, :atom r, :lo-no 0, :hi-no 1}
                             #lwb.prop.bdd.Node{:no 4, :atom q, :lo-no 0, :hi-no 1}]))
  (is (= (bdd '(impl p q)) (bdd '(or (not p) q))))
  (is (= (bdd '(or (and a b) (and a c) (and b c))) ; Knuth Fig. 21
         [#lwb.prop.bdd.Node{:no 0, :atom false, :lo-no 0, :hi-no 0}
          #lwb.prop.bdd.Node{:no 1, :atom true, :lo-no 1, :hi-no 1}
          #lwb.prop.bdd.Node{:no 2, :atom a, :lo-no 3, :hi-no 4}
          #lwb.prop.bdd.Node{:no 3, :atom b, :lo-no 0, :hi-no 5}
          #lwb.prop.bdd.Node{:no 4, :atom b, :lo-no 5, :hi-no 1}
          #lwb.prop.bdd.Node{:no 5, :atom c, :lo-no 0, :hi-no 1}]))
)


; sat ---------------------------------------------------------------

(deftest sat-test
  (is (= (sat 'p) '[p true]))
  (is (= (sat 'true) true))
  (is (= (sat '(and p q)) '[p true q true]))
  (is (= (sat '(and p q) :all) '([p true q true])))
  (is (sat? '(or p q)) true)
  (is (sat '(or p q) :all) '([p false q true] [p true q false] [p true q true]))
  (is (sat '(or p q r) :all) '([p false q false r true]
                               [p false q true r false]
                               [p false q true r true]
                               [p true q false r false]
                               [p true q false r true]
                               [p true q true r false]
                               [p true q true r true]))
  (is (= (sat '(impl p q)) '[p false q true]))
  (is (= (sat '(impl p q) :all) '([p false q false] [p false q true] [p true q true])))
  (is (= (sat '(and p (not p))) nil))
  (is (= (sat '(or p (not p))) true))
  )

(run-tests)

; manual tests -------------------------------------------------------

(comment
  (vis-pdf '(or (and x_1 x_2) (and x_1 x_3) (and x_2 x_3)) "majority1")
  (vis-pdf '(or (and x_<01> x_<02>) (and x_<01> x_<03>) (and x_<02> x_<03>)) "majority3")
  )
