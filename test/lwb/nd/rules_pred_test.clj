; lwb Logic WorkBench -- Natural deduction -- tests

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.rules-pred-test
  (:require [clojure.test :refer :all]
            [lwb.nd.rules :refer :all]
            [lwb.nd.prereqs :refer :all]
            [lwb.nd.io :as io]))

(defn setup []
  (io/import-rules "resources/nd/rules-prop-pred.clj"))

(defn fixture [f]
  (setup)
  (f))

(use-fixtures :once fixture)

(deftest forall-test
  (is (= (apply-rule :forall-i false '[(forall [x] (P x))])
         '((infer (actual _0) (substitution (P x) x _0)))))
  ; :forall-i is backward only and needs the conclusion
  
  (is (= (apply-rule :forall-e true '[(forall [x] (P x)) (actual :i)])
         '((substitution (P x) x :i)) ))
  ; :forall-e is forward only and needs the givens
  )

(deftest exists-test
  (is (= (apply-rule :exists-i false '[(exists [x] (P x))])
         '([(actual _0) (substitution (P x) x _0)])))
  (is (= (apply-rule :exists-i false '[(exists [x] (P x))] ['(actual :i)])
         ((substitution (P x) x :i))))
  ; :exists-i is backward only, needs the conclusion and optional an actual object
  
  (is (= (apply-rule :exists-e true '[(exists [x] (P x))])
         ([(infer [(actual _0) (substitution (P x) x _0)] _1) _1])))
  (is (= (apply-rule :exists-e true '[(exists [x] (P x))] '[X])
         ((infer [(actual _0) (substitution (P x) x _0)] X))))
  ; :exists-e is forward only, needs the given without infer and optional the conclusion
  )

(deftest equal-test
  (is (= (apply-rule :equal-i true [])
         '((= _0 _0))))
  ; :equal-i is forward only has no given

  (is (= (apply-rule :equal-e true '[(= a b) (P x) x])
         '((substitution (P x) x b))
        ))
  ; :equal-e is forward only 
  ;; TODO check this!! 
  
  (is (= (apply-rule :equal-reflexivity true '[(= a b)])
         '((= b a))))
  (is (= (apply-rule :equal-reflexivity false '[(= a b)])
         '((= b a))))
  ; :equal-reflexity
  )

(run-tests)
