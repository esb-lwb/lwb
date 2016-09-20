; lwb Logic WorkBench -- Natural deduction -- tests

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.rules-prop-test
  (:require [clojure.test :refer :all]
            [lwb.nd.rules :refer :all]
            [lwb.nd.io :as io]))

(defn setup []
  (io/import-rules "resources/nd/rules-prop-pred.clj"))

(defn fixture [f]
  (setup)
  (f))

(use-fixtures :once fixture)

(deftest and-test
  (is (= (apply-rule :and-i true ['A 'B])
        '((and A B) (and B A))))
  (is (= (apply-rule :and-i true ['A '(or B C)])
         '((and A (or B C)) (and (or B C) A))))
  (is (= (apply-rule :and-i true ['(P x) 'B])
         '((and (P x) B) (and B (P x)))))
  ; :and-i forward needs the two givens
  (is (= (apply-rule :and-i false ['(and A B)])
         '([A B])))
  (is (= (apply-rule :and-i false ['(and (or A1 A2) B)])
         '([(or A1 A2) B])))
  ; :and-i backward needs the conclusion
  
  (is (= (apply-rule :and-e1 true ['(and (P x) (R x y))])
         '((P x))))
  (is (= (apply-rule :and-e2 true ['(and (P x) (R x y))])
         '((R x y))))
  ; :and-e1 and :and-e2 forward needs the given
  ; they are forward only
  )

(deftest or-test
  (is (= (apply-rule :or-i1 true ['A])
         '((or A _0))))
  (is (= (apply-rule :or-i1 true ['(or A B)])
         '((or (or A B) _0))))
  (is (= (apply-rule :or-i2 true ['B])
         '((or _0 B))))
  ; :or-i1 and :or-i2 forward needs the given
  (is (= (apply-rule :or-i1 false ['(or A B)])
         '(A)))
  (is (= (apply-rule :or-i2 false ['(or A B)])
         '(B)))
  ; :or-i1 and :or-i2 backward needs the conclusion
  
  (is (= (apply-rule :or-e true ['(or A B)])
         '([(infer A _0) (infer B _0) _0])))
  (is (= (apply-rule :or-e true ['(or A B)] ['X])
         '([(infer A X) (infer B X)] [(infer B X) (infer A X)])))
  (is (= (apply-rule :or-e true ['(or A B)] ['(and X Y)])
         '([(infer A (and X Y)) (infer B (and X Y))] [(infer B (and X Y)) (infer A (and X Y))])))
  ; :or-e forward needs the givens, but not infer
  ; as an optional argument the conclusion
  (is (= (apply-rule :or-e false ['X])
         '([(or _0 _1) (infer _0 X) (infer _1 X)])))
  (is (= (apply-rule :or-e false ['X] ['(or A B)])
         '([(infer A X) (infer B X)] [(infer B X) (infer A X)])))
  ; :or-e backwards needs the conclusion, as an optional argument the given (but not infer)
  )

(deftest impl-test
  (is (= (apply-rule :impl-i false ['(impl A B)])
         '((infer A B))))
  (is (= (apply-rule :impl-i false ['(impl (impl A1 A2) B)])
         '((infer (impl A1 A2) B))))
  ; :impl-i is backward only, needs the conclusion
  
  (is (= (apply-rule :impl-e true '[A (impl A B)])
         '(B)))
  (is (= (apply-rule :impl-e true '[(impl A B) A])
         '(B)))
  ; :impl-e forward needs the givens
  ; :impl-e is forward only
  ;; Bornat in Jape allows a kind of half forward and half backward of impl-e, see p.124 of his book
  ;; is this necessary?? (And: how to do this in lwb??)
  )

(deftest not-test
  (is (= (apply-rule :not-i false '[(not A)])
         '((infer A contradiction))))
  (is (= (apply-rule :not-i false '[(not (and A B))])
         '((infer (and A B) contradiction))))
  ; :not-i is backward only and needs the conclusion
  
  (is (= (apply-rule :not-e true '[A (not A)])
        '(contradiction)))
  (is (= (apply-rule :not-e true '[(not A) A])
         '(contradiction)))
  ; :not-e forward needs the givens
  (is (= (apply-rule :not-e false '[contradiction])
         '([_0 (not _0)])))  ; without optional argument
  (is (= (apply-rule :not-e false '[contradiction] '[A])
         '((not A))))        ; with positive optional argument
  (is (= (apply-rule :not-e false '[contradiction] '[(not A)])
         '((not (not A)) A)))  ; negative optional argument
  ; :not-e backward needs the conclusion and is better used with an optional argument
  )

(deftest raa-test
  (is (= (apply-rule :raa false '[A])
        '((infer (not A) contradiction))))
  ; :raa backward only, needs the conclusion
  
  (is (= (apply-rule :efq true '[contradiction])
         '(_0))) 
  ; :efq forward needs the given contradiction
  ;; optional argument fails, why??
  (is (= (apply-rule :efq false '[X])
        '(contradiction)))
  ; :efq backward needs conclusion
  )

(run-tests)
