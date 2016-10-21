; lwb Logic WorkBench -- Natural deduction -- tests

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.roths-prop-test
  (:require [clojure.test :refer :all]
            [lwb.nd.rules :refer :all]
            [lwb.nd.repl :refer :all]))

(defn setup []
  (load-logic :prop))

(defn fixture [f]
  (setup)
  (f))

(use-fixtures :once fixture)

(deftest and-test
  (is (= (roth-structure-forward :and-i)
         [:g1 :g1 :c?] ))
  (is (= (apply-roth :and-i '(A B :?))          ;(step-f :and-i m n)
         '[(and A B)]))
  (is (= (apply-roth :and-i '(A :? :?))         ;(step-f :and-i m)
         '[_0 (and A _0)]))
  (is (= (apply-roth :and-i '(:? B :?))         ;(step-f :and-i :? n)
         '[_0 (and _0 B)]))
  
  (is (= (roth-structure-backward :and-i)
         [:cm :gb :gb] ))
  (is (= (apply-roth :and-i '(:? :? (and A B))) ; (step-b :and-i k)
         '[A B]))
  (is (= (apply-roth :and-i '(A :? (and A B))) ; (step-b :and-i k m)
         '[B]))
  (is (= (apply-roth :and-i '(:? B (and A B))) ; (step-b :and-i k :? n)
         '[A]))

  (is (= (roth-structure-forward :and-e1)
         [:gm :c?] ))
  (is (= (apply-roth :and-e1 '((and A B) :?))  ; (step-f :and-e1 m)
         '[A]))
  
  (is (= (roth-structure-backward :and-e1)
         [:cm :g?] ))
  (is (= (apply-roth :and-e1 '(:? A))          ; (step-b :and-e1 k) 
         '[(and A _0)]))
  
  ; :and-e2 analogous
  )

(deftest or-test
  (is (= (roth-structure-forward :or-i1)
         [:gm :c?] ))
  (is (= (apply-roth :or-i1 '(A :?))           ; (step-f :or-i1 m)
         '[(or A _0)]))
  
  (is (= (roth-structure-backward :or-i1)
         [:cm :g?] ))
  (is (= (apply-roth :or-i1 '(:? (or A B)))    ; (step-b :or-i1 k)
         '[A]))
  
  ; :or-i2 analogous

  (is (= (roth-structure-forward :or-e)
         [:gm :g? :g? :co] ))
  (is (= (apply-roth :or-e '((or A B) :? :? :?)) ; (step-f :or-e m)
         '[(infer A _0) (infer B _0) _0]))
  (is (= (apply-roth :or-e '((or A B) :? :? X))  ; (step-f :or-e m k)
         '[(infer A X) (infer B X)]))
  
  (is (= (roth-structure-backward :or-e)
         [:cm :go :g? :g?] ))
  (is (= (apply-roth :or-e '(:? :? :? X))        ; (step-b :or-e k) 
         '[(or _0 _1) (infer _0 X) (infer _1 X)]))
  (is (= (apply-roth :or-e '((or A B) :? :? X))  ; (step-b :or-e k m)
         '[(infer A X) (infer B X)]))
  )

(deftest impl-test
  (is (= (roth-structure-forward :impl-i)        ; backward only
         nil))
  
  (is (= (roth-structure-backward :impl-i)
         [:cm :g?] ))
  (is (= (apply-roth :impl-i '(:? (impl A B)))   ; (step-b :impl-i k)
         '[(infer A B)]))
       
          
  (is (= (roth-structure-forward :impl-e)
         [:g1 :g1 :c?] ))
  (is (= (apply-roth :impl-e '((impl A B) A :?)) ; (step-f :impl-e m n)
         '[B]))
  (is (= (apply-roth :impl-e '((impl A B) :? :?)); (step-f :impl-e m) -> (impl A B)... A B
         '[A B]))
  (is (= (apply-roth :impl-e '((impl A B) :? B)); (step-f :impl-e m :? k) -> (impl A B) ... A B
         '[A]))
  
  (is (= (roth-structure-backward :impl-e)
         [:cm :gb :gb] ))
  (is (= (apply-roth :impl-e '(:? :? B))        ; (step-b :impl-e k) ->  ... (impl V1 B) ... V1 B
         '[(impl _0 B) _0]))
  (is (= (apply-roth :impl-e '(:? A B))         ; (step-b :impl-e k ? n) ->  A ... (impl A B) B
         '[(impl A B)]))
  (is (= (apply-roth :impl-e '((impl A B) :? B)); (step-b :impl-e k m) -> (impl A B) ... A B
         '[A]))
  )

(deftest not-test
  (is (= (roth-structure-forward :not-i)        ; backward only
         nil))
  
  (is (= (roth-structure-backward :not-i)
         [:cm :g?] ))
  (is (= (apply-roth :not-i '(:? (not A)))      ; (step-b :not-i k)
         '[(infer A contradiction)]))

  (is (= (roth-structure-forward :not-e)
         [:g1 :g1 :c?] ))
  (is (= (apply-roth :not-e '((not A) A :?))    ; (step-b :not-i m n)
         '[contradiction]))
  (is (= (apply-roth :not-e '((not A) :? :?))   ; (step-f :not-e m) -> (not A) ... A contradiction 
         '[A contradiction]))
  (is (= (apply-roth :not-e '(:? A :?))         ; (step-f :not-e :? n) ->  A ... (not A) contradiction
         '[(not A) contradiction]))
  
  (is (= (roth-structure-backward :not-e)
         [:cm :gb :gb] ))
  (is (= (apply-roth :not-e '(:? :? contradiction)); (step-b :not-e k) ->  ... (not V1) ...  V1 contradiction
         '[(not _0) _0]))
  (is (= (apply-roth :not-e '((not A) :? contradiction)); (step-b :not-e k m) ->  (not A) ...  A contradiction
         '[A]))
  (is (= (apply-roth :not-e '(:? A contradiction)); (step-b :not-e k :? n) ->  A ...  (not A) contradiction
         '[(not A)]))
  )

(deftest raa-test
  (is (= (roth-structure-forward :raa)           ; backward only
         nil))
  
  (is (= (roth-structure-backward :raa)
         [:cm :g?] ))
  (is (= (apply-roth :raa '(:? A))               ; (step-b :raa k)
         '[(infer (not A) contradiction)]))

  (is (= (roth-structure-forward :efq)
         [:gm :c?] ))
  (is (= (apply-roth :efq '(contradiction :?))   ; (step-f :efq m)
         '[_0]))
  
  (is (= (roth-structure-backward :efq)
         [:cm :g?] ))
  (is (= (apply-roth :efq '(:? A))               ; (step-b :efq k)
         '[contradiction]))
  )

; theorem without given
(deftest tnd-test
  (is (= (roth-structure-forward :tnd)   
         [:c?]))
  (is (= (apply-roth :tnd '(:?))                 ; (step-f :tnd) 
         '[(or _0 (not _0))]))

  (is (= (roth-structure-backward :tnd)          ; forward only
         nil))
  )
; theorem with one given
(deftest notnot-test
  (is (= (roth-structure-forward :notnot-i)
         [:gm :c?]))
  (is (= (apply-roth :notnot-i '(A :?))          ; (step-f :notnot-i m)
         '[(not (not A))]))
  
  (is (= (roth-structure-backward :notnot-i)
         [:cm :g?]))
  (is (= (apply-roth :notnot-i '(:? (not (not A)))); (step-b :not-not-i k) 
         '[A]))
  )

; theorem with two givens
(deftest mt-test
  (is (= (roth-structure-forward :mt)
         [:g1 :g1 :c?]))
  (is (= (apply-roth :mt '((impl A B) (not B) :?)); (step-f :mt m n)
         '[(not A)]))
  (is (= (apply-roth :mt '((impl A B) :? :?))    ; (step-f :mt m)
         '[(not B) (not A)]))
  (is (= (apply-roth :mt '( :? (not B) :?))      ; (step-f :mt :? n)
         '[(impl _0 B) (not _0)]))
  
  (is (= (roth-structure-backward :mt)
         [:cm :gb :gb]))
  (is (= (apply-roth :mt '(:? :? (not A)))       ; (step-b :mt k)
         '[(impl A _0) (not _0)]))
  (is (= (apply-roth :mt '(:? (not B) (not A)))  ; (step-b :mt k ? n)
         '[(impl A B)]))
  (is (= (apply-roth :mt '((impl A B) :? (not A))); (step-b :impl-e k m) 
         '[(not B)]))
  )

(run-tests)
