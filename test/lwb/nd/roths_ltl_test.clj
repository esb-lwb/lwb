; lwb Logic WorkBench -- Natural deduction -- tests

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.roths-ltl-test
  (:require [clojure.test :refer :all]
            [lwb.nd.rules :refer :all]
            [lwb.nd.repl :refer :all]))

(defn setup []
  (load-logic :ltl))

(defn fixture [f]
  (setup)
  (f))

(use-fixtures :once fixture)

(deftest and-test
  (is (= (roth-structure-forward :and-i)
         [:g1 :g1 :c?] ))
  (is (= (apply-roth :and-i '((at [i] A) (at [i] B) :?)) ;(step-f :and-i m n)
         '[(at [i] (and A B))]))
  (is (= (apply-roth :and-i '((at [i] A) :? :?))         ;(step-f :and-i m)
         '[(at [i] _0) (at [i] (and A _0))]))
  (is (= (apply-roth :and-i '(:? (at [i] B) :?))         ;(step-f :and-i :? n)
         '[(at [i] _0) (at [i] (and _0 B))]))
  
  (is (= (roth-structure-backward :and-i)
         [:cm :gb :gb] ))
  (is (= (apply-roth :and-i '(:? :? (at [i] (and A B)))) ; (step-b :and-i k)
         '[(at [i] A) (at [i] B)]))
  (is (= (apply-roth :and-i '((at [i] A) :? (at [i] (and A B)))) ; (step-b :and-i k m)
         '[(at [i] B)]))
  (is (= (apply-roth :and-i '(:? (at [i] B) (at [i] (and A B)))) ; (step-b :and-i k :? n)
         '[(at [i] A)]))

  (is (= (roth-structure-forward :and-e1)
         [:gm :c?] ))
  (is (= (apply-roth :and-e1 '((at [i] (and A B)) :?))  ; (step-f :and-e1 m)
         '[(at [i] A)]))
  
  (is (= (roth-structure-backward :and-e1)
         [:cm :g?] ))
  (is (= (apply-roth :and-e1 '(:? (at [i] A)))          ; (step-b :and-e1 k) 
         '[(at [i] (and A _0))]))
  
  ; :and-e2 analogous
  )

(deftest or-test
  (is (= (roth-structure-forward :or-i1)
         [:gm :c?] ))
  (is (= (apply-roth :or-i1 '((at [i] A) :?))           ; (step-f :or-i1 m)
         '[(at [i] (or A _0))]))
  
  (is (= (roth-structure-backward :or-i1)
         [:cm :g?] ))
  (is (= (apply-roth :or-i1 '(:? (at [i] (or A B))))    ; (step-b :or-i1 k)
         '[(at [i] A)]))
  
  ; :or-i2 analogous

  (is (= (roth-structure-forward :or-e)
         [:gm :g? :g? :co] ))
  (is (= (apply-roth :or-e '((at [i] (or A B)) :? :? :?)) ; (step-f :or-e m)
         '[(infer (at [i] A) (at [i] _0)) (infer (at [i] B) (at [i] _0)) (at [i] _0)]))
  (is (= (apply-roth :or-e '((at [i] (or A B)) :? :? (at [i] X)))  ; (step-f :or-e m k)
         '[(infer (at [i] A) (at [i] X)) (infer (at [i] B) (at [i] X))]))
  
  (is (= (roth-structure-backward :or-e)
         [:cm :go :g? :g?] ))
  (is (= (apply-roth :or-e '(:? :? :? (at [i] X)))        ; (step-b :or-e k) 
         '[(at [i] (or _0 _1)) (infer (at [i] _0) (at [i] X)) (infer (at [i] _1) (at [i] X))]))
  (is (= (apply-roth :or-e '((at [i] (or A B)) :? :? (at [i] X)))  ; (step-b :or-e k m)
         '[(infer (at [i] A) (at [i] X)) (infer (at [i] B) (at [i] X))]))
  )

(deftest impl-test
  (is (= (roth-structure-forward :impl-i)        ; backward only
         nil))
  
  (is (= (roth-structure-backward :impl-i)
         [:cm :g?] ))
  (is (= (apply-roth :impl-i '(:? (at [i] (impl A B))))   ; (step-b :impl-i k)
         '[(infer (at [i] A) (at [i] B))]))
       
          
  (is (= (roth-structure-forward :impl-e)
         [:g1 :g1 :c?] ))
  (is (= (apply-roth :impl-e '((at [i] (impl A B)) (at [i] A) :?)) ; (step-f :impl-e m n)
         '[(at [i] B)]))
  (is (= (apply-roth :impl-e '((at [i] (impl A B)) :? :?)); (step-f :impl-e m) -> (impl A B)... A B
         '[(at [i] A) (at [i] B)]))
  (is (= (apply-roth :impl-e '((at [i] (impl A B)) :? (at [i] B))); (step-f :impl-e m :? k) -> (impl A B) ... A B
         '[(at [i] A)]))
  
  (is (= (roth-structure-backward :impl-e)
         [:cm :gb :gb] ))
  (is (= (apply-roth :impl-e '(:? :? (at [i] B)))        ; (step-b :impl-e k) ->  ... (impl V1 B) ... V1 B
         '[(at [i] (impl _0 B)) (at [i] _0)]))
  (is (= (apply-roth :impl-e '(:? (at [i] A) (at [i] B)))         ; (step-b :impl-e k ? n) ->  A ... (impl A B) B
         '[(at [i] (impl A B))]))
  (is (= (apply-roth :impl-e '((at [i] (impl A B)) :? (at [i] B))); (step-b :impl-e k m) -> (impl A B) ... A B
         '[(at [i] A)]))
  )

(deftest not-test
  (is (= (roth-structure-forward :not-i)        ; backward only
         nil))
  
  (is (= (roth-structure-backward :not-i)
         [:cm :g?] ))
  (is (= (apply-roth :not-i '(:? (at [i] (not A))))      ; (step-b :not-i k)
         '[(infer (at [i] A) (at [_0] contradiction))]))

  (is (= (roth-structure-forward :not-e)
         [:g1 :g1 :c?] ))
  (is (= (apply-roth :not-e '((at [i] (not A)) (at [i] A) :?))    ; (step-b :not-i m n)
         '[(at [_0] contradiction)]))
  (is (= (apply-roth :not-e '((at [i] (not A)) :? :?))   ; (step-f :not-e m) -> (not A) ... A contradiction 
         '[(at [i] A) (at [_0] contradiction)]))
  (is (= (apply-roth :not-e '(:? (at [i] A) :?))         ; (step-f :not-e :? n) ->  A ... (not A) contradiction
         '[(at [i] (not A)) (at [_0] contradiction)]))
  
  (is (= (roth-structure-backward :not-e)
         [:cm :gb :gb] ))
  (is (= (apply-roth :not-e '(:? :? (at [j] contradiction))); (step-b :not-e k) ->  ... (not V1) ...  V1 contradiction
         '[(at [_0] (not _1)) (at [_0] _1)]))
  (is (= (apply-roth :not-e '((at [i] (not A)) :? (at [j] contradiction))); (step-b :not-e k m) ->  (not A) ...  A contradiction
         '[(at [i] A)]))
  (is (= (apply-roth :not-e '(:? (at [i] A) (at [j] contradiction))); (step-b :not-e k :? n) ->  A ...  (not A) contradiction
         '[(at [i] (not A))]))
  )

(deftest raa-test
  (is (= (roth-structure-forward :raa)           ; backward only
         nil))
  
  (is (= (roth-structure-backward :raa)
         [:cm :g?] ))
  (is (= (apply-roth :raa '(:? (at [i] A)))               ; (step-b :raa k)
         '[(infer (at [i] (not A)) (at [_0] contradiction))]))

  (is (= (roth-structure-forward :efq)
         [:gm :c?] ))
  (is (= (apply-roth :efq '((at [i] contradiction) :?))   ; (step-f :efq m)
         '[(at [_0] _1)]))
  
  (is (= (roth-structure-backward :efq)
         [:cm :g?] ))
  (is (= (apply-roth :efq '(:? (at [i] A)))               ; (step-b :efq k)
         '[(at [_0] contradiction)]))
  )

(deftest atnext-test
  (is (= (roth-structure-forward :atnext-i)
         [:gm :gm :c?] ))
  (is (= (apply-roth :atnext-i '((at [j] A) (succ i j) :?))
         '[(at [i] (atnext A))]))
      
  (is (= (roth-structure-backward :atnext-i)
         [:cm :gb :gb] ))
  (is (= (apply-roth :atnext-i '(:? :? (at [i] (atnext A))))
         '[(at [_0] A) (succ i _0)]))
  (is (= (apply-roth :atnext-i '(:? (succ i j) (at [i] (atnext A))))
         '[(at [j] A)]))
  (is (= (apply-roth :atnext-i '((at [j] A) :? (at [i] (atnext A))))
         '[(succ i j)]))
  
  (is (= (roth-structure-forward :atnext-e)
         [:gm :gm :c?] ))
  (is (= (apply-roth :atnext-e '((at [i] (atnext A)) (succ i j) :?))
         '[(at [j] A)]))
         
  (is (= (roth-structure-backward :atnext-e)
         [:cm :gb :gb] ))
  (is (= (apply-roth :atnext-e '(:? :? (at [j] A)))
         '[(at [_0] (atnext A)) (succ _0 j)]))
  (is (= (apply-roth :atnext-e '(:? (succ i j) (at [j] A)))
         '[(at [i] (atnext A))]))
  (is (= (apply-roth :atnext-e '((at [i] (atnext A)) :? (at [j] A)))
         '[(succ i j)]))
  )

(deftest always-test
  (is (= (roth-structure-forward :always-i)             ; backward only
         nil))
  
  (is (= (roth-structure-backward :always-i)
         [:cm :g?]))
  (is (= (apply-roth :always-i '(:? (at [i] (always A))))
         '[(infer (<= i _0) (at [_0] A))]))
  
  (is (= (roth-structure-forward :always-e)
         [:g1 :g1 :c?]))
  (is (= (apply-roth :always-e '((at [i] (always A)) (<= i j) :?))
         '[(at [j] A)]))
  (is (= (apply-roth :always-e '((at [i] (always A)) :? :?))
         '[(<= i _0) (at [_0] A)]))
  (is (= (apply-roth :always-e '(:? (<= i j) :?))
         '[(at [i] (always _0)) (at [j] _0)]))
  

  (is (= (roth-structure-backward :always-e)
         [:cm :gb :gb]))
  (is (= (apply-roth :always-e '(:? :? (at [j] A)))
         '[(at [_0] (always A)) (<= _0 j)]))
  (is (= (apply-roth :always-e '((at [i] (always A)) :? (at [j] A)))
         '[(<= i j)]))
  (is (= (apply-roth :always-e '(:? (<= i j) (at [j] A)))
         '[(at [i] (always A))]))
  )

(deftest finally-test
  (is (= (roth-structure-forward :finally-i)
         [:g1 :g1 :c?] ))
  (is (= (apply-roth :finally-i '((at [j] A) (<= i j) :?))
         '[(at [i] (finally A))]))
  (is (= (apply-roth :finally-i '((at [j] A) :? :?))
         '[(<= _0 j) (at [_0] (finally A))]))
  (is (= (apply-roth :finally-i '( :? (<= i j) :?))
         '[(at [j] _0) (at [i] (finally _0))]))
         
  (is (= (roth-structure-backward :finally-i)
         [:cm :gb :gb] ))
  (is (= (apply-roth :finally-i '(:? :? (at [i] (finally A))))
         '[(at [_0] A) (<= i _0)]))
  (is (= (apply-roth :finally-i '((at [j] A) :? (at [i] (finally A))))
         '[(<= i j)]))
  (is (= (apply-roth :finally-i '(:? (<= i j )(at [i] (finally A))))
         '[(at [j] A)]))
         
  (is (= (roth-structure-forward :finally-e)
         [:gm :c? :c?] ))
  (is (= (apply-roth :finally-e '((at [i] (finally A)) :? :? ))
         '[(<= i _0) (at [_0] A)]))
         
  (is (= (roth-structure-backward :finally-e)
         nil))
  )

(deftest until-test
  (is (= (roth-structure-forward :until-i)
         [:gm :c?] ))
  (is (= (apply-roth :until-i '((at [i] B) :?))
         '[(at [i] (until _0 B))]))
         
  (is (= (roth-structure-backward :until-i)
         [:cm :g?] ))
  (is (= (apply-roth :until-i '(:? (at [i] (until A B))))
         '[(at [i] B)]))
         
  (is (= (roth-structure-forward :until-e)
         [:g1 :g1 :c?] ))
  (is (= (apply-roth :until-e '((at [i] (always (impl B X))) (at [i] (always (impl (and A (atnext X)) X))) :?))
         '[(at [i] (impl (until A B) X))]))
  (is (= (apply-roth :until-e '((at [i] (always (impl A X))) :? :?))
         '[(at [i] (always (impl (and _0 (atnext X)) X))) (at [i] (impl (until _0 A) X))]))
  (is (= (apply-roth :until-e '(:? (at [i] (always (impl (and A (atnext X)) X))) :?))
         '[(at [i] (always (impl _0 X))) (at [i] (impl (until A _0) X))]))
         
  (is (= (roth-structure-backward :until-e)
         [:cm :gb :gb] ))
  (is (= (apply-roth :until-e '(:? :? (at [i] (impl (until A B) X))))
         '[(at [i] (always (impl B X))) (at [i] (always (impl (and A (atnext X)) X)))] ))
  (is (= (apply-roth :until-e '((at [i] (always (impl B X))) :? (at [i] (impl (until A B) X))))
         '[(at [i] (always (impl (and A (atnext X)) X)))] ))
  (is (= (apply-roth :until-e '(:?  (at [i] (always (impl (and A (atnext X)) X))) (at [i] (impl (until A B) X))))
         '[(at [i] (always (impl B X)))] ))
         
  (is (= (roth-structure-forward :not-until)
         [:gm :c?] ))
  (is (= (apply-roth :not-until '((at [i] (not (until A B))) :?))
         '[(at [i] (or (always (not B)) (until (not B) (and (not A) (not B)))))] ))
          
  (is (= (roth-structure-backward :not-until)
         [:cm :g?] ))
  (is (= (apply-roth :not-until '(:? (at [i] (or (always (not B)) (until (not B) (and (not A) (not B)))))))
         '[(at [i] (not (until A B)))] ))
  )

(deftest relational
  (is (= (roth-structure-forward :reflexiv)
         [:c?]))
  (is (= (apply-roth :reflexiv '(:?))
         '[(<= _0 _0)]))
         
  (is (= (roth-structure-backward :reflexiv)
         nil))
         
  (is (= (roth-structure-forward :serial)
         [:c?]))
  (is (= (apply-roth :serial '(:?))
         '[(succ _0 _1)]))
         
  (is (= (roth-structure-backward :serial)
         nil))
         
  (is (= (roth-structure-forward :</<=)
         [:gm :c?] ))
  (is (= (apply-roth :</<= '((< i j) :?))
         '[(<= i j)]))
         
  (is (= (roth-structure-backward :</<=)
         [:cm :g?] ))
  (is (= (apply-roth :</<= '(:? (<= i j)))
         '[(_0 i j)]))
         
  (is (= (roth-structure-forward :atnext/<=)
         [:gm :c?] ))
  (is (= (apply-roth :atnext/<= '((succ i j) :?))
         '[(<= i j)]))
         
  (is (= (roth-structure-backward :atnext/<=)
         [:cm :g?] ))
  (is (= (apply-roth :atnext/<= '(:? (<= i j)))
         '[(succ i j)]))
         
  (is (= (roth-structure-forward :transitiv)
         [:g1 :g1 :c?]))
  (is (= (apply-roth :transitiv '((<= i j) (<= j k) :?))
         '[(<= i k)]))
  (is (= (apply-roth :transitiv '((<= i j) :? :?))
         '[(<= j _0) (<= i _0)]))
  (is (= (apply-roth :transitiv '(:? (<= j k) :?))
         '[(<= _0 j) (<= _0 k)]))
         
  (is (= (roth-structure-backward :transitiv)
         [:cm :gb :gb] ))
  (is (= (apply-roth :transitiv '(:? :? (<= i k)))
         '[(<= i _0) (<= _0 k)]))
  (is (= (apply-roth :transitiv '((<= i j) :? (<= i k)))
         '[(<= j k)]))
  (is (= (apply-roth :transitiv '(:? (<= j k) (<= i k)))
         '[(<= i j)]))
         
  (is (= (roth-structure-forward :linear)
         [:g1 :g1 :c?]))
  (is (= (apply-roth :linear '((<= i j) (<= i k) :?))
         '[(or (or (<= j k) (_0 j k)) (<= k j))]))
  (is (= (apply-roth :linear '(:? (<= i k) :?))
         '[(<= i _0) (or (or (<= _0 k) (_1 _0 k)) (<= k _0))]))
  (is (= (apply-roth :linear '((<= i j) :? :?))
         '[(<= i _0) (or (or (<= j _0) (_1 j _0)) (<= _0 j))]))
         
  (is (= (roth-structure-backward :linear)
         [:cm :gb :gb] ))
  (is (= (apply-roth :linear '(:? :? (or (or (<= j k) (== j k)) (<= k j))))
         '[(<= _0 j) (<= _0 k)]))
  (is (= (apply-roth :linear '((<= i j) :? (or (or (<= j k) (== j k)) (<= k j))))
         '[(<= i k)]))
  (is (= (apply-roth :linear '(:? (<= i k) (or (or (<= j k) (== j k)) (<= k j))))
         '[(<= i j)]))
  )

(run-tests)
