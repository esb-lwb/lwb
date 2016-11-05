; lwb Logic WorkBench -- Natural deduction -- tests

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.roths-pred-test
  (:require [clojure.test :refer :all]
            [lwb.nd.rules :refer :all]
            [lwb.nd.repl :refer :all]))

(defn setup []
  (load-logic :pred))

(defn fixture [f]
  (setup)
  (f))

(use-fixtures :once fixture)

(deftest forall-test
  (is (= (roth-structure-forward :forall-i)             ; backward only
         nil))
  
  (is (= (roth-structure-backward :forall-i)
         [:cm :g?] ))
  (is (= (apply-roth :forall-i '(:? (forall [x] phi)))  ; (step-b :forall-i k)
         '[(infer (actual _0) (substitution phi x _0))]))

  (is (= (roth-structure-forward :forall-e)
         [:gm :gm :c?] ))
  (is (= (apply-roth :forall-e '((forall [x] phi) (actual t) :?))  ; (step-f :forall-e m n)
         '[(substitution phi x t)]))
  
  (is (= (roth-structure-backward :forall-e)            ; forward only
         nil))
  )

(deftest exists-test
  (is (= (roth-structure-forward :exists-i)             ; backward only
         nil ))
  
  (is (= (roth-structure-backward :exists-i)
         [:cm :go :g?] ))
  (is (= (apply-roth :exists-i '(:? :? (exists [x] phi)))       ; (step-b :exists-i k)
         '[(actual _0) (substitution phi x _0)] ))
  (is (= (apply-roth :exists-i '((actual t) :? (exists [x] phi)))  ; (step-b :exists-i k m)
         '[(substitution phi x t)] ))

  (is (= (roth-structure-forward :exists-e)
         [:gm :g? :co] ))
  (is (= (apply-roth :exists-e '((exists [x] (P (x))) :? :?))      ; (step-f :exists-e m)
         '[(infer [(actual _0) (substitution (P (x)) x _0)] _1) _1] ))
  (is (= (apply-roth :exists-e '((exists [x] (P (x))) :? X))       ; (step-f :exists-e m k)
          '[(infer [(actual _0) (substitution (P (x)) x _0)] X)] ))
         
  (is (= (roth-structure-backward :exists-e)
         [:cm :go :g?] ))
  (is (= (apply-roth :exists-e '(:? :? X))                         ; (step-b :exists-e k)
         '[(exists [_0] _1) (infer [(actual _2) (substitution _1 _0 _2)] X)]))
  (is (= (apply-roth :exists-e '((exists [x] (P (x))) :? X))       ; (step-b :exists-e k m)
         '[(infer [(actual _0) (substitution (P (x)) x _0)] X)] ))
  )

(deftest equal-test
  (is (= (roth-structure-forward :equal-i)
         [:c?] ))
  (is (= (apply-roth :equal-i '(:?))                    ; (step-f :equal-i) wie tnd
         '[(= _0 _0)]))
  
  (is (= (roth-structure-backward :equal-i)             ; forward only
         nil))

  (is (= (roth-structure-forward :equal-e)
         [:gm :gm :em :em :c?] ))
  (is (= (apply-roth :equal-e '((= a b) (P a) (P z) z :?)) ; (step-f :equal-e m n e1 e2)
         '[(substitution (P z) z b)] ))
  
  (is (= (roth-structure-backward :equal-e)              ; forward only
         nil))
  )

(run-tests)
