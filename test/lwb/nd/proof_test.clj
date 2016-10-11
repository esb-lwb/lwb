; lwb Logic WorkBench -- Natural deduction -- tests

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.proof-test
  (:require [clojure.test :refer :all]
            [lwb.nd.proof :refer :all]))

(defn setup []
  (reset! plid 20))

(defn fixture [f]
  (setup)
  (f))

(use-fixtures :each fixture)

(deftest add-todo-test
  ; an unproved line has a ... above
  (is (= [{:plid 21, :body :todo, :rule nil}
          {:plid 1, :body '(or P (not P)), :rule nil}]
         (add-todo-lines
           [{:plid 1, :body '(or P (not P)), :rule nil}])))
  ; if there is already a ..., nothing happens
  (is (= [{:plid 21, :body :todo, :rule nil}
          {:plid 1, :body '(or P (not P)), :rule nil}]
         (add-todo-lines
           [{:plid 21, :body :todo, :rule nil}
            {:plid 1, :body '(or P (not P)), :rule nil}])))
  ; each unproved line has a ... above
  (is (= [{:plid 22, :body :todo, :rule nil}
          {:plid 1, :body '(or P (not P)), :rule nil}
          {:plid 23, :body :todo, :rule nil}
          {:plid 2, :body '(or P (not P)), :rule nil}]
         (add-todo-lines
           [{:plid 1, :body '(or P (not P)), :rule nil}
            {:plid 2, :body '(or P (not P)), :rule nil}])))
  ; add-todo-lines works fine with subproofs too
  (is (= [{:plid 3, :body :todo, :rule nil}
          {:plid 1, :body '(or P (not P)), :rule nil}
          [{:plid 24, :body :todo :rule nil}
           {:plid 4, :body 'A :rule nil}]]
         (add-todo-lines
           [{:plid 3, :body :todo, :rule nil}
            {:plid 1, :body '(or P (not P)), :rule nil}
            [{:plid 4, :body 'A :rule nil}]])))
  ; no ... above subproofs
  (is (= [{:plid 3, :body :todo, :rule nil}
          {:plid 1, :body '(or P (not P)), :rule nil}
          [{:plid 5, :body 'B, :rule :x}
           {:plid 25, :body :todo, :rule nil}
           {:plid 4, :body 'A, :rule nil}]]
         (add-todo-lines
           [{:plid 3, :body :todo, :rule nil}
            {:plid 1, :body '(or P (not P)), :rule nil}
            [{:plid 5, :body 'B, :rule :x}
             {:plid 4, :body 'A, :rule nil}]])))
         )

(deftest remove-todo-test
  ; if there is no ..., nothing happens
  (is (= [{:plid 1, :body '(or P (not P)), :rule nil}]
      (remove-todo-lines
        [{:plid 1, :body '(or P (not P)), :rule nil}])))
  ; a ... above a proved line gets removed
  (is (= [{:plid 1, :body '(or P (not P)), :rule :tnd}]
         (remove-todo-lines
           [{:plid 21, :body :todo, :rule nil}
            {:plid 1, :body '(or P (not P)), :rule :tnd}])))
  ; a ... between two checked lines gets removed
  (is (= [{:plid 2, :body 'A, :rule :and-e} 
          {:plid 1, :body '(or P (not P)), :rule :x}]
         (remove-todo-lines
           [{:plid 2, :body 'A, :rule :and-e}
            {:plid 21, :body :todo, :rule nil}
            {:plid 1, :body '(or P (not P)), :rule :x}])))
  ; a ... in a subproof is not removed if not followed by a checked line
  (is (= [{:plid 1, :body '(or P (not P)), :rule :x} 
          [{:plid 4, :body 'A, :rule nil}]]
         (remove-todo-lines
           [{:plid 3, :body :todo, :rule nil}
            {:plid 1, :body '(or P (not P)), :rule :x}
            [{:plid 4, :body 'A, :rule nil}]])))
  ; fn is okay in subproofs
  (is (= [{:plid 3, :body :todo, :rule nil} 
          {:plid 1, :body '(or P (not P)), :rule nil} 
          [{:plid 4, :body 'A, :rule :x}]]
         (remove-todo-lines
           [{:plid 3, :body :todo, :rule nil}
            {:plid 1, :body '(or P (not P)), :rule nil}
            [{:plid 5, :body :todo, :rule nil}
             {:plid 4, :body 'A, :rule :x}]])))
  ; a ... above a subproof gets removed
  (is (= [[{:plid 5, :body 'B, :rule :assumption} 
           {:plid 4, :body :todo, :rule nil} 
           {:plid 6, :body '(impl B A), :rule nil}]]
         (remove-todo-lines
           [{:plid 3, :body :todo, :rule nil}
            [{:plid 5, :body 'B, :rule :assumption}
             {:plid 4, :body :todo, :rule nil}
             {:plid 6, :body '(impl B A), :rule nil}]])))
  )


      (run-tests)
