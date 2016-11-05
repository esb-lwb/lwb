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
  (is (= [{:plid 21, :body :todo, :roth nil}
          {:plid 1, :body '(or P (not P)), :roth nil}]
         (add-todo-lines
           [{:plid 1, :body '(or P (not P)), :roth nil}])))
  ; if there is already a ..., nothing happens
  (is (= [{:plid 21, :body :todo, :roth nil}
          {:plid 1, :body '(or P (not P)), :roth nil}]
         (add-todo-lines
           [{:plid 21, :body :todo, :roth nil}
            {:plid 1, :body '(or P (not P)), :roth nil}])))
  ; each unproved line has a ... above
  (is (= [{:plid 22, :body :todo, :roth nil}
          {:plid 1, :body '(or P (not P)), :roth nil}
          {:plid 23, :body :todo, :roth nil}
          {:plid 2, :body '(or P (not P)), :roth nil}]
         (add-todo-lines
           [{:plid 1, :body '(or P (not P)), :roth nil}
            {:plid 2, :body '(or P (not P)), :roth nil}])))
  ; add-todo-lines works fine with subproofs too
  (is (= [{:plid 3, :body :todo, :roth nil}
          {:plid 1, :body '(or P (not P)), :roth nil}
          [{:plid 24, :body :todo :roth nil}
           {:plid 4, :body 'A :roth nil}]]
         (add-todo-lines
           [{:plid 3, :body :todo, :roth nil}
            {:plid 1, :body '(or P (not P)), :roth nil}
            [{:plid 4, :body 'A :roth nil}]])))
  ; no ... above subproofs
  (is (= [{:plid 3, :body :todo, :roth nil}
          {:plid 1, :body '(or P (not P)), :roth nil}
          [{:plid 5, :body 'B, :roth :x}
           {:plid 25, :body :todo, :roth nil}
           {:plid 4, :body 'A, :roth nil}]]
         (add-todo-lines
           [{:plid 3, :body :todo, :roth nil}
            {:plid 1, :body '(or P (not P)), :roth nil}
            [{:plid 5, :body 'B, :roth :x}
             {:plid 4, :body 'A, :roth nil}]])))
         )

(deftest remove-todo-test
  ; if there is no ..., nothing happens
  (is (= [{:plid 1, :body '(or P (not P)), :roth nil}]
      (remove-todo-lines
        [{:plid 1, :body '(or P (not P)), :roth nil}])))
  ; a ... above a proved line gets removed
  (is (= [{:plid 1, :body '(or P (not P)), :roth :tnd}]
         (remove-todo-lines
           [{:plid 21, :body :todo, :roth nil}
            {:plid 1, :body '(or P (not P)), :roth :tnd}])))
  ; a ... between two checked lines gets removed
  (is (= [{:plid 2, :body 'A, :roth :and-e} 
          {:plid 1, :body '(or P (not P)), :roth :x}]
         (remove-todo-lines
           [{:plid 2, :body 'A, :roth :and-e}
            {:plid 21, :body :todo, :roth nil}
            {:plid 1, :body '(or P (not P)), :roth :x}])))
  ; a ... in a subproof is not removed if not followed by a checked line
  (is (= [{:plid 1, :body '(or P (not P)), :roth :x} 
          [{:plid 4, :body 'A, :roth nil}]]
         (remove-todo-lines
           [{:plid 3, :body :todo, :roth nil}
            {:plid 1, :body '(or P (not P)), :roth :x}
            [{:plid 4, :body 'A, :roth nil}]])))
  ; fn is okay in subproofs
  (is (= [{:plid 3, :body :todo, :roth nil} 
          {:plid 1, :body '(or P (not P)), :roth nil} 
          [{:plid 4, :body 'A, :roth :x}]]
         (remove-todo-lines
           [{:plid 3, :body :todo, :roth nil}
            {:plid 1, :body '(or P (not P)), :roth nil}
            [{:plid 5, :body :todo, :roth nil}
             {:plid 4, :body 'A, :roth :x}]])))
  ; a ... above a subproof gets removed
  (is (= [[{:plid 5, :body 'B, :roth :assumption} 
           {:plid 4, :body :todo, :roth nil} 
           {:plid 6, :body '(impl B A), :roth nil}]]
         (remove-todo-lines
           [{:plid 3, :body :todo, :roth nil}
            [{:plid 5, :body 'B, :roth :assumption}
             {:plid 4, :body :todo, :roth nil}
             {:plid 6, :body '(impl B A), :roth nil}]])))
  )


      (run-tests)
