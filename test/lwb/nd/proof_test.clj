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

(deftest todo-test
  (is (= [{:plid 21, :body :todo, :rule nil}
          {:plid 1, :body '(or P (not P)), :rule nil}]
         (add-todo-lines
           [{:plid 1, :body '(or P (not P)), :rule nil}])))
  (is (= [{:plid 21, :body :todo, :rule nil}
          {:plid 1, :body '(or P (not P)), :rule nil}]
         (add-todo-lines
           [{:plid 21, :body :todo, :rule nil}
            {:plid 1, :body '(or P (not P)), :rule nil}])))
  (is (= [{:plid 22, :body :todo, :rule nil}
          {:plid 1, :body '(or P (not P)), :rule nil}
          {:plid 23, :body :todo, :rule nil}
          {:plid 2, :body '(or P (not P)), :rule nil}]
         (add-todo-lines
           [{:plid 1, :body '(or P (not P)), :rule nil}
            {:plid 2, :body '(or P (not P)), :rule nil}])))
  (is (= [{:plid 3, :body :todo, :rule nil}
          {:plid 1, :body '(or P (not P)), :rule nil}
          [{:plid 24, :body :todo :rule nil}
           {:plid 4, :body 'A :rule nil}]]
         (add-todo-lines
           [{:plid 3, :body :todo, :rule nil}
            {:plid 1, :body '(or P (not P)), :rule nil}
            [{:plid 4, :body 'A :rule nil}]])))
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

      (run-tests)
