(ns lwb.pred.examples.group
  (:require [lwb.pred :refer :all #_(wff? eval-phi)]))

;; Signature
(def grp-sig
  {:unit [:func 0]
   :op   [:func 2]
   :inv  [:func 1]})

;; Associativity of group operation
(def grp-ass '(forall [x y z] (= (op x (op y z)) (op (op x y) z))))

(wff? grp-ass grp-sig :msg)

;; Unit
(def grp-unit '(forall [x] (= (op x unit) x)))

(wff? grp-unit grp-sig :msg)

;; Inverses
(def grp-inv '(forall [x] (= (op x (inv x)) unit)))

(wff? grp-inv grp-sig :msg)

;; Abelian Group?
(def grp-comm '(forall [x y] (= (op x y) (op y x))))

(wff? grp-comm grp-sig :msg)

;; Group operation for cyclic groups
;; Elements are represented by keywords :0 ...

(defmacro cyclic [n]
  `(fn [~'x ~'y] (let [numx# (Integer/parseInt (name ~'x)) numy# (Integer/parseInt (name ~'y))]
              (keyword (str (rem (+ numx# numy#) ~n))))))

(defmacro invert [n]
  `(fn [~'x] (let [numx# (Integer/parseInt (name ~'x))]
            (keyword (str (rem (- ~n numx#) ~n))))))

;; Group with 1 element
(def c1
  {:univ #{:0}
   :op   [:func 2 (cyclic 1)]
   :inv  [:func 1 (invert 1)]
   :unit [:func 0 :0]})

(eval-phi grp-ass c1)
(eval-phi grp-inv c1)
(eval-phi grp-unit c1)
(eval-phi grp-comm c1)

;; Cyclic group with 6 elements
(def c6
  {:univ #{:0 :1 :2 :3 :4 :5}
   :op   [:func 2 (cyclic 6)]
   :inv  [:func 1 (invert 6)]
   :unit [:func 0 :0]})

(eval-phi grp-ass c6)
(eval-phi grp-inv c6)
(eval-phi grp-unit c6)
(eval-phi grp-comm c6)

;; Symmetric group with 6 elements
;; smallest non-abelian group

(defn s6-op [x y]
  (let [s6-table [[:0	:1	:2	:3	:4	:5]
                  [:1	:0  :4	:5 	:2	:3]
                  [:2	:5	:0	:4	:3	:1]
                  [:3	:4	:5	:0	:1	:2]
                  [:4	:3	:1	:2	:5	:0]
                  [:5	:2	:3	:1	:0	:4]]
        numx (Integer/parseInt (name x)) 
        numy (Integer/parseInt (name y))]
    (get-in s6-table [numx numy])))

(defn s6-inv [x]
  (let [s6-invs [:0 :1 :2 :3 :5 :4]
        numx (Integer/parseInt (name x))]
    (get s6-invs numx)))

(def s6
  {:univ #{:0 :1 :2 :3 :4 :5}
   :op   [:func 2 s6-op]
   :inv  [:func 1 s6-inv]
   :unit [:func 0 :0]})

(eval-phi grp-ass s6)
(eval-phi grp-inv s6)
(eval-phi grp-unit s6)
(eval-phi grp-comm s6)
; => false
