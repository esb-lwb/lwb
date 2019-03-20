; lwb Logic WorkBench -- Combinatory logic specs

; Copyright (c) 2019  Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.cl.spec
  (:require [clojure.spec.alpha :as s])
  (:import ))

(s/def ::combinator (s/and symbol?
                           #(Character/isUpperCase ^char (first (name %)))))

(comment
  (s/valid? ::combinator 'S)
  (s/valid? ::combinator 'x)
  (s/valid? ::combinator 1))

(s/def ::variable (s/and symbol?
                         #(Character/isLowerCase ^char (first (name %)))))

(comment
  (s/valid? ::variable 'S)
  (s/valid? ::variable 'x)
  (s/valid? ::variable 1))

(s/def ::simpl-expr (s/or :combinator ::combinator
                          :variable ::variable))

(s/def ::compl-expr (s/and list? #(> (count %) 1) (s/+ (s/or :simpl-expr ::simpl-expr 
                                                             :compl-expr  ::compl-expr))))

(s/def ::term (s/and vector? (s/* (s/or :simpl-expr ::simpl-expr
                                        :compl-expr ::compl-expr))))

(comment
  (s/valid? ::term '[S])
  (s/valid? ::term '[S x])
  (s/valid? ::term '[S x y z])
  (s/valid? ::term '[(S x)])
  (s/explain ::term '[(S x)])
  (s/valid? ::term '[x y z (S (x y))])
  (s/valid? ::term '[(x) y z (S (x y))])
  (s/explain ::term '[(x) y z (S (x y))])
  )
