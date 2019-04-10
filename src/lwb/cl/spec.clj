; lwb Logic WorkBench -- Combinatory logic specs

; Copyright (c) 2019  Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.cl.spec
  (:require [clojure.spec.alpha :as s])
  (:import))

;; Specification of the syntax of combinatory logic

;; A combinator is a symbol whose first character is upper case
(s/def ::combinator (s/and symbol?
                           #(Character/isUpperCase ^char (first (name %)))))

;; A variable is a symbol whose first character is lower case
(s/def ::variable (s/and symbol?
                         #(Character/isLowerCase ^char (first (name %)))))

;; A simple expression is a combinator or a variable
(s/def ::simpl-expr (s/or :combinator ::combinator
                          :variable ::variable))

;; A complex expression is a nested list of complex or simple expression
(s/def ::compl-expr (s/and list? #(> (count %) 1) (s/+ (s/or :simpl-expr ::simpl-expr
                                                             :compl-expr ::compl-expr))))

;; A term is a vector of simple or complex expressions
(s/def ::term (s/and vector? (s/* (s/or :simpl-expr ::simpl-expr
                                        :compl-expr ::compl-expr))))

;; An application expression is a nested list of applications
(s/def ::appl-expr (s/and list? #(= (count %) 2) (s/+ (s/or :simpl-expr ::simpl-expr
                                                            :appl-expr ::appl-expr))))

;; A sterm is a simple expression or a nested list of sterms with simple expressions as leaves
(s/def ::sterm (s/or :simpl-expr ::simpl-expr
                     :appl-expr ::appl-expr))
