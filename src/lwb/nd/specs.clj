; lwb Logic WorkBench -- Natural deduction, specs for rules and theorems

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.specs
  [:require [clojure.spec :as s]])

;; # Specification of rules and theorems

;; The following specs describe the representation of rules and theorems

;; Expressions in rules and theorems       
;; To be correct the expression has to be a valid formula of the current logic.
;; Since the logic is dynamically determined at run time we can't check this,
;; so we make just this very generic check.

(s/def ::expr (s/or :list list? :symbol symbol?))

;; Id of a rule or theorem (called a roth)

(s/def ::id keyword?)

;; Given premises

(s/def ::given (s/coll-of ::expr :kind vector))

;; Conclusion or conclusions

(s/def ::conclusion (s/coll-of ::expr :kind vector))

;; Prerequisites for the application of the rule

(s/def ::prereq (s/nilable (s/coll-of ::expr :kind vector?)))

;; Extra arguments for the evaluation of a rule i.e. :equal-e

(s/def ::extra (s/nilable (s/coll-of ::expr :kind vector)))


;; ## Structure of a rule

(s/def ::rule (s/keys :req-un [::id ::given ::conclusion]
                      :opt-un [::prereq ::extra]))


;; Id of a proof line

(s/def ::plid pos-int?)

;; Body of a proof line

(s/def ::body (s/or :symbol symbol? :fml list? :keyword #{:todo}))

;; Roth that justifies the proof line

(s/def ::roth (s/nilable keyword?))

;; References of the justification
(s/def ::refs (s/nilable vector?))

;; A proof line has a unique proof line id `:plid`,      
;; a `:body` which is a formula or a special keyword,     
;; then the name of the rule or theorem `:roth` and if the roth is specified the      
;; `:refs` i.e. the plids of proof lines to which the application of the roth references

(s/def ::pline
  (s/keys :req-un [::plid ::body] :opt-un [::roth ::refs]))

;; ## Structure a proof
;; A proof is a nested vector of proof lines and subproofs

(s/def ::proof (s/and vector? (s/* (s/or :pline ::pline :subproof ::proof))))

;; ## Structure of a theorem

(s/def ::theorem (s/keys :req-un [::id ::given ::conclusion ::proof]))

