; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 Tobias Völzel, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

;; all data regarding forwards/backwards of the rules are without engagement

;; AND
{:name "and-i"
 :given      [(at i a) (at i b)]
 :conclusion [(at i (and a b))]
 :forwards   true
 :backwards  true}
{:name "and-e1"
 :given      [(at i (and a b))]
 :conclusion [(at i a)]
 :forwards   true}
{:name "and-e2"
 :given      [(at i (and a b))]
 :conclusion [(at i b)]
 :forwards   true}

;; OR
{:name "or-i1"
 :given      [(at i a)]
 :conclusion [(at i (or a b))]
 :forwards   true
 :backwards  true}
{:name "or-i2"
 :given      [(at i b)]
 :conclusion [(at i (or a b))]
 :forwards   true
 :backwards  true}
{:name "or-e1"
 :given      [(at i (or a b)) (at i (not a))]
 :conclusion [(at i b)]
 :forwards   true
 :backwards  true}
;; because of the nested structure of "(at i (or a b))" we need a rule for each side (a, b)
{:name "or-e2"
 :given      [(at i (or a b)) (at i (not b))]
 :conclusion [(at i a)]
 :forwards   true
 :backwards  true}
{:name "not-or"
 :given      [(at i (not (or a b)))]
 :conclusion [(at i (and (not a) (not b)))]
 :forwards   true
 :backwards  true}

;; NOT
{:name "not-i"
 :given      [(infer (at j c) (at i (and b (not b))))]
 :conclusion [(at j (not c))]
 :backwards  true}
{:name "not-e"
 :given      [(at i (not (not a)))]
 :conclusion [(at i a)]
 :forwards   true
 :backwards  true}

;; IMPL
{:name "impl-i"
 :given      [(infer (at i c) (at i b))]
 :conclusion [(at i (impl c b))]
 :backwards  true}
{:name "impl-e"
 :given      [(at i (impl a b)) (at i a)]
 :conclusion [(at i b)]
 :forwards   true
 :backwards  true}

;; ASAP
{:name "asap-i"
 :given      [(at j a) (next i j)]
 :conclusion [(at i (asap a))]
 :forwards   true
 :backwards  true}
{:name "asap-e"
 :given      [(at i (asap a))]
 :conclusion [(at j a)]
 :forwards   true}

;; ALWAYS
{:name "always-i"
 :given      [(infer (<= i j) (at j a))]
 :conclusion [(at i (always a))]
 :backwards  true}
{:name "always-e" 
 :given      [(<= i j) (at i (always a))]
 :conclusion [(at j a)]
 :forwards   true}

;; SOMETIMES
{:name "sometime-i"
 :given      [(at j a) (<= i j)]
 :conclusion [(at i (sometime a))]
 :forwards   true
 :backwards  true}
{:name "sometime-e"
 :given      [(at i (sometime a))]
 :conclusion [(<= i j) (at j a)]
 :forwards   true}
{:name "not-sometime"
 :given      [(at i (not (sometime a)))]
 :conclusion [(at i (always (not a)))]
 :forwards   true
 :backwards  true}

;; UNTIL
{:name "until-i"
 :given      [(at i b)]
 :conclusion [(at i (until a b))]
 :forwards   true
 :backwards  true}
{:name "until-e"
 :given      [(at i (always (impl b c)))
              (at i (always (impl (and a (asap c)) c)))]
 :conclusion [(at i (impl (until a b) c))]
 :backwards  true}
{:name "not-until"
 :given      [(at i (not (until a b)))]
 :conclusion [(at i (or (always (not b))
                         (until (not b) 
                                (and (not a) (not b)))))]
 :forwards   true}

;; RELATIONAL JUDGEMENTS
{:name "reflexivity"
 :given      []
 :conclusion [(<= i j)]
 :forwards   true}
{:name "asap-seriality"
 :given      []
 :conclusion [(next i j)]
 :forwards   true}
{:name "</<="
 :given      [(< i j)]
 :conclusion [(<= i j)]
 :forwards   true}
{:name "asap/<="
 :given      [(next i j)]
 :conclusion [(<= i j)]
 :forwards   true
 :backwards  true}
{:name "transitivity"
 :given      [(<= i j) (<= j k)]
 :conclusion [(<= i k)]
 :forwards   true}
{:name "linearity"
 :given      [(<= i j) (<= i k)]
 :conclusion [(or (or (<= j k) (~= j k)) (<= k j))]
 :forwards   true}
