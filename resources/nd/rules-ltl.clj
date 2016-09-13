; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 Tobias Völzel, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

;; all data regarding forward/backward of the rules are without engagement

;; AND
{:name "and-i"
 :given      [(at i a) (at i b)]
 :conclusion [(at i (and a b))]
 :forward   true
 :backward  true}
{:name "and-e1"
 :given      [(at i (and a b))]
 :conclusion [(at i a)]
 :forward   true}
{:name "and-e2"
 :given      [(at i (and a b))]
 :conclusion [(at i b)]
 :forward   true}

;; OR
{:name "or-i1"
 :given      [(at i a)]
 :conclusion [(at i (or a b))]
 :forward   true
 :backward  true}
{:name "or-i2"
 :given      [(at i b)]
 :conclusion [(at i (or a b))]
 :forward   true
 :backward  true}
{:name "or-e1"
 :given      [(at i (or a b)) (at i (not a))]
 :conclusion [(at i b)]
 :forward   true
 :backward  true}
;; because of the nested structure of "(at i (or a b))" we need a rule for each side (a, b)
{:name "or-e2"
 :given      [(at i (or a b)) (at i (not b))]
 :conclusion [(at i a)]
 :forward   true
 :backward  true}
{:name "not-or"
 :given      [(at i (not (or a b)))]
 :conclusion [(at i (and (not a) (not b)))]
 :forward   true
 :backward  true}

;; NOT
{:name "not-i"
 :given      [(infer (at j c) (at i (and b (not b))))]
 :conclusion [(at j (not c))]
 :backward  true}
{:name "not-e"
 :given      [(at i (not (not a)))]
 :conclusion [(at i a)]
 :forward   true
 :backward  true}

;; IMPL
{:name "impl-i"
 :given      [(infer (at i c) (at i b))]
 :conclusion [(at i (impl c b))]
 :backward  true}
{:name "impl-e"
 :given      [(at i (impl a b)) (at i a)]
 :conclusion [(at i b)]
 :forward   true
 :backward  true}

;; ATNEXT
{:name "atnext-i"
 :given      [(at j a) (atnext i j)]
 :conclusion [(at i (atnext a))]
 :forward   true
 :backward  true}
{:name "atnext-e"
 :given      [(at i (atnext a))]
 :conclusion [(at j a)]
 :forward   true}

;; ALWAYS
{:name "always-i"
 :given      [(infer (<= i j) (at j a))]
 :conclusion [(at i (always a))]
 :backward  true}
{:name "always-e" 
 :given      [(<= i j) (at i (always a))]
 :conclusion [(at j a)]
 :forward   true}

;; SOMETIMES
{:name "sometime-i"
 :given      [(at j a) (<= i j)]
 :conclusion [(at i (sometime a))]
 :forward   true
 :backward  true}
{:name "sometime-e"
 :given      [(at i (sometime a))]
 :conclusion [(<= i j) (at j a)]
 :forward   true}
{:name "not-sometime"
 :given      [(at i (not (sometime a)))]
 :conclusion [(at i (always (not a)))]
 :forward   true
 :backward  true}

;; UNTIL
{:name "until-i"
 :given      [(at i b)]
 :conclusion [(at i (until a b))]
 :forward   true
 :backward  true}
{:name "until-e"
 :given      [(at i (always (impl b c)))
              (at i (always (impl (and a (atnext c)) c)))]
 :conclusion [(at i (impl (until a b) c))]
 :backward  true}
{:name "not-until"
 :given      [(at i (not (until a b)))]
 :conclusion [(at i (or (always (not b))
                         (until (not b) 
                                (and (not a) (not b)))))]
 :forward   true}

;; RELATIONAL JUDGEMENTS
{:name "reflexivity"
 :given      []
 :conclusion [(<= i j)]
 :forward   true}
{:name "atnext-seriality"
 :given      []
 :conclusion [(next i j)]
 :forward   true}
{:name "</<="
 :given      [(< i j)]
 :conclusion [(<= i j)]
 :forward   true}
{:name "atnext/<="
 :given      [(next i j)]
 :conclusion [(<= i j)]
 :forward   true
 :backward  true}
{:name "transitivity"
 :given      [(<= i j) (<= j k)]
 :conclusion [(<= i k)]
 :forward   true}
{:name "linearity"
 :given      [(<= i j) (<= i k)]
 :conclusion [(or (or (<= j k) (~= j k)) (<= k j))]
 :forward   true}
