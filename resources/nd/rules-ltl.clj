; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 - 2016 Tobias Völzel, Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

;; all data regarding forward/backward of the rules are without engagement

;; AND
{:id :and-i
 :given      [(at i a) (at i b)]
 :conclusion [(at i (and a b))]
 :forward   true
 :backward  true}
{:id :and-e1
 :given      [(at i (and a b))]
 :conclusion [(at i a)]
 :forward   true}
{:id :and-e2
 :given      [(at i (and a b))]
 :conclusion [(at i b)]
 :forward   true}

;; OR
{:id :or-i1
 :given      [(at i a)]
 :conclusion [(at i (or a b))]
 :forward   true
 :backward  true}
{:id :or-i2
 :given      [(at i b)]
 :conclusion [(at i (or a b))]
 :forward   true
 :backward  true}
{:id :or-e1
 :given      [(at i (or a b)) (at i (not a))]
 :conclusion [(at i b)]
 :forward   true
 :backward  true}
;; because of the nested structure of "(at i (or a b))" we need a rule for each side (a, b)
{:id :or-e2
 :given      [(at i (or a b)) (at i (not b))]
 :conclusion [(at i a)]
 :forward   true
 :backward  true}
{:id :not-or
 :given      [(at i (not (or a b)))]
 :conclusion [(at i (and (not a) (not b)))]
 :forward   true
 :backward  true}

;; NOT
{:id :not-i
 :given      [(infer (at j c) (at i (and b (not b))))]
 :conclusion [(at j (not c))]
 :backward  true}
{:id :not-e
 :given      [(at i (not (not a)))]
 :conclusion [(at i a)]
 :forward   true
 :backward  true}

;; IMPL
{:id :impl-i
 :given      [(infer (at i c) (at i b))]
 :conclusion [(at i (impl c b))]
 :backward  true}
{:id :impl-e
 :given      [(at i (impl a b)) (at i a)]
 :conclusion [(at i b)]
 :forward   true
 :backward  true}

;; ATNEXT
{:id :atnext-i
 :given      [(at j a) (atnext i j)]
 :conclusion [(at i (atnext a))]
 :forward   true
 :backward  true}
{:id :atnext-e
 :given      [(at i (atnext a))]
 :conclusion [(at j a)]
 :forward   true}

;; ALWAYS
{:id :always-i
 :given      [(infer (<= i j) (at j a))]
 :conclusion [(at i (always a))]
 :backward  true}
{:id :always-e 
 :given      [(<= i j) (at i (always a))]
 :conclusion [(at j a)]
 :forward   true}

;; SOMETIMES
{:id :sometime-i
 :given      [(at j a) (<= i j)]
 :conclusion [(at i (sometime a))]
 :forward   true
 :backward  true}
{:id :sometime-e
 :given      [(at i (sometime a))]
 :conclusion [(<= i j) (at j a)]
 :forward   true}
{:id :not-sometime
 :given      [(at i (not (sometime a)))]
 :conclusion [(at i (always (not a)))]
 :forward   true
 :backward  true}

;; UNTIL
{:id :until-i
 :given      [(at i b)]
 :conclusion [(at i (until a b))]
 :forward   true
 :backward  true}
{:id :until-e
 :given      [(at i (always (impl b c)))
              (at i (always (impl (and a (atnext c)) c)))]
 :conclusion [(at i (impl (until a b) c))]
 :backward  true}
{:id :not-until
 :given      [(at i (not (until a b)))]
 :conclusion [(at i (or (always (not b))
                         (until (not b) 
                                (and (not a) (not b)))))]
 :forward   true}

;; RELATIONAL JUDGEMENTS
{:id :reflexivity
 :given      []
 :conclusion [(<= i j)]
 :forward   true}
{:id :atnext-seriality
 :given      []
 :conclusion [(next i j)]
 :forward   true}
{:id :</<=
 :given      [(< i j)]
 :conclusion [(<= i j)]
 :forward   true}
{:id :atnext/<=
 :given      [(next i j)]
 :conclusion [(<= i j)]
 :forward   true
 :backward  true}
{:id :transitivity
 :given      [(<= i j) (<= j k)]
 :conclusion [(<= i k)]
 :forward   true}
{:id :linearity
 :given      [(<= i j) (<= i k)]
 :conclusion [(or (or (<= j k) (~= j k)) (<= k j))]
 :forward   true}
