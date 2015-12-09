; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 Tobias Völzel, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

;; AND
{:name "trivial-1"
 :given      [(and a false)]
 :conclusion [false]
 :forwards   true}
{:name "trivial-2"
 :given      [(and false b)]
 :conclusion [false]
 :forwards   true}
{:name "trivial-3"
 :given      [(and a true)]
 :conclusion [a]
 :forwards   true}
{:name "trivial-4"
 :given      [(and true b)]
 :conclusion [b]
 :forwards   true}
;; OR
{:name "trivial-5"
 :given      [(or a true)]
 :conclusion [true]
 :forwards   true}
{:name "trivial-6"
 :given      [(or true b)]
 :conclusion [true]
 :forwards   true}
{:name "trivial-7"
 :given      [(or a false)]
 :conclusion [a]
 :forwards   true}
{:name "trivial-8"
 :given      [(or false b)]
 :conclusion [b]
 :forwards   true}
;; IMPL
{:name "trivial-9"
 :given      [(impl false a)]
 :conclusion [true]
 :forwards   true}
{:name "trivial-9-1"
 :given      [(impl true true)]
 :conclusion [true]
 :forwards   true}
{:name "trivial-9-2"
 :given      [(impl a a)]
 :conclusion [true]
 :forwards   true}
;; NOT
{:name "trivial-10"
 :given      [(not (not a))]
 :conclusion [a]
 :forwards   true}
{:name "trivial-11"
 :given      [(not true)]
 :conclusion [false]
 :forwards   true}
{:name "trivial-12"
 :given      [(not false)]
 :conclusion [true]
 :forwards   true}

;; for temporal logic

;; ASAP
{:name "trivial-13"
 :given      [(asap false)]
 :conclusion [false]
 :forwards   true}
