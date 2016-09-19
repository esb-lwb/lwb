; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 Tobias Völzel, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

;; AND
{:id trivial-1
 :given      [(and a false)]
 :conclusion [false]
 :forward   true}
{:id trivial-2
 :given      [(and false b)]
 :conclusion [false]
 :forward   true}
{:id trivial-3
 :given      [(and a true)]
 :conclusion [a]
 :forward   true}
{:id trivial-4
 :given      [(and true b)]
 :conclusion [b]
 :forward   true}
;; OR
{:id trivial-5
 :given      [(or a true)]
 :conclusion [true]
 :forward   true}
{:id trivial-6
 :given      [(or true b)]
 :conclusion [true]
 :forward   true}
{:id trivial-7
 :given      [(or a false)]
 :conclusion [a]
 :forward   true}
{:id trivial-8
 :given      [(or false b)]
 :conclusion [b]
 :forward   true}
;; IMPL
{:id trivial-9
 :given      [(impl false a)]
 :conclusion [true]
 :forward   true}
{:id trivial-9-1
 :given      [(impl true true)]
 :conclusion [true]
 :forward   true}
{:id trivial-9-2
 :given      [(impl a a)]
 :conclusion [true]
 :forward   true}
;; NOT
{:id trivial-10
 :given      [(not (not a))]
 :conclusion [a]
 :forward   true}
{:id trivial-11
 :given      [(not true)]
 :conclusion [false]
 :forward   true}
{:id trivial-12
 :given      [(not false)]
 :conclusion [true]
 :forward   true}

;; for temporal logic

;; ATNEXT
{:id trivial-13
 :given      [(atnext false)]
 :conclusion [false]
 :forward   true}
