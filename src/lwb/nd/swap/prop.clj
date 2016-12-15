; lwb Logic WorkBench -- Natural deduction, check for unify in prop

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.swap.prop
  (:require [lwb.prop :as prop]
            [lwb.nd.error :refer :all]))

;; # Checking constraints of propositional logic

;; ## Checking the constraints for prop in swap

(defn check-swap
  "Check whether `old` and `new` can be swapped in `proof`.        
   Throws exception if not.     
   In the case of propositional logic we just assure that `new` is well-formed."
  [_ _ new]
  (if-not (prop/wff? new)
    (throw (ex-error (format "'%s' must be a well-formed formula of propositional logic." new)))))
