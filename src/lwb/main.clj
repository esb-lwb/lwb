; lwb Logic WorkBench -- For starting lwb in a repl

; Copyright (c) 2014 - 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
(ns lwb.main
  (:require [clojure.string :as str]
            [lwb.consts :refer [welcome]]))

;; # Starting lwb in a repl

;; The command is like this one:

;; `rlwrap java -Xms2G -cp ~/bin/lwb.jar clojure.main -i @lwb/main.clj -r` 


;; This command opens a repl in namespace `user` and shows the startinfo.

(defn man
  "Manual"
  []
  (let [info (str/join \newline
                       [welcome
                        "Propositional logic:"
                        "- (load \"lwb/prop\")"
                        "- (ns lwb.prop)"
                        "Predicate logic:"
                        "- (load \"lwb/pred\")"
                        "- (ns lwb.pred)"
                        "Linear temporal logic:"
                        "- (load \"lwb/ltl\")"
                        "- (ns lwb.ltl)"
                        "Natural deduction:"
                        "- (load \"lwb/nd/repl\")"
                        "- (ns lwb.nd.repl)"])]
    (println info)))

(man)

