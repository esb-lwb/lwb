; lwb Logic WorkBench -- Helper for using shell commands

; Copyright (c) 2017 Burkhardt Renz, Nicola Justus, THM.
; All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.util.shell
  (:require [clojure.spec.alpha :as s]
            [clojure.java.shell :as shell]
            [clojure.string :as str]))

(defn os
  "Get type of operating system"
  []
  (let [os-name (System/getProperty "os.name")]
    (condp #(str/includes? %2 %1) os-name
      "Linux"   :linux
      "Mac"     :mac
      "Windows" :windows
                :other)))

(s/fdef os
        :args nil?
        :ret #(contains? #{:linux :mac :windows :other} %))

(os)

; What's the right command on the windows platform??
(defn tex2pdf 
  "Calls shell to generate pdf from tex file"
  [filename]
  (let [command  (condp = (os)
                        :linux   ["texi2pdf"]
                        :mac     ["texi2pdf"]
                        :windows ["texify" "-p"]
                        :other   ["texi2pdf"])]
    (apply shell/sh (conj command filename))))

(defn open
  "Calls shell to open file with default app"
  [filename]
  (let [command (condp = (os)
                    :linux   ["xdg-open"]
                    :mac     ["open"]
                    :windows ["start"]
                    :other   ["open"])]
    (apply shell/sh (conj command filename))))


