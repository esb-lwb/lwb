(ns lwb.main-test
  (:require [lwb.prop :refer :all])
  (:require [lwb.prop.sat :refer [sat sat? valid?]])
  (:require [lwb.ltl :refer [nnf]])
  (:require [lwb.ltl.buechi :refer [fml]]))

(defn -main []
  (println (nnf '(always (equiv P Q))))
  (println (fml '(always (equiv P Q))))
  (println (sat '(equiv P Q))))