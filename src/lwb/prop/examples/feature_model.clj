(ns lwb.prop.examples.feature-model
  (:require [lwb.prop.cardinality :refer (max-kof)]))

;; Definition of the syntax for a feature model in lwb

;; TODO

;; Example 1 - Elevator-Modell

'(fm elevator
    (ft elevator (man behavior) (opt voice-output call-buttons security safety))
    (ft behavior (man modes (opt service priorities)))
    (ft call-buttons (one-of directed-call undirected-call))
    (ft security (man permission))
    (ft safety (opt overloaded))
    (ft modes (one-of sabbath fifo shortest-path))
    (ft priorities (some-of rush-hour floor-priority person-priority))
    (ft permission (some-of floor-permission permission-control))
    (ctc (or call-buttons sabbath))
    (ctc (impl directed-call shortest-path))
    (ctc (impl undirected-call (or fifo shortest-path))))

;; Example 2
'(fm renovation-factory
    (ft renovation-factory (man source-lang impl-lang))
    (ft source-lang (some-of cobol sdl sql))
    (ft impl-lang (one-of asf java) (opt traversal)))

;; Functions and Macro for generating a formula for the model

(defn opts 
  "Parts of the formula for a group of optional features"
  [ft opt-expr]
  (loop [o (next opt-expr) result '()]
    (if o
      (recur (next o) (conj result (list 'impl (first o) ft)))
      result)))

(comment
  (opts 'safety '(opt overloaded onemore))
  )

(defn mans
  "Parts of the formula for a group of mandatory features"
  [ft man-expr]
  (loop [m (next man-expr) result '()]
    (if m
      (recur (next m) (conj result (list 'equiv ft (first m))))
      result)))

(comment
  (mans 'safety '(man overloaded onemore))
  )

(defn some-ofs
  "Parts of the formula for a group of features, where at least one is mandatory"
  [ft some-of-expr]
  (conj (opts ft some-of-expr) (concat (list 'or) (next some-of-expr))))

(comment
  (some-ofs 'safety '(some-of overloaded onemore x y z))
  )

(defn one-ofs
  "Parts of the formula for a group of features, where at exactly one is mandatory"
  [ft one-of-expr]
  (concat (some-ofs ft one-of-expr) (max-kof 1 (next one-of-expr))))

(comment
  (one-ofs 'safety '(one-of overloaded onemore x y z))
  )
  
(defn fts 
  "Parts of the formula from a feature definition"
  [[_ ftname & defs]]
  (let [fts' (fn [ft & ftgroups]
          (loop [g ftgroups result '()]
            (if g
              (let [func (case (first (first g))
                           opt opts
                           man mans
                           some-of some-ofs
                           one-of one-ofs
                           (constantly nil))]
                (recur (next g) (concat result (func ft (first g)))))
              result)))]
    (apply fts' ftname defs)))

(comment
  (fts '(ft renovation-factory (man source-lang impl-lang) (opt x y z)))
  )

(defn ctcs
  "Part of the formula from a cross tree constraint"
  [[_ fml]]
  (list fml))

(comment
  (ctcs '(ctc (or call-buttons sabbath)))
  )

(defmacro fm 
  "Returns formula according to the given feature model"
  [rootname & defs]
  (let [gen-fml (fn [rootname & defs]
         (loop [d (first defs), result (list 'and rootname)]
           (if d
             (let [def (first d)
                   func (case (first def)
                          ft fts
                          ctc ctcs
                          (constantly nil))]
               (recur (next d) (concat result (func (first d)))))
             result)))]
  `(~gen-fml '~rootname '~defs)))


;; Example 1
(comment
  (fm elevator
      (ft elevator (man behavior) (opt voice-output call-buttons security safety))
      (ft behavior (man modes (opt service priorities)))
      (ft call-buttons (one-of directed-call undirected-call))
      (ft security (man permission))
      (ft safety (opt overloaded))
      (ft modes (one-of sabbath fifo shortest-path))
      (ft priorities (some-of rush-hour floor-priority person-priority))
      (ft permission (some-of floor-permission permission-control))
      (ctc (or call-buttons sabbath))
      (ctc (impl directed-call shortest-path))
      (ctc (impl undirected-call (or fifo shortest-path))))
  )

 ;; Example 2
(comment
  (fm renovation-factory
      (ft renovation-factory (man source-lang impl-lang))
      (ft source-lang (some-of cobol sdl sql))
      (ft impl-lang (one-of asf java) (opt traversal)))
  )
