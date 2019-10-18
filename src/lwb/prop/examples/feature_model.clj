(ns lwb.prop.examples.feature-model
  (:require [lwb.prop :refer :all]
            [lwb.prop.cardinality :refer :all]
            [lwb.prop.sat :refer (sat sat? valid?)]
            [lwb.prop.cardinality :refer (max-kof)]))

;; Feature Modeling

;; Feature Modeling is a technique to analyze and manage the variability in a
;; product line. In a product line concrete products share certain properties (or features)
;; and differ in other properties.
;;
;; A feature model comprises a feature diagram together with additional constraints.
;; The feature diagram is a tree of features, where subfeatures are properties that realize
;; their super feature.
;;
;; For groups of subfeatures there are four types of relationships:
;; 1. The subfeatures are mandatory to get the super feature
;; 2. The subfeatures are optional to get the super feature
;; 3. At least one of the subfeatures is needed to realize the super feature
;; 4. Exactly one of the subfeatures realizes the super feature
;;
;; The additional constraints are cross tree constraints, that define integrity conditions
;; for features across the structure of the feature tree.

;; Example:
;; In the book "Mastering Software Variability with FeatureIDE the authors (Jens Meinecke, Thomas Thüm,
;; Raimar Schröter, Fabian Bendhuhn, Thomas Leich, and Gunter Saake) have the example of a software
;; controlling an elevator as running example. The feature model für this software product line is 
;; Fig. 5.7 on page 52 of the book.  

;; From a feature model is quite straight forward to build a propositional formula that represents
;; the feature model: The features are the variables of the formula, a feature is part of a concrete
;; product if the value of the variable is true, and it is not part of the product if its truth 
;; value is false.
;;
;; SAT solvers that solve the satisfiability problem of propositional logic can therefore be 
;; used to check whether a configuration of features are valid, They can also determine all
;; possible configurations of products from a given feature model. Tools like the FeatureIDE
;; from the book above use SAT solver for analyzing and managing feature models. 

;; Definition of the syntax for a feature model in lwb

;; A feature is denoted by a Clojure symbol, e.g. elevator
;;
;; There are reserved keywords that era used in the head position of lists in the notation
;; of a feature model in lwb. They are:
;;
;; fm (feature model) 
;; fm is followed by the name of the root feature as well as feature definitions ans cross tree
;; constraints
;;
;; ft (feature)
;; ft is followed by the name of the feature together with all its groups of subfeatures
;;
;; These groups are:
;; man (mandatory) - a group of mandatory subfeatures
;; opt (optional) - a group of optional subfeatures
;; some-of - a group of subfeatures where at least one is mandatory for a valid configuration
;; one-of - a group of subfeatures where exactly one appears in a valid configuration
;;
;; ctc (cross tree constraint)
;; ctc is followed by a propositional formula that defines an integrity condition on the
;; features.

;; The example of the feature model for the elevator:

(def elevator-model
  '(fm elevator
    (ft elevator (man behavior) (opt voice-output call-buttons security safety))
    (ft behavior (man modes) (opt service priorities))
    (ft call-buttons (one-of directed-call undirected-call))
    (ft security (man permission))
    (ft safety (opt overloaded))
    (ft modes (one-of sabbath fifo shortest-path))
    (ft priorities (some-of rush-hour floor-priority person-priority))
    (ft permission (some-of floor-permission permission-control))
    (ctc (or call-buttons sabbath))
    (ctc (impl directed-call shortest-path))
    (ctc (impl undirected-call (or fifo shortest-path)))))

;; Functions and Macro for generating the propositional formula for the model

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

(comment
  (eval elevator-model)
  )

;; Using the formula

(comment
  (def elevator-phi (eval elevator-model))
  elevator-phi
  
  (sat elevator-phi)
  ; => 
  {rush-hour false,
   sabbath false,
   service false,
   floor-priority false,
   permission true,
   priorities true,
   fifo false,
   shortest-path true,
   undirected-call false,
   security true,
   modes true,
   behavior true,
   directed-call true,
   floor-permission false,
   voice-output false,
   safety false,
   call-buttons true,
   elevator true,
   person-priority true,
   permission-control true,
   overloaded false}
  )

;; One more example: the Graph Library
;; from Sven Apel, Don Batory, Christian Kästner, Gunter Saake: Feature-oriented
;; Software Product Lines

(def gl 
  '(fm graph-library
       (ft graph-library (man edge-type) (opt search weighted algorithm))
       (ft edge-type (one-of directed undirected))
       (ft search (one-of bfs dfs))
       (ft algorithm (some-of cycle shortest-path mst transpose))
       (ft mst (one-of prim kruskal))
       (ctc (impl mst (and undirected weighted)))
       (ctc (impl cycle directed))))
(comment
  (def gl-phi (eval gl))
  gl-phi
  ;; Compare this with the formula in the book of Apel et al. p.34
  (sat gl-phi)
  ; => 
  {directed false,
   prim true,
   kruskal false,
   cycle false,
   algorithm true,
   shortest-path false,
   edge-type true,
   transpose false,
   undirected true,
   search true,
   bfs true,
   graph-library true,
   dfs false,
   mst true,
   weighted true}

  (eval-phi gl-phi
            '{directed false,
             prim true,
             kruskal false,
             cycle false,
             algorithm true,
             shortest-path false,
             edge-type true,
             transpose false,
             undirected true,
             search true,
             bfs true,
             graph-library true,
             dfs false,
             mst true,
             weighted true})
  ; => true

  (eval-phi gl-phi
            '{directed false,
              prim true,
              kruskal false,
              cycle true,     ;; made true, contradicts ctc 
              algorithm true,
              shortest-path false,
              edge-type true,
              transpose false,
              undirected true,
              search true,
              bfs true,
              graph-library true,
              dfs false,
              mst true,
              weighted true})
  ; => false
  )
