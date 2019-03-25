; lwb Logic WorkBench -- Combinatory logic

; Copyright (c) 2019  Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.cl
  (:refer-clojure :exclude [==])
  (:require [lwb.cl.impl :as impl]
            [lwb.cl.printer :as printer]
            [lwb.cl.spec :as spec]
            [clojure.core.logic :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.walk :as walk]))

;; Syntax -------------------------------------------------------------------

;; Representation of combinatory logic in Clojure:
;; 1. Combinators are represented by Clojure symbols whose name begins with an upper case letter
;; 2. Variables are represented by Clojure symbols whose name begins with a lower case letter
;; 3. Application is represented by a Clojure list
;; 4. A term is represented as a Clojure vector. It may have all parentheses or they are omitted 
;;    from the left-associated application. The outermost parentheses may be omitted too.

(defn combinator?
  "Is `symbol` a symbol for a combinator?
   Caveat: That does not imply the the combinator is actually defined."
  [symb]
  (s/valid? :lwb.cl.spec/combinator symb))

(comment
  (combinator? 'S)
  (combinator? 'B')
  (combinator? 'x)
  (combinator? '(S x)))

(defn variable?
  "Is `symbol` a symbol for a variable?."
  [symb]
  (s/valid? :lwb.cl.spec/variable symb))

(comment
  (variable? 'S)
  (variable? 'x)
  (variable? '(S x)))

(defn wff?
  "Is `term` a well formed term of combinatory logic?
   `(wff? term)` returns true or false.
   `(wff? phi :exception-if-not)` returns true or throws an exception describing the error in `phi`."
  ([term]
   (wff? term :bool))
  ([term mode]
   (let [result (s/valid? :lwb.cl.spec/term term)]
     (or result (if (= mode :exception-if-not) (throw (Exception. ^String (s/explain-str :lwb.cl.spec/term term))) result)))))

(comment
  (wff? '[S x y z])
  (wff? '[(S) x y z])
  (wff? '[(S) x y z] :exception-if-not)
  (wff? '[(S x) y z]))

(comment
  (s/describe :lwb.cl.spec/simpl-expr)
  (s/describe :lwb.cl.spec/compl-expr)
  (s/describe :lwb.cl.spec/term))

;; Handling of parentheses -------------------------------------------

(defn max-parens
  "Adds parentheses according to left associative binding of application"
  [term]
  (let [seq (impl/max-parens-seq (seq term))]
    (if (= 1 (count seq))
      (vec seq)
      (vec (list seq)))))

(comment
  ; Bimbó Example 1.1.3 
  (max-parens '[(B x y) z])
  (max-parens '[B W B x (B W B x)])
  (max-parens '[B x y z])

  ; Bimbó Exercise 1.1.5 
  (max-parens '[K x (W y z) y])
  (max-parens '[x x x])
  (max-parens '[(S I) y B])
  (max-parens '[x_14 x_152])
  (max-parens '[M_1 (M_2 (M_3 x y y) z)])

  (max-parens '[(y z) (I I) x])
  (max-parens '[y z (I I) x])

  (max-parens '[(M x) ((B y) (W z))])
  (max-parens '[M x ((B y) (W z))])
  (max-parens '[M x (B y (W z))])

  (max-parens '[(W (I x (I y)) z)])
  (max-parens '[W (I x (I y)) z]))

(defn min-parens
  [term]
  (-> term
      seq
      impl/min-parens-seq
      impl/rm-outer-parens
      vec))

(comment
  ; Bimbó Example 1.1.3 
  (min-parens '[(((B x) y) z)])
  (min-parens '[((((B W) B) x) (((B W) B) x))])

  ; Bimbó Exercise 1.1.5 
  (min-parens '[(((y z) (I I)) x)])
  (min-parens '[((M x) ((B y) (W z)))])
  (min-parens '[(W (I x_145 (I x_72)) x_58)])
  (min-parens '[(((S K) K) x)])
  (min-parens '[(((S M) M) (I (N P)))]))

;; Substitution of variables in terms

(defn subst
  "Substitution of variable `var` in `term` by `st`."
  [term var st]
  (let [st' (if (= 1 (count st)) (first st) (seq st))]
    (walk/postwalk
      #(if (= var %) st' %)
      term)))

(comment
  ; Bimbó Example 1.2.3
  (subst (subst (subst '[S x y z] 'x '[I]) 'y '[y y]) 'z '[J])
  ; be cautious with simultaneous substitutions
  (subst (subst (subst '[J x y z v] 'x '[y' y']) 'y '[W]) 'y' '[y])
  ; but
  (subst (subst '[J x y z v] 'x '[y y]) 'y '[W])

  ; Bimbó Exercise 1.2.1
  (subst '[I x (W x)] 'x '[z])
  (subst '[S (x x) (y y) (x y)] 'z '[x (x y)])
  (subst (subst (subst '[x_3 x_1 (x_3 x_4 x_3)] 'x_1 '[M]) 'x_3 '[B]) 'x_4 '[W])
  (subst (subst (subst '[S x y z] 'x '[K]) 'y '[K]) 'z '[x])
  (subst '[C x y M] 'y '[P P N]))



; Bimbó Example 1.2.3
(subst (subst (subst '[S x y z] 'x '[I]) 'y '[y y]) 'z '[J])
; be cautious with simultaneous substitutions
(subst (subst (subst '[J x y z v] 'x '[y' y']) 'y '[W]) 'y' '[y])
; but
(subst (subst '[J x y z v] 'x '[y y]) 'y '[W])

; Bimbó Exercise 1.2.1
(subst '[I x (W x)] 'x '[z])
(subst '[S (x x) (y y) (x y)] 'z '[x (x y)])
(subst (subst (subst '[x_3 x_1 (x_3 x_4 x_3)] 'x_1 '[M]) 'x_3 '[B]) 'x_4 '[W])
(subst (subst (subst '[S x y z] 'x '[K]) 'y '[K]) 'z '[x])
(subst '[C x y M] 'y '[P P N])

;; Definition of combinators ------------------------------------------------

(defn def-combinator
  "Defines and registers combinator in the global storage.
   Combinators are identified by a keyword, 
   usually : followed by the name of the combinator."
  [key redex effect]
  (let [r (min-parens redex)
        e (min-parens effect)]
    (swap! impl/combs merge (impl/make-comb key r e))
    true))

(defn comb-defined?
  "Is combinator with key `comb-key` defined?"
  [comb-key]
  (seq (filter #(= comb-key %) (keys @impl/combs))))

(defn show-combinators
  "Shows the currently defined combinators."
  []
  (printer/print-combs (sort @impl/combs)))

(defn reset-combinators
  "Undefines all combinators."
  []
  (reset! impl/combs {}))

(comment
  (def-combinator :S '[S x y z] '[x z (y z)])
  (def-combinator :K '[K x y] '[x])

  (show-combinators)

  (comb-defined? :S)
  (comb-defined? :T)

  (reset-combinators))

;; One-step reduction and expansion of combinator -----------------------------

(defn app
  "Wrapper for `reduce` and `expand`."
  [term key i mode]
  (if (comb-defined? key)
    (let [t (max-parens term)
          f (get-in @impl/combs [key :logic-rel])]
      (min-parens (impl/apply* t f i mode)))
    (throw (AssertionError. (str "Combinator " key " not defined.")))))

(defn cred
  "Reduces the given `term` by applying the combinator `key` at position `i` (default 1)"
  ([term key]
   (cred term key 1))
  ([term key i]
   (app term key i :red)))

(defn cexp
  "Expands the given `term` by reversing the combinator `key` at position `i` (default 1)"
  ([term key]
   (cexp term key 1))
  ([term key i]
   (app term key i :exp)))

(comment
  (def-combinator :S '[S x y z] '[x z (y z)])
  (def-combinator :K '[K x y] '[x])

  (cred '[S x y z] :S)
  (cexp '[a c (b c)] :S)
  (cred '[K x y] :K)
  (cexp '[x] :K)
  (cexp '[x] :T))
  

;; Multi-step reduction ------------------------------------------------------------

(defn def-combinators-ski
  "Defines the combinators `:S, :K, :I`."
  []
  (def-combinator :S '[S x y z] '[x z (y z)])
  (def-combinator :K '[K x y] '[x])
  (def-combinator :I '[I x] '[x]))

;; TODO
(defn creduce'
  "Reduce the `term` using the set `combs` of combinators (defaults to `#{:S :K :I}`.
   Returns a map with :cycle true/false and :steps with all intermediate terms."
  [term combs]
  (loop [current-term term
         result {:cycle false :steps [term]}]
    (let [found (filter #(not= current-term %) (for [s combs] (cred current-term s)))]
      (cond (empty? found) result
            ;; cycle detection
            (some #(= (first found) %) (:steps result)) (update (assoc result :cycle true) :steps conj (first found))
            :else (recur (first found) (update result :steps conj (first found)))))))
    

(defn creduce
  "Reduce the `term` using the set `combs` of combinators (defaults to `#{:S :K :I}`."
  ([term]
   (def-combinators-ski)
   (creduce term #{:S :K :I}))
  ([term combs]
   (last (:steps (creduce' term combs)))))

  

(comment
  (creduce '[S (K I) a b c])
  (creduce '[S I I (S I I)]))
  

;; A big collection of combinators -------------------------------------------------

(defn def-combinatory-birds
  "Defines a huge collection of combinators, borrowed from Raymond Smullyan's To Mock a Mocking Bird."
  []
  ; Bluebird B := [S (K S) K]
  (def-combinator :B '[B a b c] '[a (b c)])
  ; Blackbird B1 := [B B B]
  (def-combinator :B1 '[B1 a b c d] '[a (b c d)])
  ; Bunting B2 := [B (B B B) B]
  (def-combinator :B2 '[B2 a b c d e] '[a (b c d e)])
  ; Becard B3 := [B (B B) B]
  (def-combinator :B3 '[B3 a b c d] '[a (b (c d))])
  ; Cardinal C := [S (B B S) (K K)]
  (def-combinator :C '[C a b c] '[a c b])
  ; Dove D := [B B]
  (def-combinator :D '[D a b c d] '[a b (c d)])
  ; Dickcissel D1 := [B (B B)]
  (def-combinator :D1 '[D1 a b c d e] '[a b c (d e)])
  ; Dovekies D2 := [B B (B B)]
  (def-combinator :D2 '[D2 a b c d e] '[a (b c) (d e)])
  ; Eagle E := [B (B B B)]
  (def-combinator :E '[E a b c d e] '[a b (c d e)])
  ; Bald Eagle E1 =: [B (B B B) (B (B B B))]
  (def-combinator :E1 '[E1 a b c d e f g] '[a (b c d) (e f g)])
  ; Finch F := [E T T E T]
  (def-combinator :F '[F a b c] '[c b a])
  ; Goldfinch G := [B B C]
  (def-combinator :G '[G a b c d] '[a d (b c)])
  ; Hummingbird H := [B W (B C)]
  (def-combinator :H '[H a b c] '[a b c b])
  ; Identity Bird, aka Idiot I := [S K K]
  (def-combinator :I '[I a] '[a])
  ; Jay J := [B (B C) (W (B C (B (B B B))))]
  (def-combinator :J '[J a b c d] '[a b (a d c)])
  ; Kestrel K
  (def-combinator :K '[K a b] '[a])
  ; Lark L := [C B M]
  (def-combinator :L '[L a b] '[a (b b)])
  ; Mockingbird M := [S I I]
  (def-combinator :M '[M a] '[a a])
  ; Double Mockingbird M2 := [B M]
  (def-combinator :M2 '[M2 a b] '[a b (a b)])
  ; Owl O := [S I]
  (def-combinator :O '[O a b] '[b (a b)])
  ; Queer Bird Q := [C B] aka B'
  (def-combinator :Q '[Q a b c] '[b (a c)])
  ; Quixotic Bird Q1 := [B C B]
  (def-combinator :Q1 '[Q1 a b c] '[a (c b)])
  ; Quizzical Bird Q2 := [C (B C B)]
  (def-combinator :Q2 '[Q2 a b c] '[b (c a)])
  ; Quirky Bird Q3 := [B T] 
  (def-combinator :Q3 '[Q3 a b c] '[c (a b)])
  ; Quacky Bird Q4 := [F* B]
  (def-combinator :Q4 '[Q4 a b c] '[c (b a)])
  ; Robin R := [B B T]
  (def-combinator :R '[R a b c] '[b c a])
  ; Starling  S
  (def-combinator :S '[S a b c] '[a c (b c)])
  ; Thrush T := [C I]
  (def-combinator :T '[T a b] '[b a])
  ; Turing U := [L O]
  (def-combinator :U '[U a b] '[b (a a b)])
  ; Vireo aka Pairing V := [B C T]
  (def-combinator :V '[V a b c] '[c a b])
  ; Warbler W := [C (B M R)]
  (def-combinator :W '[W a b] '[a b b])
  ; Converse Warbler W1 := [C W]
  (def-combinator :W1 '[W1 a b] '[b a a])
  ; Why Bird aka Sage Bird Y := [S L L]
  (def-combinator :Y '[Y a] '[a (Y a)])
  ; Identity Bird Once Removed := [S (S K)]
  (def-combinator :I* '[I* a b] '[a b])
  ; Warbler Once Removed W* := [B W]
  (def-combinator :W* '[W* a b c] '[a b c c])
  ; Cardinal Once Removed := [B C]
  (def-combinator :C* '[C* a b c d] '[a b d c])
  ; Robin Once Removed R* := [C* C*]
  (def-combinator :R* '[R* a b c d] '[a c d b])
  ; Finch Once Removed F* := [B C* R*]
  (def-combinator :F* '[F* a b c d] '[a d c b])
  ; Vireo Once Removed V* := [C* F*]
  (def-combinator :V* '[V* a b c d] '[a c b d])
  ; Identity Bird Twice Removed I**
  (def-combinator :I** '[I** a b c] '[a b c])
  ; Warbler Twice Removed W** := [B (B W)]
  (def-combinator :W** '[W** a b c d] '[a b c d d])
  ; Cardinale Twice Removed C** := [B C*]
  (def-combinator :C** '[C** a b c d e] '[a b c e d])
  ; Robin Twice Removed R** := B R*]
  (def-combinator :R** '[R** a b c d e] '[a b d e c])
  ; Finch Twice Removed
  (def-combinator :F** '[F** a b c d e] '[a b e d c])
  ; Vireo Twice Removed
  (def-combinator :V** '[V** a b c d e] '[a b e c d])
  ; Kite KI := [K I]
  (def-combinator :KI '[KI a b] '[b])
  ; Omega Omega := [M M] TODO: strange bird
  (def-combinator :KM '[KM a b] '[b b])
  ; Crossed Konstant Mocker CKM := [C (K M)]
  (def-combinator :CKM '[CKM a b] '[a a])
  ; Phi
  (def-combinator :Phi '[Phi a b c d] '[a (b d) (c d)])
  ; Psi
  (def-combinator :Psi '[Psi a b c d] '[a (b c) (b d)])
  ; Gamma
  (def-combinator :Gamma '[Gamma a b c d e] '[b (c d) (a b d e)]))
  

(comment
  (def-combinatory-birds)
  (show-combinators))

; Bracket abstraction ----------------------------------------------------------------

(defn curry
  "Classical algorithm for bracket abstraction for S K I.
  Pre: terms have max-parens."
  [variable subterm]
  (cond
    (= variable subterm) 'I
    (symbol? subterm) (list 'K subterm)
    (list? subterm) (list* 'S subterm)
    :else subterm))

;; The example of function curry shows how to define other
;; algorithms for bracket abstraction

(defn abstract
  ([variable term]
   (abstract variable term curry))
  ([variable term algo]
   (let [mterm (max-parens term)]
     (min-parens
       (walk/postwalk
         (partial algo variable)
         mterm)))))

(comment
  (abstract 'x '[a])
  (abstract 'x '[x])
  (abstract 'x '[M x x])
  (abstract 'a '[a (b c)])
  (abstract 'b (abstract 'a '[a (b c)]))
  (abstract 'c (abstract 'b (abstract 'a '[a (b c)])))
  ;; wird unheimlich lang!!
  (def X1 (abstract 'a (abstract 'b (abstract 'c '[a (b c)]))))
  (conj X1 'a 'b 'c)
  (creduce (conj X1 'a 'b 'c)))
  


