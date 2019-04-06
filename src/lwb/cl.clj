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
;;    A sterm is a symbol i.e. a variable or a combinator or a nested list of applications.

(defn combinator?
  "Is `symbol` a symbol for a combinator?
   Caveat: That does not imply the the combinator is actually defined."
  [symb]
  (s/valid? :lwb.cl.spec/combinator symb))

(defn variable?
  "Is `symbol` a symbol for a variable?."
  [symb]
  (s/valid? :lwb.cl.spec/variable symb))

(defn wff?
  "Is `term` a well formed term of combinatory logic?
   `(wff? term)` returns true or false.
   `(wff? phi :exception-if-not)` returns true or throws an exception describing the error in `phi`."
  ([term]
   (wff? term :bool))
  ([term mode]
   (let [result (s/valid? :lwb.cl.spec/term term)]
     (or result (if (= mode :exception-if-not) (throw (Exception. ^String (s/explain-str :lwb.cl.spec/term term))) result)))))

(defn variables 
  "Set of variables of `term`."
  [term]
  (impl/vars term))

(defn combinators 
  "Set of combinators of `term`."
  [term]
  (impl/combs term))

(defn vfree?
  "Does `sterm` not have `var` as a variable?"
  [var sterm]
  (if (symbol? sterm)
    (not= var sterm)
    (every? #(not= var %) (variables sterm))))

;; Handling of parentheses -------------------------------------------

(defn max-parens
  "Adds parentheses according to left associative binding of application"
  [term]
  (let [seq (impl/max-parens-seq (seq term))]
    (if (= 1 (count seq))
      (vec seq)
      (vec (list seq)))))

(defn min-parens
  "Removes unnecessary parenthesis according to left associative binding of application."
  [term]
  (-> term
      seq
      impl/min-parens-seq
      impl/rm-outer-parens
      vec))

;; Substitution of variables in terms ----------------------------------

(defn subst
  "Substitution of variable `var` in `term` by `st`."
  [term var st]
  (let [st' (if (= 1 (count st)) (first st) (seq st))]
    (walk/postwalk
      #(if (= var %) st' %)
      term)))

;; Subterms of a term --------------------------------------------------

(defn subterms
  "A sequence of a subterms of the given `term`."
  [term]
  (->> term
       (max-parens)
       seq
       (tree-seq seq? identity)
       (map vector)
       (map min-parens)
       distinct))
  
;; Definition of combinators ------------------------------------------------

(defn def-combinator
  "Defines and registers combinator in the global storage.
   Combinators are identified by a keyword build from the name of the combinator."
  [redex effect]
  (let [r (min-parens redex)
        e (min-parens effect)]
    (swap! impl/combinator-store merge (impl/make-comb r e))
    true))

(defn comb-defined?
  "Is combinator with key `comb-key` defined?"
  [comb-key]
  (seq (filter #(= comb-key %) (keys @impl/combinator-store))))

(defn show-combinator
  "Shows combinator with the given `comb-key`."
  [comb-key]
  (printer/print-comb comb-key @impl/combinator-store))

(defn show-combinators
  "Shows the currently defined combinators."
  []
  (printer/print-combs (sort @impl/combinator-store)))

(defn reset-combinators
  "Undefines all combinators."
  []
  (reset! impl/combinator-store {}))

;; One-step reduction and expansion of combinator -----------------------------

(defn- one-step-app
  "Wrapper for `reduce` and `expand`."
  [term comb-key i mode]
  (if (comb-defined? comb-key)
    (let [t (max-parens term)
          f (get-in @impl/combinator-store [comb-key :logic-rel])]
      (min-parens (impl/apply* t f i mode)))
    (throw (AssertionError. (str "Combinator " comb-key " not defined.")))))

(defn one-step-red
  "Reduces the given `term` by applying the combinator `comb-key` at position `i` (default 1)"
  ([term comb-key]
   (one-step-red term comb-key 1))
  ([term comb-key i]
   (one-step-app term comb-key i :red)))

(defn one-step-exp
  "Expands the given `term` by reversing the combinator `comb-key` at position `i` (default 1)"
  ([term comb-key]
   (one-step-exp term comb-key 1))
  ([term comb-key i]
   (one-step-app term comb-key i :exp)))

;; Multi-step reduction ------------------------------------------------------------

(defn weak-reduce'
  "Reduce the `term` using the set `combs` of combinators and the number `limit` of one-step reductions.
   Returns a map with :cycle false/true, :overrun false/true and :steps with all intermediate terms."
  [term combs limit]
  (loop [current-term term
         result {:steps [term] :cycle false :overrun false}
         counter 0]
    (let [found (filter #(not= current-term %) (for [s combs] (one-step-red current-term s)))]
      (cond (empty? found) result
            ;; cycle detection
            (some #(= (first found) %) (:steps result)) (update (assoc result :cycle true) :steps conj (first found))
            ;; overrun
            (> counter limit) (update (assoc result :overrun true) :steps conj (first found))
            :else (recur (first found) (update result :steps conj (first found)) (inc counter))))))

(defn weak-reduce
  "Reduce the `term` with the 'limit' of one-step reductions.
  The metadata of the result indicate cycle detection or overrun of the limit of steps."
  ([term]
   (weak-reduce term 100))
  ([term limit]
   (let [result (weak-reduce' term (impl/combs-keys term) limit)]
     (with-meta (last (:steps result)) {:cycle (:cycle result) :overrun (:overrun result)}))))

; Bracket abstraction ----------------------------------------------------------------

;; See Jonathan P. Seldin: The search for a reduction in combinatory logic equivalent to λβ-reduction,
;; in: Theoretical Computer Science 412 (2011), 4905-4918
;; and
;; Haskell B. Curry and Robert Feys: Combinatory Logic I, Amsterdam 1958

(defn curry-naive
  "Curry's algorithm (fab), labeled Primitive Abstraction in the paper of Seldin 2011."
  [var sterm] ;; sterm is a symbol or a list of (possibly nested) unary applications.
  (cond (= var sterm) 'I
        (symbol? sterm) (list 'K sterm)
        (list? sterm) (list (list 'S (curry-naive var (first sterm))) (curry-naive var (second sterm)))))

(defn curry-weak
  "Curry's algorithm (abf), labeled Weak Abstraction in the paper of Seldin 2011."
  [var sterm] ;; sterm is a symbol or a list of (possibly nested) unary applications.
  (cond (= var sterm) 'I
        (symbol? sterm) (list 'K sterm)
        (vfree? var sterm) (list 'K sterm)
        (list? sterm) (list (list 'S (curry-weak var (first sterm))) (curry-weak var (second sterm)))))

(defn curry-eta
  "Curry's algorithm (abcf), labeled η Abstraction in the paper of Seldin 2011."
  [var sterm] ;; sterm is a symbol or a list of (possibly nested) unary applications.
  (cond (= var sterm) 'I
        (symbol? sterm) (list 'K sterm)
        (vfree? var sterm) (list 'K sterm)
        (and (list? sterm) (= var (second sterm)) (vfree? var (first sterm))) (first sterm)
        (list? sterm) (list (list 'S (curry-eta var (first sterm))) (curry-eta var (second sterm)))))

(defn abstract
  "Bracket abstraction for the vector `var-vec` of variables for `term` performed by `algo`,
   which defaults to `curry-eta`.
   The abstractions for the variables are from right to left."
  ([var-vec term]
   (abstract var-vec term curry-eta))
  ([var-vec term algo]
   (let [sterm (first (max-parens term))
         result (loop [args (seq (reverse var-vec))
                       result sterm]
                  (if (empty? args)
                    result
                    (recur (rest args) (algo (first args) result))))]
     (min-parens (list result)))))

;; Collections of combinators -------------------------------------------------

(defn def-combinators-ski
  "Defines the combinators `:S, :K, :I`."
  []
  (def-combinator '[S x y z] '[x z (y z)])
  (def-combinator '[K x y] '[x])
  (def-combinator '[I x] '[x]))

(defn def-combinatory-birds
  "Defines a huge collection of combinators, borrowed from Raymond Smullyan's To Mock a Mocking Bird."
  []
  ; Bluebird B := [S (K S) K]
  (def-combinator '[B a b c] '[a (b c)])
  ; Blackbird B1 := [B B B]
  (def-combinator '[B1 a b c d] '[a (b c d)])
  ; Bunting B2 := [B (B B B) B]
  (def-combinator '[B2 a b c d e] '[a (b c d e)])
  ; Becard B3 := [B (B B) B]
  (def-combinator '[B3 a b c d] '[a (b (c d))])
  ; Cardinal C := [S (B B S) (K K)]
  (def-combinator '[C a b c] '[a c b])
  ; Dove D := [B B]
  (def-combinator '[D a b c d] '[a b (c d)])
  ; Dickcissel D1 := [B (B B)]
  (def-combinator '[D1 a b c d e] '[a b c (d e)])
  ; Dovekies D2 := [B B (B B)]
  (def-combinator '[D2 a b c d e] '[a (b c) (d e)])
  ; Eagle E := [B (B B B)]
  (def-combinator '[E a b c d e] '[a b (c d e)])
  ; Bald Eagle E' =: [B (B B B) (B (B B B))]
  (def-combinator '[E' a b c d e f g] '[a (b c d) (e f g)])
  ; Finch F := [E T T E T]
  (def-combinator '[F a b c] '[c b a])
  ; Goldfinch G := [B B C]
  (def-combinator '[G a b c d] '[a d (b c)])
  ; Hummingbird H := [B W (B C)]
  (def-combinator '[H a b c] '[a b c b])
  ; Identity Bird, aka Idiot I := [S K K]
  (def-combinator '[I a] '[a])
  ; Jay J := [B (B C) (W (B C (B (B B B))))]
  (def-combinator '[J a b c d] '[a b (a d c)])
  ; Kestrel K
  (def-combinator '[K a b] '[a])
  ; Lark L := [C B M]
  (def-combinator '[L a b] '[a (b b)])
  ; Mockingbird M := [S I I]
  (def-combinator '[M a] '[a a])
  ; Double Mockingbird M2 := [B M]
  (def-combinator '[M2 a b] '[a b (a b)])
  ; Owl O := [S I]
  (def-combinator '[O a b] '[b (a b)])
  ; Queer Bird Q := [C B] aka B'
  (def-combinator '[Q a b c] '[b (a c)])
  ; Quixotic Bird Q1 := [B C B]
  (def-combinator '[Q1 a b c] '[a (c b)])
  ; Quizzical Bird Q2 := [C (B C B)]
  (def-combinator '[Q2 a b c] '[b (c a)])
  ; Quirky Bird Q3 := [B T] 
  (def-combinator '[Q3 a b c] '[c (a b)])
  ; Quacky Bird Q4 := [F* B]
  (def-combinator '[Q4 a b c] '[c (b a)])
  ; Robin R := [B B T]
  (def-combinator '[R a b c] '[b c a])
  ; Starling  S
  (def-combinator '[S a b c] '[a c (b c)])
  ; Thrush T := [C I]
  (def-combinator '[T a b] '[b a])
  ; Turing U := [L O]
  (def-combinator '[U a b] '[b (a a b)])
  ; Vireo aka Pairing V := [B C T]
  (def-combinator '[V a b c] '[c a b])
  ; Warbler W := [C (B M R)]
  (def-combinator '[W a b] '[a b b])
  ; Converse Warbler W1 := [C W]
  (def-combinator '[W1 a b] '[b a a])
  ; Why Bird aka Sage Bird Y := [S L L]
  (def-combinator '[Y a] '[a (Y a)])
  ; Identity Bird Once Removed := [S (S K)]
  (def-combinator '[I* a b] '[a b])
  ; Warbler Once Removed W* := [B W]
  (def-combinator '[W* a b c] '[a b c c])
  ; Cardinal Once Removed := [B C]
  (def-combinator '[C* a b c d] '[a b d c])
  ; Robin Once Removed R* := [C* C*]
  (def-combinator '[R* a b c d] '[a c d b])
  ; Finch Once Removed F* := [B C* R*]
  (def-combinator '[F* a b c d] '[a d c b])
  ; Vireo Once Removed V* := [C* F*]
  (def-combinator '[V* a b c d] '[a c b d])
  ; Identity Bird Twice Removed I**
  (def-combinator '[I** a b c] '[a b c])
  ; Warbler Twice Removed W** := [B (B W)]
  (def-combinator '[W** a b c d] '[a b c d d])
  ; Cardinale Twice Removed C** := [B C*]
  (def-combinator '[C** a b c d e] '[a b c e d])
  ; Robin Twice Removed R** := B R*]
  (def-combinator '[R** a b c d e] '[a b d e c])
  ; Finch Twice Removed
  (def-combinator '[F** a b c d e] '[a b e d c])
  ; Vireo Twice Removed
  (def-combinator '[V** a b c d e] '[a b e c d])
  ; Kite KI := [K I]
  (def-combinator '[KI a b] '[b])
  ; Omega Omega := [M M] TODO: strange bird
  (def-combinator '[KM a b] '[b b])
  ; Crossed Konstant Mocker CKM := [C (K M)]
  (def-combinator '[CKM a b] '[a a])
  ; Phi
  (def-combinator '[Phi a b c d] '[a (b d) (c d)])
  ; Psi
  (def-combinator '[Psi a b c d] '[a (b c) (b d)])
  ; Gamma
  (def-combinator '[Gamma a b c d e] '[b (c d) (a b d e)]))
  

(comment
  (def-combinatory-birds)
  (show-combinators))



