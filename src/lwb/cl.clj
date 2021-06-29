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
            [lwb.nd.error :refer :all]
            [clojure.core.logic :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.walk :as walk]
            [clojure.java.browse :as browse])
  (:import (java.text Collator)
           (java.util Locale)))

(defn man
  "Manual"
  []
  (browse/browse-url "https://github.com/esb-lwb/lwb/wiki/cl"))

; -----------------------------------------------------------------------------
;; # Syntax 


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
     (or result (if (= mode :exception-if-not) 
                  (throw (Exception. ^String (s/explain-str :lwb.cl.spec/term term))) result)))))

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

(defn size
  "Size of a term, i.e. the number of combinators and variables"
  [term]
  (count (flatten term)))

; -----------------------------------------------------------------------------
;; # Handling of parentheses 

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

; -----------------------------------------------------------------------------
;; # Subterms and substitution 

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

(defn subst
  "Substitution of subterm `subterm` in `term` by `subterm'`."
  [term subterm subterm']
  (let [t   (first (max-parens term))
        st  (first (max-parens subterm))
        st' (first (max-parens subterm'))]
    (min-parens (vector (walk/postwalk
                  #(if (= st %) st' %)
                  t)))))
(subst '[x] '[t] '[s])

; -----------------------------------------------------------------------------
;; # Concatenation of terms 

(defn comb-concat
  "Concatenation of the given terms."
  [& terms]
  (min-parens (vec (reduce concat (map max-parens terms)))))

; -----------------------------------------------------------------------------
;; # Definition of combinators 

(defn def-combinator
  "Defines and registers combinator in the global storage.
   Combinators are identified by a keyword build from the name of the combinator."
  ([redex effect]
   (def-combinator redex effect nil))
  ([redex effect nickname]
  (let [r (min-parens redex)
        e (min-parens effect)]
    (swap! impl/combinator-store merge (impl/make-comb r e nickname))
    true)))

(defn comb-defined?
  "Is combinator with key `comb-key` defined?"
  [comb-key]
  (not (empty? (filter #(= comb-key %) (keys @impl/combinator-store)))))


(defn show-combinator
  "Shows combinator with the given `comb-key`."
  [comb-key]
  (printer/print-comb comb-key @impl/combinator-store))

(defn show-combinators
  "Shows the currently defined combinators."
  []
  (let [^Collator collator (Collator/getInstance Locale/US)]
    (printer/print-combs (sort-by #(name (key %)) collator @impl/combinator-store))))

(defn reset-combinators
  "Undefines all combinators."
  []
  (reset! impl/combinator-store {}))

; -----------------------------------------------------------------------------
;; # One-step reduction and expansion of combinator 

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

; -----------------------------------------------------------------------------
;; # Weak reduction 

(defmacro with-timeout
  [msec & body]
  `(let [f# (future (do ~@body))
         v# (gensym)
         result# (deref f# ~msec v#)]
     (if (= v# result#)
       (do
         (future-cancel f#)
         (throw (ex-error (str "Timeout after " ~msec " msecs."))))
       result#)))

(defn- weak-reduce'
  [term limit cycle trace]
  (let [sterm (first (max-parens term))]
    (if trace (println (str 0 ": " (min-parens term))))
     (let [result (impl/weak-reduce sterm 1 limit cycle trace)
            new-sterm (:reduced result)]
          (with-meta (min-parens [new-sterm]) result))))

(defn weak-reduce
  "Weak reduction of a term.
   Options {:timeout :limit :cycle :trace}
   :timeout - stop reduction after <x> msecs, x = 0 means don't stop
              default: 0
   :limit   - stop after <x> steps, x = 0 means no limit
              default: 100
   :cycle   - detect cycles?
              default: false
   :trace   - trace steps
              default: false.
   returns the result of the reduction together with a meta data map
   meta data are
   :reduced  - reduced term
   :no-steps - number of steps
   :cycle    - cycle detected?
   :steps    - steps performed when cycle detected
   :overrun  - run over limit?"                             
  ([term]
   (weak-reduce term {}))
  ([term {:keys [timeout limit cycle trace]
          :or   {timeout 0 limit 100 cycle false trace false}}]
   (if (pos? timeout)
     (with-timeout timeout (weak-reduce' term limit cycle trace))
     (weak-reduce' term limit cycle trace))))

; -----------------------------------------------------------------------------
;; # Bracket abstraction 

;; See 
;; 
;; - Jonathan P. Seldin: The search for a reduction in combinatory logic equivalent to λβ-reduction,
;; in: Theoretical Computer Science 412 (2011), 4905-4918
;; - Haskell B. Curry and Robert Feys: Combinatory Logic I, Amsterdam 1958

(defn curry-naive
  "Curry's algorithm (fab), labeled Primitive Abstraction in the paper of Seldin 2011."
  [var sterm]                                               
  ;; sterm is a symbol or a list of (possibly nested) unary applications.
  (cond (= var sterm) 'I
        (symbol? sterm) (list 'K sterm)
        (list? sterm) (list (list 'S (curry-naive var (first sterm))) (curry-naive var (second sterm)))))

(defn curry-weak
  "Curry's algorithm (abf), labeled Weak Abstraction in the paper of Seldin 2011."
  [var sterm]                                               
  ;; sterm is a symbol or a list of (possibly nested) unary applications.
  (cond (= var sterm) 'I
        (symbol? sterm) (list 'K sterm)
        (vfree? var sterm) (list 'K sterm)
        (list? sterm) (list (list 'S (curry-weak var (first sterm))) (curry-weak var (second sterm)))))

(defn curry-eta
  "Curry's algorithm (abcf), labeled η Abstraction in the paper of Seldin 2011."
  [var sterm]                                               
  ;; sterm is a symbol or a list of (possibly nested) unary applications.
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

; -----------------------------------------------------------------------------
;; # Collections of combinators 

(defn def-combinators-ski
  "Defines the combinators `:S, :K, :I`."
  []
  (def-combinator '[S x y z] '[x z (y z)])
  (def-combinator '[K x y] '[x])
  (def-combinator '[I x] '[x]))


(defn def-combinatory-birds
  "Defines a huge collection of combinators from Raymond Smullyan's To Mock a Mocking Bird."
  []
  ; Bluebird B p.95 B = [S (K S) K]
  (def-combinator '[B x y z] '[x (y z)] "Bluebird")
  ; Blackbird B1 p.97 B1 = [B B B]
  (def-combinator '[B1 x y z w] '[x (y z w)] "Blackbird")
  ; Bunting B2 p.97 B2 = [B (B B B) B]
  (def-combinator '[B2 x y z w v] '[x (y z w v)] "Bunting")
  ; Becard B3 p.98 B3 = [B (B B) B]
  (def-combinator '[B3 x y z w] '[x (y (z w))] "Becard")
  ; Cardinal C p.100 C = [S (B B S) (K K)]
  (def-combinator '[C x y z] '[x z y] "Cardinal")
  ; Cardinal Once Removed p.104 C* = [B C]
  (def-combinator '[C* x y z w] '[x y w z] "Cardinal once removed")
  ; Cardinal Twice Removed p.105  C** = [B C*]
  (def-combinator '[C** x y z1 z2 z3] '[x y z1 z3 z2] "Cardinal twice removed")
  ; Dove D p.97 D = [B B]
  (def-combinator '[D x y z w] '[x y (z w)] "Dove")
  ; Dickcissel D1 p.97 D1 = [B (B B)]
  (def-combinator '[D1 x y z w v] '[x y z (w v)] "Dickcissel")
  ; Dovekie D2 p.98 D2 = [B B (B B)]
  (def-combinator '[D2 x y z w v] '[x (y z) (w v)] "Dovekie")
  ; Eagle E p.97 E = [B (B B B)]
  (def-combinator '[E x y z w v] '[x y (z w v)] "Eagle")
  ; Bald Eagle p.98 Ê = [B (B B B) (B (B B B))]
  (def-combinator '[Ê x y1 y2 y3 z1 z2 z3] '[x (y1 y2 y3) (z1 z2 z3)] "Bald Eagle")
  ; Finch p.102  F = [E T T E T]
  (def-combinator '[F x y z] '[z y x] "Finch")
  ; Finch Once Removed p.104 F* = [B C* R*]
  (def-combinator '[F* x y z w] '[x w z y] "Finch once removed")
  ; Finch Twice Removed p.105 F** = [B F*]
  (def-combinator '[F** x y z1 z2 z3] '[x y z3 z2 z1] "Finch twice removed")
  ; Goldfinch p.107 G = [B B C]
  (def-combinator '[G x y z w] '[x w (y z)] "Goldfinch")
  ;; G1 p.124 G1 = B (B B C)
  (def-combinator '[G1 x y z w v] '[x y v (z w)] "G1")
  ;; G2 p.124 G2 = G1 (B M)
  (def-combinator '[G2 x y z w] '[x w (x w) (y z)] "G2")
  ;; Gamma p.124 Gamma = Phi (Phi (Phi B)) B
  (def-combinator '[Gamma x y z w v] '[y (z w)  (x y w v)] "Gamma")
  ; Hummingbird p.120  H = [B W (B C)]
  (def-combinator '[H x y z] '[x y z y] "Hummingbird")
  ;; H* p. 124 H* = B H
  (def-combinator '[H* x y z w] '[x y z w z] "Hummingbird once removed")
  ; Identity Bird, aka Idiot I p.78 I = [S K K]
  (def-combinator '[I x] '[x] "Identity Bird")
  ; Identity Bird Once Removed := [S (S K)]
  (def-combinator '[I* a b] '[a b] "Identity Bird Once Removed")
  ; Identity Bird Twice Removed I**
  (def-combinator '[I** a b c] '[a b c] "Identity Bird Twice Removed")
  ;; I2 p.124 I2 = B (T I) (T I)
  (def-combinator '[I2 x] '[x I I] "I2")
  ; Jaybird p.181 J = B (B C) (W (B C (B (B B B))))
  (def-combinator '[J x y z w] '[x y (x w z)] "Jaybird")
  ; J1 p.183 J1 = B J T
  (def-combinator '[J1 x y z w] '[y x (w x z)] "J1")
  ; Kestrel K p.77
  (def-combinator '[K x y] '[x] "Kestrel")
  ; Lark L p. 80 L = [C B M]
  (def-combinator '[L x y] '[x (y y)] "Lark")
  ; Mockingbird M p.73 M = [S I I]
  (def-combinator '[M x] '[x x] "Mockingbird")
  ; Double Mockingbird p.118 M2 = [B M]
  (def-combinator '[M2 x y] '[x y (x y)] "Double Mockingbird")
  ;; Owl O p.133  U = B W (C B) = S I
  (def-combinator '[O x y] '[y (x y)] "Owl")
  ;; Phoenix p. 124 Phi = B (B S) B
  (def-combinator '[Phi x y z w] '[x (y w) (z w)] "Phoenix")
  ;; Psi p.124 Psi = B H (B B (B B))
  (def-combinator '[Psi x y z w] '[x (y z) (y w)] "Psi")
  ; Queer p.105 Q = [C B] aka B'
  (def-combinator '[Q x y z] '[y (x z)] "Queer")
  ; Quixotic p.106 Q1 = [B C B]
  (def-combinator '[Q1 x y z] '[x (z y)] "Quixotic")
  ; Quizzical p.106  Q2 = [C (B C B)]
  (def-combinator '[Q2 x y z] '[y (z x)] "Quizzical")
  ; Quirky p.106 Q3 = [B T] = [V* B]
  (def-combinator '[Q3 x y z] '[z (x y)] "Quirky")
  ; Quacky p.107 Q4 = [F* B]
  (def-combinator '[Q4 x y z] '[z (y x)] "Quacky")
  ; Robin R p.101 R = [B B T] = [C C]
  (def-combinator '[R x y z] '[y z x] "Robin")
  ; Robin Once Removed p.104 R* = [C* C*]
  (def-combinator '[R* x y z w] '[x z w y] "Robin once removed")
  ; Robin Twice Removed p.105 R** = [B R*]
  (def-combinator '[R** x y z1 z2 z3] '[x y z2 z3 z1] "Robin twice removed")
  ; Starling  S p.121
  (def-combinator '[S x y z] '[x z (y z)] "Starling")
  ;; S' p.125 S' = [C S]
  (def-combinator '[S' x y z] '[y z (x z)] "S'")
  ; Thrush T p.100 T = [C I]
  (def-combinator '[T x y] '[y x] "Trush")
  ;; Turing Bird U p.132  U = [L (S (C T))] = [L O]
  (def-combinator '[U x y] '[y (x x y)] "Turing Bird")
  ; Vireo aka Pairing p.103 V = [B C T]
  (def-combinator '[V x y z] '[z x y] "Vireo")
  ; Vireo Once Removed p.104 V* = [C* F*]
  (def-combinator '[V* x y z w] '[x w y z] "Vireo once removed")
  ; Vireo Twice Removed p.105 V** = [B V*]
  (def-combinator '[V** x y z1 z2 z3] '[x y z3 z1 z2] "Vireo twice removed")
  ; Warbler W p.99 W = [C (B M R)]
  (def-combinator '[W x y] '[x y y] "Warbler")
  ; Converse Warbler p.119 W' := [C W]
  (def-combinator '[W' x y] '[y x x] "Converse Warbler")
  ; Warbler Once Removed p.120 W* = [B W]
  (def-combinator '[W* x y z] '[x y z z] "Warbler once removed")
  ; Warbler Twice Removed  p.120 W** = [B (B W)]
  (def-combinator '[W** x y z w] '[x y z w w] "Warbler twice removed")
  ; Sage Bird Y := [S L L]
  (def-combinator '[Y x] '[x (Y x)] "Sage Bird")
  )



