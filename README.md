# lwb Logic WorkBench

The Logic WorkBench (lwb) comprises tools for propositional and
predicate logic. It is written in Clojure.

lwb is a playground, it's work in progress.

## Propositional Logic

Namespace `lwb.prop` 

### Representation of propositions in Clojure

Atomic propositions, short atoms, are represented by Clojure symbols,
e.g., `p` or `q`.

The propositional constants for truth and falsity are represented
by `true` and `false`, respectively.

A proposition is an atom or a constant, or an expression composed
of boolean operators, propositional atoms and constants in the usual
lispy syntax, e.g., `(impl p (impl q p)`.

### The operators of propositional logic

operator | description | arity
-------- | ----------- | -----
not | negation | unary
and | conjunction | n-ary
or | disjunction | n-ary
nand | negated and | binary
nor | negated or | binary
impl | implication | binary
nimpl | negated implication | binary
cimpl | converse implication | binary
ncimpl | negated converse implication | binary
equiv | equivalence | binary
xor | exclusive or | binary
ite | if-then-else | ternary

### Functions for propositions

#### Well-formed propositions

Is `phi` a well-formed proposition?   
`(wff? phi)` returns true or false   
`(wff? phi :msg)` returns true or a message on the error in `phi`.   

#### Truth table

The truth table of a proposition is represented as a map with the keys:   
`:prop`  the proposition itself   
`:header` a vector of the atoms and the last entry `:result`   
`:table` a vector of vectors of boolean assignments to the corresponding atom   
         in the header as well as the result of the evaluation.   
         
`(truth-table phi)` returns the truth table of proposition `phi` (with <= 10 atoms)   
`(truth-table phi :true-only)` gives only the rows that evaluate to true   
`(truth-table phi :false-only)` gives only the rows that evaluate to false   
`(print-truth-table tt)` prints truth table `tt`  

#### Conjunctive normal form

`(cnf phi)` transforms proposition `phi` to conjunctive normal form   
`(cnf? phi)` checks whether `phi` is cnf

#### Satisfiability 

Namespace `lwb.prop.sat`

`(tseitin phi)` transforms `phi` to a proposition in cnf that is equivalent 
to `phi` with respect to satisfiability
     
An assignment vector is a vector of the atoms of a proposition each one
followed by an assigned truth value, e.g., `[p true q false]`      

`(sat phi)` returns an assignment vector if `phi` is satisfiable, `nil` otherwise   
`(sat phi :all)` returns a sequence of all satisfying valuations   

`(sat? phi)` Is `phi` satisfiable?    
`(valid? phi)` Is `phi` valid?

#### Cardinality constraints 

Namespace `lwb.prop.cardinality`

`(min-kof k coll)` -> a seq of clauses expressing that  at least k of the atoms in coll are true    
`(max-kof k coll)` -> a seq of clauses expressing that  at most k of the atoms in coll are true    
`(kof k coll)` -> a seq of clauses expressing that exactly k of the atoms in coll are true    
`(oneof coll)` -> a seq of clauses expressing that exactly 1 atoms in coll is true


## License

Copyright (C) 2014 by Burkhardt Renz, based on work of Markus Bader and
Daniel Kirsten, Technische Hochschule Mittelhessen (THM).

Distributed under the Eclipse Public License, the same as Clojure.

lwb uses [SAT4J](http://www.sat4j.org), licensed under both the Eclipse Public License and the 
GNU LGPL licence.
