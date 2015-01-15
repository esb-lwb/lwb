# lwb Logic WorkBench

The Logic WorkBench (lwb) comprises tools for propositional and
predicate logic. It is written in Clojure.

lwb is a playground, it's work in progress.

## Propositional Logic

### Representation of propositions in Clojure

Atomic propostions, short atoms are represented by Clojure symbols,
e.g., `p` or `q`.

The propositional constants for truth and falsity are represented
by `true` and `false`, respectively.

A proposition is an atom or a constant, or an expression composed
of boolean operators, propositional atoms and constants in the usual
lispy syntax, e.g. `(impl p (impl q p)`.

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



## License

Copyright (C) 2014 by Burkhardt Renz, based on work of Markus Bader and
Daniel Kirsten, Technische Hochschule Mittelhessen (THM).

Distributed under the Eclipse Public License, the same as Clojure.
