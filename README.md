# lwb Logic WorkBench

The Logic WorkBench (lwb) comprises tools for propositional, 
predicate, and linear temporal logic. It is written in Clojure.

lwb is a playground, it's work in progress.

[Documentation](https://github.com/esb-lwb/lwb/wiki)

### Revisions

- 1.0.1 [2018-01-31] First published version on Clojars
- 1.0.2 [2018-02-16] Improved use of macroexpand-1 

Latest version on Clojars [![Clojars Project](https://img.shields.io/clojars/v/lwb.svg)](https://clojars.org/lwb)

### License

Copyright (C) 2014 - 2018 by Burkhardt Renz, Technische Hochschule Mittelhessen (THM).
Contributors see [documentation](https://github.com/esb-lwb/lwb/wiki).

Distributed under the Eclipse Public License, the same as Clojure.

lwb uses 
- [SAT4J](http://www.sat4j.org), licensed under both the Eclipse Public License and the 
GNU LGPL licence,
- [kodkod](https://github.com/emina/kodkod), licensed under the MIT License,
- [JavaBDD](http://javabdd.sourceforge.net), licensed under the GNU LGPL,
- [LTL2Buchi](https://ti.arc.nasa.gov/profile/dimitra/projects-tools/#LTL2Buchi), licensed 
under the NASA Open Source Agreement (NOSA), version 1.3.

### Dependencies

lwb uses kodkod and LTL2Buchi. Both libraries are not published in a maven 
repository. 
To use these libraires and the function in lwb that depend on them, proceed 
as follows:
1. Download the libraries from the web sites 
[kodkod](https://github.com/emina/kodkod) and 
[LTL2Buchi](https://ti.arc.nasa.gov/profile/dimitra/projects-tools/#LTL2Buchi)
respectively or create the libraries from 
[esb-lwb/kodkod](https://github.com/esb-lwb/kodkod) or
[esb-lwb/ltl2buchi](https://github.com/esb-lwb/ltl2buchi)

2. Put the libraires into your local maven repository with the command 
`lein localrepo install`, using the localrepo plugin for leiningen - see 
`project.clj`.
