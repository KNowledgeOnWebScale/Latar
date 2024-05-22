# Latar

A mini [RDF Surfaces](https://w3c-cg.github.io/rdfsurfaces/) playground in Prolog.

Latar, Indonesian for 'surface', is an attempt to create a new RDF Surfaces implementation in Prolog using Peirce's [Existential Graph](https://en.wikipedia.org/wiki/Existential_graph) methods.

This code is very alpha and experimental.

# Install

Install SWIPL : https://www.swi-prolog.org/download/stable

# Run

`./test.sh`

# Examples

## Alpha

Propositional logic

- [x] alice : a simple conditional `(Alice a Human) -> (Alice a Person)` 
- [x] built-in : a simple built-in test
- [ ] contradiction4: a test to spot contradictions in a formula
- [ ] contradiction5: a test to spot contradictions in a formula 
- [ ] contradiction6: a test to spot contradictions in a formula
- [x] data: a repeated data test
- [x] disjunction : a simple disjunction `(Alice likes Coffee) OR (Alice likes Tea)`
- [ ] double-to-one: a repeated data test 
- [x] lists: a list processing test
- [x] nested_negation : 4 level deep negation eventually is an assertion
- [x] lists : a simple lists test
- [ ] reductio-ad-absurdym: a reduction at absurdum test


## Beta

First-order logic

- socrates : an OWL subClassOf example
- socrates2 : socrates but with two subClassOf statements
- abc : disjunction example
- abcd : disjunction plus triple nested negative surface example

## Notation3 (N3)

The N3 serialization of RDF Surfaces can be used using the `latar` command. We use 
[EYE](https://github.com/eyereasoner/eye) as precompiler from N3 to Prolog, run the Latar code, and translate back from Prolog to N3.

```
$ latar n3/socrates.n3s
<urn:example:test> <urn:example:is> true.
```
