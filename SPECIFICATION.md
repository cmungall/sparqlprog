# Specification

Sparqlprog is a subset of prolog, roughly equivalent to
[datalog](https://en.wikipedia.org/wiki/Datalog), that can be compiled
to SPARQL. Sparqlprog can also be executed natively by a native logic
programming engine, allowing for delegation of large database queries
to a remote service combined with local programming. See the
[README](./README.md) for more information.

## Queries and Compound Terms

A query is a boolean combination of `compound terms`. A compound term
is composed of a predicate plus zero or more arguments, where each
argument can be a term or a variable.

Predicates may be built-in or defined. The core built-in predicate is
`rdf/3` (the `/3` denotes the predicate has 3 arguments).

The most basic query is a query for all triples:

    rdf(S,P,O)

Variables are denoted by a leading uppercase symbol. In the above
example, all arguments are variables to the query succeeds for all
triples. It is equivalent to the SPARQL query

    SELECT * WHERE {?x ?p ?o}

The following query unifies the variable `Cls` with all subjects of a
triple in which the predicate is `rdf:type` and the value/object is
`owl:Class`. Note that "Class" is in single quotes to avoid being
treated as a variable:

    rdf(Cls,rdf:type,owl:'Class')

## Boolean combinations

Terms can be combined with any combinations of conjunction,
disjunction or negation. These are denoted by the symbols ',', ';' and
'\+' respectively. Formally these are all predicates (conjunctions and
disjunction are binary, negation is unary), but these can be written
using infix notation for syntacic convenience.

The ',/2' predicate denotes conjunction:

`rdf(Cls,rdf:type,owl:'Class'),rdf(Cls,rdfs:subClassOf,Super)`

The ';/2' predicate denotes disjunction:

`rdf(Obj,rdf:type,owl:'ObjectProperty');rdf(Obj,rdf:type,owl:'DataProperty')`

The '\+/1` predicate denotes negation:

...

Parentheses can be used to group conbinations

## Rules

A rule is written `Head :- Body`, where the head of the rule is a single term and the body is any boolean combination

The following rule defines `is_a/2` which is trivially equivalent to an `rdf/3` call with the RDFS subclass predicate:

```
is_a(A,B) :- rdf(A,rdfs:subClassOf,B).
```

Multiple rules are treated disjunctions. E.g. the following definition
of `is_a/2` succeeds when the subject is a subclass of the object, or
an instance.

```
is_a(A,B) :- rdf(A,rdfs:subClassOf,B).
is_a(A,B) :- rdf(A,rdf:type,B).
```

Recursive rules *cannot* be written in sparqlprog. For example, if you write:

```
is_a(A,B) :- rdf(A,rdfs:subClassOf,B).
is_a(A,B) :- is_a(A,Z),is_a(Z,B).
```

This cannot be directly converted to SPARQL. However, in this particular example, a property path can be used - see below.

For examples of rules, see some of the programs in the [ontologies](prolog/sparqlprog/ontologies/) folder

## Builtins

The builtins typically correspond to predicates defined in the
swi-prolog semweb package. Additional definitions can optionally be
imported.

The core two builtins are:

 * `rdf(Subject, Predicate, Object)` (triple queries)
 * `rdf(Subject, Predicate, Object, Graph)` (quad queries)

Additionally:

 * `rdfs_subclass_of/2` - inferred subClassOf
 * `rdfs_individual_of/2` - inferred subClassOf

There are also predicate builtins for a subset of SPARQL functions that return booleans, such as

 * `str_starts/3`
 * `str_ends/3`
 * `between/3`

The complete list is not yet implemented.

Additionally, comparison operators are also supported. We use standard prolog infix operators, e.g.

```
A @<= B
```

for string comparisons

```
A <= B
```

for numeric comparisons

## Functions

In addition to predicates, functions can be used in a query. These can be builtins, but functions can also be defined.

`bind/2` can be used to explicitly set a value. 


 * concat
 * ucase

## Predicate paths


## Aggregate Queries

## Namespaces

## Programs and Modules

## Execution of sparqlprog queries

A query can be executed using the `pl2sparql` command, or from any
language via an API call to a sparqlprog pengine. See the
[README](README.md) for details.

Additionally, queries can be executed from within a prolog program using `??/2`.

For example:

```
TODO
```

## Embedding within logic programs

Any sparqlprog program should be executable directly using a prolog
engine that defines the `rdf/3` predicate, such as swi-prolog.

Additionally, native execution and remote execution can be mixed. 









