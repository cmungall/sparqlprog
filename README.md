# sparqlprog - programming with SPARQL

SPARQL provides a declarative way of querying a triplestore. One of
its limitations is the lack of ability to *compose* queries and reuse
repeated patterns across multiple queries. Sparqlprog is an extension
of SPARQL and a subset of Prolog for relational rule-oriented
programming using SPARQL endpoints.

To illustrate, we'll start with a basic example. If you are familiar
with prolog you can skip ahead.

```
:- rdf_register_prefix(dbont,'http://dbpedia.org/ontology/').

musical_artist(X) :- rdf(X, rdf:type, dbont:'MusicalArtist').

```

TODO

## Prolog programmers guide

This package provides a more natural (from a Prolog point of view) interface
to SPARQL endpoints. There are two layers. The first, lower layer, defines a
DCG for generating SPARQL queries from a structured term. The second provides
a translation from representation that looks more or less like a Prolog goal
built from rdf/3 goals (with conjunction, disjunction etc) to a term in the
term language understood by the SPARQL DCG.

In addition, the library provides a mechanism to register known SPARQL endpoints
so that they can be referred to by a short name, or to enable a query to be
run against all registered endpoints.

The library is based on the idea implemented in Yves Raimond's swic package,
but the code has been completely re-implemented.

## Prerequisites

You just need SWI Prolog with its Semantic Web libraries.

## Simple usage

The (??)/2  and (??)/1 operators have a high precedence so that conjuction and disjunctive
queries can be written to the right of it without parentheses.
==
?- rdf_register_prefix(foaf,'http://xmlns.com/foaf/0.1/')
?- rdf_register_prefix(dbont,'http://dbpedia.org/ontology/')
?- sparql_endpoint( dbp, 'http://dbpedia.org/sparql/').
?- debug(sparkle).  % to show queries

?-	dbp ?? rdf(Class,rdf:type,owl:'Class'), rdf(Instance,rdf:type,Class).
?- dbp ?? rdf(Person,rdf:type,foaf:'Person'), 
          rdf(Person,foaf:Name,Name),
          filter(regex('Colt.*',Name)).
?- dbp ?? rdf(A,rdf:type,dbont:'Photographer'); rdf(A, rdf:type, dbont:'MusicalArtist').
==


## Clause expansion

If the following clause is defined:

==
cls(Class) :-
        rdf(Class,rdf:type,owl:'Class').
==

Then cls/1 can be used in queries, e.g.

==
?-  dbp ?? cls(X).
==

The cls/1 goal will be expanded.

More complex goals can be defined; for example, this queries for existential restrictions:

==
subclass_of(C,D) :- rdf(C,rdfs:subClassOf,Restr).
svf_edge(C,P,D) :-
        subclass_of(C,Restr),
        rdf(Restr,owl:onProperty,P),
        rdf(Restr,owl:someValuesFrom,D).
==

Only a subset of prolog can be expanded in this way. Conjunction,
disjunction (or multiple clauses), negation are supported. Terminals
rdf/3, rdf/4, and some predicates from the rdfs library are
supported. In future a wider set of constructs may be supported,
e.g. setof/3.

It is also possible to use create_sparql_construct/3 and
create_sparl_construct/4 to generate SPARQL queries for a
limited subset of pure prolog that can be executed outside
the prolog environment - effectively a limited prolog to SPARQL
compiler.

## Change log

	0.0.8: concurrent_or/3 moved to separate module, fixes to concurrent_or/3.

	0.0.7: More documentation. Renamed endpoint_query/3 to query_sparql/3.
	Non-autopaged queries now use sparkle:limit setting for default limit to
	avoid getting huge amounts of data when no limit is supplied.

	New in version 0.0.6: both (??)/2 and query_goal/3 can accept a variable
	as an endpoint. If this is the case, then multiple queries are made in parallel.
	Each successful result binds the endpoint variable to the source of that data, eg
	==
	EP ?? rdf(Class,rdf:type,owl:'Class').
	==
	will bind Class to all known classes and EP to the endpoint which reported that class.
	If the same bindings can be returned from several endpoints, then (??)/2 will produce
	those binding multiple times, with EP bound to different values each time, even if the
	query reduces to an ASK SPARQL query. It's up to you to use once/1 etc as necessary
	if this is not the behaviour you want.

	Also new in 0.0.6, autopaged queries, that is, you can forget about OFFSET and LIMIT;
	multiple queries will be made as necessary to fetch data on demand. The default
	batch size is determined by the sparkle:limit setting.



## Running Pengines Service via Docker

```
docker build . -t sprog
docker run --name sprog -d -p 4242:4242   sprog
```
