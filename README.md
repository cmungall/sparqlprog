# sparqlprog - programming with SPARQL

[![Build Status](https://travis-ci.org/cmungall/sparqlprog.svg?branch=master)](https://travis-ci.org/cmungall/sparqlprog)
[![Join the chat at https://gitter.im/sparqlprog/Lobby](https://badges.gitter.im/sparqlprog/Lobby.svg)](https://gitter.im/sparqlprog/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[**pack**](http://www.swi-prolog.org/pack/list?p=sparqlprog)

sparqlprog is a programming language and environment that can be used
to write composable modular building blocks that can be executed as
federated SPARQL queries.

Example of use (command line):

```
pl2sparql  -u sparqlprog/ontologies/ebi -u sparqlprog/ontologies/faldo  -s ebi "\
  protein_coding_gene(G), \
  location(G,L,B,E,grcm38:'11'), \
  B >= 101100523,E =< 101190725, \
  orthologous_to(G,H),in_taxon(H,taxon:'9606')" \
  "h(G,H)"
```

The command passes a *logic program query* to sparqlprog. In this
case, the query is a conjunction of conditions involving different
variables (each indicated with a leading upper-case letter):

 1. `G` is a *protein coding gene*
 2. `G` is located on mouse chromosome 11, with an interval bounded by `B` (begin) and `E` (end)
 3. The interval is within a certain range
 4. `G` is *homologus to* `H`
 5. `H` is a human gene (indicated by taxon ID 9606)
 6. The results are bound to a tuples `h(G,H)` (i.e. two column table)

This logic query compiles down to a SPARQL query for fetching G and
H. The query is then executed on the [EBI RDF
Platform](https://www.ebi.ac.uk/rdf/services/sparql), giving:

|Mouse Gene|Human Gene|
|---|---|
|ensembl:ENSMUSG00000035198|ensembl:ENSG00000131462|
|ensembl:ENSMUSG00000017167|ensembl:ENSG00000108797|
|ensembl:ENSMUSG00000044052|ensembl:ENSG00000184451|
|ensembl:ENSMUSG00000017802|ensembl:ENSG00000141699|
|ensembl:ENSMUSG00000045007|ensembl:ENSG00000037042|
|ensembl:ENSMUSG00000035172|ensembl:ENSG00000068137|

How does this work? The query compilation makes use of pre-defined
n-ary predicates, such as this one defined in the [faldo
module](https://www.swi-prolog.org/pack/file_details/sparqlprog/prolog/sparqlprog/ontologies/faldo.pl):

```
location(F,L,B,E,R) :-
  rdf(F,faldo:location,L),
  begin(L,PB),position(PB,B),reference(PB,R),
  end(L,PE),position(PE,E),reference(PE,R).
```

The `:-` connects a rule head to a rule body. In this case the body is
a conjuncation of goals. Each of these may be defined in their own
rules. Typically everything bottoms out at a call over a 3-ary
predicate `rdf(S,P,O)` which maps to a single triple. In this case the vocabulary used for genomic locations is [faldo](https://github.com/OBF/FALDO).

This approach allows for *composability* of queries. Rather that
repeating the same verbose SPARQL each time in different queries,
reusable modules can be defined.

In addition to providing a composable language that compiles to
SPARQL, this package provides a complete turing-complete environment
for mixing code and queries in a relational/logic programming
paradigm. See below for examples.

## Quick Start (for prolog hackers)

See the [sparqlprog module docs](https://www.swi-prolog.org/pack/file_details/sparqlprog/prolog/sparqlprog.pl)

See also the [specification](SPECIFICATION.md)

## Quick Start (for Python hackers)

See the [sparqlprog-python](https://github.com/cmungall/sparqlprog-python) package

This provides a Python interface to a sparqlprog service

You can also see demonstration notebooks:

 * [Basic SPARQLProg](https://nbviewer.jupyter.org/github/cmungall/sparqlprog-python/blob/master/Notebook_01_Basics.ipynb)
 * [sending programs over the wire](https://nbviewer.jupyter.org/github/cmungall/sparqlprog-python/blob/master/Notebook_02_Programs.ipynb)

## Quick Start (for everyone else)

There are a variety of ways to use this framework:

 * Executing queries on remote services via command line
 * Compiling logic queries to SPARQL queries, for use in another framework
 * Programmatically within a logic program (interleaving remote and local operations)
 * Programmatically from a language like python/javascript, using a __sparqlprog service__

Consult the appropriate section below:

### Running queries from the command line

See the [examples](./examples/) directory for all command line examples

First [install](INSTALL.md), making sure the [bin](bin) directory is
in your path. This will give you access to the the pl2sparql script.

For full options, run:

```
pl2sparql --help
```

Note you should also have a number of convenience scripts in your
path. For example the `pq-wd` script is simply a shortcut for

```
pl2sparql -s wikidata -u sparqlprog/ontologies/wikidata  ARGS
```

This will give you access to a number of convenience predicates such
as positive_therapeutic_predictor/2 (for drug queries). The `-u`
option uses the wikidata module, and the `-s` option sets the service
to the one with handle `dbpedia` (the mapping from a handle to the
full service URL is defined in the wikidata module).

The best way to learn is to look at the [examples/](examples),
together with the corresponding set of rules in
[prolog/sparqlprog/ontologies](prolog/sparqlprog/ontologies).

For example [examples/monarch-examples.sh](examples/monarch-examples.sh) has:

```
pq-mi  'label(D,DN),literal_exact_match(DN,"peroxisome biogenesis disorder"),\
   rdfs_subclass_of(D,C),owl_equivalent_class(C,E),has_phenotype(E,Z)'\
   'x(C,CN,E,Z)'
```

This finds a disease with a given name, finds equivalent classes of
transitive reflexive subclasses, and then finds phenotypes for each

### Compiling logic programs to SPARQL

You can use pl2sparql (see above for installation) to compile a
program with bindings to a SPARQL query by using the `-C` option. The
SPARQL query can then be used without any dependence on
sparqlprog. E.g.

```
pq-ebi -C "\
  protein_coding_gene(G), \
  location(G,L,B,E,grcm38:'11'), \
  B >= 101100523,E =< 101190725, \
  homologous_to(G,H),in_taxon(H,taxon:'9606')" \
  "h(G,H)"
```

will generate the following SPARQL:

```
SELECT ?g ?h WHERE {?g <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://purl.obolibrary.org/obo/SO_0001217> . ?g <http://biohackathon.org/resource/faldo#location> ?l . ?l <http://biohackathon.org/resource/faldo#begin> ?v0 . ?v0 <http://biohackathon.org/resource/faldo#position> ?b . ?v0 <http://biohackathon.org/resource/faldo#reference> <http://rdf.ebi.ac.uk/resource/ensembl/90/mus_musculus/GRCm38/11> . ?l <http://biohackathon.org/resource/faldo#end> ?v1 . ?v1 <http://biohackathon.org/resource/faldo#position> ?e . ?v1 <http://biohackathon.org/resource/faldo#reference> <http://rdf.ebi.ac.uk/resource/ensembl/90/mus_musculus/GRCm38/11> . FILTER (?b >= 101100523) . FILTER (?e <= 101190725) . ?g <http://semanticscience.org/resource/SIO_000558> ?h . ?h <http://purl.obolibrary.org/obo/RO_0002162> <http://identifiers.org/taxonomy/9606>}
```

withOUT executing it remotely

note: indentation and URI shortening are on the cards for future releases.

### Using a public sparqlprog service

Public pengines service: https://evening-falls-87315.herokuapp.com/pengine

[Pengines](http://pengines.swi-prolog.org/) is a framework for running logic program environments as a
web service. They can be used by clients in any language (client
libraries in python, javascript seem to be mature; as well as separate
prolog clients as well).

See the docs on the [pengines framework](http://pengines.swi-prolog.org/).

There is an example of how to contact this service in javascript in
[bin/sprog-client.js](bin/sprog-client.js). You will need to do a `npm
install pengines`, and change the server URL.

Pengines allows the client to send logic programs to the server, and
then to invoke them. For example:

```
pengines = require('pengines');

peng = pengines({
    server: "https://evening-falls-87315.herokuapp.com/pengine",
    ask: "q(X)",
    chunk: 100,
    sourceText: "q(X):- (wd ?? continent(X)).\n"
}
).on('success', handleSuccess).on('error', handleError);
function handleSuccess(result) {
    console.log('# Results: '+ result.data.length);
    for (var i = 0; i < result.data.length; i++) {
        console.log(result.data[i])
    }
    if (result.data.length == 0) {
        console.log("No results!")
    }
}
function handleError(result) {
    console.error(result)
}
```

Note that *any* safe subset of prolog can be passed as a program. In
this case we are passing a small program:

`q(X):- (wd ?? continent(X))`

This trivially defines a unary predicate `q/1`. The argument is bound
to any continent. The `??` is a special infix binary predicate, the
left side is the service name and the right side is the query to be
compiled.

The `ask` portion of the javascript will simply pass the query to the
server.

### Using a local sparqlprog service

You can start a sparqlprog service running locally:

    docker run -p 9083:9083 cmungall/sparqlprog

(requires docker)

This creates a pengines service at http://localhost:9083/pengine

There is an example of how to contact this service in javascript in
[sprog-client.js](bin/sprog-client.js). You will need to do:

    npm install pengines

### SWISH

TODO

### Use within logic programs

For this example, consider writing a music band recommender, based on
similarity of genres. dbpedia has triples linking bands to genres, so
we will use that.

We will write a program
[dbpedia_rules.pl](examples/dbpedia/dbpedia_rules.pl) that contains
definitions of predicates we will use.

First we define a binary predicate that counts the number of bands per genre:

```
genre_num_bands(G,Count) :-
        aggregate_group(count(distinct(B)),[G],(rdf(B,dbont:genre,G),band(B)),Count).
```

you can try this with:

`pq-dbpedia -c examples/dbpedia/dbpedia_rules.pl "genre_num_bands(G,Count)"`

this will give results like:

```
http://dbpedia.org/resource/Independent_music,184
http://dbpedia.org/resource/Funky_Club_Music,1
http://dbpedia.org/resource/Ghettotech,2
http://dbpedia.org/resource/Indian_folk_music,1
http://dbpedia.org/resource/Bakersfield_Sound,1
http://dbpedia.org/resource/Punk_Rawk,1
http://dbpedia.org/resource/Go-go,6
http://dbpedia.org/resource/Jazz_pop,3
http://dbpedia.org/resource/Dubstep,74
http://dbpedia.org/resource/Alt.folk,1
http://dbpedia.org/resource/AfroHouse,1
http://dbpedia.org/resource/Electro-disco,1
http://dbpedia.org/resource/Math_Rock,15
```

we are doing this because we want to weight band similarity according
to how rare a genre is. If two bands share the genre of 'independent
music' it is not remarkable, but if two bands share a rarer genre like
'Ghettotech' then we will weight that higher.

we can explicitly bind this to dbpedia using `??/2`:

```
get_genre_num_bands(G,Count) :-
        ??(dbpedia,genre_num_bands(G,Count)).
```

we can define the Information Content (IC) of a genre `G` as `-log2(Pr(G))`:

```
genre_ic(G,IC) :-
        get_genre_num_bands(G,Count),
        get_num_bands(Total),
        seval(-log(Count/Total)/log(2), IC).
```

This makes use of:

```
:- table get_num_bands/1.
get_num_bands(Count) :-
        ??(dbpedia,num_bands(Count)).
num_bands(Count) :-
        aggregate(count(distinct(B)),band(B),Count).
```

Note we are tabling (memoizing) the call to fetch the total number of
bands. This means it will only be called once per sparqlprog session.

Finally we can define a 3-ary predicate that compares any two bands
and bindings the 3rd arg to a similarity score that is the sum of the
ICs of all genres held in common. (for simplicity, we do not penalize
unmatched genres, or try to use sub/super genre categories yet):

```
pair_genre_ic(A,B,SumIC) :-
        get_all_genres(A,SA),
        get_all_genres(B,SB),
        ord_intersection(SA,SB,I),
        aggregate(sum(IC),G^(member(G,I),genre_ic(G,IC)),SumIC).
```

This is a normal prolog goal and can be executed in a normal prolog context, or from the command line:

`pq-dbpedia -c examples/dbpedia/dbpedia_rules.pl -e  "pair_genre_ic(dbr:'Metallica',dbr:'Megadeth',IC)"`

The `-e` option tells the script to execute the query directly rather
than try and compile everything to a single SPARQL query (this may be
possible, but could be highly inefficient). It is only when the prolog
engine executes the `??` goals that a remote SPARQL will be executed.

If we want to adapt this program to search rather than compare two
given bands, we can modify it slightly so that it does not waste
cycles querying on bands that have no genres in common:

```
pair_genre_ic(A,B,SumIC) :-
        get_all_genres(A,SA),
        ??(dbpedia,has_shared_genre(A,B,_)),
        get_all_genres(B,SB),
        ord_intersection(SA,SB,I),
        aggregate(sum(IC),G^(member(G,I),genre_ic(G,IC)),SumIC).
```

Example of running this:

`pq-dbpedia -c examples/dbpedia/dbpedia_rules.pl -e  "pair_genre_ic(dbr:'Voivod_(band)',B,IC),IC>=10"`

Note this is slow, as it will iterate across each band performing
queries to gather stats. There are various approaches to optimizing
this, but the core idea here is that the logic can be shuffled back
and forth between the portion that is compiled to SPARQL and executed
remotely, and the portion that is executed locally by a logic engine.

### Using a local triplestore

You can use sparqlprog with any local or remote triplestore that
supports the SPARQL protocol. If you have RDF files and want to get
started, here is one quick route (assuming you have docker):

 1. Place your files in [data](examples/data)
 2. Run `make bg-run`

This will run blazegraph within a docker container

## Discussion


SPARQL provides a declarative way of querying a triplestore. One of
its limitations is the lack of ability to *compose* queries and reuse
repeated patterns across multiple queries. Sparqlprog is an extension
of SPARQL and a subset of Prolog for relational rule-oriented
programming using SPARQL endpoints.

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

You just need SWI Prolog with its Semantic Web libraries.

## Simple usage

The `(??)/2`  and `(??)/1` operators have a high precedence so that conjuction and disjunctive
queries can be written to the right of it without parentheses:

```
?- rdf_register_prefix(foaf,'http://xmlns.com/foaf/0.1/')
?- rdf_register_prefix(dbont,'http://dbpedia.org/ontology/')
?- sparql_endpoint( dbp, 'http://dbpedia.org/sparql/').
?- debug(sparkle).  % to show queries

?-	dbp ?? rdf(Class,rdf:type,owl:'Class'), rdf(Instance,rdf:type,Class).
?- dbp ?? rdf(Person,rdf:type,foaf:'Person'), 
          rdf(Person,foaf:Name,Name),
          filter(regex('Colt.*',Name)).
?- dbp ?? rdf(A,rdf:type,dbont:'Photographer'); rdf(A, rdf:type, dbont:'MusicalArtist').
```


## Clause expansion

If the following clause is defined:

```
cls(Class) :-
        rdf(Class,rdf:type,owl:'Class').
```

Then cls/1 can be used in queries, e.g.

```
?-  dbp ?? cls(X).
```

The cls/1 goal will be expanded.

More complex goals can be defined; for example, this queries for existential restrictions:

```
subclass_of(C,D) :- rdf(C,rdfs:subClassOf,Restr).
svf_edge(C,P,D) :-
        subclass_of(C,Restr),
        rdf(Restr,owl:onProperty,P),
        rdf(Restr,owl:someValuesFrom,D).
```

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

## Comparison with SPIN

TODO https://spinrdf.org/

## Credits

The majority of code in this repo was developed by Samer Abdallah, as
part of the [sparkle
package](http://www.swi-prolog.org/pack/list?p=sparkle). Some of this
code came from Yves Raimond's swic package.

Extensions were implemented by Chris Mungall. In particular

 - goal rewriting
 - DCG extensions: aggregates, filter operators
 - predicate definitions for vocabularies used by various triplestores (faldo, ebi, wikidata, dbpedia, go, monarch)
