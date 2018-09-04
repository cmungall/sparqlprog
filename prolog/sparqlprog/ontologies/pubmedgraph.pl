/*

  ideally this is used in combination with ontobee
  
*/

:- module(pubmedgraph,
          [references/2,
           coreference/3,

           subclass_axiom_validation/3]).

:- use_module(library(sparqlprog)).
:- use_module(library(semweb/rdf11)).

:- sparql_endpoint( pmg, 'https://stars-app.renci.org/pubmedgraph/sparql').

:- rdf_register_prefix(dcterm,'http://purl.org/dc/terms/').
:- rdf_register_prefix(pmid,'https://www.ncbi.nlm.nih.gov/pubmed/').

references(P,T) :- rdf(P,dcterm:references,T).
coreference(P1,P2,T) :- references(P1,T),references(P2,T).

subclass_axiom_validation(A,B,P) :-
        owl:subClassOf(A,B),
        pmg ?? (references(P,A),references(P,B)).

