:- module(ontobee,
          [
           typed_in_graph/2,
           typed_in_graph/3,
           in_ontology/2,
           in_ontology/3
           ]).

:- use_module(library(sparqlprog)).
:- use_module(library(semweb/rdf11)).

:- rdf_register_prefix(obmerged,'http://purl.obolibrary.org/obo/merged/').

typed_in_graph(X,G) :-
        typed_in_graph(X,G,_).
typed_in_graph(X,G,T) :-
        rdf(X,rdf:type,T,G).


in_ontology(X,O) :-
        in_ontology(X,O,_).
in_ontology(X,O,T) :-
        typed_in_graph(X,G,T),
        G == uri(concat('http://purl.obolibrary.org/obo/merged/',ucase(O))).


