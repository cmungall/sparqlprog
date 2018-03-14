:- module(ontobee,
          [
           typed_in_graph/2,
           typed_in_graph/3,
           in_ontology/2,
           in_ontology/3,

           graph_ontology/2,

           ontsearch/4
           ]).

:- use_module(library(sparqlprog)).
:- use_module(library(semweb/rdf11)).

:- rdf_register_prefix(obmerged,'http://purl.obolibrary.org/obo/merged/').

typed_in_graph(X,G) :-
        typed_in_graph(X,G,_).
typed_in_graph(X,G,T) :-
        rdf(X,rdf:type,T,G).


:- srule(in_ontology,[entity:iri, ontology:iri, type:iri],
         'Entity is declared to be of type Type in Ontology').
in_ontology(X,O) :-
        in_ontology(X,O,_).
in_ontology(X,O,T) :-
        typed_in_graph(X,G,T),
        graph_ontology(G,O).

:- srule(graph_ontology,[graph:iri, ontology:iri], 'maps an ontobee graph IRI to an ontology name (e.g. hp, go, uberon, ncit)').
graph_ontology(G,O) :-
        G == uri(concat('http://purl.obolibrary.org/obo/merged/',ucase(O))).    

ontsearch(O,P,C,L) :- graph_ontology(G,O),rdf(C,rdfs:label,L,G),regex(str(L),P).
    
