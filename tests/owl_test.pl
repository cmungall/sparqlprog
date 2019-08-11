
:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog)).
:- use_module(library(sparqlprog/owl_util)).
:- use_module(library(sparqlprog/ontologies/owl)).

:- begin_tests(owl_test).


test_select(Q,ExpectedSPARQL) :-
        create_sparql_select(Q,SPARQL,[]),
        format(' ~q ==> ~w~n',[ Q, SPARQL ]),
        assertion( SPARQL = ExpectedSPARQL ).

test(subClassOf) :-
    test_select( subClassOf(_,_), "SELECT ?v0 ?v1 WHERE {?v0 <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?v1}").

test(subClassOf_some) :-
    test_select( subclass_of_some(_,_,_), "SELECT ?v0 ?v1 ?v2 WHERE {?v0 <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?v3 . ?v3 <http://www.w3.org/2002/07/owl#onProperty> ?v1 . ?v3 <http://www.w3.org/2002/07/owl#someValuesFrom> ?v2}").


:- end_tests(owl_test).


