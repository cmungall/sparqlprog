
:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog)).
:- use_module(library(rdf_owl/owl)).

:- begin_tests(owl_test).


test_select(Q,ExpectedSPARQL) :-
        create_sparql_select(Q,SPARQL,[]),
        format(' ~q ==> ~w~n',[ Q, SPARQL ]),
        assertion( SPARQL = ExpectedSPARQL ).


test(subClassOf) :-
    nl,
    test_select( subClassOf(_,_), _).


:- end_tests(owl_test).


